# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#

import argparse
import os
import re

from bisect import bisect_left
from collections import defaultdict
from dataclasses import dataclass
from os.path import commonprefix

from unicode.name_to_cp import (
    UnicodeCharacterNames, encode_string, FIRST_SPECIAL, LAST_SPECIAL, CHARS,
    CP_BITS, UCN_Ranges, UCN_RangesKind
)


RANGE_PREFIXES = {
    'CJK Ideograph': 'CJK UNIFIED IDEOGRAPH-',
    'Tangut Ideograph': 'TANGUT IDEOGRAPH-',
}
DEC_03_REGEX = re.compile('-[0-9]{3}')


def chunks(items, size):
    '''Break up items, an iterable, into chunks of length size.'''
    assert size > 0
    for i in range(0, len(items), size):
        yield items[i: i + size]


def is_comment(line):
    line = line.strip()
    return not line or line.startswith('#')


def cps_to_ranges(values):
    values = sorted(values)
    values.append(-1)

    result = []
    cursor = 0
    while True:
        start = values[cursor]
        if start == -1:
            break
        for size in range(1, len(values) - cursor):
            if values[cursor + size] != start + size:
                result.append((start, start + size - 1))
                cursor += size
                break
    return result


def add_range(cps, r):
    if isinstance(r, int):
        cps.add(r)
    else:
        cps.update(range(r[0], r[1] + 1))


class UnicodeDirectory:

    def __init__(self, ucd_dir):
        self.ucd_dir = ucd_dir
        self.na_lines = []

    def unicode_version(self):
        # returns e.g. "16.0.0"
        line = self.name_aliases_lines()[0]
        return re.search(r'-(.*).txt', line).groups()[0]

    def pertinent_lines(self, filename):
        return pertinent_lines(os.path.join(self.ucd_dir, filename), True)

    def unicode_data_lines(self):
        range_start = None
        for line in self.pertinent_lines('UnicodeData.txt'):
            name = line[1]
            if name[0] == '<':
                if name.endswith(', First>'):
                    line[1] = name[1:-8]
                    range_start = line
                    continue
                elif name.endswith(', Last>'):
                    assert range_start[1] == name[1:-7]
                    assert range_start[2:] == line[2:]
                    line[0] = [range_start[0], line[0]]
                    line[1] = range_start[1]
                    range_start = None
            assert range_start is None
            yield line

    def ucd_categories(self, categories):
        cps = set()
        for line in self.unicode_data_lines():
            if line[2] in categories or line[2][0] in categories:
                add_range(cps, line[0])
        return cps

    def cp_names_and_ranges(self):
        names = {}
        ucn_ranges = {}

        hex_04_prefixes = defaultdict(list)
        dec_03_prefixes = {'TANGUT COMPONENT-': []}
        range_lines = []
        skip_blocks = ('Surrogate', '<Hangul Syllable', 'Private Use')

        for line in self.unicode_data_lines():
            name = line[1]
            if name == '<control>':
                continue
            cp = line[0]
            if isinstance(cp, int):
                cp_hex = f'{cp:04X}'
                if name.endswith(cp_hex):
                    hex_04_prefixes[name[:-len(cp_hex)]].append(cp)
                elif any(name.startswith(p) for p in dec_03_prefixes):
                    dec_03_prefixes[name[:-3]].append(cp)
                else:
                    names[name] = cp
            elif all(word not in name for word in skip_blocks):
                range_lines.append(line)

        # Make UCN ranges for all remaining angled ranges, and all hex_04 prefixes
        for line in range_lines:
            range_name = line[1]
            for range_prefix, name_prefix in RANGE_PREFIXES.items():
                if range_name.startswith(range_prefix):
                    if name_prefix not in ucn_ranges:
                        ucn_ranges[name_prefix] = UCN_Ranges(UCN_RangesKind.CP_04X_SUFFIX, [])
                    ucn_ranges[name_prefix].ranges.append(line[0])

        for prefix, cp_values in hex_04_prefixes.items():
            ucn_ranges[prefix] = UCN_Ranges(UCN_RangesKind.CP_04X_SUFFIX,
                                            cps_to_ranges(cp_values))

        for prefix, cp_values in dec_03_prefixes.items():
            ranges = cps_to_ranges(cp_values)
            assert len(ranges) == 1
            ucn_ranges[prefix] = UCN_Ranges(UCN_RangesKind.CP_03D_SUFFIX, ranges)

        # for prefix, ranges in ucn_ranges.items():
        #     runs = ' '.join(f'[{start:04X}, {end:04X}]' for (start, end) in ranges.ranges)
        #     print(f'{prefix} {runs}')

        return names, ucn_ranges

    def name_aliases_lines(self):
        if not self.na_lines:
            with open(os.path.join(self.ucd_dir, 'NameAliases.txt'), 'r') as f:
                self.na_lines = f.readlines()

        return self.na_lines

    def name_aliases(self, kinds):
        '''Read all name aliases of the given kinds (e.g. 'control').

        Returns an alias->codepoint dictionary.'''
        result = {}
        for line in self.name_aliases_lines():
            if not is_comment(line):
                cp, name, kind = line.strip().split(';')
                if kind in kinds:
                    result[name] = int(cp, 16)

        return result

    def east_asian_width_categories(self, categories):
        cps = set()
        for line in self.pertinent_lines('EastAsianWidth.txt'):
            if line[1] in categories:
                add_range(cps, line[0])

        return cps

    def property_cps(self, prop):
        cps = set()
        for line in self.pertinent_lines('DerivedCoreProperties.txt'):
            if len(line) != 2:
                # Don't implement property-value pairs
                continue
            if line[1] == prop:
                add_range(cps, line[0])
        return cps

    #
    # This following description is Markus Kuhn's, taken from
    # https://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c:
    #
    # ------
    #
    # [...] define the column width of an ISO 10646 character as follows:
    #
    #    - The null character (U+0000) has a column width of 0.
    #
    #    - Other C0/C1 control characters and DEL will lead to a return
    #      value of -1.
    #
    #    - Non-spacing and enclosing combining characters (general
    #      category code Mn or Me in the Unicode database) have a
    #      column width of 0.
    #
    #    - SOFT HYPHEN (U+00AD) has a column width of 1.
    #
    #    - Other format characters (general category code Cf in the Unicode
    #      database) and ZERO WIDTH SPACE (U+200B) have a column width of 0.
    #
    #    - Hangul Jamo medial vowels and final consonants (U+1160-U+11FF)
    #      have a column width of 0.
    #
    #    - Spacing characters in the East Asian Wide (W) or East Asian
    #      Full-width (F) category as defined in Unicode Technical
    #      Report #11 have a column width of 2.
    #
    #    - All remaining characters (including all printable
    #      ISO 8859-1 and WGL4 characters, Unicode control characters,
    #      etc.) have a column width of 1.
    #
    # ------
    #
    # For our purposes the control characters (except U+0009 CHARACTER TABULATION) are all
    # unprintable so are replaced and never queried; the tab is replaced by a variable
    # number of spaces.  For definiteness these will all be treated as being of length
    # zero.  If for some reason the -1 distinction is needed it is easy to add.
    #
    # I build two character ranges - those will length zero, and those with length 2.
    # What remains has length 1.
    def width_0_cps(self):
        cps = set()
        # Add control characteres
        cps.update(range(0x00, 0x1f + 1))
        cps.update(range(0x7f, 0x9f + 1))
        # Add non-spacing and enclosing combining characters, and other format characters.
        cps.update(self.ucd_categories(('Mn', 'Me', 'Cf')))
        # Add ZERO WIDTH SPACE
        cps.add(0x200B)
        # Add Hangul Jamo medial vowels and final consonants
        cps.update(range(0x1160, 0x11ff + 1))
        # Remove SOFT HYPHEN
        cps.remove(0x00AD)
        return cps

    def width_2_cps(self):
        # Add East Asian Wide and East Asian Full Width
        return self.east_asian_width_categories(('F', 'W'))

    def printable_cps(self):
        return self.ucd_categories(['L', 'M', 'N', 'P', 'S', 'Zs'])

    def composition_exclusion_cps(self):
        cps = set()
        for line in self.pertinent_lines('CompositionExclusions.txt'):
            cps.add(line[0])
        return cps

    def print_canonical_decompositions(self, var_name):
        decompositions = []
        for line in self.unicode_data_lines():
            if line[5] and not line[5].startswith('<'):
                line = [line[0]] + list(int(cp, 16) for cp in line[5].split())
                decompositions.append(line)

        print(f'{var_name} = (')
        for chunk in chunks(decompositions, 3):
            print('    ' + ', '.join('(' + ', '.join(f'0x{elt:04x}' for elt in decomp) + ')'
                                     for decomp in chunk) + ',')
        print(')')

    def print_combining_classes(self, var_name):
        classes = defaultdict(set)
        for line in self.unicode_data_lines():
            if line[3] != '0':
                classes[line[3]].add(line[0])

        for cls, cps in classes.items():
            classes[cls] = cps_to_ranges(cps)

        ranges = sorted((start, end, cls) for cls, ranges in classes.items()
                        for start, end in ranges)

        print(f'{var_name} = (')
        for ranges_chunk in chunks(ranges, 3):
            print('    ' + ', '.join(f'(0x{start:04x}, 0x{end:04x}, {cls})'
                                     for start, end, cls in ranges_chunk) + ',')
        print(')')

    def print_header(self, comment_lines=[]):
        print('#')
        print(f'# Automatically generated from Unicode {self.unicode_version()} data files.')
        print('# Do not edit!')
        if comment_lines:
            print('#')
            for line in comment_lines:
                print(f'# {line}')
        print('#\n\n')

    def cp_range_lines(self, var_name, cps):
        ranges = cps_to_ranges(cps)
        yield f'{var_name}_ranges = ('
        for ranges_chunk in chunks(ranges, 4):
            yield ('    ' + ', '.join(f'(0x{r[0]:04x}, 0x{r[1]:04x})'
                                      for r in ranges_chunk) + ',')
        yield ')'

    def print_ranges(self, var_name, cps, *, new_line=True):
        for line in self.cp_range_lines(var_name, cps):
            print(line)
        if new_line:
            print()

    def print_normalization_form_tables(self):
        self.print_header(['Tables to handle Unicode Normalization Forms'])
        self.print_combining_classes('combining_class_ranges')
        print('\n')
        self.print_canonical_decompositions('canonical_decompositions')
        print('\n')
        self.print_ranges('composition_exclusion', self.composition_exclusion_cps(),
                          new_line=False)

    def print_all_ranges(self):
        self.print_header()
        for prop in ['XID_Start', 'XID_Continue']:
            self.print_ranges(prop, self.property_cps(prop))
        self.print_ranges('is_printable', self.printable_cps())
        self.print_ranges('width_0', self.width_0_cps())
        self.print_ranges('width_2', self.width_2_cps())

    def print_binary_string(self, comment_lines, var_name, raw):
        self.print_header(comment_lines)
        print(f'{var_name} = (')
        for raw_part in chunks(raw, 20):
            print("  b'" + ''.join(f'\\x{c:02x}' for c in raw_part) + "'")
        print(')')


@dataclass(slots=True)
class Node:
    prefix: str
    cp: int
    children: object


@dataclass(slots=True)
class NodeStats:
    node_count: int
    children_count: int
    empty_count: int
    prefixes: object


def encode_strings(strings):
    orig_strings = strings

    # total = sum(len(s) for s in strings)
    # print(f'{len(strings):,d} strings of total length {total:,d}', file=stderr)

    # Remove all strings contained in others as they are guaranteed to be found
    strings = set(strings)
    ordered = sorted(strings, key=lambda s: len(s))
    for n, s in enumerate(ordered):
        if any(s in t for t in ordered[n + 1:]):
            strings.remove(s)
    del ordered
    # total = sum(len(s) for s in strings)
    # print(f'Substrings removed; {len(strings):,d} strings remain of total length {total:,d}',
    #       file=stderr)

    def all_prefixes(s):
        return (s[:n] for n in range(1, len(s) + 1))

    def all_suffixes(s):
        return (s[n:] for n in range(1, len(s)))

    # Maintain a dictionary of all string prefixes, mapping the prefix to the strings that
    # begin with it
    prefixes = defaultdict(set)
    for s in strings:
        for p in all_prefixes(s):
            prefixes[p].add(s)
    # Sort so that remove_unuseable_prefixes can quickly get the longest prefix
    prefixes = {prefix: prefixes[prefix]
                for prefix in sorted(prefixes, key=lambda s: len(s), reverse=True)}

    def remove_unuseable_prefixes(strings, prefixes):
        suffixes = defaultdict(set)
        for s in strings:
            for suffix in all_suffixes(s):
                if not s.startswith(suffix):
                    suffixes[suffix].add(s)
        # Remove unuseable prefixes, retaining ordering by length
        for prefix in set(prefixes).difference(suffixes):
            del prefixes[prefix]
        # Return an arbitrary string providing the longest prefix
        for longest in prefixes:
            return list(suffixes[longest])[0], longest
        else:
            return '', ''

    def best_user(users):
        def score(s):
            for cursor in range(1, len(s)):
                prefix = s[cursor:]
                if prefix in prefixes:
                    return len(prefix)
            return 0

        scores = {score(s): s for s in users}
        return scores[max(scores)]

    def remove_string(s):
        strings.remove(s)
        for p in set(all_prefixes(s)).intersection(prefixes):
            prefixes[p].remove(s)
            if not prefixes[p]:
                del prefixes[p]

    # Find longest overlap given prior string
    def chains(strings, prefixes):
        def build_chain(strings, prefixes):
            s, prefix = remove_unuseable_prefixes(strings, prefixes)
            if not prefix:
                return
            # user = best_user(prefixs[prefix])
            # print(f'Beginning a new chain with <{s}> for the benefit of <{user}>')
            chain = s
            cursor = 0
            while True:
                remove_string(s)
                # Increment the cursor until a prefix is found
                for cursor in range(cursor + 1, len(chain)):
                    prefix = chain[cursor:]
                    if prefix in prefixes:
                        s = best_user(prefixes[prefix])
                        chain += s[len(prefix):]
                        break
                else:
                    return chain

        while True:
            chain = build_chain(strings, prefixes)
            if not chain:
                break
            # print(f'built chain of length {len(chain)} bytes')
            yield chain

    result = ''.join(chains(strings, prefixes))
    residual = ''.join(strings)
    result += residual
    # print(f'Adding residual {len(strings):,d} strings of size {len(residual):,d} bytes',
    #       file=stderr)

    string_offsets = {string: result.find(string) for string in orig_strings}
    return result, string_offsets


class BitStream:

    def __init__(self):
        self.raw = bytearray()
        self.cursor = 0

    def write(self, value, bit_len):
        raw = self.raw
        assert value.bit_length() <= bit_len
        new_size = (self.cursor + bit_len + 7) // 8
        raw += bytes(new_size - len(raw))
        # Shift value into position
        value <<= (new_size * 8 - self.cursor - bit_len)
        # Convert value to big-endian bytes
        vbytes = value.to_bytes((value.bit_length() + 7) // 8, 'big')
        for pos in range(-1, -len(vbytes) - 1, -1):
            raw[pos] += vbytes[pos]
        self.cursor += bit_len


class RadixTree:

    def __init__(self):
        self.root = Node('', -1, [])

    def add_names(self, names):
        for name, cp in names.items():
            self.add_name(name, cp)

    def add_name(self, name, cp):
        # Walk the tree
        node = self.root
        remaining = name
        while True:
            assert remaining
            for child_num, child in enumerate(node.children):
                # Find common prefix
                prefix = commonprefix((remaining, child.prefix))
                if prefix:
                    break
                # Continue and test the next child
            else:
                # No child had a common prefix.  Add a new child node.
                node.children.append(Node(remaining, cp, []))
                return

            # Recurse down the radix tree with the remaining name, if the child's prefix
            # is a full match
            n = len(prefix)
            if prefix == child.prefix:
                if prefix == remaining:
                    if child.cp != -1:
                        raise ValueError(f'{name} already in tree')
                    child.cp = cp
                    return
                # Descend / recurse down the nodes
                remaining = remaining[n:]
                node = child
                continue

            # The common prefix is shorter than the child's prefix.  Create a new node
            # with one or two children, one being the old node child with the common
            # prefix removed.
            child.prefix = child.prefix[n:]
            # One child is the prefix-reduced old node
            replacement_node = Node(prefix, -1, [child])
            # If the text being added is the common prefix, set the new node's value,
            # otherwise add another child
            if remaining == prefix:
                replacement_node.cp = cp
            else:
                replacement_node.children.append(Node(remaining[n:], cp, []))
            node.children[child_num] = replacement_node
            return

    def to_names(self):
        def recurse(names, node, prefix):
            if node.cp != -1:
                names[prefix] = node.cp
                for child in node.children:
                    recurse(names, child, prefix + child.prefix)

        names = {}
        recurse(names, self.root, '')
        return names

    def lookup_node(self, name):
        def recurse(node, suffix):
            if suffix == '':
                print(f'{name} found')
                return node
            print(f'{name} remaining <{suffix}>')
            for n, child in enumerate(node.children, start=1):
                print(f'child {n}/{len(node.children)} has prefix {child.prefix}')
                if suffix.startswith(child.prefix):
                    return recurse(child, suffix[len(child.prefix):])
            print(f'{name} not found')
            return None

        return recurse(self.root, name)

    def unique_strings(self, max_len=27):
        def break_up_long_strings(max_len):
            def recurse(node, max_len):
                if len(node.prefix) > max_len:
                    node.prefix, residual = node.prefix[:max_len], node.prefix[max_len:]
                    new_node = Node(residual, node.cp, node.children)
                    node.cp = -1
                    node.children = [new_node]
                for child in node.children:
                    recurse(child, max_len)

            recurse(self.root, max_len)

        def recurse_stats(stats, node):
            stats.node_count += 1
            stats.children_count += bool(node.children)
            if node.cp == -1:
                stats.empty_count += 1
            stats.prefixes.add(node.prefix)
            for child in node.children:
                recurse_stats(stats, child)

        break_up_long_strings(max_len)
        stats = NodeStats(0, 0, 0, set())
        recurse_stats(stats, self.root)

        # print(f'{stats.node_count:,d} nodes (of which {stats.children_count:,d} have children '
        #       f'and {stats.empty_count:,d} have no value)', file=stderr)
        # print(f'{len(stats.prefixes)} prefixes', file=stderr)

        return stats.prefixes

    def encode(self, string_offsets):
        # Node encoding:
        #   2 bits: cp_size
        #   1 bit: is_last_child
        #   6 bits value N:
        #      0-37: the string is the letter CHARS[N]
        #      38-63: the length of the string is N - 36 (2 ... 27)
        #   If not single letter:
        #      16 bits: the offset of the string
        #   If cp_size:
        #      bits_for_cp_size: the value
        #       1 bit: has children
        #   else:
        #      has_children = True
        #   If has children:
        #      21 bits: the bit offset
        def recurse_encode(parent):
            parent.children.sort(key=lambda node: node.cp)
            fc_cursors = [recurse_encode(node) for node in parent.children]

            fc_cursor = stream.cursor
            prior_cp = 0
            for node, node_fc_cursor in zip(parent.children, fc_cursors):
                bit_len = 9
                if len(node.prefix) <= 1:
                    value = CHARS.index(node.prefix[0])
                else:
                    value = 36 + len(node.prefix)
                    assert value < 64
                if node.cp != -1:
                    bits = (node.cp - prior_cp).bit_length()
                    cp_size = bisect_left(CP_BITS, bits)
                    assert 1 <= cp_size <= 3
                    bits = CP_BITS[cp_size]
                    value += (cp_size << 7)
                if node is parent.children[-1]:
                    value += 0x40
                if len(node.prefix) > 1:
                    offset = string_offsets[node.prefix]
                    value = (value << 16) + offset
                    bit_len += 16
                if node.cp != -1:
                    value = (((value << bits) + (node.cp - prior_cp)) << 1) + bool(node.children)
                    bit_len += bits + 1
                    prior_cp = node.cp
                if node.children:
                    value = (value << 21) + node_fc_cursor
                    bit_len += 21

                stream.write(value, bit_len)

            return fc_cursor

        stream = BitStream()
        fc_offset = recurse_encode(self.root)
        # print(f'Encoded trie has size {len(stream.raw):,d} bytes')
        return bytes(stream.raw), fc_offset


def pertinent_lines(filename, is_ucd):
    with open(filename, 'r') as f:
        for line in f.readlines():
            c = line.find('#')
            if c != -1:
                line = line[:c]
            line = line.strip()
            if line:
                parts = [part.strip() for part in line.split(';')]
                if is_ucd:
                    parts[0] = [int(cp, 16) for cp in parts[0].split('..')]
                    if len(parts[0]) == 1:
                        parts[0] = parts[0][0]
                yield parts


def print_names_db(ucd):
    names, prefixed_ucn_ranges = ucd.cp_names_and_ranges()
    aliases = ucd.name_aliases({'control', 'alternate', 'correction'})

    # Build a radix tree from the names and aliases
    tree = RadixTree()
    tree.add_names(names)
    tree.add_names(aliases)

    # Add specials
    assert len(prefixed_ucn_ranges) < LAST_SPECIAL - FIRST_SPECIAL
    tree.add_name('HANGUL SYLLABLE ', FIRST_SPECIAL)
    for n, (prefix, ucn_ranges) in enumerate(prefixed_ucn_ranges.items(), start=1):
        tree.add_name(prefix, FIRST_SPECIAL + n)

    # Process the tree
    prefixes_long, string_offsets = encode_strings(tree.unique_strings())
    binary_radix_tree, fc_offset = tree.encode(string_offsets)

    # One long string
    packed_prefixes = encode_string(prefixes_long)
    ucn = UnicodeCharacterNames(binary_radix_tree, fc_offset, packed_prefixes,
                                list(prefixed_ucn_ranges.values()))

    raw_db = ucn.to_bytes()
    ucn = UnicodeCharacterNames.from_bytes(raw_db)

    # Run tests
    tests = 0
    for line in pertinent_lines('unicode/tests/name_to_cp.txt', False):
        cp_hex, name = line[0].split(':')
        our_cp = ucn.lookup(name)
        if cp_hex:
            cp = int(cp_hex, 16)
            assert cp == our_cp
        else:
            assert our_cp == -1
        tests += 1

    # filename = os.path.join('unicode', f'name_to_cp-{ucd.unicode_version()}.db')
    # with open(filename, 'wb') as f:
    #     f.write(raw_db)
    # print(f'Wrote database to {filename}')

    avg_entry_size = len(raw_db) / (len(names) + len(aliases))
    version = ucd.unicode_version()
    comment_lines = [
        f'The Unicode {version} character names stored as a highly-compressed radix tree and',
        'prefix dictionary.',
        f'It stores {len(names):,d} character names and {len(aliases):,d} '
        f'aliases in {len(raw_db):,d} bytes for an average',
        f'of {avg_entry_size:.2f} bytes per entry.',
        f'The prefix dictionary takes {len(packed_prefixes):,d} bytes '
        f'and the radix tree {len(binary_radix_tree):,d} bytes.',
        'Lookups are performed rapidly on the compressed binary image.'
    ]
    ucd.print_binary_string(comment_lines, 'cp_name_db', raw_db)


def main():
    parser = argparse.ArgumentParser(
        'unidump',
        description='Unicode datafile processor'
    )

    parser.add_argument('ucd-dir', help='UCD directory')
    parser.add_argument('command', help='command', choices=['ranges', 'names_db', 'nft'])
    args = vars(parser.parse_args())

    ucd = UnicodeDirectory(args['ucd-dir'])
    command = args['command']
    if command == 'ranges':
        ucd.print_all_ranges()
    elif command == 'names_db':
        print_names_db(ucd)
    elif command == 'nft':   # Normalization tables
        ucd.print_normalization_form_tables()


if __name__ == '__main__':
    main()
