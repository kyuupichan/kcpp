# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#

from dataclasses import dataclass
from enum import IntEnum
from io import BytesIO
from struct import Struct

__all__ = ['name_to_cp']

# The only valid characters in unicode character names
CHARS = ' -0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'

# Private high surrogates
FIRST_SPECIAL = 0xDB80
LAST_SPECIAL = 0xDBFF
CP_BITS = [-1, 3, 14, 20]

HANGUL_LEADS = ['G', 'GG', 'N', 'D', 'DD', 'R', 'M', 'B', 'BB', 'S', 'SS', '', 'J', 'JJ', 'C',
                'K', 'T', 'P', 'H']
HANGUL_VOWELS = ['A', 'AE', 'YA', 'YAE', 'EO', 'E', 'YEO', 'YE', 'O', 'WA', 'WAE', 'OE', 'YO',
                 'U', 'WEO', 'WE', 'WI', 'YU', 'EU', 'YI', 'I']
HANGUL_TAILS = ['', 'G', 'GG', 'GS', 'N', 'NJ', 'NH', 'D', 'L', 'LG', 'LM', 'LB', 'LS', 'LT', 'LP',
                'LH', 'M', 'B', 'BS', 'S', 'SS', 'NG', 'J', 'C', 'K', 'T', 'P', 'H']


class UCN_RangesKind(IntEnum):
    CP_04X_SUFFIX = 0
    CP_03D_SUFFIX = 1


#
# Packing utils
#
struct_le_H = Struct('<H')
pack_le_uint16 = struct_le_H.pack
unpack_le_uint16 = struct_le_H.unpack
struct_le_I = Struct('<I')
pack_le_uint32 = struct_le_I.pack
unpack_le_uint32 = struct_le_I.unpack
struct_B = Struct('B')
unpack_byte = struct_B.unpack
pack_byte = struct_B.pack


def read_le_uint16(read):
    result, = unpack_le_uint16(read(2))
    return result


def read_le_uint32(read):
    result, = unpack_le_uint32(read(4))
    return result


def read_exactly(read, size):
    result = read(size)
    if len(result) != size:
        raise RuntimeError(f'could not read {size} bytes')
    return result


def read_byte(read):
    return read_exactly(read, 1)[0]


@dataclass
class UCN_Ranges:
    '''Represents a sequence of disjoint UCN ranges.'''
    kind: int
    ranges: list

    def __contains__(self, cp):
        return any(start <= cp <= end for start, end in self.ranges)

    def suffix_to_cp(self, remaining):
        if self.kind == UCN_RangesKind.CP_04X_SUFFIX:
            try:
                cp = int(remaining, 16)
            except ValueError:
                return -1
            # Require upper case hex
            if remaining == f'{cp:04X}' and cp in self:
                return cp
        elif self.kind == UCN_RangesKind.CP_03D_SUFFIX:
            try:
                dec = int(remaining)
                cp = dec - 1 + self.ranges[0][0]
            except ValueError:
                return -1
            if remaining == f'{dec:03d}' and cp in self:
                return cp
        return -1

    @classmethod
    def read(cls, read):
        kind = read_byte(read)
        count = read_byte(read)
        ranges = [(read_le_uint32(read), read_le_uint32(read))
                  for _ in range(count)]
        return cls(kind, ranges)

    @classmethod
    def from_bytes(cls, raw):
        return cls.read(BytesIO(raw).read)

    def to_bytes(self):
        def parts():
            yield pack_byte(self.kind)
            yield pack_byte(len(self.ranges))
            for r in self.ranges:
                yield pack_le_uint32(r[0])
                yield pack_le_uint32(r[1])

        return b''.join(parts())


def encode_string(s):
    # Encode s, composed only of CHARS, packing 3 characters into every 2 bytes
    def pack(s):
        lookup = {c: n for n, c in enumerate(CHARS)}
        size = len(s)
        if size % 3:
            s = s + ' ' * (3 - size % 3)
            size = len(s)
            assert size % 3 == 0
        for n in range(0, size, 3):
            v1, v2, v3 = lookup[s[n]], lookup[s[n + 1]], lookup[s[n + 2]]
            yield bytes(divmod(v1 * 1444 + v2 * 38 + v3, 256))

    return b''.join(pack(s))


def decode_substring(raw, start, size):
    # Round to 3-byte boundaries
    pack_start = start // 3 * 2
    pack_end = (start + size + 2) // 3 * 2

    def unpack(pack_start, pack_end):
        cs = CHARS
        for n in range(pack_start, pack_end, 2):
            v1, v2 = divmod(raw[n] * 256 + raw[n + 1], 1444)
            v2, v3 = divmod(v2, 38)
            yield cs[v1] + cs[v2] + cs[v3]

    s = ''.join(unpack(pack_start, pack_end))
    first = start % 3
    return s[first: first + size]


def extract_bits(raw, start, length):
    '''Assuming a big-endian encoding, return the number represented by the substring beginning
    at bit START of LENGTH in the bitstring RAW.'''
    offset, bit_offset = divmod(start, 8)
    size = (length + start % 8 + 7) // 8
    value = int.from_bytes(raw[offset: offset + size], 'big')
    # Drop unwanted LSBs
    value >>= size * 8 - bit_offset - length
    # Drop unwanted MSBs
    value &= (1 << length) - 1
    return value


def hangul_syllable_to_cp(text):
    def hangul_lead(text):
        best = 11
        for n, lead in enumerate(HANGUL_LEADS):
            if text.startswith(lead) and len(lead) > len(HANGUL_LEADS[best]):
                best = n
        return best

    def hangul_vowel(text):
        best = -1
        for n, vowel in enumerate(HANGUL_VOWELS):
            if text.startswith(vowel) and (best == -1 or len(vowel) > len(HANGUL_VOWELS[best])):
                best = n
        return best

    def hangul_tail(text):
        try:
            return HANGUL_TAILS.index(text)
        except ValueError:
            return -1

    lead = hangul_lead(text)
    text = text[len(HANGUL_LEADS[lead]):]
    vowel = hangul_vowel(text)
    if vowel == -1:
        return -1
    text = text[len(HANGUL_VOWELS[vowel]):]
    tail = hangul_tail(text)
    if tail == -1:
        return -1

    return 0xAC00 + (lead * 21 + vowel) * 28 + tail


class UnicodeCharacterNames:
    '''An object that maps unicode character names to code points.'''

    def __init__(self, radix_tree, fc_offset, prefixes, ucn_ranges):
        self.radix_tree = radix_tree
        self.fc_offset = fc_offset
        self.prefixes = prefixes       # Packed binary form
        self.ucn_ranges = ucn_ranges   # A list

    @classmethod
    def read(cls, read):
        prefixes = read_exactly(read, read_le_uint16(read))
        radix_tree = read_exactly(read, read_le_uint32(read))
        fc_offset = read_le_uint32(read)
        ucn_ranges = [UCN_Ranges.read(read) for _ in range(read_byte(read))]
        return cls(radix_tree, fc_offset, prefixes, ucn_ranges)

    @classmethod
    def from_bytes(cls, raw):
        return cls.read(BytesIO(raw).read)

    def to_bytes(self):
        def parts():
            yield pack_le_uint16(len(self.prefixes))
            yield self.prefixes
            yield pack_le_uint32(len(self.radix_tree))
            yield self.radix_tree
            yield pack_le_uint32(self.fc_offset)
            yield pack_byte(len(self.ucn_ranges))
            for ucn_range in self.ucn_ranges:
                yield ucn_range.to_bytes()

        return b''.join(parts())

    def read_node(self, start):
        # Variable-length node encoding:
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
        radix_tree = self.radix_tree

        # Consume 9 bits
        high9 = extract_bits(radix_tree, start, 9)
        start += 9

        cp_size = high9 >> 7
        is_last_child = bool(high9 & 0x40)
        letter_code = high9 & 0x3f

        # Decode this node's prefix
        if letter_code < 38:
            prefix = CHARS[letter_code]
        else:
            # Consume 16 bits
            prefix_offset = extract_bits(radix_tree, start, 16)
            start += 16
            prefix_len = letter_code - 36
            prefix = decode_substring(self.prefixes, prefix_offset, prefix_len)

        if cp_size:
            bits = 1 + CP_BITS[cp_size]
            # Consume bits bits
            cp = extract_bits(radix_tree, start, bits)
            start += bits
            has_children = bool(cp & 1)
            cp >>= 1
        else:
            has_children = True
            cp = -1

        if has_children:
            # Consume 21 bits
            children_start = extract_bits(radix_tree, start, 21)
            start += 21
        else:
            children_start = None

        return prefix, cp, children_start, (None if is_last_child else start)

    def special_encoding(self, kind, remaining):
        if kind == FIRST_SPECIAL:
            return hangul_syllable_to_cp(remaining)
        range_num = kind - FIRST_SPECIAL - 1
        if range_num < len(self.ucn_ranges):
            return self.ucn_ranges[range_num].suffix_to_cp(remaining)
        return -1

    def lookup(self, name):
        '''Lookup a character name in the radix tree.  The name must match exactly.

        Return the codepoint of the name, if present, otherwise -1.
        '''
        def scan_children(start, remaining):
            prior = 0
            while True:
                prefix, cp, children_start, start = self.read_node(start)
                if cp != -1:
                    cp += prior
                    prior = cp
                if remaining.startswith(prefix):
                    break
                # Prefix doesn't match.  Try the next sibling, or fail if there is none.
                if start is None:
                    return -1

            remaining = remaining[len(prefix):]
            if FIRST_SPECIAL <= cp <= LAST_SPECIAL:
                return self.special_encoding(cp, remaining)
            if not remaining:
                return cp
            if children_start is None:
                return -1
            # Some of the name remains and this node has children so descend the tree
            return scan_children(children_start, remaining)

        return scan_children(self.fc_offset, name)


_cnames_db = None


def name_to_cp(name):
    '''Return the Unicode code point corresponding to the given name.

    As required by C++23, aliases of category "control", "correction" and "alternate" are
    recognised.
    '''
    global _cnames_db
    if not _cnames_db:
        from .cp_name_db import cp_name_db
        _cnames_db = UnicodeCharacterNames.from_bytes(cp_name_db)
    return _cnames_db.lookup(name)
