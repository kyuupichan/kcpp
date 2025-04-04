# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#

from array import array
from bisect import bisect_left, bisect_right
from codecs import getincrementalencoder
from dataclasses import dataclass
from enum import IntEnum, auto
from typing import ClassVar
from unicodedata import is_normalized

from .char_ranges import (
    XID_Start_ranges, XID_Continue_ranges, is_printable_ranges,
    width_0_ranges, width_2_ranges,
)
from .nf_tables import canonical_decompositions, combining_class_ranges


__all__ = [
    'CodepointOutputKind', 'Charset', 'SIMPLE_ESCAPES', 'CONTROL_CHARACTER_LETTERS',
    'is_NFC', 'is_control_character', 'is_printable', 'is_XID_Start', 'is_XID_Continue',
    'printable_char', 'utf8_cp', 'REPLACEMENT_CHAR', 'terminal_charwidth', 'is_valid_codepoint',
    'codepoint_to_hex', 'is_surrogate', 'to_NFD', 'to_NFC',
]


def _split_ranges(ranges):
    return tuple(array('Q', bounds) for bounds in zip(*ranges))


XID_Start_ranges = _split_ranges(XID_Start_ranges)
XID_Continue_ranges = _split_ranges(XID_Continue_ranges)
is_printable_ranges = _split_ranges(is_printable_ranges)
width_0_ranges = _split_ranges(width_0_ranges)
width_2_ranges = _split_ranges(width_2_ranges)
combining_class_ranges = _split_ranges(combining_class_ranges)
REPLACEMENT_CHAR = 0xFFFD


def is_valid_codepoint(cp):
    '''Return True if the codepoint is valid.'''
    return 0 <= cp <= 0x10ffff


def codepoint_to_hex(cp):
    return f'U+{cp:04X}'


def is_control_character(cp):
    '''Return True if the codepoint is a control character.'''
    return 0x00 <= cp <= 0x1F or 0x7f <= cp <= 0x9f


def is_surrogate(cp):
    '''Return True if the codepoint is a surrogate.'''
    return 0xd800 <= cp <= 0xdfff


def is_printable(c):
    '''Return True if the character can be displayed on a terminal.

    For now, this is all those unicode characters with category 'L', 'M', 'N', 'P', 'S',
    or 'Zs'.  In particular, all control characters, including \t, are not printable.
    '''
    return _is_in(is_printable_ranges, c)


def is_XID_Start(c):
    '''Return True if the character can be displayed on a terminal.

    For now, this is all those unicode characters with category 'L', 'M', 'N', 'P', 'S',
    or 'Zs'.
    '''
    return _is_in(XID_Start_ranges, c)


def is_XID_Continue(c):
    '''Return True if the character can be displayed on a terminal.

    For now, this is all those unicode characters with category 'L', 'M', 'N', 'P', 'S',
    or 'Zs'.
    '''
    return _is_in(XID_Continue_ranges, c)


def printable_char(cp):
    '''Return the character represented by the codepoint if it is printable, otherwise
    a printable <U+XXXX> string.'''
    if is_printable(cp):
        return chr(cp)

    return codepoint_to_hex(cp)


def terminal_charwidth(cp):
    '''Return the number of columns used displaying the codepoint on a unicode terminal.'''
    if _is_in(width_0_ranges, cp):
        return 0

    if _is_in(width_2_ranges, cp):
        return 2

    return 1


def utf8_cp(raw, offset):
    '''Return (c, size) where c is the code point of the character given by the UTF-8 encoding
    starting at offset and size is the number of bytes consumed.

    If the encoding is invalid return (-1, size).

    If EOF is reached (-1, size) is returned as for any other invlalid sequence.  This is
    the only way that a size of 0 is returned.

    (-2, size) is returned for over-long encodings, and (-3, size) for surrogate encodings.
    '''
    n = 0
    try:
        c = raw[offset]
        size = _UTF8_sequence_len[c]
        if not size:
            return -1, 1

        c &= _UTF8_masks[size - 1]
        for n in range(1, size):
            d = raw[offset + n]
            if not (0x80 <= d < 0xc0):
                return -1, n  # Invalid encoding
            c = (c << 6) + (d & 0x3f)
    except IndexError:
        return -1, n      # EOF mid-sequence

    # Check encoded value for validity
    if c >= 0x110000:
        c = -1            # Invalid encoding - not a codepoint
    elif c < _UTF8_minima[size - 1]:
        c = -2            # Over-long encoding
    elif 0xD800 <= c <= 0xDFFF:
        c = -3            # Surrogate encoding

    return c, size


def is_NFC(text):
    # FIXME: implement independently of Python
    return is_normalized('NFC', text.decode())


def to_NFD(text):
    '''Decompose text to Unicode Normalization Form D.'''
    # FIXME: this is broken
    def combining_class(cp):
        starts, ends, cclasses = combining_class_ranges
        n = bisect_right(starts, cp)
        if n and starts[n - 1] <= cp <= ends[n - 1]:
            return cclasses[n - 1]
        return 0

    def decomposition(cps):
        cd = canonical_decompositions
        result = []
        for cp in cps:
            n = bisect_left(cd, cp, key=lambda x: x[0])
            if n == len(cd) or cp != cd[n][0]:
                result.append(cp)
            else:
                # Recursive
                result.extend(decomposition(dcp for dcp in cd[n][1:]))

        if len(result) > 1:
            prior_cc = combining_class(result[0])
            for n in range(1, len(result)):
                cc = combining_class(result[n])
                if prior_cc > cc > 0:
                    result[n - 1], result[n] = result[n], result[n - 1]
                else:
                    prior_cc = cc

        yield from result

    return ''.join(chr(cp) for cp in decomposition(ord(c) for c in text))


def to_NFC(text):
    '''Compose text to Unicode Normalization Form C.'''
    # FIXME: not implemented
    return text


#
# Internals
#

_UTF8_sequence_len = [1] * 128 + [0] * 64 + [2] * 32 + [3] * 16 + [4] * 8 + [0] * 8
_UTF8_minima = [0x00, 0x80, 0x800, 0x10000]
_UTF8_masks = [0x7f, 0x1f, 0x0f, 0x07]
assert len(_UTF8_sequence_len) == 256


def _is_in(ranges, cp):
    starts, ends = ranges
    n = bisect_right(starts, cp)
    if n:
        return starts[n - 1] <= cp <= ends[n - 1]
    return False


@dataclass(slots=True)
class Charset:
    '''A Charset abstraction for the preprocessor.'''
    name: str
    is_unicode: bool
    replacement_char: int
    encoder: any

    unicode_charsets: ClassVar[set] = {'utf32', 'utf32be', 'utf32le', 'utf16', 'utf16be',
                                       'utf16le', 'utf8', 'cp65001'}

    @classmethod
    def from_name(cls, name):
        '''Construct a Charset object from a charset name.  Raises LookupError if the
        charset name is not recognized.'''
        encoder = getincrementalencoder(name)().encode
        encoder('\0')  # Skip any BOM
        is_unicode = name.replace('_', '').replace('-', '').lower() in cls.unicode_charsets
        replacement_char = REPLACEMENT_CHAR if is_unicode else 63  # '?'
        return cls(name, is_unicode, replacement_char, encoder)

    def encoding_unit_size(self):
        '''Returns the length of encoding units of the character set in bytes.  Each character is
        encoded into one or more units of this size.
        '''
        return len(self.encoder('\0'))


class CodepointOutputKind(IntEnum):
    '''Describes how to output unicode codepoints in human-readable form.'''
    # Unicode characters themselves if printable, otherwise as_ucns.
    character = auto()
    # \uNNNN or \UNNNNNNNN sequences
    ucn = auto()
    # As hex escapes
    hex_escape = auto()

    def codepoint_or_escape(self, cp):
        if is_control_character(cp):
            if (esc := CONTROL_CHARACTER_LETTERS.get(cp)):
                # If possible, control characters get output as simple escapes
                return '\\' + esc
        elif cp <= 0x80:
            return chr(cp)

        kind = self.value
        if kind == self.character:
            if is_printable(cp):
                return chr(cp)
        if kind != self.hex_escape and cp > 0xff:
            if cp <= 0xffff:
                return f'\\u{cp:04X}'
            return f'\\U{cp:08X}'
        return escape_bytes(chr(cp).encode(), True)

    def bytes_to_string_literal(self, raw):
        '''Convert a byte sequence to a valid C or C++ string literal, escaping characters
        appropriately.  raw is a bytes-like object.'''
        result = '"'

        cursor = 0
        limit = len(raw)
        while cursor < limit:
            cp, size = utf8_cp(raw, cursor)
            assert size > 0
            if cp < 0:
                result += escape_bytes(raw[cursor: cursor + size], True)
            else:
                result += self.codepoint_or_escape(cp)
            cursor += size

        result += '"'
        return result


# A map from C escape letters (e.g. 't', 'n') to their unicode codepoints
SIMPLE_ESCAPES = {ord(c): ord(d) for c, d in zip('\\\'?"abfnrtv', '\\\'?"\a\b\f\n\r\t\v')}

# A map from control character codepoints to escape letters (e.g. 9 -> 't')
CONTROL_CHARACTER_LETTERS = {d: chr(c) for c, d in SIMPLE_ESCAPES.items() if d < 32}


def escape_bytes(raw, use_hex_escapes):
    '''Escape a sequence of bytes as a sequence of hexadecimal or octal escapes.'''
    if use_hex_escapes:
        return ''.join(f'\\x{c:02x}' for c in raw)
    return ''.join(oct(c)[2:] for c in raw)
