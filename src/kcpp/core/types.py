# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Basic types.  Should have no dependencies on external modules.'''

from bisect import bisect_left
from codecs import getincrementalencoder
from dataclasses import dataclass
from enum import IntEnum, auto
from typing import ClassVar

from ..unicode import is_control_character, is_printable, utf8_cp, REPLACEMENT_CHAR


__all__ = [
    'Buffer', 'BufferPosition', 'PresumedLocation', 'IntegerKind', 'RealKind',
    'CodepointOutputKind', 'Charset', 'SIMPLE_ESCAPES', 'CONTROL_CHARACTER_LETTERS',
]


def line_offsets_gen(raw):
    for n, c in enumerate(raw):
        if c == 10:  # '\n'
            yield n + 1
        elif c == 13:  # '\r'
            if n + 1 == len(raw) or raw[n + 1] != 10:
                yield n + 1


def sparse_line_offsets(raw, min_step):
    '''Return a sparse sorted list of (line_offset, line_number) pairs.  The list always
    starts with (0,1).
    '''
    result = [(0, 1)]

    beyond = min_step
    line_number = 1
    for line_number, offset in enumerate(line_offsets_gen(raw), start=2):
        if offset >= beyond:
            result.append((offset, line_number))
            beyond = offset + min_step

    return result


class Buffer:
    '''Represents a file being preprocessed.'''

    def __init__(self, text, *, sparsity=1_000):
        self.text = text
        # A sparse list of (offset, line_number) pairs to save memory
        self._sparse_line_offsets = None
        self.sparsity = sparsity

    def sparse_line_offsets(self):
        '''Calculate the sparse line offsets on demand, and cache them.'''
        if self._sparse_line_offsets is None:
            self._sparse_line_offsets = sparse_line_offsets(self.text, self.sparsity)
        return self._sparse_line_offsets

    def offset_to_line_info(self, offset):
        '''Convert an offset in the buffer to a (line_offset, line_number) pair, where line_offset
        is the start of the line.  The offset can range up to and including the buffer size.

        Line numbers are 1-based.
        '''
        if not 0 <= offset <= len(self.text):
            raise ValueError(f'offset {offset} out of range; max is {len(self.text)}')

        # Fix for wanting the position of '\n' in an '\r\n' sequence, as the '\r' would be
        # seen as ending and give a line number 1 too large.
        if self.text[offset - 1: offset + 1] == b'\r\n':
            offset -= 1

        line_offsets = self.sparse_line_offsets()

        pos = bisect_left(line_offsets, offset + 1, key=lambda pair: pair[0])
        line_offset, line_number = line_offsets[pos - 1]

        chunk = memoryview(self.text[line_offset: offset])
        chunk_offset = 0
        for chunk_offset in line_offsets_gen(chunk):
            line_number += 1

        return (line_offset + chunk_offset, line_number)

    def line_number_to_offset(self, line_number):
        '''Convert a line number to an offset in the buffer.'''
        if line_number < 1:
            raise ValueError('line number must be positive')
        line_offsets = self.sparse_line_offsets()
        pos = bisect_left(line_offsets, line_number + 1, key=lambda pair: pair[1])
        offset, src_line_number = line_offsets[pos - 1]

        if line_number == src_line_number:
            return offset

        chunk = memoryview(self.text[offset:])
        for chunk_offset in line_offsets_gen(chunk):
            src_line_number += 1
            if src_line_number == line_number:
                return offset + chunk_offset

        raise ValueError(f'buffer does not have {line_number} lines')

    def line_bytes(self, line_number):
        '''Returns a memoryview of the bytes of a raw line in the source; it does not include
        the newline, if any, at the end of the line.
        '''
        start = self.line_number_to_offset(line_number)
        text = self.text

        chunk = memoryview(text[start:])
        end = len(self.text)
        for offset in line_offsets_gen(chunk):
            end = start + offset
            break

        while end > start and text[end - 1] in (10, 13):
            end -= 1

        return memoryview(text[start:end])


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


class BufferPosition(IntEnum):
    '''Describes a position within a buffer.'''
    WITHIN_LINE = auto()
    END_OF_LINE = auto()
    END_OF_SOURCE = auto()


@dataclass(slots=True)
class PresumedLocation:
    '''The physical and presumed location in a buffer.'''
    # The buffer
    buffer: Buffer
    # The filname, a string literal, potentially modified by #line
    presumed_filename: str
    # The presumed line number, potentially modified by #line.  1-based, but line numbers
    # of zero can happen because we accept, with a diagnostic, '#line 0'.
    presumed_line_number: int
    # The physical line number, 1-based
    line_number: int
    # Byte offset of the location from the start of the line (0-based)
    column_offset: int
    # Byte offset of the start of the line in the buffer
    line_offset: int

    def offset(self):
        '''The offset of this location in the buffer.'''
        return self.line_offset + self.column_offset

    def buffer_position(self):
        '''Where this location lies in the buffer.'''
        text = self.buffer.text
        offset = self.offset()
        if offset == len(text):
            return BufferPosition.END_OF_SOURCE
        elif text[offset] in {10, 13}:
            return BufferPosition.END_OF_LINE
        return BufferPosition.WITHIN_LINE


class IntegerKind(IntEnum):
    '''Integer kinds.  Not all are supported by all standards.'''
    error = auto()
    bool = auto()
    char = auto()
    schar = auto()
    uchar = auto()
    short = auto()
    ushort = auto()
    int = auto()
    uint = auto()
    long = auto()
    ulong = auto()
    long_long = auto()
    ulong_long = auto()
    char8_t = auto()
    char16_t = auto()
    char32_t = auto()
    wchar_t = auto()
    enumeration = auto()

    def __repr__(self):
        return f'IntegerKind.{self.name}'


class RealKind(IntEnum):
    '''Real floating point kinds.  Not all are supported by all standards.'''
    error = auto()
    float = auto()
    double = auto()
    long_double = auto()
    float16_t = auto()
    float32_t = auto()
    float64_t = auto()
    float128_t = auto()
    bfloat16_t = auto()
    decimal32_t = auto()
    decimal64_t = auto()
    decimal128_t = auto()

    def __repr__(self):
        return f'RealKind.{self.name}'


@dataclass(slots=True)
class Charset:
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


# A map from escape letters (e.g. 't', 'n') to their unicode codepoints
SIMPLE_ESCAPES = {ord(c): ord(d) for c, d in zip('\\\'?"abfnrtv', '\\\'?"\a\b\f\n\r\t\v')}

# A map from control character codepoints to escape letters (e.g. 9 -> 't')
CONTROL_CHARACTER_LETTERS = {d: chr(c) for c, d in SIMPLE_ESCAPES.items() if d < 32}


def escape_bytes(raw, use_hex_escapes):
    '''Escape a sequence of bytes as a sequence of hexadecimal or octal escapes.'''
    if use_hex_escapes:
        return ''.join(f'\\x{c:02x}' for c in raw)
    return ''.join(oct(c)[2:] for c in raw)
