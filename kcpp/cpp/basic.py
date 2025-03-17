# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#

'''Basic definitions needed by most of the preprocessor, that don't depend on other objects.

Should not import other cpp modules.
'''

from abc import ABC, abstractmethod
from bisect import bisect_left
from dataclasses import dataclass
from enum import IntEnum, auto
from itertools import accumulate

from ..unicode import utf8_cp, is_printable, terminal_charwidth, codepoint_to_hex

__all__ = [
    'Token', 'TokenKind', 'TokenFlags', 'Encoding', 'IntegerKind', 'RealKind',
    'IdentifierInfo', 'TargetMachine', 'Buffer', 'BufferLocation', 'SourceLine',
]


class TokenSource(ABC):
    '''A source of tokens - for example, a lexer operating on a buffer, or a macro
    replacement list.'''

    @abstractmethod
    def get_token(self, token):
        pass


@dataclass(slots=True)
class Token:
    kind: int
    flags: int
    loc: int
    extra: any

    @classmethod
    def create(cls):
        return cls(-1, -1, -1, any)

    def set_to(self, src, loc):
        '''Copy a token to this one, but use the locaiton passed.'''
        self.kind = src.kind
        self.flags = src.flags
        self.loc = loc
        self.extra = src.extra

    def disable(self):
        self.flags |= TokenFlags.NO_EXPANSION

    def is_disabled(self):
        return bool(self.flags & TokenFlags.NO_EXPANSION)

    def is_literal(self):
        return self.kind in TokenKind.literal_kinds

    def repr(self):
        from kcpp.cpp.preprocessor import IdentifierInfo

        def flags_repr():
            flags = self.flags
            if flags == 0:
                yield 'NONE'
            for name, value in TokenFlags.__members__.items():
                if flags & value:
                    yield name
            flags = TokenFlags.get_encoding(flags)
            if flags:
                for name, value in Encoding.__members__.items():
                    if flags == value:
                        yield name
                        break

        extra = self.extra
        if isinstance(extra, IdentifierInfo):
            extra = extra.spelling

        return [self.kind.name, '|'.join(flags_repr()), self.loc, extra]


class TokenKind(IntEnum):
    # These are for internal use of the preprocessor and are never returned by pp.get_token()
    EOD = 0                   # end-of-directive
    WS = 1                    # whitespace
    MACRO_PARAM = 2           # for macro replacement lists; internal use
    STRINGIZE = 3             # stringification operator
    PLACEMARKER = 4           # placemarker token during function-like macro expansion

    # These can all be returned by pp.get_token()
    EOF = 5                   # end-of-file
    OTHER = 6                 # a character that is not another token
    HASH = 7                  # # %:
    CONCAT = 8                # ## %:%:
    ERROR = 9                 # Something erroneous that should not give rise to further errors

    IDENTIFIER = 10           # abc
    NUMBER = 11               # 1.2f
    CHARACTER_LITERAL = 12    # 'c'
    STRING_LITERAL = 13       # "str"

    BRACE_OPEN = 20           # { <%
    BRACE_CLOSE = 21          # } %>
    SQUARE_OPEN = 22          # [ <:
    SQUARE_CLOSE = 23         # ] :>
    PAREN_OPEN = 24           # (
    PAREN_CLOSE = 25          # )
    SEMICOLON = 26            # ;
    QUESTION_MARK = 27        # ?
    TILDE = 28                # ~
    COMMA = 29                # ,
    DOT = 30                  # .
    DOT_STAR = 31             # .*
    ELLIPSIS = 32             # ...

    COLON = 40                # :
    SCOPE = 41                # ::
    DEREF = 42                # ->
    DEREF_STAR = 43           # ->*

    ASSIGN = 50               # =
    PLUS = 51                 # +
    PLUS_ASSIGN = 52          # +=
    MINUS = 53                # -
    MINUS_ASSIGN = 54         # -=
    MULTIPLY = 55             # *
    MULTIPLY_ASSIGN = 56      # *=
    DIVIDE = 57               # /
    DIVIDE_ASSIGN = 58        # /=
    MODULUS = 59              # %
    MODULUS_ASSIGN = 60       # %=

    INCREMENT = 65            # ++
    DECREMENT = 66            # --

    BITWISE_AND = 70          # &
    BITWISE_AND_ASSIGN = 71   # &=
    BITWISE_OR = 72           # |
    BITWISE_OR_ASSIGN = 73    # |=
    BITWISE_XOR = 74          # ^
    BITWISE_XOR_ASSIGN = 75   # ^=

    LOGICAL_AND = 80          # &&
    LOGICAL_OR = 81           # ||
    LOGICAL_NOT = 82          # !

    LSHIFT = 90               # <<
    LSHIFT_ASSIGN = 91        # <<=
    RSHIFT = 92               # >>
    RSHIFT_ASSIGN = 93        # >>=

    EQ = 100                  # ==
    NE = 101                  # !=
    LT = 102                  # <
    LE = 103                  # <=
    GT = 104                  # >
    GE = 105                  # >=
    LEG = 106                 # <=>


TokenKind.literal_kinds = {TokenKind.NUMBER, TokenKind.CHARACTER_LITERAL, TokenKind.STRING_LITERAL}


class TokenFlags(IntEnum):
    NONE = 0x00
    WS = 0x01
    BOL = 0x02              # Beginning of line
    NO_EXPANSION = 0x04     # Macro expansion disabled

    # The high 8 bits hold the encoding of the character or string literal
    @staticmethod
    def encoding_bits(encoding):
        assert isinstance(encoding, Encoding)
        return encoding << 8

    @staticmethod
    def get_encoding(flags):
        return Encoding((flags >> 8) & 0xf)


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


class Encoding(IntEnum):
    '''Encodings for character and string literals.'''
    # The bottom 3 bits give the encoding kind, the 4th bit indicates if the literal is a
    # raw string literal.
    NONE = 0
    WIDE = 1
    UTF_8 = 2
    UTF_16 = 3
    UTF_32 = 4
    RAW = 8    # A flag bit
    WIDE_RAW = 9
    UTF_8_RAW = 10
    UTF_16_RAW = 11
    UTF_32_RAW = 12

    # True for raw string literals like R"(foo)"
    def is_raw(self):
        return bool(self.value & Encoding.RAW)

    def basic_encoding(self):
        '''Strips any RAW flag.'''
        return Encoding(self.value & ~Encoding.RAW)

    def integer_kind(self):
        return self.basic_integer_kinds[self.basic_encoding()]


class Unicode:
    charsets = {'utf32', 'utf32be', 'utf32le', 'utf16', 'utf16be', 'utf16le', 'utf8', 'cp65001'}

    @classmethod
    def is_unicode(cls, charset):
        '''Return True if charset looks like a Unicode charset.'''
        return charset.replace('_', '').replace('-', '').lower() in cls.charsets

    @classmethod
    def get_fixed_width(cls, charset, default):
        '''If charset is a Unicode charset and has a fixed-width encoding return that width.
        Otherwise return default.
        '''
        if cls.is_unicode(charset):
            if '32' in charset:
                return 32
            if '16' in charset:
                return 16
        return default


Encoding.basic_integer_kinds = [IntegerKind.char, IntegerKind.wchar_t, IntegerKind.char8_t,
                                IntegerKind.char16_t, IntegerKind.char32_t]


@dataclass(slots=True)
class IdentifierInfo:
    '''Ancilliary information about an identifier.'''
    # Spelling (UCNs replaced)
    spelling: str
    # Points to the macro definition, if any
    macro: object
    # If this identifier is "special", how so
    special: int
    # If this identifier is a keyword, which one
    keyword: int

    def __hash__(self):
        return hash(self.spelling)


# A dummy used for a lexed identifier when skipping
IdentifierInfo.dummy = IdentifierInfo('!', None, 0, 0)


@dataclass(slots=True)
class TargetMachine:
    '''Specification of a target machine.  Determines how numeric and character literals
    are interpreted.'''
    # If integers are stored little-endian
    is_little_endian: bool

    char_width: int
    short_width: int
    int_width: int
    long_width: int
    long_long_width: int

    char_kind: IntegerKind
    size_t_kind: IntegerKind
    wchar_t_kind: IntegerKind
    char16_t_kind: IntegerKind
    char32_t_kind: IntegerKind

    # Functions that convert characters into the execution character set
    narrow_encoding: str
    wide_encoding: str

    @classmethod
    def default(cls):
        # e.g. Apple-Silicon
        return cls(True, 8, 16, 32, 64, 64, IntegerKind.schar,
                   IntegerKind.ulong, IntegerKind.int, IntegerKind.ushort, IntegerKind.uint,
                   'UTF-8', 'UTF-32LE')

    def configure(self, command_line, environ):
        narrow = command_line.exec_charset
        if narrow:
            self.narrow_encoding = narrow
            self.char_width = Unicode.get_fixed_width(narrow, self.char_width)
        wide = command_line.wide_exec_charset
        if wide:
            self.wide_encoding = wide
            self.wchar_t_width = Unicode.get_fixed_width(wide, self.wchar_t_width)

    def pp_arithmetic_width(self):
        return self.long_long_width

    def underlying_kind(self, kind):
        if kind == IntegerKind.char:
            return self.char_kind
        if kind == IntegerKind.char8_t:
            return IntegerKind.uchar
        if kind == IntegerKind.wchar_t:
            return self.wchar_t_kind
        if kind == IntegerKind.char16_t:
            return self.char16_t_kind
        if kind == IntegerKind.char32_t:
            return self.char32_t_kind
        return kind

    def is_unsigned(self, kind):
        ukind = self.underlying_kind(kind)
        if ukind in (IntegerKind.schar, IntegerKind.short, IntegerKind.int, IntegerKind.long,
                     IntegerKind.long_long):
            return False
        if ukind in (IntegerKind.uchar, IntegerKind.ushort, IntegerKind.uint, IntegerKind.ulong,
                     IntegerKind.ulong_long):
            return True
        raise RuntimeError(f'kind {kind} not handled in is_signed()')

    def integer_width(self, kind):
        kind = self.underlying_kind(kind)
        if kind in (IntegerKind.schar, IntegerKind.uchar):
            return self.char_width
        if kind in (IntegerKind.short, IntegerKind.ushort):
            return self.short_width
        if kind in (IntegerKind.int, IntegerKind.uint):
            return self.int_width
        if kind in (IntegerKind.long, IntegerKind.ulong):
            return self.long_width
        if kind in (IntegerKind.long_long, IntegerKind.ulong_long):
            return self.long_long_width
        raise RuntimeError(f'kind {kind} not handled in is_signed()')


@dataclass(slots=True)
class SourceLine:
    # The source line as printable text.  The text is intended to be printable on a
    # unicode terminal - unprintable characters are replaced with <U+XXXX> sequences, and
    # invalid encodings are replaced with <\xAB> hex sequences, and tabs are replaced with
    # spaces.  The string does not have a terminating newline.
    text: str
    # in_widths and out_widths are byte arrays of the same length.  They hold the width,
    # in bytes, of each unicode character in the raw source line and in the returned
    # string.  For example, if a NUL character is the Nth (0-based) character on the
    # source line, then src_widths[N] will be 1 - the length of the UTF-8 encoding of NUL.
    # dst_widths[N] will be 8 - as its representation "<U+0000>" is the returned string
    # occupies 8 columns on a terminal.  This information makes it straight-forward to map
    # physical source bytes to positions in output text, and vice-versa.
    in_widths: bytearray
    out_widths: bytearray
    # List of replacements that were made.  This contains unprintable unicode characters
    # as well as replacements for bad UTF-8 encodings.  It does not contain horizontal tab
    # replacemenets.  Each is an index into the in_widths / out_widths arrays.
    replacements: list
    # The line number of this line
    line_number: int

    def convert_column_offset(self, column_offset):
        '''Given a column offset in the physical source line, return the column in the
        output text line.'''
        cursor = 0
        text_column = 0
        out_widths = self.out_widths

        for n, in_width in enumerate(self.in_widths):
            if cursor >= column_offset:
                break
            cursor += in_width
            text_column += out_widths[n]

        return text_column

    def convert_eranges_to_column_ranges(self, eranges):
        '''Given a sequence of elaborated ranges, convert each to a pair (start, end) of output
        column offsets based on where that range intersects this line.  If it intersects
        this line then end >= start, otherwise end == start == -1.

        The return value is a list of the same length and order as the input sequence.
        '''
        def convert_erange(erange):
            if erange.start.line_number <= self.line_number <= erange.end.line_number:
                if erange.start.line_number == self.line_number:
                    start = self.convert_column_offset(erange.start.column_offset)
                else:
                    start = 0
                if erange.end.line_number == self.line_number:
                    end = self.convert_column_offset(erange.end.column_offset)
                else:
                    end = sum(self.out_widths)
            else:
                start = end = -1

            return start, end

        return [convert_erange(erange) for erange in eranges]

    def truncate(self, max_width, required_column):
        '''Returns (initial output width removed, line).'''
        line_length = len(self.out_widths)
        cum_widths = list(accumulate(self.out_widths, initial=0))
        if cum_widths[-1] <= max_width:
            return 0, self

        assert 0 <= required_column <= cum_widths[-1]

        # Start with the required column.  Expand the radius until we fail.
        left = bisect_left(cum_widths, required_column)
        assert cum_widths[left] == required_column

        radius = 0
        while True:
            radius += 1
            left_end = max(0, left - radius)
            right_end = min(line_length, (left + 1) + radius)
            if cum_widths[right_end] - cum_widths[left_end] > max_width:
                radius -= 1
                left_end = max(0, left - radius)
                right_end = min(line_length, (left + 1) + radius)
                break

        # Return a new source line representing the selected text
        text = self.text[cum_widths[left_end]: cum_widths[right_end]]
        in_widths = self.in_widths[left_end: right_end]
        out_widths = self.out_widths[left_end: right_end]
        replacements = [r - left_end for r in self.replacements if left_end <= r < right_end]
        line = SourceLine(text, in_widths, out_widths, replacements, self.line_number)
        return cum_widths[left_end], line


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

    def __init__(self, text, *, name=None, sparsity=1_000):
        self.name = '<unnamed>' if name is None else name
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

    def offset_to_coords(self, offset):
        '''Convert an offset in the buffer to a (line_offset, line number, column) tuple.
        The offset can range up to and including the buffer size.

        Line numbers are 1-based.  Columns are byte offsets from the start of the line and
        zero-based.  Therefore line_offset + column == offset.'''
        line_offset, line_number = self.offset_to_line_info(offset)
        return (line_offset, line_number, offset - line_offset)

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

    def source_line(self, line_number, tabstop=8):
        '''Return a SourceLine object for the indicated source line.'''
        def parts(raw_line, in_widths, out_widths, replacements):
            cursor = 0
            limit = len(raw_line)

            while cursor < limit:
                cp, in_width = utf8_cp(raw_line, cursor)
                if cp < 0:
                    # Replace the invalid UTF-8 sequence with ASCII text.
                    out_text = ''.join(f'<{c:02X}>' for c in raw_line[cursor: cursor + in_width])
                    out_width = len(out_text)
                    replacements.append(len(in_widths))
                elif cp == 9:
                    out_width = tabstop - sum(out_widths) % tabstop
                    out_text = ' ' * out_width
                elif is_printable(cp):
                    out_width = terminal_charwidth(cp)
                    out_text = chr(cp)
                else:
                    out_text = f'<{codepoint_to_hex(cp)}>'
                    out_width = len(out_text)
                    replacements.append(len(in_widths))

                in_widths.append(in_width)
                out_widths.append(out_width)
                yield out_text
                cursor += in_width

        raw_line = self.line_bytes(line_number)
        in_widths = bytearray()
        out_widths = bytearray()
        replacements = []
        text = ''.join(parts(raw_line, in_widths, out_widths, replacements))

        return SourceLine(text, in_widths, out_widths, replacements, line_number)


@dataclass(slots=True)
class BufferLocation:
    '''Represents a location in a buffer.'''
    # The buffer
    buffer: Buffer
    # The offset of the location in the buffer
    offset: int


DIGIT_VALUES = {ord(c): ord(c) - 48 for c in '0123456789'}
HEX_DIGIT_VALUES = {
    ord('a'): 10, ord('b'): 11, ord('c'): 12, ord('d'): 13, ord('e'): 14, ord('f'): 15,
    ord('A'): 10, ord('B'): 11, ord('C'): 12, ord('D'): 13, ord('E'): 14, ord('F'): 15,
}
HEX_DIGIT_VALUES.update(DIGIT_VALUES)


def value_width(value):
    if value >= 0:
        return value.bit_length()
    return (-value - 1).bit_length() + 1
