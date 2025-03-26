# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#

'''Basic definitions needed by most of the preprocessor, that don't depend on other objects.

Should not import other cpp modules.
'''

from abc import ABC, abstractmethod
from argparse import Namespace
from codecs import getincrementalencoder
from dataclasses import dataclass
from enum import IntEnum, auto
from typing import ClassVar

from ..unicode import REPLACEMENT_CHAR, is_printable


__all__ = [
    'Token', 'TokenKind', 'TokenFlags', 'Encoding', 'IntegerKind', 'RealKind',
    'IdentifierInfo', 'SpecialKind', 'TargetMachine', 'Environment',
    'SIMPLE_ESCAPES', 'CONTROL_CHARACTER_ESCAPES', 'quoted_string',
]


@dataclass(slots=True)
class Environment:
    '''The command line and environment variables passed to kcpp.'''
    # Command line arguments
    command_line: Namespace
    # A dictionary of environment variables
    variables: dict
    # A list of Diagnostic objects issued when processing the above.  It is the
    # responsibility of the creator of the Environment object to pass these diagnostics to
    # a DiagnosticConsumer at the appropriate time.
    diagnostics: list

    def diag(self, did, args):
        from ..diagnostics import Diagnostic, location_command_line
        self.diagnostics.append(Diagnostic(did, location_command_line, args))


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

    def copy_spacing_flags_from(self, flags):
        # Don't copy BOL for now - otherwise we can treat an initial # as a directive
        mask = TokenFlags.WS
        self.flags &= ~mask
        self.flags |= flags & mask

    def disable(self):
        self.flags |= TokenFlags.NO_EXPANSION

    def is_disabled(self):
        return bool(self.flags & TokenFlags.NO_EXPANSION)

    def is_literal(self):
        return self.kind in TokenKind.literal_kinds

    def to_text(self):
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

        flags = '|'.join(flags_repr())
        extra = self.extra
        if isinstance(extra, IdentifierInfo):
            extra = extra.to_text()
        elif isinstance(extra, tuple):
            extra = (extra[0].decode(), extra[1])
        return f'Token(kind={self.kind.name}, flags={flags}, loc={self.loc}, extra={extra})'

    def to_short_text(self):
        if self.kind == TokenKind.IDENTIFIER:
            return f'Token({self.kind.name}, {self.extra.spelling.decode()})'
        if self.kind == TokenKind.CHARACTER_LITERAL or self.kind == TokenKind.STRING_LITERAL:
            spelling, _ = self.extra
            return f'Token({self.kind.name}, {spelling.decode()})'
        return f'Token({self.kind.name})'


class TokenKind(IntEnum):
    # These are for internal use of the preprocessor and are never returned by pp.get_token()
    PEEK_AGAIN = 0            # Only for use in peek_token_kind().
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


Encoding.basic_integer_kinds = [IntegerKind.char, IntegerKind.wchar_t, IntegerKind.char8_t,
                                IntegerKind.char16_t, IntegerKind.char32_t]


class SpecialKind(IntEnum):
    NOT_SPECIAL = 0
    VA_IDENTIFIER = 1      # These tokens are restricted to limited contexts
    ALT_TOKEN = 2
    ENCODING_PREFIX = 3
    BUILT_IN_MACRO = 4


class BuiltInKind(IntEnum):
    '''The type of built-in dynamic macro.  There are only a handful.

    Predefined macros always have the same value no matter where expanded and across
    different invocations of the compiler with the same command line switches.  Built-in
    macros are dynamic - their expansion changes depending on location, or with each
    compilation.  For example, respectively, __LINE__ and __TIME__ are builtins.
    __cplusplus is not a builtin, it is a predefined macro.
    '''
    DATE = 0
    TIME = 1
    FILE = 2
    LINE = 3


@dataclass(slots=True)
class IdentifierInfo:
    '''Ancilliary information about an identifier.'''
    # Spelling (UCNs replaced)
    spelling: bytes
    # Points to the macro definition, if any
    macro: object
    # If this identifier is "special", how so
    special: int
    # If this identifier is a keyword, which one
    keyword: int

    def __hash__(self):
        return hash(self.spelling)

    def to_text(self):
        return f'{self.spelling.decode()}'

    def special_kind(self):
        return SpecialKind(self.special & 0xf)

    def alt_token_kind(self):
        assert self.special_kind() == SpecialKind.ALT_TOKEN
        return TokenKind(self.special >> 4)

    def encoding(self):
        assert self.special_kind() == SpecialKind.ENCODING_PREFIX
        return Encoding(self.special >> 4)

    def built_in_kind(self):
        assert self.special_kind() == SpecialKind.BUILT_IN
        return BuiltInKind(self.special >> 4)

    def set_special(self, kind):
        self.special = kind

    def set_alt_token(self, token_kind):
        self.special = (token_kind << 4) + SpecialKind.ALT_TOKEN

    def set_encoding(self, encoding):
        self.special = (encoding << 4) + SpecialKind.ENCODING_PREFIX

    def set_built_in(self, builtin_kind):
        self.special = (builtin_kind << 4) + SpecialKind.BUILT_IN_MACRO


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

    narrow_charset: Charset
    wide_charset: Charset

    @classmethod
    def default(cls):
        # e.g. Apple-Silicon
        return cls(True, 8, 16, 32, 64, 64, IntegerKind.schar,
                   IntegerKind.ulong, IntegerKind.int, IntegerKind.ushort, IntegerKind.uint,
                   Charset.from_name('UTF-8'), Charset.from_name('UTF-32LE'))

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


SIMPLE_ESCAPES = {ord(c): ord(d) for c, d in zip('\\\'?"abfnrtv', '\\\'?"\a\b\f\n\r\t\v')}

'''Maps a codepoint to a C escape sequence if it is a control character and representable
   as a C escape sequence.

    For example, 9 -> "\\t".

'''
CONTROL_CHARACTER_ESCAPES = {d: '\\' + chr(c) for c, d in SIMPLE_ESCAPES.items() if d < 32}


def quoted_string(s):
    '''Place s in quotes in a C-string, such that the string literal would be interpreted
    back to s.'''
    result = '"'

    for c in s:
        cp = ord(c)
        if is_printable(cp):
            if cp == 34 or cp == 92:
                result += '\\'
            result += c
        else:
            esc = CONTROL_CHARACTER_ESCAPES.get(cp)
            if esc:
                result += esc
            else:
                for cp in c.encode():
                    result += f'\\x{cp:02x}'
    result += '"'
    return result
