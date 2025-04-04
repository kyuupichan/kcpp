# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#

'''Basic definitions needed by most of the preprocessor, that don't depend on other objects.

Should not import other cpp modules.
'''

from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import IntEnum, auto

from ..core import IntegerKind


__all__ = [
    'Token', 'TokenKind', 'TokenFlags', 'Encoding', 'IdentifierInfo', 'SpecialKind',
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
        if extra is None:
            extra = ''
        elif isinstance(extra, IdentifierInfo):
            extra = f', {extra.to_text()}'
        elif isinstance(extra, tuple):
            extra = f', {extra[0].decode()}'
        return f'Token({self.kind.name}, {flags}, {self.loc}{extra})'

    def to_short_text(self):
        if self.kind == TokenKind.IDENTIFIER:
            return f'Token({self.kind.name}, {self.extra.spelling.decode()})'
        if self.kind == TokenKind.CHARACTER_LITERAL or self.kind == TokenKind.STRING_LITERAL:
            spelling, _ = self.extra
            return f'Token({self.kind.name}, {spelling.decode()})'
        return f'Token({self.kind.name})'


class TokenKind(IntEnum):
    # These are for internal use of the preprocessor and are never returned by pp.get_token()
    PEEK_AGAIN = auto()          # Only for use in peek_token_kind()
    WS = auto()                  # whitespace - internal to lexer
    MACRO_PARAM = auto()         # only appears in macro replacement lists
    STRINGIZE = auto()           # only appears in macro replacement lists
    PLACEMARKER = auto()         # used in function-like macro expansion
    HEADER_NAME = auto()         # A header-name

    # These can all be returned by pp.get_token()
    EOF = auto()                 # EOF to the preprocessor; end of source to a front end
    OTHER = auto()               # a character that is not another token, e.g. @
    HASH = auto()                # # %:
    CONCAT = auto()              # ## %:%:
    ERROR = auto()               # Something erroneous that should not give rise to further errors

    IDENTIFIER = auto()          # abc
    NUMBER = auto()              # 1.2f
    CHARACTER_LITERAL = auto()   # 'c'
    STRING_LITERAL = auto()      # "str"

    BRACE_OPEN = auto()          # { <%
    BRACE_CLOSE = auto()         # } %>
    SQUARE_OPEN = auto()         # [ <:
    SQUARE_CLOSE = auto()        # ] :>
    PAREN_OPEN = auto()          # (
    PAREN_CLOSE = auto()         # )
    SEMICOLON = auto()           # ;
    QUESTION_MARK = auto()       # ?
    TILDE = auto()               # ~
    COMMA = auto()               # ,
    DOT = auto()                 # .
    DOT_STAR = auto()            # .*
    ELLIPSIS = auto()            # ...

    COLON = auto()               # :
    SCOPE = auto()               # ::
    DEREF = auto()               # ->
    DEREF_STAR = auto()          # ->*

    ASSIGN = auto()              # =
    PLUS = auto()                # +
    PLUS_ASSIGN = auto()         # +=
    MINUS = auto()               # -
    MINUS_ASSIGN = auto()        # -=
    MULTIPLY = auto()            # *
    MULTIPLY_ASSIGN = auto()     # *=
    DIVIDE = auto()              # /
    DIVIDE_ASSIGN = auto()       # /=
    MODULUS = auto()             # %
    MODULUS_ASSIGN = auto()      # %=

    INCREMENT = auto()           # ++
    DECREMENT = auto()           # --

    BITWISE_AND = auto()         # &
    BITWISE_AND_ASSIGN = auto()  # &=
    BITWISE_OR = auto()          # |
    BITWISE_OR_ASSIGN = auto()   # |=
    BITWISE_XOR = auto()         # ^
    BITWISE_XOR_ASSIGN = auto()  # ^=

    LOGICAL_AND = auto()         # &&
    LOGICAL_OR = auto()          # ||
    LOGICAL_NOT = auto()         # !

    LSHIFT = auto()              # <<
    LSHIFT_ASSIGN = auto()       # <<=
    RSHIFT = auto()              # >>
    RSHIFT_ASSIGN = auto()       # >>=

    EQ = auto()                  # ==
    NE = auto()                  # !=
    LT = auto()                  # <
    LE = auto()                  # <=
    GT = auto()                  # >
    GE = auto()                  # >=
    LEG = auto()                 # <=>

    # Keywords
    kw_alignas = auto()
    kw_alignof = auto()
    kw_asm = auto()
    kw_auto = auto()
    kw_bool = auto()
    kw_break = auto()
    kw_case = auto()
    kw_catch = auto()
    kw_char = auto()
    kw_char16_t = auto()
    kw_char32_t = auto()
    kw_char8_t = auto()
    kw_class = auto()
    kw_co_await = auto()
    kw_co_return = auto()
    kw_co_yield = auto()
    kw_concept = auto()
    kw_const = auto()
    kw_const_cast = auto()
    kw_consteval = auto()
    kw_constexpr = auto()
    kw_constinit = auto()
    kw_continue = auto()
    kw_decltype = auto()
    kw_default = auto()
    kw_delete = auto()
    kw_do = auto()
    kw_double = auto()
    kw_dynamic_cast = auto()
    kw_else = auto()
    kw_enum = auto()
    kw_explicit = auto()
    kw_export = auto()
    kw_extern = auto()
    kw_false = auto()
    kw_float = auto()
    kw_for = auto()
    kw_friend = auto()
    kw_goto = auto()
    kw_if = auto()
    kw_inline = auto()
    kw_int = auto()
    kw_long = auto()
    kw_mutable = auto()
    kw_namespace = auto()
    kw_new = auto()
    kw_noexcept = auto()
    kw_nullptr = auto()
    kw_operator = auto()
    kw_private = auto()
    kw_protected = auto()
    kw_public = auto()
    kw_register = auto()
    # kw_restrict = auto()
    kw_reinterpret_cast = auto()
    kw_requires = auto()
    kw_return = auto()
    kw_short = auto()
    kw_signed = auto()
    kw_sizeof = auto()
    kw_static = auto()
    kw_static_assert = auto()
    kw_static_cast = auto()
    kw_struct = auto()
    kw_switch = auto()
    kw_template = auto()
    kw_this = auto()
    kw_thread_local = auto()
    kw_throw = auto()
    kw_true = auto()
    kw_try = auto()
    kw_typedef = auto()
    kw_typeid = auto()
    kw_typename = auto()
    # kw_typeof = auto()
    # kw_typeof_unqual = auto()
    kw_union = auto()
    kw_unsigned = auto()
    kw_using = auto()
    kw_virtual = auto()
    kw_void = auto()
    kw_volatile = auto()
    kw_wchar_t = auto()
    kw_while = auto()
    # kw__Atomic = auto()
    # kw__BitInt = auto()
    # kw__Complex = auto()
    # kw__Decimal128 = auto()
    # kw__Decimal32 = auto()
    # kw__Decimal64 = auto()
    # kw__Generic = auto()
    # kw__Imaginary = auto()
    # kw__Noreturn = auto()


TokenKind.literal_kinds = {TokenKind.NUMBER, TokenKind.HEADER_NAME, TokenKind.CHARACTER_LITERAL,
                           TokenKind.STRING_LITERAL}


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


Encoding.basic_integer_kinds = [IntegerKind.char, IntegerKind.wchar_t, IntegerKind.char8_t,
                                IntegerKind.char16_t, IntegerKind.char32_t]


class SpecialKind(IntEnum):
    '''These act as independent flags; more than one may be set (e.g. 'if').  High bits of
    the 'special' can encode more information.'''
    # e.g. 'if', 'error', 'define'.  High bits unused.
    DIRECTIVE = 0x01
    # e.g. 'if', 'const', 'double'.  High bits encode the token kind.
    KEYWORD = 0x02
    # '__VA_ARGS__' or '__VA_OPT__'.  High bits unused.  Restricted use.
    VA_IDENTIFIER = 0x04
    # e.g. 'not', 'and', 'xor_eq'.  High bits encode the token kind.
    ALT_TOKEN = 0x08
    # e.g. 'L', 'uR'.  High bits encode the Encoding enum.
    ENCODING_PREFIX = 0x10


@dataclass(slots=True)
class IdentifierInfo:
    '''Ancilliary information about an identifier.'''
    # Spelling (UCNs replaced)
    spelling: bytes
    # Points to the macro definition, if any
    macro: object
    # If this identifier is "special", how so
    special: int

    def __hash__(self):
        return hash(self.spelling)

    def to_text(self):
        return f'{self.spelling.decode()}'

    def alt_token_kind(self):
        assert self.special & SpecialKind.ALT_TOKEN
        return TokenKind(self.special >> 6)

    def encoding(self):
        assert self.special & SpecialKind.ENCODING_PREFIX
        return Encoding(self.special >> 6)

    def set_alt_token(self, token_kind):
        self.special = (token_kind << 6) + SpecialKind.ALT_TOKEN

    def set_directive(self):
        self.special |= SpecialKind.DIRECTIVE

    def set_encoding(self, encoding):
        self.special = (encoding << 6) + SpecialKind.ENCODING_PREFIX

    def set_keyword(self, token_kind):
        self.special |= (token_kind << 6) + SpecialKind.KEYWORD

    def set_va_identifier(self):
        self.special |= SpecialKind.VA_IDENTIFIER


# A dummy used for a lexed identifier when skipping
IdentifierInfo.dummy = IdentifierInfo('!', None, 0)


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
