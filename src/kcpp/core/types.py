# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Basic types.  Should have no dependencies on external modules.'''

from bisect import bisect_left
from dataclasses import dataclass
from enum import IntEnum, auto


__all__ = [
    'Buffer', 'BufferPosition', 'PresumedLocation', 'IntegerKind', 'RealKind',
    'IdentifierInfo', 'Token', 'TokenKind', 'TokenFlags', 'Encoding', 'SpecialKind',
]

UTF8_BOM = b'\xef\xbb\xbf'


def line_offsets_gen(raw):
    for n, c in enumerate(raw):
        if c == 10:  # '\n'
            yield n + 1
        elif c == 13:  # '\r'
            if n + 1 == len(raw) or raw[n + 1] != 10:
                yield n + 1


def sparse_line_offsets(raw, bom_length, min_step):
    '''Return a sparse sorted list of (line_offset, line_number) pairs.  The list always
    starts with (0,1) or (3, 1) for a text file with a UTF-8 BOM.
    '''
    result = [(bom_length, 1)]

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
        self.bom_length = 3 if text.startswith(UTF8_BOM) else 0
        # A sparse list of (offset, line_number) pairs to save memory
        self._sparse_line_offsets = None
        self.sparsity = sparsity

    def sparse_line_offsets(self):
        '''Calculate the sparse line offsets on demand, and cache them.'''
        if self._sparse_line_offsets is None:
            self._sparse_line_offsets = sparse_line_offsets(self.text, self.bom_length,
                                                            self.sparsity)
        return self._sparse_line_offsets

    def offset_to_line_info(self, offset):
        '''Convert an offset in the buffer to a (line_offset, line_number) pair, where line_offset
        is the start of the line.  The offset can range up to and including the buffer size.

        Line numbers are 1-based.
        '''
        if not 0 <= offset <= len(self.text):
            raise ValueError(f'offset {offset} out of range; max is {len(self.text)}')

        offset = max(offset, self.bom_length)

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
        if offset == len(text) - 1:
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
    # 'module', 'import' or 'export'.  Can start a module or import directive.
    MODULE_KEYWORD = 0x20


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

    def module_keyword_kind(self):
        assert self.special & SpecialKind.MODULE_KEYWORD
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

    def set_module_keyword(self, token_kind):
        self.special |= (token_kind << 6) + SpecialKind.MODULE_KEYWORD

    def set_va_identifier(self):
        self.special |= SpecialKind.VA_IDENTIFIER


@dataclass(slots=True)
class Token:
    kind: int
    flags: int
    loc: int
    extra: any

    def disable(self):
        self.flags |= TokenFlags.NO_EXPANSION

    def is_disabled(self):
        return bool(self.flags & TokenFlags.NO_EXPANSION)

    def carries_spelling(self):
        return self.kind in TokenKind.spelling_kinds

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
        elif isinstance(extra, int):
            extra = f', {extra}'
        return f'Token({self.kind.name}, {flags}, {self.loc}{extra})'

    def to_short_text(self):
        if self.kind == TokenKind.IDENTIFIER:
            return f'Token({self.kind.name}, {self.extra.spelling.decode()})'
        if (self.kind == TokenKind.CHARACTER_LITERAL or self.kind == TokenKind.STRING_LITERAL
                or self.kind == TokenKind.HEADER_NAME):
            spelling, _ = self.extra
            return f'Token({self.kind.name}, {spelling.decode()})'
        if self.kind == TokenKind.CHARACTER:
            return f'Token({self.kind.name}, {self.extra})'
        return f'Token({self.kind.name})'


class TokenKind(IntEnum):
    # These are for internal use of the preprocessor and are never returned by pp.get_token()
    PEEK_AGAIN = auto()          # Only for use in peek_token_kind()
    WS = auto()                  # whitespace - internal to lexer
    DIRECTIVE_HASH = auto()      # The hash that starts a directive
    MACRO_PARAM = auto()         # only appears in macro replacement lists
    CONCAT = auto()              # only appears in macro replacement lists
    STRINGIZE = auto()           # only appears in macro replacement lists
    PLACEMARKER = auto()         # used in function-like macro expansion
    HEADER_NAME = auto()         # A header-name

    # These can all be returned by pp.get_token()
    EOF = auto()                 # EOF to the preprocessor; end of source to a front end
    CHARACTER = auto()           # a character that is not another token, e.g. @
    HASH = auto()                # # %:
    HASH_HASH = auto()           # ## %:%:
    UNTERMINATED = auto()        # An unterminated character or string literal

    IDENTIFIER = auto()          # abc
    NUMBER = auto()              # 1.2f
    CHARACTER_LITERAL = auto()   # 'c' with optional encoding prefix and ud_suffix
    STRING_LITERAL = auto()      # "str" with optional encoding prefix and ud_suffix

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
    ELLIPSIS = auto()            # ...

    COLON = auto()               # :
    SCOPE = auto()               # ::
    DEREF = auto()               # ->

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

    # C++ only tokens
    DOT_STAR = auto()            # .*
    DEREF_STAR = auto()          # ->*
    LEG = auto()                 # <=>

    # Common keywords (C23, C++23)
    kw_alignas = auto()
    kw_alignof = auto()
    kw_auto = auto()
    kw_bool = auto()
    kw_break = auto()
    kw_case = auto()
    kw_char = auto()
    kw_const = auto()
    kw_constexpr = auto()
    kw_continue = auto()
    kw_default = auto()
    kw_do = auto()
    kw_double = auto()
    kw_else = auto()
    kw_enum = auto()
    kw_extern = auto()
    kw_false = auto()
    kw_float = auto()
    kw_for = auto()
    kw_goto = auto()
    kw_if = auto()
    kw_inline = auto()
    kw_int = auto()
    kw_long = auto()
    kw_nullptr = auto()
    kw_register = auto()
    kw_return = auto()
    kw_short = auto()
    kw_signed = auto()
    kw_sizeof = auto()
    kw_static = auto()
    kw_static_assert = auto()
    kw_struct = auto()
    kw_switch = auto()
    kw_thread_local = auto()
    kw_true = auto()
    kw_typedef = auto()
    kw_union = auto()
    kw_unsigned = auto()
    kw_void = auto()
    kw_volatile = auto()
    kw_while = auto()

    # C only keywords
    kw__Atomic = auto()
    kw__BitInt = auto()
    kw__Complex = auto()
    kw__Decimal128 = auto()
    kw__Decimal32 = auto()
    kw__Decimal64 = auto()
    kw__Generic = auto()
    kw__Imaginary = auto()
    kw__Noreturn = auto()
    kw_restrict = auto()
    kw_typeof = auto()
    kw_typeof_unqual = auto()
    # Obsolescent
    kw__Alignas = auto()
    kw__Alignof = auto()
    kw__Bool = auto()
    kw__Static_assert = auto()
    kw__Thread_local = auto()

    # C++ only keywords
    kw_asm = auto()
    kw_catch = auto()
    kw_char16_t = auto()
    kw_char32_t = auto()
    kw_char8_t = auto()
    kw_class = auto()
    kw_concept = auto()
    kw_const_cast = auto()
    kw_consteval = auto()
    kw_constinit = auto()
    kw_co_await = auto()
    kw_co_return = auto()
    kw_co_yield = auto()
    kw_decltype = auto()
    kw_delete = auto()
    kw_dynamic_cast = auto()
    kw_explicit = auto()
    kw_export = auto()
    kw_friend = auto()
    kw_mutable = auto()
    kw_namespace = auto()
    kw_new = auto()
    kw_noexcept = auto()
    kw_operator = auto()
    kw_private = auto()
    kw_protected = auto()
    kw_public = auto()
    kw_reinterpret_cast = auto()
    kw_requires = auto()
    kw_static_cast = auto()
    kw_template = auto()
    kw_this = auto()
    kw_throw = auto()
    kw_try = auto()
    kw_typeid = auto()
    kw_typename = auto()
    kw_using = auto()
    kw_virtual = auto()
    kw_wchar_t = auto()
    # These keywords have no spelling
    kw_export_keyword = auto()
    kw_import_keyword = auto()
    kw_module_keyword = auto()


TokenKind.spelling_kinds = {TokenKind.NUMBER, TokenKind.CHARACTER_LITERAL, TokenKind.HEADER_NAME,
                            TokenKind.STRING_LITERAL}
TokenKind.literal_kinds = {TokenKind.NUMBER, TokenKind.CHARACTER_LITERAL,
                           TokenKind.STRING_LITERAL}


class TokenFlags(IntEnum):
    NONE = 0x00
    WS = 0x01
    NO_EXPANSION = 0x02     # Macro expansion disabled

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
