# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Interpretation of character, string and numeric literals.'''

from dataclasses import dataclass
from enum import IntEnum
from struct import Struct

from ..core import (
    IntegerKind, RealKind, TokenKind, TokenFlags, Encoding, IdentifierInfo, Token,
)
from ..diagnostics import DID, SpellingRange, Diagnostic
from ..unicode import (
    utf8_cp, printable_char, is_surrogate, is_valid_codepoint, name_to_cp,
    codepoint_to_hex, SIMPLE_ESCAPES, CONTROL_CHARACTER_LETTERS, REPLACEMENT_CHAR, Charset,
)

__all__ = [
    'LiteralInterpreter', 'IntegerLiteral', 'FloatingPointLiteral', 'StringLiteral',
    'printable_form', 'destringize',
]


OCTAL_DIGITS = {ord(c) for c in '01234567'}
DIGIT_VALUES = {ord(c): ord(c) - 48 for c in '0123456789'}
HEX_DIGIT_VALUES = {
    ord('a'): 10, ord('b'): 11, ord('c'): 12, ord('d'): 13, ord('e'): 14, ord('f'): 15,
    ord('A'): 10, ord('B'): 11, ord('C'): 12, ord('D'): 13, ord('E'): 14, ord('F'): 15,
}
HEX_DIGIT_VALUES.update(DIGIT_VALUES)
# Map bases to diagnostic arguments for DID.invalid_digit
bases = [2, 8, 10, 16]


def value_width(value):
    if value >= 0:
        return value.bit_length()
    return (-value - 1).bit_length() + 1


@dataclass(slots=True)
class UserDefinedSuffix:
    '''A user-defined suffix.  The location is provided in case it needs to be diagnosed.'''
    ident: IdentifierInfo
    loc: SpellingRange

    def __repr__(self):
        return f'UserDefinedSuffix(ident={self.ident.to_text()}, loc={self.loc!r}'


@dataclass(slots=True)
class IntegerLiteral:
    kind: IntegerKind
    value: int
    # An instance of UserDefinedSuffix, or None
    ud_suffix: object

    @classmethod
    def erroneous_literal(cls):
        return cls(IntegerKind.error, 0, None)

    def to_short_text(self):
        ud_suffix_part = f', {self.ud_suffix.ident.spelling.decode()}' if self.ud_suffix else ''
        return f'IntegerLiteral({self.kind.name}, {self.value}{ud_suffix_part})'


@dataclass(slots=True)
class FloatingPointLiteral:
    kind: RealKind
    value: int
    post_dot_digits: int
    exponent: int
    radix: int
    # An instance of UserDefinedSuffix, or None
    ud_suffix: object

    @classmethod
    def erroneous_literal(cls):
        return cls(RealKind.error, 0, 0, 0, 0, None)

    def to_short_text(self):
        ud_suffix_part = f', {self.ud_suffix.ident.spelling.decode()}' if self.ud_suffix else ''
        return (f'FloatingPointLiteral({self.kind.name}, {self.value}, {self.post_dot_digits}, '
                f'{self.exponent}, {self.radix}{ud_suffix_part})')


@dataclass(slots=True)
class ElaboratedEncoding:
    '''Details of an encoding of a string or character literal.'''
    # The encoding prefix of a string literal
    encoding: Encoding
    # The element (character type) kind
    char_kind: IntegerKind
    # The size, in bytes, of an element
    char_size: int
    # If char_kind, the element type, is unsigned
    is_unsigned: bool
    # If the encoding is little-endian
    is_little_endian: bool
    # The execution character set
    charset: Charset
    # A function that packs an unsigned value into bytes of the correct endianness
    pack: any
    # A function that unpacks part of an image into an unsigned value
    unpack: any

    @classmethod
    def for_encoding_and_interpreter(cls, encoding, interpreter):
        # FIXME: pre-calculate these
        target = interpreter.pp.target
        is_little_endian = target.is_little_endian
        basic = encoding.basic_encoding()
        if basic == Encoding.NONE:
            charset = target.narrow_charset
        elif basic == Encoding.WIDE:
            charset = target.wide_charset
        elif basic == Encoding.UTF_8:
            charset = Charset.from_name('UTF-8')
        elif basic == Encoding.UTF_16:
            charset = Charset.from_name('UTF-16LE' if is_little_endian else 'UTF-16BE')
        elif basic == Encoding.UTF_32:
            charset = Charset.from_name('UTF-32LE' if is_little_endian else 'UTF-32BE')
        else:
            raise RuntimeError('unimplemented encoding')

        kind = encoding.integer_kind()
        is_unsigned = target.is_unsigned(kind)
        char_size = target.integer_width(kind) // 8
        ps = packing_struct(char_size, is_little_endian)
        return cls(encoding, kind, char_size, is_unsigned, is_little_endian, charset,
                   ps.pack, ps.unpack)

    @classmethod
    def for_filename_encoding(cls):
        '''Appropriate values for encoding a string literal to filename bytes.  We store these as
        internally as bytes; characters in the string literal should be stored as UTF-8.
        '''
        return cls(Encoding.NONE, IntegerKind.uchar, 1, True, True, Charset.from_name('UTF-8'),
                   structB.pack, structB.unpack)


@dataclass(slots=True)
class StringLiteral:
    # The string literal as encoded in memory for the target machine, including the
    # terminating NUL.  An erroneous string literal is of zero length.
    image: bytearray
    # Information about the image's encoding.  None if erroneous.
    encoding: ElaboratedEncoding
    # An instance of UserDefinedSuffix, or None
    ud_suffix: object

    @classmethod
    def erroneous_literal(cls):
        return cls(bytearray(), None, None)

    def char_count(self):
        return len(self.image) // self.encoding.char_size

    def char_value(self, n):
        '''Return the unsigned value of the 'n'th character.'''
        assert 0 <= n < self.char_count()
        char_size = self.encoding.char_size
        value, = self.encoding.unpack(self.image[n * char_size: (n + 1) * char_size])
        return value

    def to_short_text(self):
        if self.encoding:
            type_name = self.encoding.char_kind.name
        else:
            type_name = IntegerKind.error.name
        if self.ud_suffix:
            ud_suffix = self.ud_suffix.ident.spelling.decode()
            return f'StringLiteral({bytes(self.image)}, {type_name}, {ud_suffix})'
        else:
            return f'StringLiteral({bytes(self.image)}, {type_name})'


def printable_form(cp):
    '''Like printable_char, except that C escape sequences are used if available.'''
    letter = CONTROL_CHARACTER_LETTERS.get(cp)
    if letter:
        return '\\' + letter
    return printable_char(cp)


#
# Implementation
#

struct_le_L = Struct('<L')
struct_le_H = Struct('<H')
struct_be_L = Struct('>L')
struct_be_H = Struct('>H')
structB = Struct('B')


class NumericEscapeKind(IntEnum):
    NONE = 0
    OCTAL = 1
    HEXADECIMAL = 2

def packing_struct(octets, is_little_endian):
    if octets == 1:
        return structB
    if octets == 2:
        return struct_le_H if is_little_endian else struct_be_H
    assert octets == 4
    return struct_le_L if is_little_endian else struct_be_L


class LiteralInterpreter:

    def __init__(self, pp, pp_arithmetic):
        target = pp.target
        self.pp = pp
        self.pp_arithmetic = pp_arithmetic
        self.permit_named_universal_characters = pp.language.permit_named_universal_characters()
        self.permit_braced_escape_sequences = pp.language.permit_braced_escape_sequences()

        if pp.language.is_cxx():
            self.plain_char_kind = IntegerKind.char
            self.permit_long_long = pp.language.year >= 2011
            self.permit_size_suffix = pp.language.year >= 2023
            # At some point these suffixes should become conditionally supported in the
            # preprocessor configuration
            self.permit_fixed_width_float_suffixes = pp.language.year >= 2023
            self.permit_bit_precise_suffix = False
            self.permit_decimal_suffixes = False
            self.permit_ud_suffix = not pp_arithmetic
        else:
            self.plain_char_kind = IntegerKind.int
            self.permit_long_long = pp.language.year >= 1999
            self.permit_size_suffix = False
            self.permit_fixed_width_float_suffixes = False
            self.permit_bit_precise_suffix = pp.language.year >= 2023
            # At some point these suffixes should become conditionally supported in the
            # preprocessor configuration
            self.permit_decimal_suffixes = pp.language.year >= 2023
            self.permit_ud_suffix = False

        self.integer_precisions = [
            (IntegerKind.int, target.int_width - 1),
            (IntegerKind.uint, target.int_width),
            (IntegerKind.long, target.long_width - 1),
            (IntegerKind.ulong, target.long_width),
        ]
        if self.permit_long_long:
            self.integer_precisions.extend([
                (IntegerKind.long_long, target.long_long_width - 1),
                (IntegerKind.ulong_long, target.long_long_width),
            ])
        n = [kind for kind, _precision in self.integer_precisions].index(target.size_t_kind)
        assert n % 2 == 1
        self.size_t_precisions = self.integer_precisions[n - 1: n + 1]

    def integer_width(self, kind):
        target = self.pp.target
        if self.pp_arithmetic:
            return target.pp_arithmetic_width()
        else:
            return target.integer_width(kind)

    def empty_string_literal(self, encoding):
        encoding = ElaboratedEncoding.for_encoding_and_interpreter(encoding, self)
        return StringLiteral(bytearray(), encoding, None)

    def mb_might_neq_wc(self):
        def value(encoding, c):
            target = self.pp.target
            elab = ElaboratedEncoding.for_encoding_and_interpreter(encoding, self)
            image = elab.charset.encoder(c)
            char_size = target.integer_width(elab.char_kind) // 8
            struct = packing_struct(char_size, target.is_little_endian)
            value, = struct.unpack(image[:char_size])
            return value

        return value(Encoding.NONE, 'a') != value(Encoding.WIDE, 'a')

    def diag(self, state, did, args=None):
        self.emit(state, Diagnostic(did, state.token_loc, args))

    def diag_char(self, state, did, start, args=None):
        self.diag_char_range(state, did, start, start + 1, args)

    def diag_char_range(self, state, did, start, end, args=None):
        self.emit(state, Diagnostic(did, SpellingRange(state.token_loc, start, end), args))

    def emit(self, state, diagnostic):
        if not state.is_erroneous:
            state.is_erroneous = self.pp.emit(diagnostic)

    def interpret(self, token):
        '''Return an IntegerLiteral or FloatingPointLiteral instance.'''
        if token.kind == TokenKind.NUMBER:
            return self.interpret_number(token)
        else:
            return self.interpret_character_literal(token)

    #
    # Numeric literals
    #

    def interpret_number(self, token):
        '''Read a numeric literal and return an IntegerLiteral or FloatingPointLiteral.'''
        state = State.from_pp_number(token)
        first = state.get_byte(0)
        length = len(state.spelling)

        # Fast track single digits - they form about 90% of numeric literals
        if length == 1:
            result = IntegerLiteral(IntegerKind.int, first - 48, None)
        else:
            # Binary literals, octal literals and hexadecimal literals all begin with 0,
            # but what starts out looking like an octal integer literal can turn out to be
            # a decimal floating point literal.  Decimal literals begin with 1-9.  If the
            # first character is a dot it is a decimal floating point literal.  What
            # starts looking like a decimal or hexadecimal integer literal can turn out to
            # be a floating point literal in the presence of decimal points or exponents.
            #
            # If user-defined suffixes are a thing, then we also have to be careful when
            # advancing past a letter that might begin one.  For example, "0b2m" should be
            # read as 0 with a ud-suffix of "b2m", and not an invalid binary number.  This
            # consideration applies to consuming the 0x prefix, the 0b prefix, and also
            # possible exponents 'e' in floating point numbers, e.g., 12ergs.
            radix = 10
            if first == 48:
                second = state.get_byte(1)
                third = state.get_byte(2)
                if self.permit_ud_suffix:
                    third_value = HEX_DIGIT_VALUES.get(third, -1)
                else:
                    third_value = 0
                # 'B' 'b' for binary literals
                if second in (66, 98) and 0 <= third_value < 2:
                    radix = 2
                    cursor, value, _count = self.read_radix_digits(state, 2, 2, True)
                    result = IntegerLiteral(IntegerKind.int, value, None)
                # 'X' 'x' for hexadecimal literals followed by a hex digit or '.'
                elif second in (88, 120) and (0 <= third_value < 16 or third == 46):
                    radix = 16
                    cursor, result = self.read_possible_floating_point(state, 2, 16)
                elif state.first_non_decimal_digit(1) in (46, 69, 101):  # '.' 'E' 'e'
                    cursor, result = self.read_possible_floating_point(state, 0, 10)
                else:
                    radix = 8
                    cursor, value, _count = self.read_radix_digits(state, 1, 8, False)
                    result = IntegerLiteral(IntegerKind.int, value, None)
            else:
                cursor, result = self.read_possible_floating_point(state, 0, 10)

            # A suffix is either a standard-defined suffix or a user-defined suffix (when
            # permitted).  The two are distinct and cannot co-exist, so "1UC" is a
            # user-defined suffix of UC, not a standard suffix of U and a user-defined
            # suffix of C.
            is_floating_point = isinstance(result, FloatingPointLiteral)
            if is_floating_point:
                kind = self.read_floating_point_suffix(state, cursor)
            else:
                kind = self.read_integer_suffix(state, cursor, result.value, radix)

            if kind is None:
                ud_suffix = None
                suffix = state.spelling[cursor:]
                if self.permit_ud_suffix:
                    ud_suffix = self.pp.maybe_identifier(suffix)
                    if ud_suffix:
                        loc = SpellingRange(token.loc, cursor, length)
                        result.ud_suffix = UserDefinedSuffix(ud_suffix, loc)
                if ud_suffix is None:
                    self.diag_char_range(state, DID.invalid_numeric_suffix, cursor, length,
                                         [1 if is_floating_point else 0])
            else:
                result.kind = kind

            if state.is_erroneous:
                result = result.erroneous_literal()

        return result

    def interpret_line_number(self, token, max_value):
        '''Interpret the line number token of a #line directive and issue diagnostics for
        erroneous values.  It shall be a digit-sequence - i.e. digits 0-9 with optional '
        separators.
        '''
        if token.kind != TokenKind.NUMBER:
            self.pp.diag(DID.line_number_must_be_digit_sequence, token.loc)
            return -1
        state = State.from_pp_number(token)
        cursor, line_number, _ = self.read_radix_digits(state, 0, 10, True)
        if cursor != state.limit:
            self.diag_char(state, DID.line_number_must_be_digit_sequence, cursor)
        if line_number == 0 or line_number > max_value:
            self.diag(state, DID.line_number_out_of_range, [f'{max_value:,d}'])
        return -1 if state.is_erroneous else line_number

    def interpret_filename(self, token):
        '''Interpret a filename for a #line directive.  Return a byte string on success, or None
        on error.

        The token must be a string literal.  Encoding prefixes and user-defined suffixes
        are rejected.  No attempt is made to interpret escape sequences, etc.
        '''
        if token.kind != TokenKind.STRING_LITERAL:
            self.pp.diag(DID.filename_should_be_string, token.loc)
            return None

        spelling, ud_suffix = token.extra
        if TokenFlags.get_encoding(token.flags) != Encoding.NONE:
            selector = 0
        elif ud_suffix:
            selector = 1
        else:
            selector = None

        if selector is not None:
            self.pp.diag(DID.invalid_in_filename, self.string_spelling_range(token, selector),
                         [selector])
            return None

        encoding = ElaboratedEncoding.for_filename_encoding()
        literal = StringLiteral(bytearray(), encoding, None)
        state = self.encode_and_append(literal, token)
        if state.is_erroneous:
            return None
        return bytes(literal.image)

    def read_radix_digits(self, state, cursor, base, require_digit):
        '''Return a triple (cursor, value, count).  Count is the number of digits consumed
        (which need not be the number of characters, because of the apostrophe).'''
        count = 0
        value = 0
        while True:
            digit = state.get_byte(cursor)
            if digit == 39:  # "'"
                cursor += 1
                digit = state.get_byte(cursor)
                require_digit = True
            dvalue = HEX_DIGIT_VALUES.get(digit, -1)
            # If this is a digit but not valid for the radix, it is an error only if we
            # require a digit, or it is a decimal digit outside the range of binary or
            # octal because a digit cannot begin a literal suffix.
            if not 0 <= dvalue < base:
                if require_digit or 2 <= dvalue < 10:
                    self.diag_char(state, DID.invalid_digit, cursor, [bases.index(base)])
                return cursor, value, count
            cursor += 1
            count += 1
            value = value * base + dvalue
            require_digit = False

    def read_possible_floating_point(self, state, cursor, base):
        '''Return a (cursor, literal) pair, where literal is an instance of IntegerLiteral or
        FloatingPointLiteral.
        '''
        fraction = None
        value = 0

        # Read integer value, if any
        if state.get_byte(cursor) != 46:  # '.'
            cursor, value, _count = self.read_radix_digits(state, cursor, base, True)

        # Read fraction value, if any
        if state.get_byte(cursor) == 46:  # '.'
            cursor, fraction, count = self.read_radix_digits(state, cursor + 1, base, False)
            value = value * pow(base, count) + fraction

        # Read exponent, if any.  There must be an exponent for hexadecimal floats.
        cursor, exponent = self.read_exponent(state, cursor, base,
                                              fraction is not None and base == 16)

        if fraction is None and exponent is None:
            result = IntegerLiteral(IntegerKind.int, value, None)
        elif self.pp_arithmetic:
            self.diag(state, DID.floating_point_in_pp_expr)
            result = IntegerLiteral.erroneous_literal()
        else:
            post_dot_digits = 0 if fraction is None else count
            exponent = exponent or 0
            result = FloatingPointLiteral(RealKind.double, value, post_dot_digits,
                                          exponent, base, None)
        return cursor, result

    def read_exponent(self, state, cursor, base, require_exponent):
        cp = state.get_byte(cursor)
        if base == 16:
            is_exponent = cp in (80, 112)  # 'P' 'p'
        else:
            is_exponent = cp in (69, 101)  # 'E' 'e'

        if is_exponent and not require_exponent and self.permit_ud_suffix:
            # Only treat as an exponent if it is lexically an exponent: look for a sign
            # (which cannot appear in a user-defined suffix) or a decimal digit.
            next_cp = state.get_byte(cursor + 1)
            if not (48 <= next_cp <= 57) and next_cp not in (43, 45):  # '+' '-'
                is_exponent = False

        if is_exponent:
            cursor += 1
            sign = state.get_byte(cursor)
            if sign in (43, 45):   # '+' '-'
                cursor += 1
            cursor, value, _count = self.read_radix_digits(state, cursor, 10, True)
            exponent = -value if sign == 45 else value
        else:
            exponent = None
            if require_exponent:
                self.diag_char(state, DID.hexadecimal_exponent_required, cursor)

        return cursor, exponent

    def read_integer_suffix(self, state, cursor, value, radix):
        '''Return an integer kind for the literal based on the suffix or lack thereof.'''
        is_unsigned = False
        suffix = ''

        # Unsigned prefix?
        letter = state.get_byte(cursor)
        if letter in (85, 117):  # 'U' 'u'
            is_unsigned = True
            cursor += 1
            letter = state.get_byte(cursor)

        if letter in (90, 122) and self.permit_size_suffix:   # 'Z' 'z'
            suffix = 'Z'
            cursor += 1
        elif letter in (76, 108):  # 'L' 'l'
            cursor += 1
            # Must be same case
            if state.get_byte(cursor) == letter:
                cursor += 1
                suffix = 'LL'
            else:
                suffix = 'L'
        elif letter in (87, 119) and self.permit_bit_precise_suffix:  # 'W' 'w'
            if state.get_byte(cursor + 1) == letter - 21:
                cursor += 2
                suffix = 'WB'

        # Unsigned suffix?
        if not is_unsigned:
            letter = state.get_byte(cursor)
            if letter in (85, 117):  # 'U' 'u'
                is_unsigned = True
                cursor += 1

        kind = None
        if cursor == state.limit:
            kind = self.integer_kind_for_suffix(value, radix, is_unsigned, suffix)
            if kind == IntegerKind.error:
                self.diag(state, DID.integer_too_large)
        return kind

    def integer_kind_for_suffix(self, value, radix, is_unsigned, suffix):
        '''This function implements the logic of Table 8 — Types of integer-literals
        [tab:lex.icon.type].
        '''
        if suffix == 'WB':
            return IntegerKind.ubit_precise if is_unsigned else IntegerKind.bit_precise

        if suffix == 'Z':
            precisions = self.size_t_precisions
            start = 0
        else:
            precisions = self.integer_precisions
            start = ['', 'L', 'LL'].index(suffix) * 2
        start += int(is_unsigned)
        step = 2 if (radix == 10 or is_unsigned) else 1

        width = value_width(value)
        for n in range(start, len(precisions), step):
            kind, precision = precisions[n]
            if precision >= width:
                return kind
        return IntegerKind.error

    # floating-point-suffix: one of
    #     f l
    #     f16 f32 f64 f128 bf16 F L F16 F32 F64 F128 BF16    [from C++23]
    #     dd df dl DD DF DL                                  [from C23]
    def read_floating_point_suffix(self, state, cursor):
        '''If the remaining text beginning at cursor is a floating point suffix (including the
        case of no suffix), return an instance of RealKind.  Otherwise return None.
        '''
        kind = RealKind.double
        letter = state.get_byte(cursor)

        if letter in (70, 102):  # 'F' 'f'
            cursor += 1
            kind = RealKind.float
            if self.permit_fixed_width_float_suffixes:
                two = state.get_bytes(cursor, 2)
                if two == b'16':
                    cursor += 2
                    kind = RealKind.float16_t
                elif two == b'32':
                    cursor += 2
                    kind = RealKind.float32_t
                elif two == b'64':
                    cursor += 2
                    kind = RealKind.float64_t
                elif two == b'12' and state.get_byte(cursor + 2) == 56:  # '8'
                    cursor += 3
                    kind = RealKind.float128_t
        elif letter in (76, 108):  # 'L' 'l'
            cursor += 1
            kind = RealKind.long_double
        elif letter in (68, 100) and self.permit_decimal_suffixes:  # 'D' 'd'
            next_c = state.get_byte(cursor + 1)
            if next_c == letter:                   # 'D' or 'd' of same case
                kind = RealKind.decimal32_t
                cursor += 2
            elif next_c == letter + 2:             # 'F' or 'f' of same case
                kind = RealKind.decimal64_t
                cursor += 2
            elif next_c == letter + 8:             # 'L' or 'l' of same case
                kind = RealKind.decimal128_t
                cursor += 2
        elif letter in (66, 98) and self.permit_fixed_width_float_suffixes:  # 'B' 'b'
            if (state.get_byte(cursor + 1) == letter + 4       # 'f' or 'F' of same case
                    and state.get_bytes(cursor + 2, 2) == b'16'):
                cursor += 4
                kind = RealKind.bfloat16_t

        return kind if cursor == state.limit else None

    #
    # Character literals
    #

    def interpret_character_literal(self, token):
        '''Return an IntegerLiteral representing the value of the character literal.'''
        assert token.kind == TokenKind.CHARACTER_LITERAL
        # First, encode the characters as for string literals
        encoding = TokenFlags.get_encoding(token.flags)
        result = self.empty_string_literal(encoding)

        state = self.encode_and_append(result, token)
        # Reject user-defined suffixes in preprocessor expressions.
        if result.ud_suffix and self.pp_arithmetic:
            self.emit(state, Diagnostic(DID.user_defined_suffix_in_pp_expr, result.ud_suffix.loc))

        # C is much more relaxed than C++.  The differences are that a) the type of an
        # unsuffixed character literal is 'int' b) Wide multicharacter literals must be
        # accepted with an implementation-defined value.  c) Literals with no encoding
        # prefix or with the L prefix, must accept characters that encode to multiple code
        # units.  d) Wide and unsuffixed character and string literals must accept
        # characters not in the target charset with an implementation-defined value
        did = None
        count = result.char_count()
        if count <= 1:
            if count == 0:
                self.diag(state, DID.empty_character_literal)
                value = 0
            else:
                value = result.char_value(0)
            value_kind = result.encoding.char_kind
            if encoding == Encoding.NONE:   # In C, it's an 'int'.
                result_kind = self.plain_char_kind
            else:
                result_kind = value_kind
        else:
            # In C++ a multicharacter literal shall not have an encoding prefix.  If it
            # contains a c-char that is not encodable as a single code unit in the
            # ordinary literal encoding, the program is ill-formed.
            value = 0
            value_kind = result_kind = IntegerKind.int
            if encoding != Encoding.NONE:
                did = DID.multicharacter_literal_with_prefix
            else:
                did = DID.multicharacter_literal
                width = result.encoding.char_size * 8
                for n in range(count):
                    value = (value << width) + result.char_value(n)

        # Truncate the value if necessary (only happens if multichar) and treat it as signed
        # if the value kind is signed
        limit = 1 << self.integer_width(value_kind)
        if value >= limit:
            did = DID.multicharacter_literal_truncated
            value &= limit - 1
        if not self.pp.target.is_unsigned(value_kind) and value & (limit >> 1):
            value -= limit

        if did:
            self.diag(state, did)

        if state.is_erroneous:
            return IntegerLiteral.erroneous_literal()
        else:
            return IntegerLiteral(result_kind, value, result.ud_suffix)

    #
    # String literals
    #

    def string_spelling_range(self, token, selector):
        '''Returns a SpellingRange for a string token's encoding prefix (if selector is 0)
        or user-defined suffix (if selector is 1).'''
        spelling, _ = token.extra
        body, limit = find_literal_body(spelling)
        if selector == 0:
            return SpellingRange(token.loc, 0, body - 1)
        else:
            return SpellingRange(token.loc, limit + 1, len(spelling))

    def concatenate_strings(self, token):
        '''Concatenate and interpret consecutive string literals beginning with token.

        Return a pair (literal, next_token).  literal is an instance of StringLiteral, and
        next_token is the first token consumed that was not a string literal.
        '''
        def diagnose_conflict(token, bad_tokens, selector):
            args = [selector]
            args.extend([Diagnostic(DID.string_concatenation_prior,
                                    self.string_spelling_range(bad_token, selector), [selector])
                         for bad_token in bad_tokens])
            diag_loc = self.string_spelling_range(token, selector)
            self.pp.diag(DID.string_concatenation_conflict, diag_loc, args)

        assert token.kind == TokenKind.STRING_LITERAL
        assert not self.pp_arithmetic

        # Read adjacent string literal tokens.  This leaves the next non-string-literal
        # token in token.
        is_erroneous = False
        tokens = []
        while token.kind == TokenKind.STRING_LITERAL:
            tokens.append(token)
            token = self.pp.get_token()
        next_token = token

        # Determine a common encoding prefix and user-defined suffix.  All concatenated
        # tokens with an encoding prefix must have the same one, and similarly all with a
        # user-defined suffix must have the same one.
        encodings = []
        ud_suffixes = []
        for token in tokens:
            encoding = TokenFlags.get_encoding(token.flags)
            if encoding != Encoding.NONE:
                encodings.append((token, encoding))
            _, ud_suffix = token.extra
            if ud_suffix:
                ud_suffixes.append((token, ud_suffix))

        common_encoding = Encoding.NONE
        if encodings:
            token, common_encoding = encodings[-1]
            bad_tokens = [token for token, encoding in encodings if encoding != common_encoding]
            if bad_tokens:
                diagnose_conflict(token, bad_tokens, 0)
                is_erroneous = True

        common_ud_suffix = None
        if ud_suffixes:
            token, common_ud_suffix = ud_suffixes[-1]
            bad_tokens = [token for token, ud_suffix in ud_suffixes
                          if ud_suffix != common_ud_suffix]
            if bad_tokens:
                diagnose_conflict(token, bad_tokens, 1)
                is_erroneous = True
            common_ud_suffix = UserDefinedSuffix(common_ud_suffix,
                                                 self.string_spelling_range(token, 1))

        # Now loop through the tokens and encode them.
        result = self.empty_string_literal(common_encoding)
        for token in tokens:
            state = self.encode_and_append(result, token)
            is_erroneous = is_erroneous or state.is_erroneous
        result.ud_suffix = common_ud_suffix

        if is_erroneous:
            result = result.erroneous_literal()
        else:
            # Revert to initial shift state, and append a NUL byte to the string.
            encoder = result.encoding.charset.encoder
            result.image.extend(encoder('', final=True) + encoder('\0'))

        return result, next_token

    def encode_and_append(self, string_literal, token):
        '''Encode the delimited literal token onto the end of string_literal.  Return a state
        object.
        '''
        state, cursor, string_literal.ud_suffix = State.from_delimited_literal(token)
        single_code_unit = token.kind == TokenKind.CHARACTER_LITERAL
        image = string_literal.image
        encoding = string_literal.encoding
        encoding_prefix = encoding.encoding
        char_size = encoding.char_size
        mask = (1 << char_size * 8) - 1
        is_raw = encoding_prefix.is_raw()
        encoder = encoding.charset.encoder

        # Decide on a replacement character.  For unicode encodings, unless single byte and
        # for a character literal, use the replacement character.  Otherwise use '?'
        if encoding_prefix.is_unicode() and (char_size > 1 or not single_code_unit):
            replacement_char = REPLACEMENT_CHAR
        else:
            replacement_char = 63   # '?'

        while cursor < state.limit:
            # Erroneous characters or values are returned as -1
            start = cursor
            cp, cursor = state.get_char(cursor)
            if cp == 92 and not is_raw:   # '\\'
                cp, cursor, kind = self.escape_sequence_or_UCN(state, cursor)
            else:
                kind = NumericEscapeKind.NONE

            if kind != NumericEscapeKind.NONE:
                # Numeric escapes are good as long as their value fits in width bits.
                if cp > mask:
                    args = [kind - 1, f'0x{cp:02X}', encoding.char_kind.name]
                    self.diag_char_range(state, DID.escape_sequence_value_too_large,
                                         start, cursor, args)
                    cp &= mask
                elif cp == -1:
                    # Replace erroneous values with 0.
                    cp = 0
                image_part = encoding.pack(cp)
            else:
                # Characters must have their codepoints validated.
                if cp != -1:
                    did = validate_codepoint(cp)
                    if did:
                        self.diag_char_range(state, did, start, cursor, [codepoint_to_hex(cp)])
                        cp = -1
                # Replace erroneous values.
                if cp == -1:
                    cp = replacement_char
                # Convert the character into the execution character set.  This can fail
                # (if the character is not available).  It can result in 1 or more
                # encoded codepoints in the execution character set.
                #
                # [lex.ccon 3.1]: If the specified character lacks representation in the
                # literal’s associated character encoding or if it cannot be encoded as a
                # single code unit, then the program is ill-formed.
                try:
                    image_part = encoder(chr(cp))
                except UnicodeError:
                    args = [printable_char(cp), encoding.charset.name]
                    self.diag_char_range(state, DID.character_does_not_exist, start, cursor, args)
                    image_part = encoder(chr(replacement_char))

                # Only take the trailing bytes of the encoding if it is too wide to fit in
                # a single c-char for character constants
                if single_code_unit and len(image_part) > char_size:
                    args = [printable_char(cp), encoding.char_kind.name, encoding.charset.name]
                    self.diag_char_range(state, DID.character_not_single_code_unit,
                                         start, cursor, args)
                    image_part = image_part[-char_size:]

            image.extend(image_part)

        return state

    def escape_sequence_or_UCN(self, state, cursor):
        '''We have just read a backslash.'''
        backslash_loc = cursor - 1
        cp, cursor = state.get_char(cursor)
        assert cp != -1

        # Handle simple escape sequences
        escape_cp = SIMPLE_ESCAPES.get(cp)
        if escape_cp is not None:
            return escape_cp, cursor, NumericEscapeKind.NONE

        # Handle old-style octal escape sequences
        if cp in OCTAL_DIGITS:
            kind = NumericEscapeKind.OCTAL
            cp = HEX_DIGIT_VALUES[cp]
            # Read up to two more octal digits
            for _ in range(2):
                c, ncursor = state.get_char(cursor)
                if c not in OCTAL_DIGITS:
                    break
                cursor = ncursor
                cp = cp * 8 + HEX_DIGIT_VALUES[c]
        elif cp == 111 and self.permit_braced_escape_sequences:  # 'o'
            kind = NumericEscapeKind.OCTAL
            cp, cursor = self.braced_escape_sequence(state, cursor, 8)
        elif cp == 120:  # 'x'
            kind = NumericEscapeKind.HEXADECIMAL
            cp, cursor = self.hexadecimal_escape_sequence(state, cursor)
        else:
            kind = NumericEscapeKind.NONE
            cp, cursor, is_ucn = self.maybe_ucn(state, cursor, cp)
            if not is_ucn:
                self.diag_char_range(state, DID.unrecognized_escape_sequence, backslash_loc,
                                     cursor, ['\\' + printable_form(cp)])

        return cp, cursor, kind

    def hexadecimal_escape_sequence(self, state, cursor):
        if state.get_byte(cursor) == 123 and self.permit_braced_escape_sequences:  # '{'
            return self.braced_escape_sequence(state, cursor, 16)

        count = 0
        value = 0
        start_loc = cursor

        while True:
            c_cursor = cursor
            c, cursor = state.get_char(cursor)
            cvalue = HEX_DIGIT_VALUES.get(c)
            if cvalue is not None:
                value = value * 16 + cvalue
                count += 1
                continue
            if c == -1:
                break
            cursor = c_cursor
            break

        if count == 0:
            self.diag_char_range(state, DID.missing_digit_sequence, start_loc, cursor, [1])
            value = -1

        return value, cursor

    def braced_escape_sequence(self, state, cursor, radix):
        assert radix in (8, 16)
        brace_loc = cursor
        c, cursor = state.get_char(cursor)
        if c != 123:  # '{'
            self.diag_char(state, DID.expected_open_brace, brace_loc)
            return -1, cursor

        count = 0
        value = 0
        while True:
            c_cursor = cursor
            c, cursor = state.get_char(cursor)
            cvalue = HEX_DIGIT_VALUES.get(c, -1)
            if 0 <= cvalue < radix:
                if value != -1:
                    value = value * radix + cvalue
                count += 1
                continue

            if c == 125:  # '}'
                if count == 0 and value != -1:
                    self.diag_char_range(state, DID.missing_digit_sequence, brace_loc, cursor,
                                         [int(radix == 16)])
                    value = -1
                return value, cursor

            if c == -1:
                self.expected_close_brace(state, cursor, brace_loc)
                return -1, cursor

            if value != -1:
                self.diag_char(state, DID.invalid_digit, c_cursor, [bases.index(radix)])
                value = -1

    def maybe_ucn(self, state, cursor, cp):
        '''On entry, a backslash has been consumed.  Return a triple (cp, cursor).

        If lexically the backslash does not form a UCN, then cp is -1.  If lexically it
        does form a UCN, then the codepoint is checked for validity in context.  If the
        codepoint is valid it is returned, otherise -1 is returned.
        '''
        is_ucn = True
        if cp == 85 or cp == 117:  # 'U' 'u'
            cp, cursor = self.hex_ucn(state, cursor, cp == 85)
        elif cp == 78 and self.permit_named_universal_characters:  # 'N'
            cp, cursor = self.named_character(state, cursor)
        else:
            is_ucn = False
        return cp, cursor, is_ucn

    def hex_ucn(self, state, cursor, is_U):
        # '{'
        if not is_U and state.get_byte(cursor) == 123 and self.permit_braced_escape_sequences:
            return self.braced_escape_sequence(state, cursor, 16)

        count = 0
        cp = 0
        limit = 8 if is_U else 4
        while True:
            c_cursor = cursor
            c, cursor = state.get_char(cursor)
            cvalue = HEX_DIGIT_VALUES.get(c)
            if cvalue is not None:
                cp = cp * 16 + cvalue
                count += 1
                if count != limit:
                    continue
                break

            self.diag_char(state, DID.invalid_digit, c_cursor, [3])
            cp = -1
            break

        return cp, cursor

    def named_character(self, state, cursor):
        brace_loc = cursor
        c, cursor = state.get_char(cursor)
        if c != 123:  # '{'
            self.diag_char(state, DID.expected_open_brace, brace_loc)
            return -1, brace_loc

        name = ''
        name_loc = cursor
        while True:
            c, cursor = state.get_char(cursor)
            if c == 125:  # '}'
                cp = name_to_cp(name)
                if cp == -1:
                    self.diag_char_range(state, DID.unrecognized_universal_character_name,
                                         name_loc, max(name_loc + 1, cursor - 1), [name])
                return cp, cursor

            if c == -1:
                self.expected_close_brace(state, cursor, brace_loc)
                return -1, cursor

            name += chr(c)

    def expected_close_brace(self, state, cursor, brace_loc):
        loc = SpellingRange(state.token_loc, brace_loc, brace_loc + 1)
        note = Diagnostic(DID.prior_match, loc, ['{'])
        self.diag_char(state, DID.expected_close_brace, cursor, [note])


@dataclass(slots=True)
class State:
    '''Maintains state whilst interpreting a literal.'''

    token_loc: int
    spelling: bytes      # UTF-8 encoded
    limit: int
    is_erroneous: bool

    @classmethod
    def from_pp_number(cls, token):
        '''Token is a preprocessing number.  Return a State object.'''
        spelling, ud_suffix = token.extra
        limit = len(spelling)
        # Sanity check
        assert ud_suffix is None
        assert limit and 48 <= spelling[0] <= 57 or (spelling[0] == 46 and limit > 1)
        return cls(token.loc, spelling, limit, False)

    @classmethod
    def from_delimited_literal(cls, token):
        '''Token is a string or character literal.  Return a (state, cursor, ud_suffix) tuple.'''
        spelling, ud_suffix = token.extra
        length = len(spelling)
        assert length >= 2
        body, limit = find_literal_body(spelling)
        if ud_suffix:
            suffix_loc = SpellingRange(token.loc, limit + 1, length)
            ud_suffix = UserDefinedSuffix(ud_suffix, suffix_loc)
        return cls(token.loc, spelling, limit, False), body, ud_suffix

    def first_non_decimal_digit(self, cursor):
        '''Return the first character starting from cursor that is not a decimal digit.'''
        while True:
            c = self.get_byte(cursor)
            if not (48 <= c <= 57):
                return c
            cursor += 1

    def get_byte(self, cursor):
        if cursor < self.limit:
            return self.spelling[cursor]
        return -1

    def get_bytes(self, cursor, length):
        result = bytearray()
        for n in range(length):
            c = self.get_byte(cursor + n)
            if c == -1:
                break
            result.append(c)
        return bytes(result)

    def get_char(self, cursor):
        if cursor < self.limit:
            cp, size = utf8_cp(self.spelling, cursor)
            assert cp != -1 or size == 0
            return cp, cursor + size
        return -1, cursor


def destringize(token):
    assert token.kind == TokenKind.STRING_LITERAL
    spelling, _ = token.extra
    cursor, limit = find_literal_body(spelling)
    result = bytearray()
    while cursor < limit:
        if spelling[cursor] == 92 and spelling[cursor + 1] in (34, 92):
            cursor += 1
        result.append(spelling[cursor])
        cursor += 1
    return bytes(result)


def find_literal_body(spelling):
    delim = 0
    while spelling[delim] not in (34, 39):  # '"' and "'"
        delim += 1

    limit = len(spelling) - 1
    while spelling[limit] != spelling[delim]:
        limit -= 1

    # Raw string literals need to find the parentheses
    if delim and spelling[delim - 1] == 82:  # 'R'
        while spelling[delim] != 40:   # '('
            delim += 1
        while spelling[limit] != 41:   # ')'
            limit -= 1

    assert delim < limit, spelling
    return delim + 1, limit


def validate_codepoint(cp):
    if is_surrogate(cp):
        return DID.codepoint_surrogate
    if not is_valid_codepoint(cp):
        return DID.codepoint_invalid
    return None
