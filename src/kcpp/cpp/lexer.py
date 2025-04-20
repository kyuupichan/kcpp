# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#

from dataclasses import dataclass
from enum import IntEnum, auto

from ..core import Token, TokenKind, TokenFlags, SpecialKind
from ..diagnostics import BufferRange, Diagnostic, DID
from ..unicode import (
    name_to_cp, utf8_cp, REPLACEMENT_CHAR, is_NFC, is_valid_codepoint,
    is_control_character, codepoint_to_hex, is_XID_Start, is_XID_Continue,
    is_surrogate
)

from .literals import printable_form, HEX_DIGIT_VALUES

__all__ = ['Lexer']


ASCII_DIGITS = set(b'0123456789')
ASCII_IDENT_START = set(b'ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz')
ASCII_IDENT_CONTINUE = set.union(ASCII_IDENT_START, ASCII_DIGITS)
SIGNS = set(b'+-')
EPep = set(b'EPep')
NL_WS = set(b'\r\n')
NON_NL_WS = set(b' \t\v\f')
DELIMITER_INVALID = set(b' ()\\\t\v\f\r\n')


class CharacterKind(IntEnum):
    '''Categorizes a source file character.'''
    # A valid character that can start or continue an identifier
    valid_start = auto()
    # A valid character that can continue and identifier
    valid_continue = auto()
    # A valid character that cannot appear in identifiers
    valid_other = auto()
    # A lexically well-formed UCN sequence that is invalid
    invalid_ucn = auto()
    # Invalid UTF-8 - bad encoding, surrogate or over-long
    invalid_utf8 = auto()


@dataclass(slots=True)
class Character:
    '''A source file character.'''
    # The valid kinds are spelt as their UTF-8 encoding.  invalid UCNs are spelt as their
    # UCN spelling.  Invalid UTF-8 is spelled as the UTF-8 encoding of the replacement
    # character.
    kind: CharacterKind
    value: int                # -1 or a valid Unicode codepoint
    diagnostic: object        # A diagnostic or None.  Only set for invalid_ucn or valid_other.


class Lexer:
    '''A lexer.  It tokenizes a buffer terminated with a NUL.'''

    def __init__(self, pp, buff, start_loc, pp_state):
        assert isinstance(buff, (bytes, bytearray, memoryview))
        assert buff and buff[-1] == 0
        self.pp = pp
        self.buff = buff
        self.start_loc = start_loc
        self.pp_state = pp_state
        self.cursor = 0
        self.is_start_of_line = bool(pp_state)
        self.in_header_name = False
        self.clean = True

    def cursor_loc(self):
        return self.cursor + self.start_loc

    @staticmethod
    def initialize():
        on_char = {}
        for n in range(256):
            on_char[n] = Lexer.on_identifier
        for c in ASCII_DIGITS:
            on_char[c] = Lexer.on_number
        on_char.update({
            0: Lexer.on_nul,
            ord(' '): Lexer.on_ws,
            ord('\t'): Lexer.on_ws,
            ord('\f'): Lexer.on_vertical_ws,
            ord('\v'): Lexer.on_vertical_ws,
            ord('\r'): Lexer.on_nl_ws,
            ord('\n'): Lexer.on_nl_ws,
            ord('{'): Lexer.on_brace_open,
            ord('}'): Lexer.on_brace_close,
            ord('['): Lexer.on_square_open,
            ord(']'): Lexer.on_square_close,
            ord('('): Lexer.on_paren_open,
            ord(')'): Lexer.on_paren_close,
            ord(';'): Lexer.on_semicolon,
            ord('?'): Lexer.on_query,
            ord('~'): Lexer.on_tilde,
            ord('\\'): Lexer.on_backslash,
            ord(','): Lexer.on_comma,
            ord('#'): Lexer.on_hash,
            ord('!'): Lexer.on_not,
            ord('.'): Lexer.on_dot,
            ord('='): Lexer.on_assign,
            ord('*'): Lexer.on_multiply,
            ord('/'): Lexer.on_divide,
            ord('+'): Lexer.on_plus,
            ord('-'): Lexer.on_minus,
            ord('<'): Lexer.on_lt,
            ord('>'): Lexer.on_gt,
            ord(':'): Lexer.on_colon,
            ord('%'): Lexer.on_modulus,
            ord('&'): Lexer.on_bitwise_and,
            ord('|'): Lexer.on_bitwise_or,
            ord('^'): Lexer.on_bitwise_xor,
            ord("'"): Lexer.on_delimited_literal,
            ord('"'): Lexer.on_delimited_literal,
        })
        Lexer.on_char = on_char

    def diag(self, did, loc, args=None):
        self.pp.emit(self.diagnostic(did, loc, args))

    def diag_range(self, did, start, end, args=None):
        '''Diagnose a range of characters.  start and end are byte offsets, and start must not be
        in the middle of a multibyte character.  end >= start must hold.  If end == start,
        the single character at that position is diagnosed.  If end is in the middle of a
        multi-byte character, the diagnosis extends to the end of that character.
        '''
        self.pp.emit(self.diagnostic_range(did, start, end, args))

    def diagnostic(self, did, loc, args=None):
        return Diagnostic(did, loc + self.start_loc, args)

    def diagnostic_range(self, did, start, end, args=None):
        return Diagnostic(did, BufferRange(start + self.start_loc, end + self.start_loc), args)

    def read_logical_byte(self, cursor):
        # Return the next byte skipping escaped newlines
        buff = self.buff
        while True:
            c = buff[cursor]
            cursor += 1
            if c != 92:    # '\\'
                break
            is_nl, ncursor = self.skip_escaped_newline(cursor)
            if not is_nl:
                break
            cursor = ncursor

        return c, cursor

    def skip_escaped_newline(self, cursor):
        # Called after a backslash to skip a single escaped newline.  If the rest of the
        # line is whitespace and a newline sequence, return (True, cursor) where cursor is
        # after the newline sequence.  Otherwise return (False, cursor) where cursor is
        # the cursor passed into the function.
        saved_cursor = cursor
        buff = self.buff
        while True:
            c = buff[cursor]
            cursor += 1
            if c in NON_NL_WS:
                continue
            if c in NL_WS:
                break
            return False, saved_cursor

        d = buff[cursor]
        if c == 13 and d == 10:  # '\r' and '\n'
            cursor += 1
        self.clean = False
        return True, cursor

    def read_char(self, cursor, replacement_char=REPLACEMENT_CHAR):
        '''Return (c, cursor) where c is the extended character at the given cursor posiion.

        If the encoding is invalid, return REPLACEMENT_CHAR and cursor such that progress
        will be made.
        '''
        c, size = utf8_cp(self.buff, cursor)
        assert size
        if c >= 0:
            return c, cursor + size

        if c == -1:
            did = DID.utf8_invalid
        elif c == -2:
            did = DID.utf8_overlong
        elif c == -3:
            did = DID.utf8_surrogate
        else:
            raise RuntimeError

        # Encoding errors are emitted even when skipping
        self.diag_range(did, cursor, cursor + size)
        self.clean = False
        return replacement_char, cursor + size

    def read_logical_char(self, cursor):
        '''Read a logical character, after skipping escaped newlines.'''
        c, cursor = self.read_logical_byte(cursor)
        if c >= 0x80:
            c, cursor = self.read_char(cursor - 1)
        return c, cursor

    def get_token(self):
        # Internally to the lexer, all locations are ofsets in the buffer.  They are
        # adjusted by self.start_loc when interfacing externally - when exiting this
        # function and by Lexer.diag().
        token = Token(-1, 0, 0, None)
        buff = self.buff
        cursor = self.cursor

        while True:
            self.clean = True
            token.loc = cursor  # token.loc is used by some on_char handlers
            c = buff[cursor]
            kind, cursor = self.on_char[c](self, token, cursor + 1)
            if kind != TokenKind.WS:
                break

        self.is_start_of_line = kind == TokenKind.kw_export_keyword
        token.loc += self.start_loc
        token.kind = kind
        self.cursor = cursor
        return token

    def get_token_quietly(self):
        '''Lex a token, without issuing diagnostics, and return it.'''
        self.pp.ignore_diagnostics += 1
        token = self.get_token()
        self.pp.ignore_diagnostics -= 1
        return token

    def peek_token_kind(self):
        '''Peek the next token and return its kind.'''
        cursor = self.cursor
        token = self.get_token_quietly()
        self.cursor = cursor
        return token.kind

    def on_ws(self, token, cursor):
        token.flags |= TokenFlags.WS
        return TokenKind.WS, cursor

    def on_vertical_ws(self, token, cursor):
        if self.pp.in_directive:
            kind = self.buff[cursor - 1] - 11  # 11 = '\v'  12 = '\f'
            if not self.pp.skipping:
                self.diag_range(DID.vertical_whitespace_in_directive, cursor - 1, cursor, [kind])
        token.flags |= TokenFlags.WS
        return TokenKind.WS, cursor

    def on_nl_ws(self, token, cursor):
        if self.pp.in_directive:
            # Stick on the newline
            return TokenKind.EOF, cursor - 1
        # When collecting arguments, newlines are treated as whitespace
        if self.pp.collecting_arguments:
            token.flags |= TokenFlags.WS
        else:
            token.flags &= ~TokenFlags.WS
        self.is_start_of_line = True
        return TokenKind.WS, cursor

    def on_nul(self, token, cursor):
        if cursor == len(self.buff):
            return TokenKind.EOF, cursor - 1
        return self.on_identifier(token, cursor)

    def on_backslash(self, token, cursor):
        is_nl, ncursor = self.skip_escaped_newline(cursor)
        if is_nl:
            return TokenKind.WS, ncursor
        return self.on_identifier(token, cursor)

    def on_brace_open(self, token, cursor):
        return TokenKind.BRACE_OPEN, cursor

    def on_brace_close(self, token, cursor):
        return TokenKind.BRACE_CLOSE, cursor

    def on_square_open(self, token, cursor):
        return TokenKind.SQUARE_OPEN, cursor

    def on_square_close(self, token, cursor):
        return TokenKind.SQUARE_CLOSE, cursor

    def on_paren_open(self, token, cursor):
        return TokenKind.PAREN_OPEN, cursor

    def on_paren_close(self, token, cursor):
        return TokenKind.PAREN_CLOSE, cursor

    def on_semicolon(self, token, cursor):
        return TokenKind.SEMICOLON, cursor

    def on_query(self, token, cursor):
        return TokenKind.QUESTION_MARK, cursor

    def on_tilde(self, token, cursor):
        return TokenKind.TILDE, cursor

    def on_comma(self, token, cursor):
        return TokenKind.COMMA, cursor

    def on_hash(self, token, cursor):
        # Handle "# ##"
        c, ncursor = self.read_logical_byte(cursor)
        if c == 35:  # '#'
            return TokenKind.HASH_HASH, ncursor
        if self.is_start_of_line:
            return TokenKind.DIRECTIVE_HASH, cursor
        else:
            return TokenKind.HASH, cursor

    def on_not(self, token, cursor):
        # Handle "! !="
        c, ncursor = self.read_logical_byte(cursor)
        if c == 61:  # '='
            return TokenKind.NE, ncursor
        return TokenKind.LOGICAL_NOT, cursor

    def on_multiply(self, token, cursor):
        # Handle "* *="
        c, ncursor = self.read_logical_byte(cursor)
        if c == 61:  # '='
            return TokenKind.MULTIPLY_ASSIGN, ncursor
        return TokenKind.MULTIPLY, cursor

    def on_assign(self, token, cursor):
        # Handle "= =="
        c, ncursor = self.read_logical_byte(cursor)
        if c == 61:  # '='
            return TokenKind.EQ, ncursor
        return TokenKind.ASSIGN, cursor

    def on_colon(self, token, cursor):
        # Handle ": :: :>"
        c, ncursor = self.read_logical_byte(cursor)
        if c == 58:  # ':'
            return TokenKind.SCOPE, ncursor
        if c == 62:  # '>'
            return TokenKind.SQUARE_CLOSE, ncursor
        return TokenKind.COLON, cursor

    def on_lt(self, token, cursor):
        # Handle "< <= <% <: <=> << <<="
        def permit_alternative_token(cursor):
            # [lex.pptoken 3.2] If the next three characters are <:: and the subsequent
            # character is neither : nor >, the < is treated as a preprocessing token by
            # itself and not as the first character of the alternative token <:.
            c, cursor = self.read_logical_byte(cursor)
            if c != 58:  # ':'
                return True
            c, cursor = self.read_logical_byte(cursor)
            return c == 58 or c == 62  # ':' or '>'

        # An angled header?
        if self.pp.in_header_name or self.in_header_name:
            kind, ncursor = self.on_delimited_literal(token, cursor)
            if kind == TokenKind.HEADER_NAME:
                return kind, ncursor
            # Lex as a normal token now, including <<, <=.  #include is not affected as it
            # forms header name tokens from token spellings, but __has_include is
            # affected, it requires a '<' token.

        c, ncursor = self.read_logical_byte(cursor)
        if c == 61:  # '='
            if self.pp.language.is_cxx():
                c, cursor = self.read_logical_byte(ncursor)
                if c == 62:  # '>'
                    return TokenKind.LEG, cursor
            return TokenKind.LE, ncursor
        if c == 60:  # '<'
            c, cursor = self.read_logical_byte(ncursor)
            if c == 61:  # '='
                return TokenKind.LSHIFT_ASSIGN, cursor
            return TokenKind.LSHIFT, ncursor
        if c == 37:  # '%'
            return TokenKind.BRACE_OPEN, ncursor
        if c == 58 and permit_alternative_token(ncursor):  # ':'
            return TokenKind.SQUARE_OPEN, ncursor
        return TokenKind.LT, cursor

    def on_gt(self, token, cursor):
        # Handle "> >= >> >>="
        c, ncursor = self.read_logical_byte(cursor)
        if c == 61:  # '='
            return TokenKind.GE, ncursor
        if c != 62:  # '>'
            return TokenKind.GT, cursor
        c, cursor = self.read_logical_byte(ncursor)
        if c == 61:  # '='
            return TokenKind.RSHIFT_ASSIGN, cursor
        return TokenKind.RSHIFT, ncursor

    def on_minus(self, token, cursor):
        # Handle "- -- -= -> ->*"
        c, ncursor = self.read_logical_byte(cursor)
        if c == 45:  # '-'
            return TokenKind.DECREMENT, ncursor
        if c == 61:  # '='
            return TokenKind.MINUS_ASSIGN, ncursor
        if c == 62:  # '>'
            if self.pp.language.is_cxx():
                c, cursor = self.read_logical_byte(ncursor)
                if c == 42:  # '*'
                    return TokenKind.DEREF_STAR, cursor
            return TokenKind.DEREF, ncursor
        return TokenKind.MINUS, cursor

    def on_plus(self, token, cursor):
        # Handle "+ ++ +="
        c, ncursor = self.read_logical_byte(cursor)
        if c == 43:  # '+'
            return TokenKind.INCREMENT, ncursor

        if c == 61:  # '='
            return TokenKind.PLUS_ASSIGN, ncursor

        return TokenKind.PLUS, cursor

    def on_bitwise_and(self, token, cursor):
        # Handle "& &= &&"
        c, ncursor = self.read_logical_byte(cursor)
        if c == 38:  # '&'
            return TokenKind.LOGICAL_AND, ncursor
        if c == 61:  # '='
            return TokenKind.BITWISE_AND_ASSIGN, ncursor
        return TokenKind.BITWISE_AND, cursor

    def on_bitwise_or(self, token, cursor):
        # Handle "| |= ||"
        c, ncursor = self.read_logical_byte(cursor)
        if c == 124:  # '|'
            return TokenKind.LOGICAL_OR, ncursor
        if c == 61:  # '='
            return TokenKind.BITWISE_OR_ASSIGN, ncursor
        return TokenKind.BITWISE_OR, cursor

    def on_bitwise_xor(self, token, cursor):
        # Handle "^ ^="
        c, ncursor = self.read_logical_byte(cursor)
        if c == 61:  # '='
            return TokenKind.BITWISE_XOR_ASSIGN, ncursor
        return TokenKind.BITWISE_XOR, cursor

    def on_modulus(self, token, cursor):
        # Handle "% %= %> %: %:%:"
        c, ncursor = self.read_logical_byte(cursor)
        if c == 61:  # '='
            return TokenKind.MODULUS_ASSIGN, ncursor
        if c == 62:  # '>'
            return TokenKind.BRACE_CLOSE, ncursor
        if c != 58:  # ':'
            return TokenKind.MODULUS, cursor
        # %: or %:%:
        c, cursor = self.read_logical_byte(ncursor)
        if c == 37:  # '%'
            c, cursor = self.read_logical_byte(cursor)
            if c == 58:  # ':'
                return TokenKind.HASH_HASH, cursor
        if self.is_start_of_line:
            return TokenKind.DIRECTIVE_HASH, ncursor
        else:
            return TokenKind.HASH, ncursor

    def on_line_comment(self, token, cursor):
        # A line comment.  Delegate handling of EOF and newlines.
        while True:
            c, cursor = self.read_logical_char(cursor)
            if c in NL_WS or (c == 0 and cursor == len(self.buff)):
                return TokenKind.WS, cursor - 1

    def on_block_comment(self, token, cursor, start):
        # A block comment.
        token.flags |= TokenFlags.WS
        end = cursor
        buff = self.buff
        while True:
            c = buff[cursor]
            cursor += 1
            while c == 42:  # '*'
                c, cursor = self.read_logical_byte(cursor)
                if c == 47:  # '/'
                    return TokenKind.WS, cursor
            if c == 0 and cursor == len(buff):
                self.diag_range(DID.unterminated_block_comment, start, end)
                # Return WS so the EOF token gets the correct placement
                return TokenKind.WS, cursor - 1
            if c >= 0x80:
                c, cursor = self.read_char(cursor - 1, -1)

    def on_divide(self, token, cursor):
        # Handle "/ /=" and line and block comments
        c, ncursor = self.read_logical_byte(cursor)
        if c == 61:  # '='
            return TokenKind.DIVIDE_ASSIGN, ncursor
        if c == 47:  # '/'
            return self.on_line_comment(token, ncursor)
        if c == 42:  # '*'
            return self.on_block_comment(token, ncursor, cursor - 1)

        return TokenKind.DIVIDE, cursor

    def on_dot(self, token, cursor):
        # Handle ". ... .* .3"
        c, ncursor = self.read_logical_byte(cursor)
        if c in ASCII_DIGITS:
            return self.on_number(token, cursor)
        if c == 46:  # '.'
            c, ncursor = self.read_logical_byte(ncursor)
            if c == 46:  # '.'
                return TokenKind.ELLIPSIS, ncursor
        if c == 42 and self.pp.language.is_cxx():  # '*'
            return TokenKind.DOT_STAR, ncursor
        return TokenKind.DOT, cursor

    # pp-number:
    #     digit
    #    . digit
    #    pp-number identifier-continue
    #    pp-number ’ digit
    #    pp-number ’ nondigit
    #    pp-number e sign
    #    pp-number E sign
    #    pp-number p sign
    #    pp-number P sign
    #    pp-number.
    # identifier-continue:
    #    digit
    #    non-digit
    #    a UCN or UTF-8 character with XID_continue
    # digit:
    #    [0-9]
    # non-digit:
    #    [A-Z] [a-z] _
    def on_number(self, token, cursor):
        '''Lex a pp-number.'''
        start = cursor - 1
        c = None
        buff = self.buff
        while True:
            char_start = cursor
            prevc = c
            c = buff[cursor]
            cursor += 1
            # Fast-track standard ASCII numbers
            if c in ASCII_IDENT_CONTINUE:
                continue
            character, cursor = self.read_extended_character(cursor - 1)
            c = character.value
            if (character.kind is CharacterKind.valid_start
                    or character.kind is CharacterKind.valid_continue):
                continue
            if c == 46:  # '.'
                continue
            if c in SIGNS and prevc in EPep:
                continue
            if c == 39:   # "'"
                c, cursor = self.read_logical_byte(cursor)
                if c in ASCII_IDENT_CONTINUE:
                    continue

            if not self.pp.skipping:
                token.extra = (self.fast_utf8_spelling(start, char_start), None)
            return TokenKind.NUMBER, char_start

    def on_identifier(self, token, cursor, ud_suffix=False):
        '''Return a (token_kind, cursor) pair.  If lexing a ud-suffix, set it to True.

        The token kind is normally TokenKind.IDENTIFIER, but conversion to alternative
        tokens is handled.  In either case token.extra is the IdentifierInfo object.

        Encoding-prefixed strings are also handled, in which case token.extra is as for
        strings.  If it is a non-identifier character, kind is CHARACTER and token.extra
        is its value.  kind can also be a module keyword.
        '''
        cursor -= 1
        start = cursor
        buff = self.buff
        quick_chars = ASCII_IDENT_START

        while True:
            char_start = cursor
            c = buff[cursor]
            cursor += 1
            # Fast-track standard ASCII identifiers
            if c not in quick_chars:
                character, cursor = self.read_extended_character(cursor - 1)
                if character.kind == CharacterKind.valid_start:
                    pass
                elif character.kind == CharacterKind.valid_continue and char_start != start:
                    pass
                else:
                    break
            quick_chars = ASCII_IDENT_CONTINUE

        if char_start == start:
            # Emit diagnostics only at the start of a token
            if character.diagnostic and not self.pp.skipping and not ud_suffix:
                self.pp.emit(character.diagnostic)
            token.extra = character.value
            return TokenKind.CHARACTER, cursor

        cursor = char_start
        if not self.pp.skipping:
            spelling = self.fast_utf8_spelling(start, cursor)
            if not is_NFC(spelling):
                self.diag(DID.identifier_not_NFC, start, [spelling])
            ident = self.pp.get_identifier(spelling)
            token.extra = ident
            if ident.special:
                return self.handle_special_identifier(token, start, cursor, ud_suffix)

        return TokenKind.IDENTIFIER, cursor

    def handle_special_identifier(self, token, start, cursor, ud_suffix):
        '''Handle __VA_ARGS__, alternative tokens, etc.'''
        ident = token.extra
        special = ident.special
        kind = TokenKind.IDENTIFIER

        # Is this an encoding prefix to a string or character literal?
        if special & SpecialKind.ENCODING_PREFIX and not ud_suffix:
            encoding = ident.encoding()
            c, ncursor = self.read_logical_byte(cursor)
            if ident.spelling[-1] == 82:  # 'R'
                # There is no such thing as a raw character literal
                if c == 34:  # '"'
                    token.extra = None
                    token.flags |= TokenFlags.encoding_bits(encoding)
                    return self.raw_string_literal(token, ncursor)
            elif c == 34 or c == 39:  # '"' and "'"
                token.extra = None
                token.flags |= TokenFlags.encoding_bits(encoding)
                return self.on_delimited_literal(token, ncursor)
        elif special & SpecialKind.VA_IDENTIFIER:
            if not self.pp.in_variadic_macro_definition:
                self.diag(DID.invalid_variadic_identifier_use, start, [ident.spelling])
        elif special & SpecialKind.ALT_TOKEN:
            kind = ident.alt_token_kind()
        elif self.is_start_of_line and special & SpecialKind.MODULE_KEYWORD and not ud_suffix:
            # The special keyword shall not be an object-like macro
            if ident.macro and not ident.macro.is_function_like():
                note = Diagnostic(DID.macro_defined_here, ident.macro.name_loc, [ident.spelling])
                self.diag(DID.macro_in_module_directive, start, [ident.spelling, note])
            else:
                return self.maybe_module_keyword(ident, cursor), cursor
        return kind, cursor

    def maybe_module_keyword(self, ident, cursor):
        '''Called by the lexer on lexing what is perhaps a module keyword.'''
        mk_kind = ident.module_keyword_kind()
        if mk_kind == TokenKind.kw_module_keyword:
            choices = (TokenKind.IDENTIFIER, TokenKind.COLON, TokenKind.SEMICOLON)
        elif mk_kind == TokenKind.kw_export_keyword:
            choices = (TokenKind.kw_module_keyword, TokenKind.kw_import_keyword)
        else:
            assert mk_kind == TokenKind.kw_import_keyword
            choices = (TokenKind.HEADER_NAME, TokenKind.LT, TokenKind.IDENTIFIER,
                       TokenKind.STRING_LITERAL, TokenKind.COLON)

        assert not self.in_header_name
        was_in_directive = self.pp.in_directive
        self.pp.in_directive = True               # To obtain EOF at end-of-line
        self.in_header_name = mk_kind == TokenKind.kw_import_keyword
        self.cursor = cursor
        peeked_kind = self.peek_token_kind()
        # Ensure a subsequent lex gets the header name
        self.in_header_name = peeked_kind == TokenKind.HEADER_NAME
        self.pp.in_directive = was_in_directive
        return mk_kind if peeked_kind in choices else TokenKind.IDENTIFIER

    def on_delimited_literal(self, token, cursor):
        '''Character literals, non-raw string literals, and header names.'''
        # The encoding prefix, if any, and the opening quote are already lexed
        buff = self.buff
        delimeter = buff[cursor - 1]
        in_header = self.pp.in_header_name or self.in_header_name
        self.in_header_name = False

        if delimeter == 34:     # '"'
            kind = TokenKind.HEADER_NAME if in_header else TokenKind.STRING_LITERAL
        elif delimeter == 39:   # "'"
            kind = TokenKind.CHARACTER_LITERAL
        elif delimeter == 60:   # '<'
            kind = TokenKind.HEADER_NAME
            delimeter = 62      # '>'
        else:
            raise RuntimeError

        while True:
            # Fast-track standard ASCII contents
            c = buff[cursor]
            if c == 92:  # '\\'
                # Handle escaped newlines
                c, cursor = self.read_logical_byte(cursor)
                # Skip escape sequences or UCNs unless in a header.  We do not check
                # syntax.
                if c == 92 and not in_header:
                    c, cursor = self.read_logical_char(cursor)
                    continue
            else:
                cursor += 1
            if c >= 0x80:
                c, cursor = self.read_char(cursor - 1)
                # No need to validate the character - it is always valid in a literal
            elif c == delimeter:
                break
            elif c == 0 and cursor == len(self.buff):
                cursor -= 1
                break
            elif c == 10 or c == 13:  # '\n' '\r'
                # Don't swallow the newline indicator
                cursor -= 1
                break

        if c != delimeter:
            # Don't diagnose unterminated headers as #include will complain its own way
            if not in_header and not self.pp.skipping:
                selector = 0 if kind == TokenKind.CHARACTER_LITERAL else 1
                self.diag(DID.unterminated_literal, token.loc, [selector])
            token.extra = None
            return TokenKind.UNTERMINATED, cursor

        # Header names have no ud_suffix
        if in_header:
            ud_suffix = None
        else:
            ud_suffix, cursor = self.maybe_user_defined_suffix(token, cursor)
        if not self.pp.skipping:
            spelling = self.fast_utf8_spelling(token.loc, cursor)
            token.extra = (spelling, ud_suffix)
        return kind, cursor

    # user-defined_suffix:
    #     identifier
    def maybe_user_defined_suffix(self, token, cursor):
        '''Lex a user-defined suffix, lexically an identifier.

        If it is not a user-defined suffix, return (None, cursor) with cursor unchanged.

        Otherwise return (ident, cursor) where ident is an IdentifierIno object.
        '''
        if self.pp.language.is_cxx():
            kind, ncursor = self.on_identifier(token, cursor + 1, ud_suffix=True)
            if kind == TokenKind.IDENTIFIER:
                return token.extra, ncursor
        return None, cursor

    # raw-string:
    #     "d-char-sequence[opt] (r-char-sequence[opt]) d-char-sequence[opt]"
    # r-char-sequence:
    #     r-char
    #     r-char-sequence r-char
    # r-char:
    #     any member of the translation character set, except a u+0029 right parenthesis followed
    #     by the initial d-char-sequence (which may be empty) followed by a u+0022 quotation mark
    # d-char-sequence:
    #     d-char
    #     d-char-sequence d-char
    # d-char:
    #  any member of the basic character set except:
    #  u+0020 space, u+0028 left parenthesis, u+0029 right parenthesis, u+005c reverse solidus,
    #  u+0009 character tabulation, u+000b line tabulation, u+000c form feed, and new-line
    def raw_string_literal(self, token, cursor):
        '''The encoding prefix, if any, and the opening quote are already lexed.  The token began
        at token_loc.
        '''
        def lex_delimeter(buff, cursor, basic_charset):
            '''Return the byte terminating the delimeter, and the cursor position beyond it.'''
            while True:
                c = buff[cursor]
                if c not in basic_charset or c in DELIMITER_INVALID:
                    return c, cursor
                cursor += 1

        # Raw string spellings cannot simply be taken from the buffer
        self.clean = False
        buff = self.buff
        diagnose = not self.pp.skipping
        delim_start = cursor
        c, cursor = lex_delimeter(buff, cursor, self.pp.basic_charset)
        delimeter = buff[delim_start: cursor]
        # This seems arbitrary and pointless requirement...
        if len(delimeter) > 16 and diagnose:
            self.diag_range(DID.delimeter_too_long, delim_start, cursor)

        if c != 40:  # '(':
            is_eof = c == 0 and cursor + 1 == len(self.buff)
            if is_eof:
                c = 10
            else:
                c, _ = self.read_char(cursor)
            if diagnose:
                self.diag_range(DID.delimeter_invalid_character, cursor, cursor,
                                [printable_form(c)])
            # Recover by skipping to end-of-line or EOF.  Note this will find ill-formed UTF-8.
            while c != 10 and c != 13 and cursor != len(self.buff):
                c, cursor = self.read_char(cursor)
            return TokenKind.UNTERMINATED, cursor

        # Lex the raw part
        delimeter += bytes([34])  # '"'
        cursor += 1
        while True:
            c, cursor = self.read_char(cursor)
            if c == 0 and cursor == len(self.buff):
                if diagnose:
                    self.diag(DID.unterminated_literal, token.loc, [2])
                return TokenKind.UNTERMINATED, cursor - 1
            # ')'
            if c == 41 and buff[cursor: cursor + len(delimeter)] == delimeter:
                cursor += len(delimeter)
                break

        ud_suffix, cursor = self.maybe_user_defined_suffix(token, cursor)
        # No point calling fast_utf8_spelling
        spelling = self.utf8_spelling(token.loc, cursor)
        token.extra = (spelling, ud_suffix)

        return TokenKind.STRING_LITERAL, cursor

    #
    # Extended characters and UCNs
    #

    def read_extended_character(self, cursor):
        '''Return a pair (character, cursor).'''
        c, cursor = self.read_logical_byte(cursor)
        if c < 0x80:
            if c in ASCII_IDENT_START:
                return Character(CharacterKind.valid_start, c, None), cursor
            if c in ASCII_IDENT_CONTINUE:
                return Character(CharacterKind.valid_continue, c, None), cursor
            if c == 92:  # '\\'
                self.clean = False
                return self.maybe_ucn(cursor)
            return Character(CharacterKind.valid_other, c, None), cursor
        # Handle UTF-8 extended identifiers
        c, cursor = self.read_char(cursor - 1, -1)
        if c == -1:
            return Character(CharacterKind.invalid_utf8, -1, None), cursor
        return self.extended_character(c), cursor

    # universal-character-name:
    #     \u hex-quad
    #     \U hex-quad hex-quad
    #     \u{ simple-hexadecimal-digit-sequence }
    #     named-universal-character
    #
    # hex-quad:
    #     hexadecimal-digit hexadecimal-digit hexadecimal-digit hexadecimal-digit
    #
    # simple-hexadecimal-digit-sequence:
    #    hexadecimal-digit
    #    simple-hexadecimal-digit-sequence hexadecimal-digit
    def hex_ucn(self, cursor, is_U, ucn_start):
        '''Lex a hexadecimal UCN; backslash-u or U has been consumed.  Return a (character,
        cursor) pair.
        '''
        count = 0
        cp = 0
        limit = 8 if is_U else 4
        u_cursor = cursor - 1

        while True:
            c, cursor = self.read_logical_byte(cursor)
            cvalue = HEX_DIGIT_VALUES.get(c, -1)
            if cvalue != -1:
                cp = cp * 16 + cvalue
                count += 1
                if count != limit:
                    continue
                break

            # '{'
            if c == 123 and count == 0 and limit == 4:
                limit = -1
                continue

            # '}' Consume a closing brace if valid
            if c == 125 and limit == -1:
                if count:
                    break
            else:
                cursor -= 1

            # This was an incomplete UCN.  Continue future lexing from the 'u' or 'U'.
            diagnostic = self.diagnostic_range(DID.incomplete_UCN_as_tokens, ucn_start, cursor)
            return Character(CharacterKind.valid_other, 92, diagnostic), u_cursor

        return self.ucn_character(cp, ucn_start, cursor)

    # named-universal-character:
    #    \N{ n-char-sequence }
    # n-char: any member of the translation character set except the u+007d right curly
    #         bracket or new-line character
    # n-char-sequence:
    #    n-char
    #    n-char-sequence n-char
    def named_character(self, cursor, ucn_start):
        '''Lex a named univeral character; backslash-N has been consumed.  Return a (character,
        cursor) pair.
        '''
        N_cursor = cursor - 1
        name = ''
        name_loc = None
        while True:
            char_start = cursor
            c, cursor = self.read_logical_char(cursor)
            if name_loc is None:
                if c != 123:  # '{'
                    break
                name_loc = cursor
                continue
            # '}' '\n' '\r'
            if c == 125 or c == 10 or c == 13 or (c == 0 and cursor == len(self.buff)):
                break
            name += chr(c)

        if c == 125 and name:  # '}'
            cp = name_to_cp(name)
            if cp != -1:
                return self.ucn_character(cp, ucn_start, cursor)
            diagnostic = self.diagnostic_range(DID.unrecognized_universal_character_name,
                                               name_loc, cursor - 1, [name])
            return Character(CharacterKind.invalid_ucn, -1, diagnostic), cursor

        # This was an incomplete UCN.  Continue future lexing from the 'N'.
        diagnostic = self.diagnostic_range(DID.incomplete_UCN_as_tokens, ucn_start, char_start)
        return Character(CharacterKind.valid_other, 92, diagnostic), N_cursor

    def maybe_ucn(self, cursor):
        '''Called after a logical backslash.  Work out what the character really is.'''
        ucn_start = cursor - 1
        c, cursor = self.read_logical_byte(cursor)
        if c == 85 or c == 117:  # 'U' 'u'
            return self.hex_ucn(cursor, c == 85, ucn_start)
        if c == 78:  # 'N'
            return self.named_character(cursor, ucn_start)
        # Not a UCN, just the backslash.  Later lexing can start at the logical byte
        return Character(CharacterKind.valid_other, 92, None), cursor - 1

    def ucn_character(self, c, ucn_start, cursor):
        '''Validate the value C of a UCN over the range ucn_start to cursor.  Return a (character,
        cursor) pair.
        '''
        if not is_valid_codepoint(c):
            did = DID.codepoint_invalid
        elif is_surrogate(c):
            did = DID.codepoint_surrogate
        elif is_control_character(c):
            did = DID.codepoint_control_character
        elif c in self.pp.basic_charset:
            did = DID.codepoint_basic_character_set
        else:
            return self.extended_character(c), cursor
        diagnostic = self.diagnostic_range(did, ucn_start, cursor, [codepoint_to_hex(c)])
        return Character(CharacterKind.invalid_ucn, -1, diagnostic), cursor

    def extended_character(self, c):
        '''Return a Character instance representing an extended character of value 'c'.'''
        if is_XID_Start(c):
            return Character(CharacterKind.valid_start, c, None)
        elif is_XID_Continue(c):
            return Character(CharacterKind.valid_continue, c, None)
        else:
            return Character(CharacterKind.valid_other, c, None)

    def token_spelling_at_cursor(self):
        prior_cursor = self.cursor
        self.get_token_quietly()
        return self.fast_utf8_spelling(prior_cursor, self.cursor)

    def fast_utf8_spelling(self, start, end):
        '''Return the spelling of the token just lexed between start and end as a bytes-like
        object.  If the token was clean, we take the spelling straight from the buffer.
        Otherwise slowly via self.utf8_spelling().
        '''
        if self.clean:
            return bytes(self.buff[start: end])   # Virtual tokens are bytearrays
        return self.utf8_spelling(start, end)

    def utf8_spelling(self, start, end, offsets=None):
        '''Return the spelling of the logical characters from [start, end) as valid UTF-8 in a
        bytearray.  Escaped newlines will have been removed.  Invalid UTF-8 is replaced
        with a replacement character.  This function must be quiet, i.e., it must not emit
        diagnostics.

        If offsets is a list, then it is an increasing sequence of byte offsets into the
        spelling, which is replaced with the source location of those offsets.
        '''
        assert 0 <= start < end <= len(self.buff)

        result = bytearray()
        buff = self.buff
        offsets = offsets or []
        n = 0

        count = -1
        cursor = start
        escaped = False
        delim = None
        rs_limit = None
        while cursor < end:
            count += 1
            if rs_limit:
                char_start = cursor
                if cursor == rs_limit:
                    rs_limit = None
                # Between the delimeters of a raw string, there are no escaped newlines as
                # each UTF-8 character represents itself.
                cp = buff[cursor]
                cursor += 1
                # However, newline sequences become '\n'
                if cp == 13:  # '\r'
                    if cursor < end and buff[cursor] == 10:   # '\n'
                        cursor += 1
                    cp = 10
            else:
                # Skip escaped newlines.
                cp, cursor = self.read_logical_byte(cursor)
                char_start = cursor - 1
                if delim:
                    # In delimeted literals leave UCNs alone.  However we must track
                    # backslashes in order to determine if a delimeter is escaped or not.
                    if cp == 92:  # '\\'
                        escaped = not escaped
                    elif cp == delim and not escaped:
                        delim = None
                elif cp == 34:  # '"'
                    # Handle raw strings ('R')
                    if result and result[-1] == 82:
                        # Search backwards (over a potential user-defined suffix) to find
                        # the closing delimeter
                        rs_limit = end - 1
                        while buff[rs_limit] != 34:
                            rs_limit -= 1
                    else:
                        delim = cp
                elif cp == 39:  # "'"
                    delim = cp

            if n < len(offsets) and offsets[n] == len(result):
                offsets[n] = char_start + self.start_loc
                n += 1

            if cp != 92:
                escaped = False

            if cp < 0x80:
                if cp == 92 and delim is None and rs_limit is None:
                    character, cursor = self.maybe_ucn(cursor)
                    if character.kind == CharacterKind.invalid_ucn:
                        encoding = self.clean_spelling(char_start, cursor)
                    else:
                        encoding = chr(character.value).encode()
                else:
                    result.append(cp)
                    continue
            else:
                cursor -= 1
                cp, size = utf8_cp(self.buff, cursor)
                if cp < 0:
                    encoding = b'\xef\xbf\xbd'   # Replacement character
                else:
                    encoding = self.buff[cursor: cursor + size]
                cursor += size

            result.extend(encoding)
            assert cursor <= end

        while n < len(offsets) and offsets[n] >= len(result):
            offsets[n] = end + self.start_loc
            n += 1

        assert n == len(offsets)

        return bytes(result)

    def clean_spelling(self, cursor, end):
        result = bytearray()
        while cursor < end:
            c, cursor = self.read_logical_char(cursor)
            result.extend(chr(c).encode())
        return result


Lexer.initialize()
