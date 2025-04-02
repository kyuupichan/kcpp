# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#

from ..unicode import (
    name_to_cp, utf8_cp, REPLACEMENT_CHAR, is_NFC, is_valid_codepoint,
    is_control_character, codepoint_to_hex, is_XID_Start, is_XID_Continue,
    is_surrogate
)

from ..diagnostics import BufferRange, DID, location_in_args

from .basic import (
    Token, TokenKind, TokenFlags, TokenSource, IdentifierInfo, SpecialKind, HEX_DIGIT_VALUES
)
from .literals import printable_form

__all__ = ['Lexer']


ASCII_DIGITS = {ord(c) for c in '0123456789'}
ASCII_IDENT_START = set(ord(c) for c in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz')
ASCII_IDENT_CONTINUE = set.union(ASCII_IDENT_START, ASCII_DIGITS)
SIGNS = {ord(c) for c in '+-'}
EPep = {ord(c) for c in 'EPep'}
NL_WS = {ord('\r'), ord('\n')}
NON_NL_WS = {ord(c) for c in ' \t\v\f'}

BASIC_CHARSET = set.union(ASCII_IDENT_CONTINUE,
                          (ord(c) for c in '\t\v\f \n\r!"#$%&\'()*+,-./:;<=>?@[\\]^`{|}~'))
DCHARS = set.difference(BASIC_CHARSET, (ord(c) for c in ' ()\\\t\v\f\r\n'))


class Lexer(TokenSource):

    # The preprocessor is necessary to look up e.g. if an identifier is special, or
    # an alternative token, or language options.
    def __init__(self, pp, buff, start_loc):
        assert isinstance(buff, (bytes, bytearray, memoryview))
        assert buff and buff[-1] == 0
        self.pp = pp
        self.buff = buff
        self.start_loc = start_loc
        self.cursor = 0
        self.clean = True

    def cursor_loc(self):
        return self.cursor + self.start_loc

    def skip_bom(self):
        bom = '\ufeff'.encode()  # b'\xef\xbb\xbf'
        return len(bom) if self.buff.startswith(bom) else 0

    @staticmethod
    def initialize():
        on_char = {}
        for n in range(128):
            on_char[n] = Lexer.on_other
        for n in range(128, 256):
            on_char[n] = Lexer.on_identifier
        for c in ASCII_IDENT_START:
            on_char[c] = Lexer.on_identifier
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
        self.pp.diag(did, loc + self.start_loc, args)

    def diag_range(self, did, start, end, args=None):
        '''Diagnose a range of characters.  start and end are byte offsets, and start must not be
        in the middle of a multibyte character.  end >= start must hold.  If end == start,
        the single character at that position is diagnosed.  If end is in the middle of a
        multi-byte character, the diagnosis extends to the end of that character.
        '''
        args = args or []
        args.append(BufferRange(start + self.start_loc, end + self.start_loc))
        self.pp.diag(did, location_in_args, args)

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

    def get_token(self, token):
        cursor = self.cursor
        if cursor == 0:
            token.flags = TokenFlags.BOL
            cursor = self.skip_bom()
        else:
            token.flags = 0
        token.extra = None
        buff = self.buff

        while True:
            # Internally to the Lexer, all locations are ofsets in the buffer.  They are
            # adjusted by self.start_loc when interfacing externally - when exiting
            # get_token() and by Lexer.diag.
            self.clean = True
            token.loc = cursor
            c = buff[cursor]
            kind, cursor = self.on_char[c](self, token, cursor + 1)
            if kind != TokenKind.WS:
                break

        token.loc += self.start_loc
        token.kind = kind
        self.cursor = cursor

    def get_token_quietly(self):
        '''Lex a token, without issuing diagnostics, and return it.'''
        prior = self.pp.set_diagnostic_consumer(None)
        token = Token.create()
        self.get_token(token)
        self.pp.set_diagnostic_consumer(prior)
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
        token.flags |= TokenFlags.BOL
        return TokenKind.WS, cursor

    def on_nul(self, token, cursor):
        if cursor == len(self.buff):
            return TokenKind.EOF, cursor - 1
        return self.on_other(token, cursor)

    def on_other(self, token, cursor):
        return TokenKind.OTHER, cursor

    def on_backslash(self, token, cursor):
        is_nl, cursor = self.skip_escaped_newline(cursor)
        if is_nl:
            return TokenKind.WS, cursor
        kind, ident, ncursor = self.maybe_identifier(token, cursor - 1)
        if kind is not None:
            # No alternative tokens begin with a UCN (yet!)
            assert kind == TokenKind.IDENTIFIER
            token.extra = ident
            return kind, ncursor
        return TokenKind.OTHER, cursor

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
            return TokenKind.CONCAT, ncursor
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
        if self.pp.in_header_name:
            kind, ncursor = self.on_delimited_literal(token, cursor)
            if kind == TokenKind.HEADER_NAME:
                return kind, ncursor

        c, ncursor = self.read_logical_byte(cursor)
        if c == 61:  # '='
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
        c, cursor = self.read_logical_byte(ncursor)
        if c == 37:  # '%'
            c, cursor = self.read_logical_byte(cursor)
            if c == 58:  # ':'
                return TokenKind.CONCAT, cursor
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
        if c == 42:  # '*'
            return TokenKind.DOT_STAR, ncursor
        if c == 46:  # '.'
            c, ncursor = self.read_logical_byte(ncursor)
            if c == 46:  # '.'
                return TokenKind.ELLIPSIS, ncursor
        return TokenKind.DOT, cursor

    # pp-number:
    #     digit
    #    . digit
    #    pp-number identifier-continue
    #    pp-number’ digit
    #    pp-number’ nondigit
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
    #
    def on_number(self, token, cursor):
        # Handle 0 1 2 3 4 5 6 7 8 9 .digit
        start = cursor - 1
        c = None
        buff = self.buff
        while True:
            prevc = c
            saved_cursor = cursor
            c = buff[cursor]
            cursor += 1

            # Fast-track standard ASCII numbers
            if c in ASCII_IDENT_CONTINUE:
                continue

            # continues_identifier() handles escaped newlines (in which case c is the character
            # after them), UTF-8 characters, and UCNs
            is_valid, c, cursor = self.continues_identifier(token, cursor, False, c)
            if is_valid:
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
                token.extra = (self.fast_utf8_spelling(start, saved_cursor), None)
            return TokenKind.NUMBER, saved_cursor

    def continues_identifier(self, token, cursor, is_ident_start, c):
        '''Return a triple (is_valid, code_point, cursor).

        If it is not lexically an identifier (e.g. a backslash, or a double quote,
        is_valid is False and code_point is the character concerned.

        If it is lexically an identifier (e.g. it starts a UCN) then is_valid is true even
        if lexically incomplete.  code_point is -1 if lexically incomplete, or if the
        character is semantically invalid.
        '''
        char_loc = cursor - 1
        if c == 92:  # '\\'
            # Maybe the backslash started an escaped newline
            c, cursor = self.read_logical_byte(cursor - 1)

        # Handle UCNs
        if c == 92:  # '\\'
            # A backslash has been consumed.  A UCN starts / continues the identifier.
            c, cursor = self.maybe_ucn(cursor, char_loc, is_ident_start, False)
            if c == -1:
                return False, c, char_loc
            self.clean = False
        elif c < 0x80:
            valid_chars = ASCII_IDENT_START if is_ident_start else ASCII_IDENT_CONTINUE
            return c in valid_chars, c, cursor
        else:
            # Handle UTF-8 extended identifiers.  Have invalid UTF-8 returned as -1,
            # and substitute the replacement character.
            c, cursor = self.read_char(cursor - 1, -1)
            if c == -1:
                c = REPLACEMENT_CHAR
            else:
                self.validate_codepoint(c, is_ident_start, char_loc, cursor, False)

        return True, c, cursor

    def on_identifier(self, token, cursor):
        '''Return a (token_kind, cursor) pair.

        The token kind is normally TokenKind.IDENTIFIER, but conversion to alternative
        tokens is handled.  In either case token.extra is the IdentifierInfo object.

        Encoding-prefixed strings are also handled, in which case token.extra is as for
        strings.
        '''
        # Start again at the beginning
        kind, ident, cursor = self.maybe_identifier(token, token.loc)
        assert ident is not None

        # Is this an encoding prefix to a string or character literal?
        if kind == TokenKind.IDENTIFIER and ident.special & SpecialKind.ENCODING_PREFIX:
            encoding = ident.encoding()
            c, ncursor = self.read_logical_byte(cursor)
            if ident.spelling[-1] == 82:  # 'R'
                # There is no such thing as a raw character literal
                if c == 34:  # '"'
                    token.flags |= TokenFlags.encoding_bits(encoding)
                    return self.raw_string_literal(token, ncursor)
            elif c == 34 or c == 39:  # '"' and "'"
                token.flags |= TokenFlags.encoding_bits(encoding)
                return self.on_delimited_literal(token, ncursor)

        token.extra = ident
        return kind, cursor

    def maybe_identifier(self, token, cursor):
        '''Lex what may be an identifier, and return a tuple (kind, ident, cursor).

        If it is lexically an identifier, ident is an IdentifierInfo object.  If that
        identifier becomes an alternative token, kind is that token kind, oterhwise it is
        TokenKind.IDENTIFIER.

        If it is not an identifier kind is None and cursor is unchanged from the one
        passed in.
        '''
        start = cursor
        buff = self.buff
        is_start = True
        quick_chars = ASCII_IDENT_START

        while True:
            c = buff[cursor]
            ncursor = cursor + 1
            # Fast-track standard ASCII identifiers
            if c not in quick_chars:
                is_valid, _c, ncursor = self.continues_identifier(token, ncursor, is_start, c)
                if not is_valid:
                    break
            cursor = ncursor
            is_start = False
            quick_chars = ASCII_IDENT_CONTINUE

        if cursor == start:
            return None, None, cursor

        kind = TokenKind.IDENTIFIER
        if self.pp.skipping:
            return kind, IdentifierInfo.dummy, cursor

        spelling = self.fast_utf8_spelling(start, cursor)
        if not is_NFC(spelling):
            self.diag(DID.identifier_not_NFC, start, [spelling])

        # Handle __VA_ARGS__, alternative tokens, etc.
        ident = self.pp.get_identifier(spelling)
        if ident.special & SpecialKind.VA_IDENTIFIER:
            if not self.pp.in_variadic_macro_definition:
                self.diag(DID.invalid_variadic_identifier_use, start, [spelling])
        elif ident.special & SpecialKind.ALT_TOKEN:
            kind = ident.alt_token_kind()

        return kind, ident, cursor

    # Character and (non-raw) string literals
    def on_delimited_literal(self, token, cursor):
        '''The encoding prefix, if any, and the opening quote are already lexed.'''
        buff = self.buff
        delimeter = buff[cursor - 1]
        in_header = self.pp.in_header_name

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
            # Don't diagnose unterminated headers
            if not in_header and not self.pp.skipping:
                selector = 0 if kind == TokenKind.CHARACTER_LITERAL else 1
                self.diag(DID.unterminated_literal, token.loc, [selector])
            # Unterminated literals become the error token
            return TokenKind.ERROR, cursor

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
        kind, ident, ncursor = self.maybe_identifier(token, cursor)
        # An alternative token must not be treated as a user-defined suffix.
        if kind != TokenKind.IDENTIFIER:
            return None, cursor
        return ident, ncursor

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
        def lex_delimeter(buff, cursor):
            '''Return the byte terminating the delimeter, and the cursor position beyond it.'''
            while True:
                c = buff[cursor]
                if c not in DCHARS:
                    return c, cursor
                cursor += 1

        # Raw string spellings cannot simply be taken from the buffer
        self.clean = False
        buff = self.buff
        diagnose = not self.pp.skipping
        delim_start = cursor
        c, cursor = lex_delimeter(buff, cursor)
        delimeter = buff[delim_start: cursor]
        # This seems arbitrary and pointless requirement...
        if len(delimeter) > 16 and diagnose:
            self.diag_range(DID.delimeter_too_long, delim_start, cursor)

        if c == 40:  # '(':
            # Lex the raw part
            delimeter += bytes([34])  # '"'
            cursor += 1
            while True:
                c, cursor = self.read_char(cursor)
                if c == 0 and cursor == len(self.buff):
                    if diagnose:
                        self.diag(DID.unterminated_literal, token.loc, [2])
                    # Unterminated literals become the error token
                    return TokenKind.ERROR, cursor - 1
                # ')'
                if c == 41 and buff[cursor: cursor + len(delimeter)] == delimeter:
                    cursor += len(delimeter)
                    break
        elif diagnose:
            is_eof = c == 0 and cursor + 1 == len(self.buff)
            bad_loc = cursor
            if is_eof:
                c = 10
            else:
                c, cursor = self.read_char(cursor)
            self.diag_range(DID.delimeter_invalid_character, bad_loc, bad_loc,
                            [printable_form(c)])
            # Recover by skipping to end-of-line or EOF.  Note this will find ill-formed UTF-8.
            while c != 10 and c != 13 and cursor != len(self.buff):
                c, cursor = self.read_char(cursor)
            # Unterminated literals become the error token
            return TokenKind.ERROR, cursor

        ud_suffix, cursor = self.maybe_user_defined_suffix(token, cursor)
        # No point calling fast_utf8_spelling
        spelling = self.utf8_spelling(token.loc, cursor)
        token.extra = (spelling, ud_suffix)

        return TokenKind.STRING_LITERAL, cursor

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
    #
    def hex_ucn(self, cursor, is_U):
        '''Try to lex the portion of a hexadecimal univeral character after the backslash-u or
        backslash-U prefix has been consumed.

        Return a pair (cp, cursor).  If lexically invalid (insufficient digits, no closing
        brace), codepoint, cp is -1.  Otherwise cp is the hexadecimal value.
        '''
        count = 0
        cp = 0
        limit = 8 if is_U else 4

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

            return -1, cursor

        return cp, cursor

    # named-universal-character:
    #    \N{ n-char-sequence }
    # n-char: any member of the translation character set except the u+007d right curly
    #         bracket or new-line character
    # n-char-sequence:
    #    n-char
    #    n-char-sequence n-char
    def named_character(self, cursor, quiet):
        '''Try to lex the portion of a named univeral character after the backslash-N prefix
        has been consumed.

        Return a triple (lexically_valid, value, cursor).  If lexically invalid, value is
        -1 and cursor is the lexically invalid character.  If lexically invalid but the
        named character is unknown (which is diagnosed) then value is -1.
        '''
        c, cursor = self.read_logical_byte(cursor)
        if c != 123:  # '{'
            cursor -= 1
        else:
            name = ''
            name_loc = cursor
            while True:
                c, cursor = self.read_logical_byte(cursor)
                if c == 125:  # '}'
                    break
                if c == 0 and cursor == len(self.buff):
                    cursor -= 1
                    break
                if c == 10 or c == 13:  # '\n' '\r'
                    # Don't swallow the newline sequence
                    cursor -= 1
                    break
                name += chr(c)

            if c == 125 and name:  # '}'
                cp = name_to_cp(name)
                if cp == -1 and not self.pp.skipping and not quiet:
                    self.diag_range(DID.unrecognized_universal_character_name,
                                    name_loc, cursor - 1, [name])
                return True, cp, cursor

        return False, -1, cursor

    def maybe_ucn(self, cursor, escape_loc, is_ident_start, quiet):
        '''On entry, a backslash has been consumed.  Return a triple (cp, cursor).

        If lexically the backslash does not form a UCN, then cp is -1.  If lexically it
        does form a UCN, then the codepoint is checked for validity in context.  If the
        codepoint is valid it is returned, otherise REPLACEMENT_CHAR is returned.
        '''
        saved_cursor = cursor

        c, cursor = self.read_logical_byte(cursor)
        if c == 85 or c == 117:  # 'U' 'u'
            cp, cursor = self.hex_ucn(cursor, c == 85)
            is_ucn = cp != -1
        elif c == 78:  # 'N'
            is_ucn, cp, cursor = self.named_character(cursor, quiet)
        else:
            # It didn't begin anything resembling a UCN sequence
            return -1, saved_cursor

        if is_ucn:
            # -1 is an unknown named character.
            if cp != -1:
                cp = self.validate_codepoint(cp, is_ident_start, escape_loc, cursor, quiet)
            if cp == -1:
                cp = REPLACEMENT_CHAR
        else:
            assert cp == -1
            if is_ident_start and not quiet and not self.pp.skipping:
                self.diag_range(DID.incomplete_UCN_as_tokens, escape_loc, cursor)

        return cp, cursor

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
                    cp, cursor = self.maybe_ucn(cursor, cursor - 1, count == 0, True)
                    if cp == -1:
                        cp = 92
                if cp < 0x80:
                    result.append(cp)
                    continue
                encoding = chr(cp).encode()
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

    def validate_codepoint(self, cp, is_ident_start, start, end, quiet):
        assert cp != -1
        did = did_for_codepoint(cp, is_ident_start)
        if did:
            if not quiet and not self.pp.skipping:
                self.diag_range(did, start, end, [codepoint_to_hex(cp)])
            cp = REPLACEMENT_CHAR
        return cp


# Requirements on UCNs in the C++23 standard:
#
# 1) \u \U UCNs must be a unicode scalar value (in-range and not a surrogate)
# 2) \N must be its character name in UCD, or "control/correction/alternate" in NameAliases.txt
# 3) Outside a c-char, s-char or r-char sequence, of a character or string literal, is a control
#    character or in the basic character set, the program is ill-formed.
# 4) Identifier characters must have XID_Start and XID_Continue properties
# 5) Identifier needs to conform to Normalization Form C.
def did_for_codepoint(cp, is_ident_start):
    '''Diagnose bad UCN values.  Return True if valid.'''
    assert cp >= 0

    if not is_valid_codepoint(cp):
        return DID.codepoint_invalid

    if is_surrogate(cp):
        return DID.codepoint_surrogate

    # If a universal-character-name outside the c-char-sequence, s-char-sequence, or
    # r-char-sequence of a character-literal or string-literal (in either case,
    # including within a user-defined-literal) corresponds to a control character or
    # to a character in the basic character set, the program is ill-formed.
    if is_control_character(cp):
        return DID.codepoint_control_character

    if cp in BASIC_CHARSET:
        return DID.codepoint_basic_character_set

    if is_ident_start:
        if not is_XID_Start(cp):
            return DID.codepoint_cannot_begin_identifier
    elif not is_XID_Continue(cp):
        return DID.codepoint_cannot_continue_identifier

    return None


Lexer.initialize()
