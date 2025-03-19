# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#

import sys
from copy import copy
from dataclasses import dataclass
from enum import IntEnum
from functools import partial

from ..diagnostics import (
    DID, ElaboratedLocation, ElaboratedRange, BufferRange, SpellingRange, TokenRange, Diagnostic,
    DiagnosticEngine, location_command_line, location_none,
)

from .basic import (
    Buffer, IdentifierInfo, Token, TokenKind, TokenFlags, Encoding, TargetMachine, IntegerKind,
    Charset,
)
from .expressions import ExprParser
from .lexer import Lexer
from .literals import LiteralInterpreter
from .locator import Locator
from .macros import Macro, MacroFlags, ObjectLikeExpansion


__all__ = ['Preprocessor']


class Specials(IntEnum):
    NOT_SPECIAL = 0
    # These tokens are restricted to limited contexts
    VA_ARGS = 1
    VA_OPT = 2


class Keywords(IntEnum):
    NOT_KEYWORD = 0


@dataclass
class IfSection:
    '''Represents a conditional preprocessing group.'''
    # True if the preprocessor was skipping on entry to the #if
    was_skipping: bool
    # True if one of the if / elif conditions in this group was true
    true_condition_seen: bool
    # If #else has been seen, its location, otherwise -1
    else_loc: int
    # Location of opening directive
    opening_loc: int


class Preprocessor:

    handlers = {}
    condition_directives = set(b'if ifdef ifndef elif elifdef elifndef else endif'.split())

    ALT_TOKENS = {
        b'and': TokenKind.LOGICAL_AND,
        b'or': TokenKind.LOGICAL_OR,
        b'bitand': TokenKind.BITWISE_AND,
        b'bitor': TokenKind.BITWISE_OR,
        b'xor': TokenKind.BITWISE_XOR,
        b'compl': TokenKind.TILDE,
        b'and_eq': TokenKind.BITWISE_AND_ASSIGN,
        b'or_eq': TokenKind.BITWISE_OR_ASSIGN,
        b'xor_eq': TokenKind.BITWISE_XOR_ASSIGN,
        b'not': TokenKind.LOGICAL_NOT,
        b'not_eq': TokenKind.NE,
    }

    ENCODING_PREFIXES = {
        b'': Encoding.NONE,
        b'L': Encoding.WIDE,
        b'u8': Encoding.UTF_8,
        b'u': Encoding.UTF_16,
        b'U': Encoding.UTF_32,
        b'R': Encoding.RAW,
        b'LR': Encoding.WIDE_RAW,
        b'u8R': Encoding.UTF_8_RAW,
        b'uR': Encoding.UTF_16_RAW,
        b'UR': Encoding.UTF_32_RAW,
    }

    def __init__(self, env, *, target=None):
        self.target = target or TargetMachine.default()
        self.configure(env)

        # Internal state.
        self.identifiers = {}
        self.locator = Locator()
        self.expand_macros = True
        self.handlers = {name.encode(): getattr(self, f'on_{name}')
                         for name in self.directive_names()}
        self.skipping = False
        self.in_directive = False
        self.directive_name_loc = None
        self.in_filename = False
        self.in_variadic_macro_definition = False
        # These can be modified by language options
        self.alt_tokens = Preprocessor.ALT_TOKENS
        self.encoding_prefixes = Preprocessor.ENCODING_PREFIXES
        self.diagnostic_engine = DiagnosticEngine(self, env)
        # Literal interpreter for a front end
        self.literal_interpreter = LiteralInterpreter(self, False)
        # A preprocessing expression parser
        self.expr_parser = ExprParser(self)
        # Token sources.
        self.sources = []
        self.diags = []
        self.initialize()

    @classmethod
    def add_arguments(cls, pp_group, diag_group):
        '''Add command line arugments to the group.'''
        pp_group.add_argument('-exec-charset', type=str)
        pp_group.add_argument('-wide-exec-charset', type=str)
        DiagnosticEngine.add_arguments(diag_group)

    def configure(self, env):
        def set_charset(attrib, charset_name, integer_kind):
            if charset_name:
                charset = Charset.from_name(charset_name)
                encoding_unit_size = charset.encoding_unit_size()
                unit_width = self.target.integer_width(integer_kind)
                if encoding_unit_size * 8 != unit_width:
                    diagnostic = Diagnostic(DID.invalid_charset, location_command_line,
                                            [charset_name, integer_kind.name, unit_width])
                    env.diagnostics.append(diagnostic)
                setattr(self.target, attrib, charset)

        set_charset('narrow_charset', env.command_line.exec_charset, IntegerKind.char)
        set_charset('wide_charset', env.command_line.wide_exec_charset, IntegerKind.wchar_t)

    def initialize(self):
        specials = [
            (b'__VA_ARGS__', Specials.VA_ARGS),
            (b'__VA_OPT__', Specials.VA_OPT),
        ]
        for spelling, kind in specials:
            self.get_identifier(spelling).special = kind

    def interpret_literal(self, token):
        return self.literal_interpreter.interpret(token)

    def add_diagnostic_consumer(self, consumer):
        self.diagnostic_engine.add_diagnostic_consumer(consumer)

    def diag(self, did, loc, args=None):
        diag = Diagnostic(did, loc, args)
        self.diags.append(diag)
        self.diagnostic_engine.emit(diag)

    def get_identifier(self, spelling):
        ident = self.identifiers.get(spelling)
        if not ident:
            ident = IdentifierInfo(spelling, None, 0, 0)
            self.identifiers[spelling] = ident
        return ident

    def lexed_identifier(self, spelling):
        diag = None
        ident = self.get_identifier(spelling)
        if ident.special:
            if ident.special in (Specials.VA_ARGS, Specials.VA_OPT):
                if not self.in_variadic_macro_definition:
                    diag = DID.invalid_variadic_identifier_use
        return ident, diag

    def maybe_identifier(self, spelling):
        '''Returns an IdentifierInfo is spelling is the spelling of a valid identifier, otherwise
        None.
        '''
        lexer = Lexer(self, spelling, 1, quiet=True)
        token = Token.create()
        lexer.get_token(token)
        # It must be an identifier and have consumed the entire spelling.
        if token.kind == TokenKind.IDENTIFIER and lexer.cursor == len(spelling):
            return token.extra
        return None

    def token_spelling(self, loc):
        buffer, offset = self.locator.loc_to_buffer_and_offset(loc)
        lexer = Lexer(self, buffer.text, loc - offset, quiet=True)
        return lexer.token_spelling(offset)

    def token_length(self, loc):
        '''The length of the token in bytes in the physical file.  This incldues, e.g., escaped
        newlines.  The result can be 0, for end-of-source indicator tokens like EOF and EOD.
        '''
        buffer, offset = self.locator.loc_to_buffer_and_offset(loc)
        lexer = Lexer(self, buffer.text, loc - offset, quiet=True)
        token = Token.create()
        lexer.cursor = offset
        lexer.get_token(token)
        return lexer.cursor - offset

    def directive_names(self):
        return ('include define undef line error warning pragma if ifdef ifndef '
                'elif elifdef elifndef else endif').split()

    def push_source_file(self, filename):
        '''Push a source file onto the preprocessor's source file stack.

        filename is the path to the filename.  '-' reads from stdin (all at once -
        processing doesn't begin until EOF).  Alternatively it can be a file-like object
        opened for reading in binary mode.
        '''
        if filename == '-':
            raw = sys.stdin.buffer.read()
            filename = '<stdin>'
        else:
            # FIXME: more mature error handling.
            try:
                with open(filename, 'rb') as f:
                    raw = f.read()
            except OSError as e:
                print(f'error: unable to open {filename}: {e}', file=sys.stderr)
                return
        return self.push_buffer(raw, name=filename)

    def push_buffer(self, text, *, name=None):
        buffer = Buffer(text, name=name)
        # Allow a location for the buffer's EOF.
        first_loc = self.locator.new_buffer_loc(-1, len(text) + 1, buffer)
        source = Lexer(self, text, first_loc)
        source.if_sections = []
        self.push_source(source)
        return source

    def push_source(self, source):
        self.sources.append(source)

    def pop_source(self, token):
        if len(self.sources) > 1:
            self.sources.pop()
            self.get_token(token)

    def get_token(self, token):
        # Take tokens from the currently active source.
        source = self.sources[-1]

        while True:
            source.get_token(token)

            # Handle preprocessing directives.  This must happen before macro expansion.
            if token.kind == TokenKind.HASH and token.flags & TokenFlags.BOL:
                self.handle_directive(source, token)
                # FIXME: this won't be true when we handle #include
                assert source is self.sources[-1]
                continue

            if self.skipping and token.kind not in {TokenKind.EOD, TokenKind.EOF}:
                continue

            if token.kind == TokenKind.IDENTIFIER:
                self.maybe_enter_macro(token)

            return

    def maybe_enter_macro(self, token):
        '''token is an identifier.  If it is an enabled macro, enter its expansion.'''
        if not self.expand_macros or token.is_disabled():
            return
        macro = token.extra.macro
        if not macro:
            return
        if macro.is_disabled():
            # Disable this token forever from later expansion
            token.disable()
            return
        if macro.flags & MacroFlags.IS_FUNCTION_LIKE:
            raise NotImplementedError
        else:
            source = ObjectLikeExpansion(self, macro, token)
        self.push_source(source)
        self.get_token(token)

    def handle_directive(self, lexer, token):
        '''Handle a directive to and including the EOD token.  We have read the '#' introducing a
        directive.'''
        def get_handler(lexer, token):
            # Turn off skipping whilst getting the directive name so that identifier
            # information is attached, and vertical whitespace is caught.
            was_skipping = self.skipping
            self.skipping = False
            lexer.get_token(token)
            self.skipping = was_skipping
            # Save the directive name's location
            self.directive_name_loc = token.loc
            if token.kind == TokenKind.IDENTIFIER:
                # If skipping ignore everything except for conditional directives
                if self.skipping and token.extra.spelling not in self.condition_directives:
                    return self.ignore_directive
                return self.handlers.get(token.extra.spelling, self.invalid_directive)
            # Ignore the null directive, and unknown directives when skipping.
            if self.skipping or token.kind == TokenKind.EOD:
                return self.ignore_directive
            # Unknown directive.
            return self.invalid_directive

        assert isinstance(lexer, Lexer)
        self.in_directive = True
        self.expand_macros = False
        handler = get_handler(lexer, token)
        handler(lexer, token)
        self.expand_macros = True
        self.in_directive = False

    def on_include(self, lexer, token):
        self.expand_macros = True
        self.in_filename = True
        self.get_token(token)
        self.in_filename = False
        # FIXME
        self.skip_to_eod(token, False)

    def on_define(self, lexer, token):
        '''#define directive processing.'''
        lexer.get_token(token)
        if self.is_macro_name(token):
            macro_ident = token.extra
            macro = self.read_macro_definition(lexer, token)
            if macro:
                self.define_macro(macro_ident, macro)
            else:
                self.skip_to_eod(token, False)
        else:
            self.skip_to_eod(token, False)

    def read_macro_definition(self, lexer, token):
        '''Lex a macro definition.  Return a macro definition, or None.'''
        macro = Macro(token.loc, 0, [], '')

        # Is this a function-like macro?
        lexer.get_token(token)
        is_function_like = (token.kind == TokenKind.PAREN_OPEN
                            and not (token.flags & TokenFlags.WS))
        if is_function_like:
            params, macro.flags = self.read_macro_parameter_list(lexer, token)
            if params is None:
                return None
            # If we ever support GCC extensions then this needs to be updated
            self.in_variadic_macro_definition = bool(macro.flags & MacroFlags.IS_VARIADIC)
            # Get the real first token of the replacement list
            lexer.get_token(token)
        else:
            # [cpp.replace 4] There shall be whitespace between the identifier and the
            # replacement list in the definition of an object-like macro.
            if not token.flags & TokenFlags.WS and token.kind != TokenKind.EOD:
                self.diag(DID.macro_name_whitespace, token.loc)

        tokens = macro.replacement_list
        while token.kind != TokenKind.EOD:
            tokens.append(copy(token))
            lexer.get_token(token)

        self.in_variadic_macro_definition = False

        if tokens:
            # [cpp.concat 1] A ## preprocessing token shall not occur at the beginning or
            # at the end of a replacement list for either form of macro definition.
            if tokens[0].kind == TokenKind.CONCAT:
                self.diag(DID.macro_definition_starts_with_concat, tokens[0].loc)
                return None

            if tokens[-1].kind == TokenKind.CONCAT:
                self.diag(DID.macro_definition_ends_with_concat, tokens[-1].loc)
                return None

            # This validation must be done even if there are no parameters.
            if is_function_like and not self.check_function_like_replacement(macro, params):
                return None

        if is_function_like:
            sorted_params = sorted((n, ident.spelling) for ident, n in params.items())
            macro.param_names = b' '.join(spelling for _, spelling in sorted_params)
        return macro

    def check_va_opt_syntax(self, tokens, pos, va_opt):
        '''Return the number of tokens including the open and closing parens.
        Return 0 on failure.'''
        # Ugly hack
        def next_token(n):
            if n < len(tokens):
                return tokens[n]
            token = Token.create()
            self.get_token(token)
            assert token.kind == TokenKind.EOD
            return token

        n = pos + 1
        token = next_token(n)
        if token.kind != TokenKind.PAREN_OPEN:
            self.diag(DID.expected_open_paren, token.loc)
            return 0

        paren_locs = [token.loc]
        while True:
            n += 1
            token = next_token(n)
            if token.kind == TokenKind.PAREN_OPEN:
                paren_locs.append(token.loc)
            elif token.kind == TokenKind.CONCAT:
                if n - pos == 2:
                    self.diag(DID.va_opt_starts_with_concat, token.loc)
                    return 0
                if n + 1 < len(tokens) and tokens[n + 1].kind == TokenKind.PAREN_CLOSE:
                    self.diag(DID.va_opt_ends_with_concat, token.loc)
                    return 0
            elif token.kind == TokenKind.PAREN_CLOSE:
                paren_locs.pop()
                if not paren_locs:
                    return n - pos
            elif token.kind == TokenKind.EOD:
                while paren_locs:
                    note = Diagnostic(DID.prior_match, paren_locs.pop(), ['('])
                    self.diag(DID.expected_close_paren, token.loc, [note])
                return 0
            elif token.kind == TokenKind.IDENTIFIER and token.extra == va_opt:
                self.diag(DID.nested_va_opt, token.loc)
                return 0

    def check_function_like_replacement(self, macro, params):
        tokens = macro.replacement_list
        if params:
            va_opt = self.identifiers[b'__VA_OPT__']
            # Replace macro parameters
            for n, token in enumerate(tokens):
                if token.kind == TokenKind.IDENTIFIER:
                    if token.extra == va_opt:
                        count = self.check_va_opt_syntax(tokens, n, va_opt)
                        if not count:
                            return False
                        # Convert to a special parameter token
                        token.kind = TokenKind.MACRO_PARAM
                        token.extra = -count
                    else:
                        # Convert parameters to parameter tokens
                        param_index = params.get(token.extra, -1)
                        if param_index != -1:
                            token.kind = TokenKind.MACRO_PARAM
                            token.extra = param_index

        # Check stringize operators
        for n, token in enumerate(tokens):
            if token.kind == TokenKind.HASH:
                if n + 1 == len(tokens) or tokens[n + 1].kind != TokenKind.MACRO_PARAM:
                    self.diag(DID.hash_requires_macro_parameter, token.loc)
                    return False
                token.kind = TokenKind.STRINGIZE

        return True

    # parameter-list:
    #    lparen identifier-list[opt] )
    #    lparen ... )
    #    lparen identifier-list, ... )
    # identifier-list:
    #    identifier
    #    identifier-list, identifier
    def read_macro_parameter_list(self, lexer, token):
        '''Return a (params, macro flags) pair.  params is a dictionary mapping IdentifierInfo
        objects to 0-bassed parameter index.  Anonymous variable
        arguments are represented by the __VA_ARGS__ identifier.

        The opening parenthesis is taken to have been consumed.
        '''
        params = {}
        flags = MacroFlags.IS_FUNCTION_LIKE
        paren_loc = token.loc

        # Valid tokens are identifiers, ')', ',' and '...'.
        while True:
            prior_kind = token.kind
            assert prior_kind in (TokenKind.PAREN_OPEN, TokenKind.IDENTIFIER,
                                  TokenKind.ELLIPSIS, TokenKind.COMMA)
            lexer.get_token(token)

            # ')' terminates the parameter list but cannot appear after a comma
            if token.kind == TokenKind.PAREN_CLOSE:
                if prior_kind == TokenKind.COMMA:
                    break
                return params, flags | MacroFlags.from_param_count(len(params))

            # EOD is always invalid.  An ellipsis must be followed by ')'.  An identifier
            # must be followed by ',' or ')'.
            if token.kind == TokenKind.EOD or prior_kind == TokenKind.ELLIPSIS:
                note = Diagnostic(DID.prior_match, paren_loc, ['('])
                self.diag(DID.expected_close_paren, token.loc, [note])
                return None, flags

            if token.kind == TokenKind.COMMA:
                if prior_kind != TokenKind.IDENTIFIER:
                    break
            elif prior_kind == TokenKind.IDENTIFIER:
                self.diag(DID.expected_comma_in_parameter_list, token.loc)
                return None, flags
            elif token.kind == TokenKind.IDENTIFIER:
                if token.extra in params:
                    self.diag(DID.duplicate_macro_parameter, token.loc, [token.extra.spelling])
                    return None, flags
                params[token.extra] = len(params)
            elif token.kind == TokenKind.ELLIPSIS:
                params[self.identifiers[b'__VA_ARGS__']] = len(params)
                flags |= MacroFlags.IS_VARIADIC
            else:
                break

        self.diag(DID.expected_macro_parameter, token.loc)
        return None, flags

    def define_macro(self, macro_ident, macro):
        prior = macro_ident.macro
        if prior and not self.compare_macro_definitions(prior, macro):
            self.diag(DID.macro_redefined, macro.name_loc, [
                macro_ident.spelling,
                Diagnostic(DID.prior_macro_definition, prior.name_loc),
            ])
        macro_ident.macro = macro

    def compare_macro_definitions(self, lhs, rhs):
        # Fast checks first.  Check flags and parameter counts match.
        if lhs.flags != rhs.flags:
            return False
        # Check parameter names match
        if lhs.param_names != rhs.param_names:
            return False
        # Check replacement lists match
        if len(lhs.replacement_list) != len(rhs.replacement_list):
            return False
        for lhs_token, rhs_token in zip(lhs.replacement_list, rhs.replacement_list):
            if lhs_token.kind != rhs_token.kind:
                return False
            if lhs_token.flags != rhs_token.flags:
                return False
            if self.token_spelling(lhs_token.loc) != self.token_spelling(rhs_token.loc):
                return False
        return True

    def on_undef(self, lexer, token):
        '''#undef directive processing.'''
        lexer.get_token(token)
        is_macro_name = self.is_macro_name(token)
        if is_macro_name:
            token.extra.macro = None
        self.skip_to_eod(token, is_macro_name)

    def on_line(self, lexer, token):
        self.expand_macros = True
        # FIXME
        self.skip_to_eod(token, False)

    def on_error(self, lexer, token):
        self.diagnostic_directive(lexer, token, DID.error_directive)

    def on_warning(self, lexer, token):
        self.diagnostic_directive(lexer, token, DID.warning_directive)

    def on_pragma(self, lexer, token):
        # FIXME
        self.skip_to_eod(token, False)

    def ignore_directive(self, _lexer, _token):
        pass

    def enter_if_section(self, lexer, token, condition):
        section = IfSection(
            self.skipping,      # was_skipping
            False,              # true_condition_seen
            -1,                 # else_loc
            token.loc           # opening_loc
        )
        lexer.if_sections.append(section)
        if not self.skipping:
            section.true_condition_seen = condition(lexer, token)
            self.skipping = not section.true_condition_seen

    def else_section(self, lexer, token, condition):
        if not lexer.if_sections:
            self.diag(DID.else_without_if, token.loc, [self.token_spelling(token.loc)])
            return

        section = lexer.if_sections[-1]
        if section.was_skipping:
            self.skip_to_eod(token, False)
            return
        if section.else_loc != -1:
            self.diag(DID.else_after_else, token.loc, [
                self.token_spelling(token.loc),
                Diagnostic(DID.else_location, section.else_loc),
            ])
            self.skip_to_eod(token, False)
            return

        if condition:  # conditional else
            if section.true_condition_seen:
                self.skipping = True
                self.skip_to_eod(token, False)
            else:
                self.skipping = False
                section.true_condition_seen = condition(lexer, token)
                self.skipping = not section.true_condition_seen
        else:  # unconditional else
            section.else_loc = token.loc
            self.skipping = section.true_condition_seen
            section.true_condition_seen = True
            self.skip_to_eod(token, True)

    def on_if(self, lexer, token):
        self.enter_if_section(lexer, token, self.evaluate_pp_expression)

    def on_ifdef(self, lexer, token):
        self.enter_if_section(lexer, token, partial(self.test_defined, False))

    def on_ifndef(self, lexer, token):
        self.enter_if_section(lexer, token, partial(self.test_defined, True))

    def on_elif(self, lexer, token):
        self.else_section(lexer, token, self.evaluate_pp_expression)

    def on_elifdef(self, lexer, token):
        self.else_section(lexer, token, partial(self.test_defined, False))

    def on_elifndef(self, lexer, token):
        self.else_section(lexer, token, partial(self.test_defined, True))

    def on_else(self, lexer, token):
        self.else_section(lexer, token, None)

    def on_endif(self, lexer, token):
        if not lexer.if_sections:
            self.diag(DID.endif_without_if, token.loc)
        else:
            if_section = lexer.if_sections.pop()
            self.skipping = if_section.was_skipping
        self.skip_to_eod(token, True)

    def skip_to_eod(self, token, diagnose):
        if diagnose is True:
            self.get_token(token)
        if token.kind == TokenKind.EOD:
            return
        if diagnose:
            spelling = self.token_spelling(self.directive_name_loc)
            self.diag(DID.extra_directive_tokens, token.loc, [spelling])
        # For efficiency, drop out of macro contexts to a lexer
        while True:
            lexer = self.sources[-1]
            if isinstance(lexer, Lexer):
                break
            self.sources.pop()
        while token.kind != TokenKind.EOD:
            lexer.get_token(token)

    def invalid_directive(self, lexer, token):
        self.diag(DID.invalid_directive, token.loc, [self.token_spelling(token.loc)])
        self.skip_to_eod(token, False)

    def diagnostic_directive(self, lexer, token, did):
        '''Handle #error and #warning.'''
        lexer.quiet = True
        diag_loc = token.loc
        text = bytearray()
        while True:
            lexer.get_token(token)
            if token.kind == TokenKind.EOD:
                break
            if token.flags & TokenFlags.WS and text:
                text.append(32)
            text.extend(lexer.fast_utf8_spelling(token.loc - lexer.start_loc, lexer.cursor))
        self.diag(did, diag_loc, [bytes(text)])
        lexer.quiet = False

    def evaluate_pp_expression(self, lexer, token):
        self.expand_macros = True
        value, token = self.expr_parser.parse_and_evaluate_constant_expr()
        # 1 rather than True means "do not consume token"
        self.skip_to_eod(token, int(not value.is_erroneous))
        return bool(value.value)

    def is_macro_name(self, token):
        '''Return True if token is a macro name.  If it is not a diagnostic is issued.'''
        if token.kind == TokenKind.IDENTIFIER:
            return True
        if token.kind == TokenKind.EOD:
            self.diag(DID.expected_macro_name, token.loc)
        else:
            self.diag(DID.macro_name_not_identifier, token.loc)
        return False

    def is_defined(self, token):
        '''Test is a macro is defined.  Return a pair (is_defined, is_macro_name).  is_macro_name
        is True if it is a valid identifier.  If it is not a diagnostic is issued.
        '''
        is_macro_name = self.is_macro_name(token)
        if is_macro_name:
            return bool(token.extra.macro), True
        return False, False

    def test_defined(self, negate, lexer, token):
        lexer.get_token(token)
        is_defined, is_macro_name = self.is_defined(token)
        self.skip_to_eod(token, is_macro_name)
        return not is_defined if negate else bool(is_defined)

    def elaborated_location(self, loc):
        '''Convert a location to a (line_offset, line number, column) tuple.
        The offset can range up to and including the buffer size.
        '''
        if loc <= location_none:
            return ElaboratedLocation(loc, None)
        return ElaboratedLocation(loc, self.locator.loc_to_buffer_coords(loc))

    def elaborated_range(self, source_range):
        if isinstance(source_range, SpellingRange):
            # Convert the SpellingRange to a BufferRange
            assert source_range.start < source_range.end
            token_loc = source_range.token_loc
            buffer, offset = self.locator.loc_to_buffer_and_offset(token_loc)
            lexer = Lexer(self, buffer.text, token_loc - offset, quiet=True)
            token = Token.create()
            lexer.cursor = offset
            lexer.get_token(token)
            offsets = [source_range.start, source_range.end]
            lexer.utf8_spelling(offset, lexer.cursor, offsets)
            source_range = BufferRange(offsets[0], offsets[1])

        if isinstance(source_range, BufferRange):
            start = self.elaborated_location(source_range.start)
            end = self.elaborated_location(source_range.end)
            assert start.coords.buffer is end.coords.buffer
        elif isinstance(source_range, TokenRange):
            start = self.elaborated_location(source_range.start)
            if source_range.start == source_range.end:
                end = start
            else:
                end = self.elaborated_location(source_range.end)
            if source_range.start > location_none:
                token_end = source_range.end + self.token_length(end.loc)
                end = self.elaborated_location(token_end)
        else:
            raise RuntimeError(f'unhandled source range {source_range}')

        return ElaboratedRange(start, end)

    def diagnostic_contexts(self, context):
        '''Expand the diagnostic context stack for the given diagnostic context.'''
        # Apart from the caret range, all ranges must be TokenRange instances
        assert all(isinstance(source_range, TokenRange)
                   for source_range in context.source_ranges[1:])

        # Special ranges don't have source text, and only a single location code
        if context.source_ranges[0].start <= location_none:
            assert len(context.source_ranges) == 1
            contexts = [context]
        else:
            contexts = self.locator.diagnostic_contexts(self, context)

        for context in contexts:
            # Remove duplicates
            source_ranges = context.source_ranges
            source_ranges = [source_range for n, source_range in enumerate(source_ranges)
                             if source_range not in source_ranges[:n]]
            context.source_ranges = [self.elaborated_range(source_range)
                                     for source_range in source_ranges]

        return contexts
