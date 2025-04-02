# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#

import sys
from copy import copy
from dataclasses import dataclass
from enum import IntEnum, auto
from functools import partial

from ..basic import Buffer, Host, UnicodeKind
from ..diagnostics import (
    DID, Diagnostic, location_command_line, location_none,
)

from .basic import (
    IdentifierInfo, SpecialKind, Token, TokenKind, TokenFlags, Encoding,
    TargetMachine, IntegerKind, Charset
)
from .expressions import ExprParser
from .file_manager import FileManager, SearchResult
from .lexer import Lexer
from .literals import LiteralInterpreter
from .locator import Locator, ScratchEntryKind
from .macros import (
    Macro, MacroFlags, ObjectLikeExpansion, FunctionLikeExpansion, BuiltinMacroExpansion,
    BuiltinKind, predefines,
)


__all__ = ['Preprocessor', 'PreprocessorActions']


@dataclass(slots=True)
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


@dataclass(slots=True)
class Language:
    kind: str         # Should be 'C' or 'C++'
    year: int

    def is_cxx(self):
        return self.kind == 'C++'


class SourceFileChangeReason(IntEnum):
    enter = auto()    # via #include, command line, predefine buffer, etc.
    leave = auto()    # end of file reached
    line = auto()     # line directive


class PreprocessorActions:
    '''These functions are called when the preprocessor performs certain actions.  Subclass or
    instantiate to customize behaviour.
    '''

    def on_source_file_change(self, loc, reason):
        '''Called when entering a new soure file, leaving a source file, or on a #line directive
        (even if the file name remains unchanged).  loc is the first location of the new
        context, and reason is a SourcefileChangeReason.'''
        pass

    def on_macro_defined(self, macro):
        '''Called when a macro is defined.'''
        pass


class Preprocessor:

    condition_directives = set(b'if ifdef ifndef elif elifdef elifndef else endif'.split())
    read_stdin = sys.stdin.buffer.read

    def __init__(self):
        '''Perform initialization that is not dependent on customization of, e.g., choice of
        language standard or target.  Such context-sensitive initialization is done by the
        caller calling initialize(), which must happen before pushing the main file.
        '''
        self.language = Language('C++', 2023)
        # Helper objects.
        self.identifiers = {}
        # The host abstraction
        self.host = Host.host()
        # Tracks locations
        self.locator = Locator(self)
        # Caches header lookups and file contents
        self.file_manager = FileManager(self.host)
        # Diagnostics are sent here
        self.diagnostic_consumer = None
        # Action listener
        self.actions = None
        # The expression parser parses and evaluates preprocessor expressions.  The
        # literal interpreter interprets literal values as they would be for a front-end.
        # Both are depdendent on the compilation target so their initialization is
        # deferred to initialize().
        self.target = None
        self.expr_parser = None                 # Deferred
        self.literal_interpreter = None         # Deferred

        # Token source stack
        self.sources = []

        # Internal state
        self.collecting_arguments = False
        self.directive_name_loc = None
        self.error_limit = 20
        self.expand_macros = True
        self.halt = False
        self.in_directive = False
        self.in_header_name = False
        self.in_variadic_macro_definition = False
        self.lexing_scratch = False
        self.skipping = False
        self.predefining_macros = False
        # The date and time of compilation if __DATE__ or __TIME__ is seen.
        self.time_str = None
        self.date_str = None
        self.command_line_buffer = None

    def initialize(self, target=None, exec_charset=None, wide_exec_charset=None):
        def set_charset(attrib, charset_name, integer_kind):
            if charset_name:
                try:
                    charset = Charset.from_name(charset_name)
                except LookupError:
                    self.diag(DID.unknown_charset, location_command_line, [charset_name])
                    return

                encoding_unit_size = charset.encoding_unit_size()
                unit_width = self.target.integer_width(integer_kind)
                if encoding_unit_size * 8 != unit_width:
                    self.diag(DID.invalid_charset, location_command_line,
                              [charset_name, integer_kind.name, unit_width])
                    return
                setattr(self.target, attrib, charset)

        # Set the target first
        self.target = target or TargetMachine.default()

        # These are dependent on target so must come later
        self.literal_interpreter = LiteralInterpreter(self, False)
        self.expr_parser = ExprParser(self)
        if exec_charset:
            set_charset('narrow_charset', exec_charset, IntegerKind.char)
        if wide_exec_charset:
            set_charset('wide_charset', wide_exec_charset, IntegerKind.wchar_t)

        # Standard search paths
        self.file_manager.add_standard_search_paths(
            self.host.standard_search_paths(self.language.is_cxx()))

        # Alternative tokens exist only in C++.  In C they are macros in <iso646.h>.
        if self.language.is_cxx():
            alt_tokens = {
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
            for spelling, token_kind in alt_tokens.items():
                self.get_identifier(spelling).set_alt_token(token_kind)

        # Encoding prefixes and directive names should all be modified by language
        for spelling in (b'include define undef line error warning pragma if ifdef ifndef '
                         b'elif elifdef elifndef else endif').split():
            self.get_identifier(spelling).set_directive()

        encoding_prefixes = {
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
        for spelling, encoding in encoding_prefixes.items():
            self.get_identifier(spelling).set_encoding(encoding)

        # The variadic macro identifiers
        for spelling in (b'__VA_ARGS__', b'__VA_OPT__'):
            self.get_identifier(spelling).set_va_identifier()

        # Built-in macros
        self.get_identifier(b'__DATE__').macro = BuiltinKind.DATE
        self.get_identifier(b'__TIME__').macro = BuiltinKind.TIME
        self.get_identifier(b'__FILE__').macro = BuiltinKind.FILE
        self.get_identifier(b'__LINE__').macro = BuiltinKind.LINE

    def set_command_line_macros(self, defines, undefines):
        def buffer_lines():
            for define in defines:
                pair = define.split('=', maxsplit=1)
                if len(pair) == 1:
                    name, definition = pair[0], '1'
                else:
                    name, definition = pair
                yield f'#define {name} {definition}'
            for name in undefines:
                yield f'#undef {name}'
            yield ''   # So join() adds a final newline

        # The command line buffer is processed when the main buffer is pushed.
        self.command_line_buffer = '\n'.join(buffer_lines()).encode()

    def interpret_literal(self, token):
        return self.literal_interpreter.interpret(token)

    def set_diagnostic_consumer(self, consumer):
        '''Set the consumer, return the previous one.'''
        result = self.diagnostic_consumer
        self.diagnostic_consumer = consumer
        return result

    def diag(self, did, loc, args=None):
        self.emit(Diagnostic(did, loc, args))

    def emit(self, diagnostic):
        # Suppress diagnostics with source locations
        if self.halt and diagnostic.loc >= 0:
            return
        # Emit these instead as invalid token concatentation or stringizing
        if self.lexing_scratch and diagnostic.did in (
                DID.unterminated_block_comment, DID.incomplete_UCN_as_tokens,
                DID.unterminated_literal):
            return False

        consumer = self.diagnostic_consumer
        if consumer:
            consumer.emit(diagnostic)
            if consumer.fatal_error_count or consumer.error_count >= self.error_limit:
                self.halt_compilation()

    def get_identifier(self, spelling):
        ident = self.identifiers.get(spelling)
        if not ident:
            ident = IdentifierInfo(spelling, None, 0)
            self.identifiers[spelling] = ident
        return ident

    def lex_spelling_quietly(self, spelling):
        '''Lex a token from the spelling.  Return the token and the number of bytes consumed.'''
        lexer = Lexer(self, spelling + b'\0', 1)
        token = lexer.get_token_quietly()
        return token, lexer.cursor

    def maybe_identifier(self, spelling):
        '''Returns an IdentifierInfo is spelling is the spelling of a valid identifier, otherwise
        None.
        '''
        # It must be an identifier and have consumed the entire spelling.
        token, consumed = self.lex_spelling_quietly(spelling)
        if token.kind == TokenKind.IDENTIFIER and consumed == len(spelling):
            return token.extra
        return None

    def lexer_at_loc(self, loc):
        '''Return a new lexer ready to lex the spelling of the token at loc.'''
        text, offset = self.locator.buffer_text_and_offset(loc)
        lexer = Lexer(self, text + b'\0', loc - offset)
        lexer.cursor = offset
        return lexer

    def token_spelling_at_loc(self, loc):
        '''Return the spelling of the token at loc.'''
        return self.lexer_at_loc(loc).token_spelling_at_cursor()

    def token_spelling(self, token):
        '''Return the spelling of a token.  Faster than token_spelling_at_loc(), so is preferable
        if a token rather than alocation is available.
        '''
        assert isinstance(token, Token)
        if token.kind == TokenKind.IDENTIFIER:
            return token.extra.spelling
        if token.is_literal():
            spelling, _ = token.extra
            return spelling
        if token.kind == TokenKind.PLACEMARKER:
            return b''
        # FIXME: can spell most (all?) other tokens immediately too
        return self.token_spelling_at_loc(token.loc)

    def read_file(self, path, diag_loc):
        raw = self.file_manager.read_file(path)
        if isinstance(raw, str):
            error_str = raw
            filename = self.filename_to_string_literal(path)
            self.diag(DID.cannot_read_file, diag_loc, [filename, error_str])
            raw = None
        return raw

    def push_main_source_file(self, filename):
        '''Push the main source file onto the preprocessor's source file stack.  Return True on
        success.  Otherwise return False, and the caller must abandon the compilation and
        not call get_token().

        filename is the path to the filename.  '-' reads from stdin (all at once -
        processing doesn't begin until EOF).
        '''
        assert not self.sources

        if filename == '-':
            filename = '<stdin>'
            raw = self.read_stdin()
        else:
            # Push an empty buffer on failure
            raw = self.read_file(filename, location_command_line) or b''

        self.push_buffer(raw, filename)
        if self.halt:
            self.halt_compilation()
        else:
            if self.command_line_buffer:
                self.push_buffer(self.command_line_buffer, '<command line>')
            raw_predefines = predefines(self).encode()
            self.push_buffer(raw_predefines, '<predefines>')
            self.predefining_macros = True

    def halt_compilation(self):
        self.halt = True
        if not self.sources:
            # push_main_source_file() will call us back
            return
        # Move the main lexer to EOF and drop other token sources, so that frontends exit
        # immediately
        lexer = self.sources[0]
        lexer.cursor = len(lexer.buff) - 1  # The NUL byte
        self.sources = [lexer]

    def finish(self):
        '''Emit a compilation summary and return an exit code.

        The preprocessor frontend should call this when it has finished processing, and it will
        no longer call get_token().'''
        assert len(self.sources) == 1

        fatal_error_count = self.diagnostic_consumer.fatal_error_count
        error_count = self.diagnostic_consumer.error_count
        filename = self.locator.primary_source_file_name()
        if fatal_error_count:
            self.emit(Diagnostic(DID.compilation_halted, location_none))
            if error_count:
                self.emit(Diagnostic(DID.fatal_error_and_error_summary, location_none,
                                     [fatal_error_count, error_count, filename]))
            else:
                self.emit(Diagnostic(DID.fatal_error_summary, location_none,
                                     [fatal_error_count, filename]))
            return 4
        if error_count:
            if error_count >= self.error_limit:
                self.emit(Diagnostic(DID.error_limit_reached, location_none))
            self.emit(Diagnostic(DID.error_summary, location_none,
                                 [error_count, filename]))
            return 2
        return 0

    def push_buffer(self, raw, filename):
        '''Push a lexer token source for the raw bytes, and return it.

        Also push an entry in the file manager's file stack, and inform listeners that
        the source file changed.
        '''
        # Deal with stacking an entry in the file manager
        if isinstance(filename, SearchResult):
            search_result = filename
        else:
            search_result = self.file_manager.dummy_search_result(filename)
        self.file_manager.enter_file(search_result)
        # Get the filename as a string literal and create the lexer token source
        filename_literal = self.filename_to_string_literal(search_result.path)
        raw += b'\0'
        buffer = Buffer(raw)
        first_loc = self.locator.new_buffer_loc(buffer, filename_literal, -1)
        lexer = Lexer(self, raw, first_loc)
        lexer.if_sections = []
        self.push_source(lexer)
        if self.actions:
            self.actions.on_source_file_change(first_loc, SourceFileChangeReason.enter)
        return lexer

    def push_source(self, source):
        self.sources.append(source)

    def pop_source_and_get_token(self, token):
        self.pop_source()
        self.get_token(token)

    def pop_source(self):
        source = self.sources.pop()
        if isinstance(source, Lexer):
            self.file_manager.leave_file()
            self.predefining_macros = False
            if self.actions:
                cursor_loc = self.sources[-1].cursor_loc()
                self.actions.on_source_file_change(cursor_loc, SourceFileChangeReason.leave)
        return self.sources[-1]

    def filename_to_string_literal(self, filename):
        '''Convert a python command-line string to a string literal.

        Strings passed to the program from the environment or command line need not be
        unicode.  Python has a nice hack (PEP 383 https://peps.python.org/pep-0383/) to
        hide this from Python code, so strings passed to file system code (such as open())
        the original byte sequence is recovered.  Unix permits arbirary byte sequences to
        be file names (except that they are NUL-terminated), but some MacOSX native
        filesystems require filenames to be valid NFD unicode (Mac OS versions 8.1 through
        10.2.x used decompositions based on Unicode 2.1. Mac OS X version 10.3 and later
        use decompositions based on Unicode 3.2).  Windows uses UTF-16 to encode
        filenames.

        Apart from when dealing with the filesytem, file names are stored in the
        preprocessor as string literals.  This is the appropriate form for diagnostics and
        __FILE__, and is what appears in #line directives (see on_line() for how they are
        handled).
        '''
        # Python passes around magically encoded strings if they are not valid UTF-8.
        # Convert them to their original byte form.
        if isinstance(filename, str):
            filename = filename.encode(sys.getfilesystemencoding(), 'surrogateescape')
        # Some language standards should degrade the UnicodeKind so the string literals
        # can be read back in.
        return UnicodeKind.character.bytes_to_string_literal(filename)

    def pass_through_eof(self, source):
        # EOF is currently generated in 3 cases: 1) by the lexer at end-of-buffer, 2) by
        # the lexer at end of directive (if pp.in_directive is true), and 3) by
        # UnexpandedArgument, when pre-expanding a macro argument to terminate it.  We
        # must pass through cases 2) and 3).  For case 1), we must pass it through if we
        # are collecting arguments, or if there are no more source buffers, otherwise we
        # remove the current source buffer and continue.
        if isinstance(source, Lexer):
            return self.in_directive or self.collecting_arguments or len(self.sources) == 1

        # Terminate macro argument pre-expansion
        return True

    def get_token(self, token):
        # Take tokens from the currently active source.
        source = self.sources[-1]

        while True:
            source.get_token(token)

            # Handle preprocessing directives.  This must happen before macro expansion.
            if token.kind == TokenKind.HASH and token.flags & TokenFlags.BOL:
                self.handle_directive(source, token)
                source = self.sources[-1]
                continue

            if token.kind == TokenKind.EOF:
                if self.pass_through_eof(source):
                    pass
                else:
                    source = self.pop_source()
                    continue
            elif self.skipping:
                continue

            if token.kind == TokenKind.IDENTIFIER:
                self.maybe_enter_macro(token)

            return

    def maybe_enter_macro(self, token):
        '''token is an identifier.  If it is an enabled macro, enter its expansion.'''
        if not self.expand_macros or token.is_disabled():
            return
        macro = token.extra.macro
        if macro is None:
            return
        if isinstance(macro, BuiltinKind):
            self.push_source(BuiltinMacroExpansion(self, token.loc, macro))
        else:
            if macro.is_disabled():
                # Disable this token forever from later expansion
                token.disable()
                return
            if macro.flags & MacroFlags.IS_FUNCTION_LIKE:
                if self.peek_token_kind() != TokenKind.PAREN_OPEN:
                    return
                # Collect the arguments.  Macro expansion is disabled whilst doing this.
                assert not self.collecting_arguments
                self.collecting_arguments = True
                self.expand_macros = False
                arguments = macro.collect_arguments(self, token)
                self.expand_macros = True
                self.collecting_arguments = False
                if arguments is not None:
                    self.push_source(FunctionLikeExpansion(self, macro, token, arguments))
            else:
                self.push_source(ObjectLikeExpansion(self, macro, token))

        # We get the first token (or the next token if collect_arguments() failed).
        self.get_token(token)

    def peek_token_kind(self):
        '''Peek the next token without expanding macros, and return its kind.'''
        for source in reversed(self.sources):
            kind = source.peek_token_kind()
            if kind != TokenKind.PEEK_AGAIN:
                return kind
        raise RuntimeError('no sources left to peek from')

    def handle_directive(self, lexer, token):
        '''Handle a directive to and including the EOF token.  We have read the '#' introducing a
        directive.'''
        def get_handler(lexer, token):
            # Save the directive name's location
            self.directive_name_loc = token.loc
            if token.kind == TokenKind.IDENTIFIER and token.extra.special & SpecialKind.DIRECTIVE:
                # If skipping ignore everything except for conditional directives
                if self.skipping and token.extra.spelling not in self.condition_directives:
                    return self.ignore_directive
                return getattr(self, f'on_{token.extra.spelling.decode()}')
            # Ignore the null directive, and unknown directives when skipping.
            if self.skipping or token.kind == TokenKind.EOF:
                return self.ignore_directive
            # Unknown directive.
            return self.invalid_directive

        assert isinstance(lexer, Lexer)
        self.in_directive = True
        self.expand_macros = False
        # Turn off skipping whilst getting the directive name so that identifier
        # information is attached, and vertical whitespace is caught.
        was_skipping = self.skipping
        self.skipping = False
        lexer.get_token(token)
        self.skipping = was_skipping
        handler = get_handler(lexer, token)
        handler(lexer, token)
        self.expand_macros = True
        self.in_directive = False

    def search_for_header(self, header_token):
        spelling = self.token_spelling(header_token)
        header_name = spelling[1:-1].decode(sys.getfilesystemencoding(), 'surrogateescape')
        if spelling[0] == 60:    # '<'
            return self.file_manager.search_angled_header(header_name)
        else:
            return self.file_manager.search_quoted_header(header_name)

    def on_include(self, lexer, token):
        self.expand_macros = True
        header_token = self.get_header_name(token, self.get_token)
        self.skip_to_eod(token, header_token.kind == TokenKind.HEADER_NAME)
        search_result = self.search_for_header(header_token)
        if search_result:
            raw = self.read_file(search_result.path, header_token.loc)
            if raw is not None:
                self.push_buffer(raw, search_result)
        else:
            spelling, _ = header_token.extra
            self.diag(DID.header_file_not_found, header_token.loc, [spelling])

    def get_header_name(self, token, get_token):
        self.in_header_name = True
        get_token(token)
        self.in_header_name = False
        diag_loc = token.loc
        if token.kind == TokenKind.HEADER_NAME:
            header_token = copy(token)
        else:
            # Try to construct a header name from the spelling of individual tokens
            parent_loc = token.loc
            spelling = bytearray()
            while token.kind != TokenKind.EOF:
                if spelling and token.flags & TokenFlags.WS:
                    spelling.append(32)
                spelling.extend(self.token_spelling(token))
                get_token(token)
            self.in_header_name = True
            header_token, all_consumed = self.lex_from_scratch(spelling, parent_loc,
                                                               ScratchEntryKind.header)
            self.in_header_name = False
            if header_token.kind != TokenKind.HEADER_NAME or not all_consumed:
                self.diag(DID.expected_header_name, diag_loc)
                header_token.kind = TokenKind.ERROR
        return header_token

    def lex_from_scratch(self, spelling, parent_loc, kind):
        '''Place the spelling in a scratch buffer and return a pair (token, all_consumed).
        all_consumed is True if lexing consumed the whole spelling.'''
        # Get a scratch buffer location for the new token
        scratch_loc = self.locator.new_scratch_token(spelling, parent_loc, kind)
        lexer = Lexer(self, spelling + b'\0', scratch_loc)
        token = Token.create()
        self.lexing_scratch = True
        lexer.get_token(token)
        self.lexing_scratch = False
        # Remove the fake BOL flag
        token.flags &= ~TokenFlags.BOL
        return token, lexer.cursor >= len(spelling)

    def on_define(self, lexer, token):
        '''#define directive processing.'''
        lexer.get_token(token)
        is_good = self.is_macro_name(token, 1)
        if is_good:
            macro_ident = token.extra
            macro = self.read_macro_definition(lexer, token)
            if macro:
                self.define_macro(macro_ident, macro)
                if self.actions:
                    self.actions.on_macro_defined(macro)
            else:
                is_good = False
        self.skip_to_eod(token, is_good)

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
            if not token.flags & TokenFlags.WS and token.kind != TokenKind.EOF:
                self.diag(DID.macro_name_whitespace, token.loc)

        tokens = macro.replacement_list
        while token.kind != TokenKind.EOF:
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
            macro.param_names = ', '.join(spelling.decode() for _, spelling in sorted_params)
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
            assert token.kind == TokenKind.EOF
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
            elif token.kind == TokenKind.EOF:
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

            # EOF is always invalid.  An ellipsis must be followed by ')'.  An identifier
            # must be followed by ',' or ')'.
            if token.kind == TokenKind.EOF or prior_kind == TokenKind.ELLIPSIS:
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
        # predefined macro redefinitions were already diagnosed
        if (prior is not None and not prior.is_predefined()
                and not self.compare_macro_definitions(prior, macro)):
            self.diag(DID.macro_redefined, macro.name_loc, [
                macro_ident.spelling,
                Diagnostic(DID.prior_macro_definition, prior.name_loc),
            ])
        macro_ident.macro = macro
        if self.predefining_macros:
            macro.flags |= MacroFlags.IS_PREDEFINED

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
            if self.token_spelling(lhs_token) != self.token_spelling(rhs_token):
                return False
        return True

    def on_undef(self, lexer, token):
        '''#undef directive processing.'''
        lexer.get_token(token)
        is_macro_name = self.is_macro_name(token, 2)
        if is_macro_name:
            token.extra.macro = None
        self.skip_to_eod(token, is_macro_name)

    def on_line(self, lexer, token):
        self.expand_macros = True
        # Read the line number - a digit-sequence (i.e. 0-9 with optional ')
        self.get_token(token)
        line_number = self.literal_interpreter.interpret_line_number(token, 2147483648)
        if line_number != -1:
            self.get_token(token)
            if token.kind == TokenKind.EOF:
                filename = self.locator.prior_file_name
            else:
                filename = self.literal_interpreter.interpret_filename(token)
                if filename is not None:
                    filename = self.filename_to_string_literal(filename)

        is_good = line_number != -1 and filename is not None
        self.skip_to_eod(token, is_good)
        # Have the line number take effect from the first character of the next line
        if is_good:
            start_loc = token.loc + 1
            self.locator.add_line_range(start_loc, filename, line_number)
            if self.actions:
                self.actions.on_source_file_change(start_loc, SourceFileChangeReason.line)

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
            self.diag(DID.else_without_if, token.loc, [self.token_spelling(token)])
            return

        section = lexer.if_sections[-1]
        if section.was_skipping:
            self.skip_to_eod(token, False)
            return
        if section.else_loc != -1:
            self.diag(DID.else_after_else, token.loc, [
                self.token_spelling(token),
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
        if token.kind == TokenKind.EOF:
            return
        if diagnose:
            spelling = self.token_spelling_at_loc(self.directive_name_loc)
            self.diag(DID.extra_directive_tokens, token.loc, [spelling])
        # For efficiency, drop out of macro contexts to a lexer
        while True:
            lexer = self.sources[-1]
            if isinstance(lexer, Lexer):
                break
            self.sources.pop()
        while token.kind != TokenKind.EOF:
            lexer.get_token(token)

    def invalid_directive(self, lexer, token):
        self.diag(DID.invalid_directive, token.loc, [self.token_spelling(token)])
        self.skip_to_eod(token, False)

    def diagnostic_directive(self, lexer, token, did):
        '''Handle #error and #warning.'''
        diag_loc = token.loc
        text = bytearray()
        while True:
            token = lexer.get_token_quietly()
            if token.kind == TokenKind.EOF:
                break
            if token.flags & TokenFlags.WS and text:
                text.append(32)
            text.extend(lexer.fast_utf8_spelling(token.loc - lexer.start_loc, lexer.cursor))
        self.diag(did, diag_loc, [bytes(text)])

    def evaluate_pp_expression(self, lexer, token):
        self.expand_macros = True
        value, token = self.expr_parser.parse_and_evaluate_constant_expr()
        # 1 rather than True means "do not consume token"
        self.skip_to_eod(token, int(not value.is_erroneous))
        return bool(value.value)

    def is_macro_name(self, token, define_or_undef):
        '''Return True if token is a macro name and valid for its context.  define_or_undef is 1
        for #define, 2 for #undef, and 0 otherwise (#ifdef, unary defined etc.).  A
        diagnostic is issued if appropriate.
        '''
        if token.kind == TokenKind.IDENTIFIER:
            if not define_or_undef:
                return True
            selector = define_or_undef - 1
            # There are several restrictions on identifiers that are defined or undefined
            if (ident := token.extra) is self.expr_parser.defined:
                self.diag(DID.cannot_be_defined, token.loc, [ident.spelling, selector])
                return False
            if (macro := ident.macro) is None:
                return True
            if macro.is_builtin():
                self.diag(DID.builtin_macro_redefined, token.loc, [ident.spelling, selector])
                return False
            if macro.is_predefined():
                note = Diagnostic(DID.prior_macro_definition, macro.name_loc)
                self.diag(DID.predefined_macro_redefined, token.loc,
                          [ident.spelling, selector, note])
            return True

        if token.kind == TokenKind.EOF:
            self.diag(DID.expected_macro_name, token.loc)
        else:
            self.diag(DID.macro_name_not_identifier, token.loc)
        return False

    def is_defined(self, token):
        '''Test is a macro is defined.  Return a pair (is_defined, is_macro_name).  is_macro_name
        is True if it is a valid identifier.  If it is not a diagnostic is issued.
        '''
        is_macro_name = self.is_macro_name(token, 0)
        if is_macro_name:
            return bool(token.extra.macro), True
        return False, False

    def test_defined(self, negate, lexer, token):
        lexer.get_token(token)
        is_defined, is_macro_name = self.is_defined(token)
        self.skip_to_eod(token, is_macro_name)
        return not is_defined if negate else bool(is_defined)
