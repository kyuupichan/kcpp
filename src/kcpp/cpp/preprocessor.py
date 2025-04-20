# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#

import sys
from contextlib import contextmanager
from copy import copy
from dataclasses import dataclass
from enum import IntEnum, auto
from functools import partial

from ..core import (
    Buffer, IntegerKind, IdentifierInfo, SpecialKind, Token, TokenKind, TokenFlags,
    Encoding, targets, host
)
from ..diagnostics import DID, Diagnostic, TokenRange, location_command_line, location_none
from ..parsing import ParserState
from ..unicode import Charset, CodepointOutputKind

from .expressions import ExprParser
from .file_manager import FileManager, DirectoryKind
from .lexer import Lexer
from .literals import LiteralInterpreter, destringize
from .locator import Locator, ScratchEntryKind
from .macros import (
    Macro, MacroFlags, MacroExpansion, BuiltinKind, expand_builtin_macro, predefines,
)


__all__ = ['Preprocessor', 'PreprocessorActions', 'Config', 'Language']


@dataclass(slots=True)
class IfSection:
    '''Represents a conditional preprocessing group.'''
    # True if the preprocessor was skipping on entry to the #if
    was_skipping: bool
    # True if one of the if / elif conditions in this group so far is true.  Used to
    # determine whether a following #else is skipped, or a following #elif is processed.
    true_condition_seen: bool
    # If #else has been seen, its location, otherwise -1
    else_loc: int
    # Location of opening directive
    opening_loc: int


@dataclass(slots=True)
class BufferState:
    '''Maintains per-bufer preprocessor state.'''
    if_sections: list


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

    def on_define(self, macro):
        '''Called when a macro is defined.  macro is a Macro object.'''
        pass

    def on_pragma(self, token):
        '''Called on #pragma whose namespace is not registered.  token is the first token after
        the #pragma (the namespace token).  Obtain subsequent tokens with pp.get_token().
        A token will have kind TokenKind.EOF at the end of the directive.

        Return True to diagnose extra tokens in the directive on return, False to not
        diagnose extra tokens.
        '''
        return False

    def on_source_file_change(self, loc, reason):
        '''Called when entering a new soure file, leaving a source file, or on a #line directive
        (even if the file name remains unchanged).  Not called on leaving the primary
        source file.

        loc is the first location of the new context, and reason is a SourcefileChangeReason.
        '''
        pass

    def on_undef(self, token):
        '''Called when a macro is undefined.  token is the identifier token.'''
        pass


@dataclass(slots=True)
class Config:
    '''Configures the preprocessor.  Usually these are set from the command line or
    environment variables.
    '''
    output: str
    language: Language
    target_name: str
    narrow_exec_charset: str
    wide_exec_charset: str
    source_date_epoch: str
    defines: list
    undefines: list
    includes: list
    quoted_dirs: list
    angled_dirs: list
    system_dirs: list
    max_include_depth: int
    trace_includes: bool

    @classmethod
    def default(cls):
        return cls(
            '',                          # output
            Language('C++', 2023),       # language
            '',                          # target_name
            '', '',                      # narrow and wide exec charsets
            '',                          # source date epoch
            [], [], [],                  # defines, undefines, includes
            [], [], [],                  # quoted, angled, system dirs
            -1,                          # max include depth
            False,                       # trace_includes
        )


class Preprocessor:
    '''Object that coordinates preprocessing.'''

    condition_directives = set(b'if ifdef ifndef elif elifdef elifndef else endif'.split())
    read_stdin = sys.stdin.buffer.read
    tz = None    # for __DATE__ and __TIME__; None means local timezone

    def __init__(self, diag_manager):
        '''Basic initialization.  Customization, and initialization based on that, comes
        with a call to push_main_source_file().
        '''
        # Language and target
        self.language = None
        self.target = None

        # Tracks locations
        self.locator = Locator(self)
        # Caches header lookups and file contents
        self.file_manager = FileManager()
        # Diagnostics
        diag_manager.locator = self.locator
        diag_manager.file_manager = self.file_manager
        self.diag_manager = diag_manager

        # Output files
        self.stdout = sys.stdout

        # Helper objects.
        self.identifiers = {}
        # Action listener
        self.actions = None

        # The expression parser parses and evaluates preprocessor expressions.  The
        # literal interpreter interprets literal values as they would be for a front-end.
        # Both are depdendent on the compilation target so their initialization is
        # deferred to initialize().
        self.expr_parser = None                 # Deferred
        self.literal_interpreter = None         # Deferred
        self.max_include_depth = 100

        # Token source stack
        self.sources = []
        # Map from pragma namespace identifier strings (in binary) to callbacks.
        self.pragma_namespaces = {}
        # The basic charset and raw string delimiter characters
        self.basic_charset = set(
            b'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz'
            b'!"#%&\'*+,-./:;<=>?[]^{|}~ ()\\\t\v\f\r\n'
        )

        # Internal state
        self.collecting_arguments = []
        self.directive_name_loc = None
        self.expand_macros = True
        self.ignore_diagnostics = 0
        self.in_directive = False
        self.in_if_elif_directive = False
        self.in_header_name = False
        self.in_variadic_macro_definition = False
        self.lexing_scratch = False
        self.predefining_macros = False
        self.skip_to_eod = False
        self.skipping = False
        self.trace_includes = False
        # Collected whilst in a macro-expanding directive.  Handled when leaving the
        # directive.
        self._Pragma_strings = []

        # The date and time of compilation if __DATE__ or __TIME__ is seen.
        self.date_str = None
        self.time_str = None
        self.counter = 0                       # For __COUNTER__.
        self.source_date_epoch = None          # For reproducible timestamps
        self.command_line_buffer = None

    def _configure(self, config):
        '''Configure the preprocessor.'''
        config = config or Config.default()

        if config.output:
            result = host.open_file_for_writing(config.output)
            if isinstance(result, str):
                self.diag(DID.cannot_write_file, location_command_line, [config.output, result])
            else:
                self.stdout = result

        self.language = config.language

        # Next the target as others depend on this
        target = None
        if config.target_name:
            target = targets.get(config.target_name)
            if not target:
                self.diag(DID.unknown_target, location_command_line, [config.target_name])
        if not target:
            target = targets['aarch64-apple-darwin']
        self.target = copy(target)

        # Extended basic character set?  This will depend on language
        if False:
            self.basic_charset.update('$@`')

        # Set the narrow and wide charsets
        def set_charset(attrib, charset_name, integer_kind):
            if not charset_name:
                return
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

        set_charset('narrow_charset', config.narrow_exec_charset, IntegerKind.char)
        set_charset('wide_charset', config.wide_exec_charset, IntegerKind.wchar_t)

        # Standard search paths; language-dependent
        self.file_manager.add_standard_search_paths(
            host.standard_search_paths(self.language.is_cxx()))
        # User-defined search paths
        self.file_manager.add_search_paths(config.quoted_dirs, DirectoryKind.quoted)
        self.file_manager.add_search_paths(config.angled_dirs, DirectoryKind.angled)
        self.file_manager.add_search_paths(config.system_dirs, DirectoryKind.system)

        # Source date epoch
        if config.source_date_epoch:
            max_epoch = 253402300799
            invalid = True
            if config.source_date_epoch.isdigit():
                epoch = int(config.source_date_epoch)
                if epoch >= 0 and epoch <= max_epoch:
                    self.source_date_epoch = epoch
                    invalid = False
            if invalid:
                self.diag(DID.bad_source_date_epoch, location_command_line, [max_epoch])

        # The command line buffer
        def buffer_lines(config):
            for define in config.defines:
                pair = define.split('=', maxsplit=1)
                if len(pair) == 1:
                    name, definition = pair[0], '1'
                else:
                    name, definition = pair
                yield f'#define {name} {definition}'
            for name in config.undefines:
                yield f'#undef {name}'
            # Note: because the command line pseudo-file-name does not have a path, the
            # current file has no path name, so includes are looked up relative to the
            # current working directory.  This is the same as GCC.
            for filename in config.includes:
                yield f'#include "{filename}"'
            yield ''   # So join() adds a final newline

        # The command line buffer is processed when the main buffer is pushed.
        self.command_line_buffer = '\n'.join(buffer_lines(config)).encode()
        # Max include depth
        if config.max_include_depth >= 0:
            self.max_include_depth = config.max_include_depth
        self.trace_includes = config.trace_includes

    def initialize(self, config):
        '''Configure and then finish general initialization.'''
        self._configure(config)

        # These are dependent on target so come after configuration
        self.literal_interpreter = LiteralInterpreter(self, False)
        self.expr_parser = ExprParser(self)

        for spelling in (b'include define undef line error warning pragma if ifdef ifndef '
                         b'elif elifdef elifndef else endif').split():
            self.get_identifier(spelling).set_directive()

        encoding_prefixes = {
            b'': Encoding.NONE,
            b'L': Encoding.WIDE,
            b'u8': Encoding.UTF_8,
            b'u': Encoding.UTF_16,
            b'U': Encoding.UTF_32,
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
        self.get_identifier(b'__COUNTER__').macro = BuiltinKind.COUNTER
        self.get_identifier(b'_Pragma').macro = BuiltinKind.Pragma

        # Built-in has-feature pseudo-macros
        self.get_identifier(b'__has_include').macro = BuiltinKind.has_include

        # C++-specific features
        if self.language.is_cxx():
            # Alternative tokens exist only in C++.  In C they are macros in <iso646.h>.
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

            # Raw strings
            encoding_prefixes = {
                b'R': Encoding.RAW,
                b'LR': Encoding.WIDE_RAW,
                b'u8R': Encoding.UTF_8_RAW,
                b'uR': Encoding.UTF_16_RAW,
                b'UR': Encoding.UTF_32_RAW,
            }
            for spelling, encoding in encoding_prefixes.items():
                self.get_identifier(spelling).set_encoding(encoding)

            # Module keywords.  "export" is overloaded as a standard keyword too....
            self.get_identifier(b'export').set_module_keyword(TokenKind.kw_export_keyword)
            self.get_identifier(b'import').set_module_keyword(TokenKind.kw_import_keyword)
            self.get_identifier(b'module').set_module_keyword(TokenKind.kw_module_keyword)
            self.get_identifier(b'__has_cpp_attribute').macro = BuiltinKind.has_cpp_attribute
            self.attributes_by_scope = {
                b'': {
                    b'assume': '202207L',
                    b'carries_dependency': '200809L',
                    b'deprecated': '201309L',
                    b'fallthrough': '201603L',
                    b'likely': '201803L',
                    b'maybe_unused': '201603L',
                    b'no_unique_address': '201803L',
                    b'nodiscard': '201907L',
                    b'noreturn': '200809L',
                    b'unlikely': '201803L'
                }
        }

        return not self.diag_manager.should_halt_compilation()

    def register_pragma_namespace(self, spelling, handler):
        '''Register a pragma namespace with the given handler, and return the previously
        registered handler (or None).  The handler has the same signature and semantics as
        actions.on_pragma().  It is passed the namespace token.
        '''
        assert isinstance(spelling, bytes)
        prior_handler = self.pragma_namespaces.get(spelling)
        self.pragma_namespaces[spelling] = handler
        return prior_handler

    def interpret_literal(self, token):
        return self.literal_interpreter.interpret(token)

    def diag(self, did, loc, args=None):
        self.emit(Diagnostic(did, loc, args))

    def emit(self, diagnostic):
        if not self.ignore_diagnostics:
            # Emit these instead as invalid token concatentation
            if self.lexing_scratch and diagnostic.did in (
                    DID.unterminated_block_comment, DID.incomplete_UCN_as_tokens):
                return
            if self.diag_manager.emit(diagnostic):
                self.halt_compilation()

    def ice(self, loc, text):
        self.emit(Diagnostic(DID.internal_compiler_error, loc, [text]))

    def get_identifier(self, spelling):
        ident = self.identifiers.get(spelling)
        if not ident:
            ident = IdentifierInfo(spelling, None, 0)
            self.identifiers[spelling] = ident
        return ident

    def lex_spelling_quietly(self, spelling):
        '''Lex a token from the spelling.  Return the token and the number of bytes consumed.
        Its location is meaningless.'''
        lexer = Lexer(self, spelling + b'\0', 1, None)
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

    def token_spelling_at_loc(self, loc):
        '''Return the spelling of the token at loc.'''
        return self.locator.lexer_at_loc(loc).token_spelling_at_cursor()

    def token_spelling(self, token):
        '''Return the spelling of a token.  Faster than token_spelling_at_loc(), so is preferable
        if a token is available.
        '''
        assert isinstance(token, Token)
        if token.kind == TokenKind.IDENTIFIER:
            return token.extra.spelling
        if token.carries_spelling():
            spelling, _ = token.extra
            return spelling
        if token.kind == TokenKind.PLACEMARKER:
            return b''
        # FIXME: can spell most (all?) other tokens immediately too
        return self.token_spelling_at_loc(token.loc)

    def read_file(self, file, diag_loc):
        '''Diagnoses read errors at diag_loc.  Return file on success, None on failure.'''
        result = self.file_manager.read_file(file)
        if isinstance(result, bytes):
            return file
        filename = self.filename_to_string_literal(file.path)
        self.diag(DID.cannot_read_file, diag_loc, [filename, result])
        return None

    def push_main_source_file(self, filename, multiple):
        '''Push the main source file onto the preprocessor's source file stack.
        filename is the path to the filename.  '-' reads from stdin (all at once -
        processing doesn't begin until EOF).
        '''
        assert not self.sources
        # Emit a message saying we are starting the compilation of filename if we are
        # compiling multiple sources.
        if multiple:
            filename_literal = self.filename_to_string_literal(filename)
            self.diag(DID.starting_compilation, location_none, [filename_literal])

        if filename == '-':
            file = self.file_manager.virtual_file('<stdin>', self.read_stdin())
        else:
            file = self.file_manager.file_for_path(filename)
            self.read_file(file, location_command_line)

        # Push a buffer even if we are going to halt so that get_token() returns EOF.
        self.push_buffer(file)
        if not self.diag_manager.should_halt_compilation():
            if self.command_line_buffer:
                self.push_virtual_buffer('<command line>', self.command_line_buffer)
            raw_predefines = predefines(self).encode()
            self.push_virtual_buffer('<predefines>', raw_predefines)
            self.predefining_macros = True

    def halt_compilation(self):
        self.actions = None   # Prevent spurious linemarkers, etc.
        if not self.sources:
            return
        # Move the main lexer to EOF and drop other token sources, so that frontends exit
        # immediately
        while len(self.sources) > 1:
            self.pop_source()
        lexer = self.sources[0]
        lexer.cursor = len(lexer.buff) - 1  # The NUL byte

    def finish(self, primary_source_filename):
        '''Emit a compilation summary and return an exit code.

        The preprocessor frontend should call this when it has finished processing, and it will
        no longer call get_token().'''
        assert not self.sources
        # Returns None if no primary source file was pushed
        filename = self.locator.primary_source_file_name()
        if filename is None:
            filename = self.filename_to_string_literal(primary_source_filename)
        exit_code = self.diag_manager.emit_compilation_summary(filename)
        if self.stdout not in (sys.stderr, sys.stdout):
            self.stdout.close()
        return exit_code

    def push_buffer(self, file):
        '''Push a lexer token source for the raw bytes of filename (which can be a string or a
        file manager File object) , and return it.  Also push an entry in the file
        manager's file stack, and inform listeners that the source file changed.
        '''
        # Get the filename as a string literal and create the lexer token source
        filename_literal = self.filename_to_string_literal(file.path)
        # Stack an entry in the file manager
        self.file_manager.enter_file(file)
        if self.trace_includes and not file.is_virtual:
            depth = self.file_manager.include_depth()
            if depth > 0:
                # Write the literal, not the path, so that odd characters are quoted and
                # output can't be messed up
                self.diag_manager.write(f'{"." * depth} {filename_literal}')
        buffer = Buffer(file.nul_terminated_contents())
        first_loc = self.locator.new_buffer_loc(buffer, filename_literal)
        lexer = Lexer(self, buffer.text, first_loc, BufferState([]))
        lexer.cursor = buffer.bom_length
        self.push_source(lexer)
        if self.actions:
            self.actions.on_source_file_change(first_loc, SourceFileChangeReason.enter)
        return lexer

    def push_virtual_buffer(self, name, raw):
        return self.push_buffer(self.file_manager.virtual_file(name, raw))

    def push_source(self, source):
        self.sources.append(source)

    def pop_source_and_get_token(self):
        self.pop_source()
        return self.get_token()

    def pop_source(self):
        source = self.sources.pop()
        if isinstance(source, Lexer):
            self.pop_buffer(source)

    def pop_buffer(self, lexer):
        # Diagnose unclosed conditional blocks
        for if_section in reversed(lexer.pp_state.if_sections):
            if_loc = if_section.opening_loc
            self.diag(DID.unclosed_if_block, if_loc, [self.token_spelling_at_loc(if_loc)])
        self.file_manager.leave_file()
        self.predefining_macros = False
        if self.actions and self.sources:
            cursor_loc = self.sources[-1].cursor_loc()
            self.actions.on_source_file_change(cursor_loc, SourceFileChangeReason.leave)

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
            if filename == '-':
                filename = '<stdin>'
            filename = filename.encode(sys.getfilesystemencoding(), 'surrogateescape')
        # Some language standards should degrade the CodepointOutputKind so the string
        # literals can be read back in.
        return CodepointOutputKind.character.bytes_to_string_literal(filename)

    def current_lexer(self):
        for source in reversed(self.sources):
            if isinstance(source, Lexer):
                return source

    @contextmanager
    def collect_arguments(self, macro):
        '''Start collecting arguments for the given macro (None for _Pragma).  When collecting
        arguments, an EOF token is returned when the current lexer's end-of-file is reached.
        '''
        self.collecting_arguments.append((self.current_lexer(), macro))
        try:
            yield None
        finally:
            self.collecting_arguments.pop()

    def pass_through_eof(self, source):
        if isinstance(source, Lexer):
            # Pop the buffer unless collecting arguments or in a directive; that state
            # needs to be cleared first.  get_token() will continue with the enclosing
            # buffer, or pass through the EOF if there are none.
            self.skip_to_eod = False
            if self.in_directive:
                return True
            if self.collecting_arguments:
                lexer, _ = self.collecting_arguments[-1]
                if lexer is source:
                    return True
            return False
        # Terminate macro argument pre-expansion
        return True

    def get_token(self):
        while True:
            # Take tokens from the currently active source.
            source = self.sources[-1]
            token = source.get_token()

            # Handle preprocessing directives.  This must happen before macro expansion.
            if token.kind == TokenKind.DIRECTIVE_HASH:
                self.handle_directive(source, token, None)
                continue

            if token.kind == TokenKind.EOF:
                if not self.pass_through_eof(source):
                    self.pop_source()
                    # Continue if there are more sources, otherwise pass on the EOF
                    if self.sources:
                        continue
            elif self.skipping:
                continue
            elif token.kind == TokenKind.IDENTIFIER:
                token = self.maybe_enter_macro(token)

            return token

    def maybe_enter_macro(self, token):
        '''token is an identifier.  If it is an enabled macro, enter its expansion.'''
        if not self.expand_macros or token.is_disabled():
            return token
        macro = token.extra.macro
        if macro is None:
            return token
        if isinstance(macro, BuiltinKind):
            if token.extra.macro == BuiltinKind.Pragma:
                self.on_Pragma(token)
            elif macro.is_has_feature():
                # __has_include, etc., are handled by the preprocessor expression parser.
                # However, this captures invalid uses outside of #if / #elif.
                if not self.in_if_elif_directive:
                    self.diag(DID.builtin_macro_only_if_elif, token.loc,
                              [self.token_spelling(token)])
                return token
            else:
                return expand_builtin_macro(self, token)
        else:
            if macro.is_disabled():
                # Disable this token forever from later expansion
                token.disable()
                return token
            if macro.flags & MacroFlags.IS_FUNCTION_LIKE:
                if self.peek_token_kind() != TokenKind.PAREN_OPEN:
                    return token
                # Eat the opening parenthesis that was already peeked
                assert self.get_token().kind == TokenKind.PAREN_OPEN
            self.push_source(MacroExpansion(self, macro, token.loc, token.flags & TokenFlags.WS))

        # We get the first token (or the next token if collect_arguments() failed, or for
        # has_feature pseudo-macros, or after _Pragma.
        return self.get_token()

    def peek_token_kind(self):
        '''Peek the next token without expanding macros, and return its kind.'''
        for source in reversed(self.sources):
            kind = source.peek_token_kind()
            if kind != TokenKind.PEEK_AGAIN:
                return kind
        raise RuntimeError('no sources left to peek from')

    def handle_directive(self, lexer, token, handler=None):
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
        assert not self.in_directive
        if self.collecting_arguments:
            _, macro = self.collecting_arguments[-1]
            if macro:
                self.diag(DID.directive_in_macro_arguments, token.loc, [macro.macro_name(self)])
        was_expanding_macros = self.expand_macros
        self.expand_macros = False
        self.in_directive = True
        self.skip_to_eod = True
        if handler is None:
            # Turn off skipping whilst getting the directive name so that identifier
            # information is attached, and vertical whitespace is caught.
            was_skipping = self.skipping
            self.skipping = False
            token = lexer.get_token()
            self.skipping = was_skipping
            handler = get_handler(lexer, token)
        handler(token)
        # It is important we don't skip to EOD after pushing an include file!
        if self.skip_to_eod:
            token = self.get_token()
            while token.kind != TokenKind.EOF:
                token = self.get_token()
        self.in_directive = False
        strings = self._Pragma_strings
        if strings:
            self._Pragma_strings = []
            for string in strings:
                self.process_Pragma_string(string)
        self.expand_macros = was_expanding_macros

    def has_attribute_spelling(self, scope_spelling, attrib_spelling):
        values = self.attributes_by_scope.get(scope_spelling)
        if values is not None:
            return values.get(attrib_spelling, '0')
        return '0'

    def read_header_file(self, header_token, *, diagnose_if_not_found):
        spelling = self.token_spelling(header_token)
        header_name = spelling[1:-1].decode(sys.getfilesystemencoding(), 'surrogateescape')
        if spelling[0] == 60:    # '<'
            file = self.file_manager.search_angled_header(header_name)
        else:
            file = self.file_manager.search_quoted_header(header_name)

        # Set in_header_name so that diagnostics lex correctly
        self.in_header_name = True
        if file is None:
            if diagnose_if_not_found:
                self.diag(DID.header_file_not_found, header_token.loc, [header_name])
            result = None
        else:
            result = self.read_file(file, header_token.loc)
        self.in_header_name = False
        return result

    def on_include(self, token):
        self.expand_macros = True
        header_token = self.create_header_name(in__has_include=False)
        if header_token:
            self.diagnose_extra_tokens(None)
            file = self.read_header_file(header_token, diagnose_if_not_found=True)
            if file is not None:
                if self.file_manager.include_depth() >= self.max_include_depth:
                    self.diag(DID.max_include_depth_reached, header_token.loc,
                              [self.max_include_depth])
                else:
                    self.push_buffer(file)

    def create_header_name(self, *, in__has_include):
        '''Read a header name, returning a new token or None.  Return a token of kind
        TokenKind.HEADER_NAME if a valid header name was read.

        This method is used 1) immediately after #include, and 2) after __has_include
        followed by '('.  Their behaviour is similar but subtly different.

        Both must accept a header-name, which is any sequence of characters between the
        "" or <> delimiters other than newline and the terminating delimeter.

        If not a lexical header name, it must be treated as a sequence of macro-expandable
        tokens.  #include expects the result to match a header name in an
        implementation-defined way, which can only happen if the first token lexically
        begins with '<' character and the final one ends with a '>' character.
        __has_include() requires the first token to be a '<' pp-token ('<=' for example
        won't do), and continues until a '>' pp-token.
        '''
        self.in_header_name = True
        token = self.get_token()
        self.in_header_name = False
        if token.kind == TokenKind.HEADER_NAME:
            return token

        first_loc = token.loc
        form_spelling = True
        if in__has_include:
            # __has_include() requires TokenKind.LT or TokenKind.STRING_LITERAL
            form_spelling = token.kind == TokenKind.LT or token.kind == TokenKind.STRING_LITERAL
        if form_spelling:
            # Try to construct a header name from the individual tokens
            spelling = bytearray()
            while token.kind != TokenKind.EOF:
                if spelling and token.flags & TokenFlags.WS:
                    spelling.append(32)
                spelling.extend(self.token_spelling(token))
                # has_include stops on TokenKind.GT
                if in__has_include and (token.kind == TokenKind.GT
                                        or token.kind == TokenKind.STRING_LITERAL):
                    break
                token = self.get_token()

            self.in_header_name = True
            parent_range = TokenRange(first_loc, first_loc)
            token, entirely = self.lex_from_scratch(spelling, parent_range,
                                                    ScratchEntryKind.header)
            self.in_header_name = False
            if token.kind == TokenKind.HEADER_NAME and entirely:
                return token
        self.diag(DID.expected_header_name, first_loc)
        return None

    def lex_from_scratch(self, spelling, parent_range, kind):
        '''Place the spelling in a scratch buffer and return a pair (token, all_consumed).
        all_consumed is True if lexing consumed the whole spelling.'''
        # Get a scratch buffer location for the new token
        scratch_loc = self.locator.new_scratch_token(spelling, parent_range, kind)
        lexer = Lexer(self, spelling + b'\0', scratch_loc, None)
        self.lexing_scratch = True
        token = lexer.get_token()
        self.lexing_scratch = False
        return token, lexer.cursor >= len(spelling)

    def on_define(self, token):
        '''#define directive processing.'''
        lexer = self.sources[-1]
        token = lexer.get_token()
        if self.is_macro_name(token, 1):
            macro_ident = token.extra
            macro = self.read_macro_definition(lexer, token)
            if macro:
                self.define_macro(macro_ident, macro)
                if self.actions:
                    self.actions.on_define(macro)

    def read_macro_definition(self, lexer, token):
        '''Lex a macro definition.  Return a macro definition, or None.'''
        macro = Macro(token.loc, 0, [], '')

        # Is this a function-like macro?
        token = lexer.get_token()
        is_function_like = (token.kind == TokenKind.PAREN_OPEN
                            and not (token.flags & TokenFlags.WS))
        if is_function_like:
            params, macro.flags = self.read_macro_parameter_list(lexer, token)
            if params is None:
                return None
            # If we ever support GCC extensions then this needs to be updated
            self.in_variadic_macro_definition = bool(macro.flags & MacroFlags.IS_VARIADIC)
            # Get the real first token of the replacement list
            token = lexer.get_token()
        else:
            # [cpp.replace 4] There shall be whitespace between the identifier and the
            # replacement list in the definition of an object-like macro.
            if not token.flags & TokenFlags.WS and token.kind != TokenKind.EOF:
                self.diag(DID.macro_name_whitespace, token.loc)

        tokens = macro.replacement_list
        while token.kind != TokenKind.EOF:
            if token.kind == TokenKind.HASH_HASH:
                token.kind = TokenKind.CONCAT
            tokens.append(token)
            token = lexer.get_token()

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
            token = self.get_token()
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
            token = lexer.get_token()

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

    def on_undef(self, token):
        '''#undef directive processing.'''
        lexer = self.sources[-1]
        token = lexer.get_token()
        is_macro_name = self.is_macro_name(token, 2)
        if is_macro_name:
            token.extra.macro = None
            if self.actions:
                self.actions.on_undef(token)
            self.diagnose_extra_tokens(None)

    def on_line(self, token):
        self.expand_macros = True
        # Read the line number - a digit-sequence (i.e. 0-9 with optional ')
        token = self.get_token()
        line_number = self.literal_interpreter.interpret_line_number(token, 2147483648)
        if line_number != -1:
            token = self.get_token()
            if token.kind == TokenKind.EOF:
                filename = self.locator.prior_file_name
            else:
                filename = self.literal_interpreter.interpret_filename(token)
                if filename is not None:
                    filename = self.filename_to_string_literal(filename)

        # Have the line number take effect from the first character of the next line
        if line_number != -1 and filename is not None:
            token = self.diagnose_extra_tokens(None)
            start_loc = token.loc + 1
            self.locator.add_line_range(start_loc, filename, line_number)
            if self.actions:
                self.actions.on_source_file_change(start_loc, SourceFileChangeReason.line)

    def on_error(self, token):
        self.diagnostic_directive(token, DID.error_directive)

    def on_warning(self, token):
        self.diagnostic_directive(token, DID.warning_directive)

    def diagnostic_directive(self, token, did):
        '''Handle #error and #warning.'''
        lexer = self.sources[-1]
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

    def on_pragma(self, token):
        '''The passed-in token is a string literal (for _Pragma) or the 'pragma' identifier.
        In any case it is ignored.'''
        # Get the namespace token, if any
        token = self.get_token()
        handler = None
        if token.kind == TokenKind.IDENTIFIER:
            handler = self.pragma_namespaces.get(token.extra.spelling)
        if not handler and self.actions:
            handler = self.actions.on_pragma
        if handler and handler(token):
            self.diagnose_extra_tokens(None)

    def read_Pragma_string(self):
        with self.collect_arguments(None):
            token = self.get_token()
            if token.kind != TokenKind.PAREN_OPEN:
                self.diag(DID.expected_open_paren, token.loc)
                return None

            state = ParserState.from_pp(self)
            state.enter_context(token.kind, token.loc)
            string = state.get_token()
            if string.kind != TokenKind.STRING_LITERAL:
                # Unterminated literals already diagnosed
                if string.kind != TokenKind.UNTERMINATED:
                    self.diag(DID.expected_string_literal, string.loc)
                state.recover(string)
                string = None
            state.leave_context()
            return string

    def process_Pragma_string(self, string):
        raw = destringize(string)
        parent_range = TokenRange(string.loc, string.loc)
        scratch_loc = self.locator.new_scratch_token(raw, parent_range, ScratchEntryKind.pragma)
        lexer = Lexer(self, raw + b'\0', scratch_loc, None)
        self.push_source(lexer)
        self.handle_directive(lexer, string, self.on_pragma)
        self.sources.pop()

    def on_Pragma(self, token):
        string = self.read_Pragma_string()
        if string is not None:
            if self.in_directive:
                self._Pragma_strings.append(string)
            else:
                self.process_Pragma_string(string)

    def ignore_directive(self, token):
        pass

    def enter_if_section(self, token, condition):
        section = IfSection(
            self.skipping,      # was_skipping
            False,              # true_condition_seen
            -1,                 # else_loc
            token.loc           # opening_loc
        )
        buffer_state = self.sources[-1].pp_state
        buffer_state.if_sections.append(section)
        if not self.skipping:
            section.true_condition_seen = condition(token)
            self.skipping = not section.true_condition_seen

    def else_section(self, token, condition):
        buffer_state = self.sources[-1].pp_state
        if not buffer_state.if_sections:
            self.diag(DID.else_without_if, token.loc, [self.token_spelling(token)])
            return

        section = buffer_state.if_sections[-1]
        if section.was_skipping:
            return
        if section.else_loc != -1:
            self.diag(DID.else_after_else, token.loc, [
                self.token_spelling(token),
                Diagnostic(DID.else_location, section.else_loc),
            ])
            return

        if condition:  # conditional else
            if section.true_condition_seen:
                self.skipping = True
            else:
                self.skipping = False
                section.true_condition_seen = condition(token)
                self.skipping = not section.true_condition_seen
        else:  # unconditional else
            section.else_loc = token.loc
            # Note - skipping is deliberately set before checking for extra tokens.  This
            # is required in C++17 and C17 and a defect in earlier standards.
            #
            # Consider:
            # #if 1          vs         #if 1
            # #else foo                 #elif
            # #endif                    #endif
            #
            # The excess token in #else and the lack of an expression in #elif should
            # either both be diagnosable or neither (recent standards say neither).
            # Clang, GCC and EDG don't get this right.
            self.skipping = section.true_condition_seen
            self.diagnose_extra_tokens(None)

    def on_if(self, token):
        self.in_if_elif_directive = True
        self.enter_if_section(token, self.evaluate_pp_expression)
        self.in_if_elif_directive = False

    def on_ifdef(self, token):
        self.enter_if_section(token, partial(self.test_defined, False))

    def on_ifndef(self, token):
        self.enter_if_section(token, partial(self.test_defined, True))

    def on_elif(self, token):
        self.in_if_elif_directive = True
        self.else_section(token, self.evaluate_pp_expression)
        self.in_if_elif_directive = False

    def on_elifdef(self, token):
        self.else_section(token, partial(self.test_defined, False))

    def on_elifndef(self, token):
        self.else_section(token, partial(self.test_defined, True))

    def on_else(self, token):
        self.else_section(token, None)

    def on_endif(self, token):
        try:
            if_section = self.sources[-1].pp_state.if_sections.pop()
            self.skipping = if_section.was_skipping
            self.diagnose_extra_tokens(None)
        except IndexError:
            self.diag(DID.endif_without_if, token.loc)

    def diagnose_extra_tokens(self, token):
        if token is None:
            token = self.get_token()
        if token.kind != TokenKind.EOF:
            first_loc = token.loc
            while token.kind != TokenKind.EOF:
                last_loc = token.loc
                token = self.get_token()
            self.diag(DID.extra_directive_tokens, TokenRange(first_loc, last_loc),
                      [self.token_spelling_at_loc(self.directive_name_loc)])
        return token

    def invalid_directive(self, token):
        self.diag(DID.invalid_directive, token.loc, [self.token_spelling(token)])

    def evaluate_pp_expression(self, token):
        self.expand_macros = True
        value, token = self.expr_parser.parse_and_evaluate_constant_expr()
        if not value.is_erroneous:
            self.diagnose_extra_tokens(token)
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

    def test_defined(self, negate, token):
        lexer = self.sources[-1]
        token = lexer.get_token()
        is_defined, is_macro_name = self.is_defined(token)
        if is_macro_name:
            self.diagnose_extra_tokens(None)
        return not is_defined if negate else bool(is_defined)
