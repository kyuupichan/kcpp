# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The diagnostic subsystem.'''

import re
import sys
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import IntEnum

from ..core import BufferPosition, PresumedLocation, host

from .definitions import DID, DiagnosticGroup, DiagnosticSeverity, diagnostic_definitions


__all__ = [
    'Diagnostic', 'DiagnosticConsumer', 'DiagnosticManager', 'DiagnosticConfig',
    'DiagnosticContext', 'DiagnosticListener', 'DiagnosticPrinter',
    'BufferRange', 'SpellingRange', 'TokenRange', 'RangeCoords',
    'location_command_line', 'location_none',
]


# Locations for diagnostics with a special meaning.
location_none = -1
location_command_line = -2


@dataclass(slots=True)
class TokenRange:
    '''Refers to a range of tokens.'''
    # Start and end are both locations of tokens.  If only one token is in the range then
    # they are the same.
    start: int
    end: int

    def caret_loc(self):
        return self.start


@dataclass(slots=True)
class BufferRange:
    '''Refers to a range of characters in a single buffer (virtual or physical).'''
    # Start and end are locations (not offsets) so that the buffer can be located.
    start: int
    end: int

    def caret_loc(self):
        return self.start


@dataclass(slots=True)
class SpellingRange:
    '''Refers to a range of characters in the spelling of a token.'''
    # The location of the token.  Spellings are in UTF-8.
    token_loc: int
    # Offsets into spelling.  End is not included in the range.
    start: int
    end: int

    def caret_loc(self):
        return self.token_loc


@dataclass(slots=True)
class RangeCoords:
    '''A source range where both start and end are instances of PresumedLocation.
    Diagnostics issued by the preprocessor will always have start and end in the same
    buffer (ignoring the issue of scratch buffers used during macro expansion.  However
    diagnostics issued by a front end can have their start and end in different buffers
    owing to #include, so do not assume start and end lie in the same buffer.
    '''
    start: PresumedLocation
    end: PresumedLocation


@dataclass(slots=True)
class DiagnosticContext:
    '''A diagnostic normally has only one context - the source location it arose from.
    However, if the diagnostic location is part of a macro expansion stack, then there
    is a stack of contexts, one per level of the macro expansion.  The first context
    is that of the macro name that started the macro expansion, and each subsequent context
    arises from nested expansions.

    Each context comes with its own diagnostic ID and substitutions.  A DiagnosticContext
    is then turned into a MessageContext by performing the substitutions in the
    diagnostic's definition text.
    '''
    did: DID
    severity: DiagnosticSeverity
    substitutions: list
    caret_range: object
    source_ranges: list


@dataclass(slots=True)
class MessageContext:
    '''A MessageContext object is formed from a DiagnosticContext - see its docstring.

    For teminal output, each message context is later further enhanced with lines from the
    original source code and highlight lines before being written out to the terminal.'''
    # The caret highlight
    caret_highlight: RangeCoords
    # Additional highlighted ranges - a list of RangeCoords objects
    highlights: list
    # The main diagnostic message.  A list of pairs (text, kind) where text is translated
    # text.  kind is formatting information.  It can be 'message' or 'quote', the latter
    # indicating it is quoted text from the user's source code (or a keyword, etc) and as
    # such it should not be broken across lines.  'message' is used for standard parts of
    # a diagnostic body.  If the diagnostic has a location, an entry's text can include a
    # file path, and possibly line and column information; its kind should be 'path'.  If
    # the diagnostic has a severity to show, its text describes the severity, and kind
    # should be one of 'error', 'warning', 'note' or 'remark'.
    message_parts: list


@dataclass(slots=True)
class ElaboratedDiagnostic:
    '''A processed diagnostic.'''
    # The diagnostic ID.
    did: int
    # A list of MessageContext objects; see the docstring of MessageContext.
    message_contexts: list
    # A list of zero or more nested ElaboratedDiagnostics
    nested_diagnostics: list


class Diagnostic:
    '''Diagnostic captures the details of a diagnostic emitted by the preprocessor /
    front-end.'''

    def __init__(self, did, loc, args=None):
        assert loc
        self.did = did
        self.loc = loc
        self.arguments = args or []
        # The severity will be set by the diagnostic manager before calling decompose()
        self.severity = None
        assert all(isinstance(arg, (int, str, bytes, Diagnostic, BufferRange,
                                    SpellingRange, TokenRange))
                   for arg in self.arguments)

    def __eq__(self, other):
        return (isinstance(other, Diagnostic)
                and self.loc == other.loc
                and self.did == other.did
                and self.arguments == other.arguments)

    def __repr__(self):
        return f'Diagnostic(did={self.did!r}, loc={self.loc}, args={self.arguments!r})'

    def to_short_text(self):
        return f'Diagnostic({self.did.name}, {self.loc}, {self.arguments!r})'

    def decompose(self):
        '''Decompose a diagnostic and return a pair (diagnostic_context, nested_diagnostics).'''
        assert self.severity is not None

        substitutions = []
        source_ranges = []
        nested_diagnostics = []

        caret_range = self.loc
        if isinstance(caret_range, int):
            caret_range = TokenRange(caret_range, caret_range)

        for arg in self.arguments:
            if isinstance(arg, (str, bytes, int)):
                substitutions.append(arg)
            elif isinstance(arg, (TokenRange, SpellingRange, BufferRange)):
                source_ranges.append(arg)
            elif isinstance(arg, Diagnostic):
                nested_diagnostics.append(arg)
            else:
                raise RuntimeError(f'unhandled argument: {arg}')

        context = DiagnosticContext(self.did, self.severity, substitutions,
                                    caret_range, source_ranges)
        return context, nested_diagnostics


class Translations:
    '''Manages translating diagnostic strings.'''

    def __init__(self, texts=None):
        self.texts = texts or {}

    def diagnostic_text(self, did):
        return self.texts.get(did, diagnostic_definitions[did].text)


class StrictKind(IntEnum):
    '''The strict mode of the compiler.'''
    none = 0         # not strict (the default)
    warnings = 1     # --strict-warnings
    errors = 2       # --strict-errors


@dataclass(slots=True)
class DiagnosticConfig:
    error_output: str
    error_limit: int
    # These take a comma-separated list of diagnostic groups
    diag_suppress: str
    diag_remark: str
    diag_warning: str
    diag_error: str
    diag_once: str
    worded_locations: bool           # "line 5", "at end of source", etc.
    show_columns: bool               # if column numbers appear in diagnostics
    remarks: bool                    # whether to emit remarks
    warnings: bool                   # whether to emit warnings
    errors: bool                     # if True, warnings are turned into errors
    strict: str                      # 'e' for strict errors, 'w' for strict warnings
    translations: Translations

    @classmethod
    def default(cls):
        return cls(
            '',                      # error_output
            100,                     # error_limit
            '', '', '', '',          # diag_suppress, diag_remark, diag_warning, diag_error,
            '',                      # diag_once
            True,                    # worded_locations
            False,                   # show_columns
            False, True, False,      # remarks, warnings, errors
            '',                      # not strict mode
            Translations(),
        )

    def parse_group_settings(self, group_severities, group_onces):
        '''Handle user-requested severity overrides.'''
        def parse_groups(groups):
            '''A generator yielding group IDs.'''
            groups = groups.split(',')
            for group in groups:
                if group:
                    # Be strict - don't strip or canonicalize case
                    try:
                        yield DiagnosticGroup[group].value
                    except KeyError:
                        unknown_groups.add(group)

        unknown_groups = set()
        for group_id in parse_groups(self.diag_suppress):
            group_severities[group_id] = DiagnosticSeverity.ignored
        for group_id in parse_groups(self.diag_remark):
            group_severities[group_id] = DiagnosticSeverity.remark
        for group_id in parse_groups(self.diag_warning):
            group_severities[group_id] = DiagnosticSeverity.warning
        for group_id in parse_groups(self.diag_error):
            group_severities[group_id] = DiagnosticSeverity.error
        for group_id in parse_groups(self.diag_once):
            group_onces[group_id] = True

        return unknown_groups


class DiagnosticConsumer(ABC):
    '''This interface is passed diagnostics emitted by the preprocessor.  It can do what it
    wants with them - simply capture them for later analysis or emission, or pretty-print
    them to stderr.
    '''
    # If False, the argument to emit() is a Diagnostic as are its nested_diagnostics.
    # Otherwise they are ElaboratedDiagnostic instances.
    elaborate = False

    def __init__(self):
        self.stderr = sys.stderr

    @abstractmethod
    def emit(self, diagnostic):
        pass


class DiagnosticListener(DiagnosticConsumer):
    '''A simple diagnostic consumer that simply collects the emitted diagnostics.'''

    def __init__(self):
        super().__init__()
        self.diagnostics = []

    def emit(self, diagnostic):
        self.diagnostics.append(diagnostic)


class DiagnosticPrinter(DiagnosticConsumer):
    '''A simple diagnostic consumer that prints a summary of the emitted diagnostics to
    stdout.  Used for debugging.
    '''

    def __init__(self):
        super().__init__()
        self.stderr = sys.stdout

    def emit(self, diagnostic):
        # Don't emit compilation summary, etc.
        if diagnostic.loc != location_none:
            print(diagnostic.to_short_text(), file=self.stderr)


class DiagnosticManager:

    formatting_code = re.compile('%(([a-z]+)({.[^}]*})?)?([0-9]?)')
    severity_map = {
        DiagnosticSeverity.remark: (DID.severity_remark, 'remark'),
        DiagnosticSeverity.note: (DID.severity_note, 'note'),
        DiagnosticSeverity.warning: (DID.severity_warning, 'warning'),
        DiagnosticSeverity.error: (DID.severity_error, 'error'),
        DiagnosticSeverity.fatal: (DID.severity_fatal, 'error'),
    }

    def __init__(self, *, config=None, consumer=None):
        from .terminal import UnicodeTerminal
        # The locator is only needed for diagnostics with a source file location
        self.locator = None
        # Used to determine if we're in a system header
        self.file_manager = None
        self.error_count = 0
        self.fatal_error_count = 0

        # Configuration, starting with the consumer and the error output file
        config = config or DiagnosticConfig.default()
        self.consumer = consumer or UnicodeTerminal()
        self.translations = config.translations
        self.error_limit = config.error_limit
        self.worded_locations = config.worded_locations
        self.show_columns = config.show_columns
        self.remarks = config.remarks
        self.warnings = config.warnings
        self.errors = config.errors
        if config.strict == 'w':
            self.strict = StrictKind.warnings
        elif config.strict == 'e':
            self.strict = StrictKind.errors
        else:
            self.strict = StrictKind.none

        # Per-group severities and once-only settings
        max_group_id = max(DiagnosticGroup)
        self.group_severities = bytearray(max_group_id + 1)
        self.group_onces = bytearray(max_group_id + 1)

        unknown_groups = config.parse_group_settings(self.group_severities, self.group_onces)
        for group in sorted(unknown_groups):
            self.emit(Diagnostic(DID.unknown_diagnostic_group, location_command_line, [group]))

        if filename := config.error_output:
            result = host.open_file_for_writing(filename)
            if isinstance(result, str):
                self.emit(Diagnostic(DID.cannot_write_file, location_command_line,
                                     [filename, result]))
            else:
                self.consumer.stderr = result

    def severity(self, did):
        '''Return a possibly remapped severity for the diagnostic.  Update error counts.'''
        defn = diagnostic_definitions[did]
        severity = defn.severity

        # For discretionary diagnostics (those with a group), handle user severity
        # override and once-only requests.
        if defn.group is not DiagnosticGroup.none:
            # Silence the diagnostic in a system header.
            if self.file_manager.in_system_header():
                return DiagnosticSeverity.ignored

            # For strict groups, upgrade severity based on strict mode
            if DiagnosticGroup.strict_start <= defn.group <= DiagnosticGroup.strict_end:
                if self.strict is StrictKind.warnings:
                    severity = max(severity, DiagnosticSeverity.warning)
                elif self.strict is StrictKind.errors:
                    severity = max(severity, DiagnosticSeverity.error)

            # Now honour user-selected group overrides (which take priority over strictness)
            group_severity = self.group_severities[defn.group]
            if group_severity:
                severity = group_severity

            # Honour diagnose once only requests
            if self.group_onces[defn.group]:
                self.group_severities[defn.group] = DiagnosticSeverity.ignored

        if severity is DiagnosticSeverity.remark:
            if not self.remarks:
                severity = DiagnosticSeverity.ignored
        elif severity is DiagnosticSeverity.warning:
            if self.errors:
                severity = DiagnosticSeverity.error
            elif not self.warnings:
                severity = DiagnosticSeverity.ignored

        # Update the error counts we maintain
        if severity >= DiagnosticSeverity.error:
            if severity == DiagnosticSeverity.fatal:
                self.fatal_error_count += 1
            else:
                self.error_count += 1

        return severity

    def should_halt_compilation(self):
        return self.fatal_error_count or self.error_count >= self.error_limit

    def emit(self, diagnostic):
        '''Emit a diagnostic, return True if compilation should be halted because there
        has been a fatal error, or the error limit has been reached.'''
        # Suppress diagnostics with source locations
        if self.should_halt_compilation() and (
                diagnostic.loc not in (location_none, location_command_line)):
            return True

        diagnostic.severity = self.severity(diagnostic.did)
        if diagnostic.severity != DiagnosticSeverity.ignored:
            # Elaborate the diagnostic if the consumer wants elaborated ones.
            if self.consumer.elaborate:
                diagnostic = self.elaborate(diagnostic)
            self.consumer.emit(diagnostic)

        return self.should_halt_compilation()

    def emit_compilation_summary(self, filename):
        '''Emit a compilation summary.  filename is the name of the primary source file.'''
        if self.fatal_error_count:
            self.emit(Diagnostic(DID.compilation_halted, location_none))
            if self.error_count:
                self.emit(Diagnostic(DID.fatal_error_and_error_summary, location_none,
                                     [self.fatal_error_count, self.error_count, filename]))
            else:
                self.emit(Diagnostic(DID.fatal_error_summary, location_none,
                                     [self.fatal_error_count, filename]))
            exit_code = 4
        elif self.error_count:
            if self.error_count >= self.error_limit:
                self.emit(Diagnostic(DID.error_limit_reached, location_none))
            self.emit(Diagnostic(DID.error_summary, location_none, [self.error_count, filename]))
            exit_code = 2
        else:
            exit_code = 0

        # Close the error output file
        if self.consumer.stderr not in (sys.stderr, sys.stdout):
            self.consumer.stderr.close()

        return exit_code

    def write(self, text):
        '''Write text to stdder (or error output) followed by a newline.'''
        print(text, file=self.consumer.stderr)

    def location_text(self, caret_loc):
        '''Return a pair (text, show_source).  The text is empty for a diagnostic with no
        location, something like 'kcpp' for command-line errors, and otherwise something
        like '"file_name": line 25" for file locations.
        '''
        if caret_loc == location_none:
            return '', False
        if caret_loc == location_command_line:
            return 'kcpp', False

        # Column numbers, like line numbers, are 1-based.  They could reasonably be any of
        # 1) the column on a terminal, 2) the byte offset in the line, or 3) the count of
        # codepoints (logical characters).  Clang seems to give the byte offset and GCC
        # the terminal column (but GCC is inconsistent w.r.t. wide and unprintable
        # characters like \v).  For now, like Clang, we give the byte offset on the line
        # plus 1, which is at least well-defined.
        location = self.locator.presumed_location(caret_loc, False)
        arguments = [location.presumed_filename, location.presumed_line_number,
                     location.column_offset + 1]
        buffer_position = location.buffer_position()
        show_source = buffer_position != BufferPosition.END_OF_SOURCE
        if self.worded_locations:
            if buffer_position == BufferPosition.END_OF_SOURCE:
                did = DID.at_file_end
            elif buffer_position == BufferPosition.END_OF_LINE:
                did = DID.at_file_and_end_of_line
            elif self.show_columns:
                did = DID.at_file_line_and_column
            else:
                did = DID.at_file_and_line
        else:
            if self.show_columns:
                did = DID.brief_at_file_line_and_column
            else:
                did = DID.brief_at_file_and_line
        msg = self.translations.diagnostic_text(did)
        parts = self.substitute_arguments(msg, arguments)
        return ''.join(part for (part, _kind) in parts), show_source

    def elaborate(self, diagnostic):
        '''Returns an ElaboratedDiagnostic instance.'''
        main_context, nested_diagnostics = diagnostic.decompose()
        for nested in nested_diagnostics:
            nested.severity = self.severity(nested.did)
        message_contexts = [self.message_context(dc)
                            for dc in self.diagnostic_contexts(main_context)]
        nested_diagnostics = [self.elaborate(diagnostic) for diagnostic in nested_diagnostics]
        return ElaboratedDiagnostic(main_context.did, message_contexts, nested_diagnostics)

    def diagnostic_contexts(self, main_context):
        '''For diagnostics with a location that is in a source file, return the macro context
        stack.  Otherwise (e.g. compilation summaries, commmand line diagnostics) return
        just the main context.
        '''
        if main_context.caret_range.start <= location_none:
            return [main_context]
        return self.locator.diagnostic_contexts(main_context)

    def message_context(self, main_context):
        '''Convert a diagnostic into text (a MessageContext object). '''
        # Determine the message.  The location is determined by the main highlight,
        # which is the first one in the list.
        text = self.translations.diagnostic_text(main_context.did)
        caret_range = main_context.caret_range
        caret_loc = caret_range.caret_loc()

        text_parts = []
        location_text, show_source = self.location_text(caret_loc)
        if location_text:
            text_parts.append((location_text + ': ', 'path'))
        # Add the severity text unless it is none
        severity = main_context.severity
        if severity != DiagnosticSeverity.none:
            severity_did, hint = self.severity_map[severity]
            text_parts.append((self.translations.diagnostic_text(severity_did) + ': ', hint))
        # Now add the diagnostic's text
        text_parts.extend(self.substitute_arguments(text, main_context.substitutions))
        # Finally, if the diagnostic has a group, inform the user of it
        defn = diagnostic_definitions[main_context.did]
        if defn.group is not DiagnosticGroup.none:
            text_parts.append((f'  [{defn.group.name}]', 'message'))

        # Now convert each range to RangeCoords
        if show_source:
            caret_range = self.locator.range_coords(caret_range)
            source_ranges = [self.locator.range_coords(source_range)
                             for source_range in main_context.source_ranges]
        else:
            caret_range = RangeCoords(None, None)
            source_ranges = []
        return MessageContext(caret_range, source_ranges, text_parts)

    def substitute_arguments(self, format_text, arguments):
        def select(text, arg):
            assert isinstance(arg, int)
            parts = text.split('|')
            if not (0 <= arg < len(parts)):
                raise RuntimeError(f'diagnostic select{text} passed out-of-range value {arg}')
            return (parts[arg], 'message')

        def plural(text, arg):
            assert isinstance(arg, int)
            parts = text.split('|')
            for part in parts:
                test_arg = arg
                expr, text = part.split(':')
                if not expr:
                    break
                if expr[0] == '%':
                    modulo, expr = expr.split('=')
                    test_arg %= int(modulo)
                if expr[0] == '[' and expr[-1] == ']':
                    start, end = expr[1:-1].split(',')
                    if int(start) < test_arg < int(end):
                        break
                else:
                    value = int(expr)
                    if value == test_arg:
                        break
            else:
                raise RuntimeError('bad diagnostic format')
            return (f'{arg:,d} {text}', 'message')

        def quote(text, arg):
            assert not (text and arg)
            return (f"'{text or arg}'", 'quote')

        def substitute_match(match):
            func, func_arg, arg_index = match.groups()[1:]
            if func_arg:
                # Drop the {}
                func_arg = func_arg[1:-1]
            if arg_index == '':
                argument = None
            else:
                argument = arguments[int(arg_index)]
                if isinstance(argument, bytes):
                    argument = argument.decode()

            if func == 'select':
                return select(func_arg, argument)
            if func == 'plural':
                return plural(func_arg, argument)
            if func == 'q':
                return quote(func_arg, argument)
            assert not func
            assert isinstance(argument, (str, int))
            return (str(argument), 'message')

        def placeholder_matches(text):
            cursor = 0
            while True:
                match = self.formatting_code.search(text, cursor)
                if not match:
                    return
                yield match
                cursor = match.end()

        def parts(format_text):
            cursor = 0
            for match in placeholder_matches(format_text):
                yield (format_text[cursor: match.start()], 'message')
                yield substitute_match(match)
                cursor = match.end()

            yield (format_text[cursor:], 'message')

        return list(parts(format_text))
