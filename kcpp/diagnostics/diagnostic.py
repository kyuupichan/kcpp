# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The diagnostic subsystem.'''

import re
from dataclasses import dataclass

from ..cpp import BufferCoords, BufferPosition
from .definitions import (
    DID, DiagnosticSeverity, diagnostic_definitions,
)


__all__ = [
    'Diagnostic', 'DiagnosticConsumer', 'DiagnosticEngine', 'DiagnosticContext',
    'BufferRange', 'SpellingRange', 'TokenRange', 'ElaboratedLocation', 'ElaboratedRange',
    'location_command_line', 'location_none', 'location_in_args',
]


# Note: column numbers, like line numbers, are 1-based.  They could reasonably be any of
# 1) the column on a terminal, 2) the byte offset in the line, or 3) the count of
# codepoints (logical characters).  Clang seems to give the byte offset and GCC the
# terminal column.  For now, like Clang, we give the byte offset on the line plus 1.

# Locations for diagnostics with a special meaning.
location_none = -1
location_command_line = -2
location_in_args = -3


@dataclass(slots=True)
class ElaboratedLocation:
    '''Detailed informatino about location within a buffer.'''
    # The original location
    loc: int
    # The location as coordinates.  None for diagnostics with special locations
    # (location_command_line, location_none).
    coords: BufferCoords


@dataclass(slots=True)
class TokenRange:
    '''Refers to a range of tokens.'''
    # Start and end are both locations of tokens.  If only one token is in the range then
    # they are the same.
    start: int
    end: int


@dataclass(slots=True)
class BufferRange:
    '''Refers to a range of characters in a single buffer (virtual or physical).'''
    # Start and end are locations (not offsets) so that the buffer can be located.
    start: int
    end: int


@dataclass(slots=True)
class SpellingRange:
    '''Refers to a range of characters in the spelling of a token.'''
    # The location of the token.  Spellings are in UTF-8.
    token_loc: int
    # Offsets into spelling.  End is not included in the range.
    start: int
    end: int


@dataclass(slots=True)
class ElaboratedRange:
    '''A source range where both start and end are instances of ElaboratedLocation.
    Diagnostics issued by the preprocessor will always have start and end in the same
    buffer (ignoring the issue of scratch buffers used during macro expansion.  However
    diagnostics issued by a front end can have their start and end in different buffers
    owing to #include, so we must not assume start and end lie in the same buffer.
    '''
    start: ElaboratedLocation
    end: ElaboratedLocation


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
    substitutions: list
    caret_range: object
    source_ranges: list


@dataclass(slots=True)
class MessageContext:
    '''A MessageContext object is formed from a DiagnosticContext - see its docstring.

    For teminal output, each message context is later further enhanced with lines from the
    original source code and highlight lines before being written out to the terminal.'''
    # The caret highlight
    caret_highlight: ElaboratedRange
    # Additional highlighted ranges - a list of ElaboratedRange objects
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
        self.emit = None
        assert isinstance(loc, int)
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

    def decompose(self):
        '''Decompose a diagnostic and return a pair (diagnostic_context, nested_diagnostics).'''
        substitutions = []
        source_ranges = []
        nested_diagnostics = []

        if self.loc != location_in_args:
            source_ranges.append(TokenRange(self.loc, self.loc))
        for arg in self.arguments:
            if isinstance(arg, (str, int)):
                substitutions.append(arg)
            elif isinstance(arg, bytes):
                substitutions.append(arg.decode())
            elif isinstance(arg, (TokenRange, SpellingRange, BufferRange)):
                source_ranges.append(arg)
            elif isinstance(arg, Diagnostic):
                nested_diagnostics.append(arg)
            else:
                raise RuntimeError(f'unhandled argument: {arg}')

        assert source_ranges
        context = DiagnosticContext(self.did, substitutions, source_ranges[0], source_ranges[1:])
        return context, nested_diagnostics


class DiagnosticEngine:

    formatting_code = re.compile('%(([a-z]+)({.+})?)?([0-9]?)')
    severity_map = {
        DiagnosticSeverity.remark: (DID.severity_remark, 'remark'),
        DiagnosticSeverity.note: (DID.severity_note, 'note'),
        DiagnosticSeverity.warning: (DID.severity_warning, 'warning'),
        DiagnosticSeverity.error: (DID.severity_error, 'error'),
        DiagnosticSeverity.fatal: (DID.severity_fatal, 'error'),
    }

    def __init__(self, pp, env, translations=None):
        self.pp = pp
        # A DiagnosticTranslations object
        self.translations = translations or DiagnosticTranslations({})
        self.worded_locations = True
        self.show_columns = False
        self.diagnostic_consumers = []
        self.error_count = 0
        self.fatal_count = 0

    @classmethod
    def add_arguments(cls, group):
        '''Add command line arugments to the group.'''
        pass

    def add_diagnostic_consumer(self, consumer):
        self.diagnostic_consumers.append(consumer)

    def emit(self, diagnostics):
        '''Emit one or more diagnostics.  diagnostics is a single Diagnostic or a list of them.'''
        if not isinstance(diagnostics, list):
            diagnostics = [diagnostics]
        for diagnostic in diagnostics:
            elaborated_diagnostic = self.elaborate(diagnostic)
            for consumer in self.diagnostic_consumers:
                consumer.emit(elaborated_diagnostic)

    def emit_error_count(self):
        if self.error_count:
            self.emit(Diagnostic(DID.errors_generated, location_none, [self.error_count]))

    def location_text(self, elaborated_loc):
        '''Return the location text for the elaborated location.  This is empty for a diagnostic
        with no location, something like '<command line>: ' for command-line errors, and
        otherwise something like '"file_name": line 25: " for file locations.
        '''
        if elaborated_loc.loc == location_command_line:
            return 'kcpp'

        coords = elaborated_loc.coords
        arguments = [coords.buffer.name, coords.line_number, coords.column_offset + 1]

        if self.worded_locations:
            pos = coords.buffer_position()
            if pos == BufferPosition.END_OF_SOURCE:
                did = DID.at_file_end
            elif pos == BufferPosition.END_OF_LINE:
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
        return ''.join(part for (part, _kind) in parts)

    def elaborate(self, diagnostic):
        '''Returns an ElaboratedDiagnostic instance.'''
        diagnostic_context, nested_diagnostics = diagnostic.decompose()
        message_contexts = [self.message_context(dc) for
                            dc in self.pp.diagnostic_contexts(diagnostic_context)]
        nested_diagnostics = [self.elaborate(diagnostic) for diagnostic in nested_diagnostics]
        return ElaboratedDiagnostic(diagnostic_context.did, message_contexts, nested_diagnostics)

    def message_context(self, diagnostic_context):
        '''Convert a diagnostic into text (a MessageContext object). '''
        # Determine the message.  The location is determined by the main highlight,
        # which is the first one in the list.
        severity_enum = diagnostic_definitions[diagnostic_context.did].severity
        if severity_enum >= DiagnosticSeverity.error:
            self.error_count += 1
            if severity_enum == DiagnosticSeverity.fatal:
                self.fatal_count += 1

        text = self.translations.diagnostic_text(diagnostic_context.did)
        caret_highlight = diagnostic_context.caret_range

        text_parts = []
        if caret_highlight.start.loc != location_none:
            text_parts.append((self.location_text(caret_highlight.start) + ': ', 'path'))
        # Add the severity text unless it is none
        if severity_enum != DiagnosticSeverity.none:
            severity_did, hint = self.severity_map[severity_enum]
            text_parts.append((self.translations.diagnostic_text(severity_did) + ': ', hint))
        text_parts.extend(self.substitute_arguments(text, diagnostic_context.substitutions))
        return MessageContext(caret_highlight, diagnostic_context.source_ranges, text_parts)

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


class DiagnosticTranslations:
    '''Manages translating diagnostic strings.'''

    def __init__(self, texts):
        self.texts = texts

    def diagnostic_text(self, did):
        return self.texts.get(did, diagnostic_definitions[did].text)


class DiagnosticConsumer:
    '''This interface determines what happens when a diagnostic is emitted by the preprocessor
    / front-end.  For example, UnicodeTerminal writes text to stderr.
    '''
    def __init__(self):
        pass

    def emit(self, elab_diagnostic):
        pass
