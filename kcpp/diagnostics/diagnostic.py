# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The diagnostic subsystem.'''

import re
from dataclasses import dataclass
from enum import IntEnum

from .definitions import (
    DID, DiagnosticSeverity, diagnostic_definitions,
)


__all__ = [
    'Diagnostic', 'DiagnosticConsumer', 'DiagnosticEngine',
    'BufferRange', 'SpellingRange', 'TokenRange', 'ElaboratedLocation', 'ElaboratedRange',
    'location_command_line', 'location_none',
]


# Note: column numbers, like line numbers, are 1-based.  They could reasonably be any of
# 1) the column on a terminal, 2) the byte offset in the line, or 3) the count of
# codepoints (logical characters).  Clang seems to give the byte offset and GCC the
# terminal column.  For now, like Clang, we give the byte offset on the line plus 1.

class BufferPosition(IntEnum):
    '''Describes a position within a buffer.'''
    WITHIN_LINE = 0
    END_OF_LINE = 1
    END_OF_SOURCE = 2


# Locations for diagnostics with a special meaning.
location_command_line = -1
location_none = 0


@dataclass(slots=True)
class ElaboratedLocation:
    '''Detailed informatino about location within a buffer.'''
    # The original location
    loc: int
    # Byte offset of the start of the line in the buffer containing the location (0-nased)
    line_offset: int
    # Byte offset of the location from the start of the line (0-based)
    column_offset: int
    # Line number in the buffer (1-based)
    line_number: int
    # The buffer containing the location.  None for diagnostics with special locations
    # (location_command_line, location_none).
    buffer: object

    def buffer_position(self):
        offset = self.line_offset + self.column_offset
        if offset == len(self.buffer.text):
            return BufferPosition.END_OF_SOURCE
        if self.buffer.text[offset] in {10, 13}:
            return BufferPosition.END_OF_LINE
        return BufferPosition.WITHIN_LINE


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
    '''A source range where both start and end are elaborated.'''
    start: ElaboratedLocation
    end: ElaboratedLocation

    def contains(self, line_number):
        return self.start.line_number <= line_number <= self.end.line_number


@dataclass(slots=True)
class DiagnosticContext:
    '''A diagnostic normally has only one context - the source location it arose from.
    However, if the diagnostic location is part of a macro expansion stack, then there
    is a stack of contexts, one per level of the macro expansion.  The first context
    is that of the macro name that started the macro expansion, and each subsequent context
    arises from nested expansions.

    Each context comes with its own message.'''
    # A list of ElaboratedRange objects, one per highlight for this context.
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
    # A list of contexts for this diagnostic; see the docstring of DiagnosticContext.
    contexts: list
    # A list of zero or more nested ElaboratedDiagnostics
    nested_diagnostics: list


class Diagnostic:
    '''Diagnostic captures the details of a diagnostic emitted by the preprocessor /
    front-end.'''

    def __init__(self, did, loc, args=None):
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
        '''Decompose a diagnostic to its diagnostic ID, format string arguments, source ranges,
        and nested diagnostics.  Return them as a 4-tuple.
        '''
        args = []
        ranges = []
        diags = []

        ranges.append(TokenRange(self.loc, self.loc))
        for arg in self.arguments:
            if isinstance(arg, (str, int)):
                args.append(arg)
            elif isinstance(arg, bytes):
                args.append(arg.decode())
            elif isinstance(arg, (TokenRange, SpellingRange, BufferRange)):
                ranges.append(arg)
            elif isinstance(arg, Diagnostic):
                diags.append(arg)
            else:
                raise RuntimeError(f'unhandled argument: {arg}')

        return (self.did, args, ranges, diags)


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

        arguments = [elaborated_loc.buffer.name, str(elaborated_loc.line_number),
                     str(elaborated_loc.column_offset + 1)]

        if self.worded_locations:
            pos = elaborated_loc.buffer_position()
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
        did, substitution_args, source_ranges, nested = diagnostic.decompose()
        contexts = self.diagnostic_contexts(did, substitution_args, source_ranges)
        nested = [self.elaborate(diagnostic) for diagnostic in nested]
        return ElaboratedDiagnostic(did, contexts, nested)

    def diagnostic_contexts(self, did, substitution_args, source_ranges):
        '''Return a diagnostic context stack for the source ranges to be highlighted.  The first
        range is that of the diagnosed location, subsequent ones are highlight ranges.
        '''
        def context(did, substitution_args, highlights):
            '''Return a DiagnosticContext object.'''
            # Determine the message.  The location is determined by the main highlight,
            # which is the first one in the list.
            severity_enum = diagnostic_definitions[did].severity
            if severity_enum >= DiagnosticSeverity.error:
                self.error_count += 1
                if severity_enum == DiagnosticSeverity.fatal:
                    self.fatal_count += 1
            text = self.translations.diagnostic_text(did)
            text_parts = []
            main_highlight = highlights[0]
            if main_highlight.start.loc != location_none:
                text_parts.append((self.location_text(main_highlight.start) + ': ', 'path'))
            if main_highlight.start.buffer is None:
                highlights = []
            # Add the severity text unless it is none
            if severity_enum != DiagnosticSeverity.none:
                severity_did, hint = self.severity_map[severity_enum]
                text_parts.append((self.translations.diagnostic_text(severity_did) + ': ', hint))
            text_parts.extend(self.substitute_arguments(text, substitution_args))
            return DiagnosticContext(highlights, text_parts)

        return [context(*t) for t in self.pp.context_stack(did, substitution_args, source_ranges)]

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
