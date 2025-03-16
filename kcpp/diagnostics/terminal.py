# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Handle the details of outputting diagnostics to an ASCII, or unicode-aware,
terminal.'''

import os
import sys

from .diagnostic import DiagnosticConsumer


__all__ = ['UnicodeTerminal']


class UnicodeTerminal(DiagnosticConsumer):
    '''Write formatted diagnostics to stderr, in a way that they should be suitable for
    display on a Unicode-enabled terminal.
    '''

    DEFAULT_KCPP_COLOURS = (
        'error=1;31:warning=1;35:note=1;36:remark=1;34:'
        'path=1:caret=1;32:locus=1;32:range1=34:range2=34:quote=1:unprintable=7'
    )

    def __init__(self, command_line, environ,  *, file=sys.stderr):
        '''Diagnostics are written to file, with colour formatting information if
        colours is True.  Sourcefile tabs are space-expanded to the given tabstop.'''
        self.file = file
        self.nested_indent = 4
        self.terminal_width = 0
        self.tabstop = command_line.tabstop
        self.enhancement_codes = {}
        if command_line.colours:
            self.enhancement_codes = self.parse_colours(environ)
        if self.file.isatty():
            self.terminal_width = os.get_terminal_size(self.file.fileno()).columns

    def parse_colours(self, environ):
        '''Parse the KCPP_COLOURS environment variable.'''
        def terminal_supports_colours():
            '''Return True if the terminal appears to support colours.'''
            term = environ.get('TERM', '')
            if term in 'ansi cygwin linux'.split():
                return True
            if any(term.startswith(prefix) for prefix in 'screen xterm vt100 rxvt'.split()):
                return True
            return term.endswith('color')

        def colour_assignments():
            '''A generator returning colour assignments for specified colour hint names.'''
            if terminal_supports_colours():
                colours = environ.get('KCPP_COLOURS', self.DEFAULT_KCPP_COLOURS)
                parts = colours.split(':')
                for part in parts:
                    vals = part.split('=', maxsplit=1)
                    if len(vals) == 2:
                        yield vals

        return {kind: value for kind, value in colour_assignments()}

    def enhance_text(self, text, kind):
        '''Emit enhanced text if an enhancement has been assigned for the hint kind.'''
        code = self.enhancement_codes.get(kind)
        if code:
            return f'\x1b[{code}m{text}\x1b[0;39m'
        return text

    def emit(self, elaborated_diag):
        '''Emit a diagnostic.'''
        self.emit_recursive(elaborated_diag, 0)

    def emit_recursive(self, elaborated_diag, indent):
        '''Emit the top-level diagnostic at the given indentation level.  Then emit nested
        diagnostics at an increased indentation level.
        '''
        for n, context in enumerate(elaborated_diag.contexts):
            if n == 1:
                indent += self.nested_indent
            for line in self.diagnostic_lines(context):
                print(f'{" " * indent}{line}', file=self.file)
        for nested in elaborated_diag.nested_diagnostics:
            self.emit_recursive(nested, indent + self.nested_indent)

    def diagnostic_lines(self, context):
        '''Generate all the lines to display for the diagnostic conext - one for the message, and
        perhaps several source lines and highlights.
        '''
        # The main diagnostic message, perhaps prefixed with file and line number, and
        # diagnostic severity, that let's the user know what the complaint is.
        yield ''.join(self.enhance_text(*part) for part in context.message_parts)

        # Now the source text and highlights (if appropriate)
        if context.highlights:
            yield from self.show_highlighted_source(context.highlights)

    def show_highlighted_source(self, highlights):
        '''Generate one or more source lines.  For each line comes with highlights indicating the
        part of the line at issue.
        '''
        def line_margins(line_number):
            margin = f'{line_number:5d}'
            return margin + ' | ', ' ' * len(margin) + ' | '

        main_highlight = highlights[0]
        for line_number, line in enumerate(self.source_lines(main_highlight),
                                           start=main_highlight.start.line_number):
            margins = line_margins(line_number)
            room = self.terminal_width - 1 - len(margins[0])
            texts = self.source_and_highlight_lines(line, highlights, room)
            for margin, text in zip(margins, texts):
                yield margin + text

    def source_lines(self, erange):
        '''Return a SourceLine object for each line in the elaborated range.'''
        start, end = erange.start, erange.end
        # Sanity checks.
        assert start.buffer is end.buffer
        # Equality applies for zero-width end-of-source indicators
        assert (start.line_number, start.column_offset) <= (end.line_number, end.column_offset)

        return [start.buffer.source_line(line_number, self.tabstop)
                for line_number in range(start.line_number, end.line_number + 1)]

    def source_and_highlight_lines(self, line, highlights, room):
        '''Return a (source_line, highlight_line) pair of strings for the SourceLine
        object passed in with the given highlights.'''
        col_ranges = line.convert_eranges_to_column_ranges(highlights)

        # Must show the caret location
        if col_ranges[0][0] != col_ranges[0][1]:
            required_column = col_ranges[0][0]
        else:
            required_column = min(col_range[0] for col_range in col_ranges)

        # Truncate the line if necessary
        removed_chars, line = line.truncate(room, required_column)

        # Collect highlight ranges in terms of column offset ranges
        char_ranges = []
        for n, (highlight, (start, end)) in enumerate(zip(highlights, col_ranges)):
            if start == -1:
                continue
            start -= removed_chars
            end -= removed_chars
            # Special handling of caret range first character, for the caret
            if n == 0:
                if highlight.start.line_number == line.line_number:
                    char_ranges.append(((start, start + 1), 'caret'))
                    if start + 1 < end:
                        char_ranges.append(((start + 1, end), 'locus'))
                else:
                    char_ranges.append(((start, end), 'locus'))
            else:
                char_ranges.append(((start, end), 'range1' if n == 1 else 'range2'))

        char_ranges = sorted(char_ranges, key=lambda cr: cr[0][0])

        def highlight_char(kind):
            return '^' if kind == 'caret' else '~'

        def highlight_parts(char_ranges):
            last_end = 0
            for (start, end), kind in char_ranges:
                yield ' ' * (start - last_end)
                highlight_text = highlight_char(kind) * (end - start)
                yield self.enhance_text(highlight_text, kind)
                last_end = end

        def source_line_parts(line):
            cursor = 0
            for r in line.replacements:
                ncursor = sum(line.out_widths[:r])
                yield line.text[cursor: ncursor]
                cursor = ncursor + line.out_widths[r]
                replacement = line.text[ncursor:cursor]
                yield self.enhance_text(replacement, 'unprintable')
            yield line.text[cursor:]

        return ''.join(source_line_parts(line)), ''.join(highlight_parts(char_ranges))
