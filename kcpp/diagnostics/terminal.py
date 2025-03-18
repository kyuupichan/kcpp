# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Handle the details of outputting diagnostics to an ASCII, or unicode-aware,
terminal.'''

import argparse
import os
import sys
from bisect import bisect_left
from dataclasses import dataclass
from itertools import accumulate

from .diagnostic import DiagnosticConsumer
from ..unicode import (
    utf8_cp, is_printable, terminal_charwidth, codepoint_to_hex,
)


__all__ = ['UnicodeTerminal']


class UnicodeTerminal(DiagnosticConsumer):
    '''Write formatted diagnostics to stderr, in a way that they should be suitable for
    display on a Unicode-enabled terminal.
    '''

    DEFAULT_KCPP_COLOURS = (
        'error=1;31:warning=1;35:note=1;36:remark=1;34:'
        'path=1:caret=1;32:locus=1;32:range1=34:range2=34:quote=1:unprintable=7'
    )

    def __init__(self, env, *, file=sys.stderr):
        '''Diagnostics are written to file, with colour formatting information if
        colours is True.  Sourcefile tabs are space-expanded to the given tabstop.'''
        self.file = file
        self.nested_indent = 4
        self.terminal_width = 120
        self.tabstop = env.command_line.tabstop
        self.enhancement_codes = {}
        if env.command_line.colours:
            self.enhancement_codes = self.parse_colours(env.variables)
        if self.file.isatty():
            self.terminal_width = os.get_terminal_size(self.file.fileno()).columns

    @classmethod
    def add_arguments(cls, group):
        '''Add command line arugments to the group.'''
        group.add_argument('--tabstop', nargs='?', default=8, type=int)
        group.add_argument('--colours', action=argparse.BooleanOptionalAction, default=True)

    def parse_colours(self, variables):
        '''Parse the KCPP_COLOURS environment variable.'''
        def terminal_supports_colours():
            '''Return True if the terminal appears to support colours.'''
            term = variables.get('TERM', '')
            if term in 'ansi cygwin linux'.split():
                return True
            if any(term.startswith(prefix) for prefix in 'screen xterm vt100 rxvt'.split()):
                return True
            return term.endswith('color')

        def colour_assignments():
            '''A generator returning colour assignments for specified colour hint names.'''
            if terminal_supports_colours():
                colours = variables.get('KCPP_COLOURS', self.DEFAULT_KCPP_COLOURS)
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

        return [SourceLine.from_buffer(start.buffer, line_number, self.tabstop)
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


@dataclass(slots=True)
class SourceLine:
    # The source line as printable text.  The text is intended to be printable on a
    # unicode terminal - unprintable characters are replaced with <U+XXXX> sequences, and
    # invalid encodings are replaced with <\xAB> hex sequences, and tabs are replaced with
    # spaces.  The string does not have a terminating newline.
    text: str
    # in_widths and out_widths are byte arrays of the same length.  They hold the width,
    # in bytes, of each unicode character in the raw source line and in the returned
    # string.  For example, if a NUL character is the Nth (0-based) character on the
    # source line, then src_widths[N] will be 1 - the length of the UTF-8 encoding of NUL.
    # dst_widths[N] will be 8 - as its representation "<U+0000>" is the returned string
    # occupies 8 columns on a terminal.  This information makes it straight-forward to map
    # physical source bytes to positions in output text, and vice-versa.
    in_widths: bytearray
    out_widths: bytearray
    # List of replacements that were made.  This contains unprintable unicode characters
    # as well as replacements for bad UTF-8 encodings.  It does not contain horizontal tab
    # replacemenets.  Each is an index into the in_widths / out_widths arrays.
    replacements: list
    # The buffer and line number of this line.
    buffer: 'Buffer'
    line_number: int

    def convert_column_offset(self, column_offset):
        '''Given a column offset in the physical source line that begins a source character,
        return the byte offset in the output text line that corresponds to that character.

        If the column offset is in the middle of a source multibyte-character sequence, the
        return value corresponds to the start of the subsequent source character.

        If the column offset is at the source EOL, the return value is the output EOL.

        Sanity: raise ValueError if column_offset is negative or beyong the source EOL.
        '''
        if column_offset < 0:
            raise ValueError
        cursor = 0
        text_column = 0
        out_widths = self.out_widths

        # Advance a source character at a time
        for n, in_width in enumerate(self.in_widths):
            if cursor >= column_offset:
                break
            cursor += in_width
            text_column += out_widths[n]
        else:
            if column_offset > cursor:
                raise ValueError

        return text_column

    def convert_eranges_to_column_ranges(self, eranges):
        '''Given a sequence of elaborated ranges, return a list of (start, end) pairs of terminal
        columns based on where that range intersects this source line.  If it does not intersect
        this line then end == start == -1, otherwise end >= start.
        '''
        def convert(start, end):
            if start.line_number <= self.line_number <= end.line_number:
                if start.line_number == self.line_number:
                    start = self.convert_column_offset(start.column_offset)
                else:
                    start = 0
                if end.line_number == self.line_number:
                    end = self.convert_column_offset(end.column_offset)
                else:
                    end = sum(self.out_widths)
            else:
                start = end = -1

            return start, end

        return [convert(erange.start, erange.end) for erange in eranges]

    def truncate(self, max_width, required_column):
        '''Returns (initial output width removed, line).'''
        line_length = len(self.out_widths)
        cum_widths = list(accumulate(self.out_widths, initial=0))
        if cum_widths[-1] <= max_width:
            return 0, self

        assert 0 <= required_column <= cum_widths[-1]

        # Start with the required column.  Expand the radius until we fail.
        left = bisect_left(cum_widths, required_column)
        assert cum_widths[left] == required_column

        radius = 0
        while True:
            radius += 1
            left_end = max(0, left - radius)
            right_end = min(line_length, (left + 1) + radius)
            if cum_widths[right_end] - cum_widths[left_end] > max_width:
                radius -= 1
                left_end = max(0, left - radius)
                right_end = min(line_length, (left + 1) + radius)
                break

        # Return a new source line representing the selected text
        text = self.text[cum_widths[left_end]: cum_widths[right_end]]
        in_widths = self.in_widths[left_end: right_end]
        out_widths = self.out_widths[left_end: right_end]
        replacements = [r - left_end for r in self.replacements if left_end <= r < right_end]
        line = SourceLine(text, in_widths, out_widths, replacements, self.buffer, self.line_number)
        return cum_widths[left_end], line

    @classmethod
    def from_buffer(cls, buffer, line_number, tabstop=8):
        '''Return a SourceLine object for the indicated source line.'''
        def parts(raw_line, in_widths, out_widths, replacements):
            cursor = 0
            limit = len(raw_line)

            while cursor < limit:
                cp, in_width = utf8_cp(raw_line, cursor)
                if cp < 0:
                    # Replace the invalid UTF-8 sequence with ASCII text.
                    out_text = ''.join(f'<{c:02X}>' for c in raw_line[cursor: cursor + in_width])
                    out_width = len(out_text)
                    replacements.append(len(in_widths))
                elif cp == 9:
                    out_width = tabstop - sum(out_widths) % tabstop
                    out_text = ' ' * out_width
                elif is_printable(cp):
                    out_width = terminal_charwidth(cp)
                    out_text = chr(cp)
                else:
                    out_text = f'<{codepoint_to_hex(cp)}>'
                    out_width = len(out_text)
                    replacements.append(len(in_widths))

                in_widths.append(in_width)
                out_widths.append(out_width)
                yield out_text
                cursor += in_width

        raw_line = buffer.line_bytes(line_number)
        in_widths = bytearray()
        out_widths = bytearray()
        replacements = []
        text = ''.join(parts(raw_line, in_widths, out_widths, replacements))

        return cls(text, in_widths, out_widths, replacements, buffer, line_number)
