# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Handle the details of outputting diagnostics to an ASCII, or unicode-aware,
terminal.'''

from bisect import bisect_left
from dataclasses import dataclass
from itertools import accumulate

from ..core import Buffer, PresumedLocation, host
from ..unicode import (
    utf8_cp, is_printable, terminal_charwidth, codepoint_to_hex,
)
from .diagnostic import Diagnostic, DiagnosticConsumer


__all__ = ['UnicodeTerminal']


class UnicodeTerminal(DiagnosticConsumer):
    '''Write formatted diagnostics to stderr, in a way that they should be suitable for
    display on a Unicode-enabled terminal.
    '''
    elaborate = True

    def __init__(self):
        '''Diagnostics are written to file.  Colour formatting information, whether colours are
        enabled, and the tabstop are taken from env.  Source file tabs are space-expanded
        to the tabstop.  Diagnostics are adjusted for the terminal width, which we attempt
        to determine from file.
        '''
        super().__init__()
        self.nested_indent = 4
        self.sgr_codes = {}
        self.tabstop = 8
        # Take from stderr terminal (if it is a terminal)
        self.terminal_width = 0

    def set_sgr_code_assignments(self, colour_string):
        '''Parse an SGR assignments string.'''
        def hint_sgr_code_pairs(colour_string):
            '''A generator returning (hint, sgr_code) pairs.'''
            for part in colour_string.split(':'):
                vals = part.split('=', maxsplit=1)
                if len(vals) == 2:
                    yield vals
        self.sgr_codes = {hint: sgr_code for hint, sgr_code in hint_sgr_code_pairs(colour_string)}

    def enhance_text(self, text, hint):
        '''Emit enhanced text if an SGR code has been assigned for the hint kind.'''
        code = self.sgr_codes.get(hint)
        if code:
            return f'\x1b[{code}m{text}\x1b[0;39m'
        return text

    def emit(self, diagnostic: Diagnostic):
        '''Called when the preprocessor emits a diagnostic.'''
        # Configure the terminal width if not already set
        if self.terminal_width <= 0:
            if host.is_a_tty(self.stderr):
                self.terminal_width = host.terminal_width(self.stderr)
            else:
                self.terminal_width = 120
        self.emit_recursive(diagnostic, 0)

    def emit_recursive(self, diagnostic, indent):
        '''Emit the top-level diagnostic at the given indentation level.  Then emit nested
        diagnostics at an increased indentation level.
        '''
        orig_indent = indent
        for n, message_context in enumerate(diagnostic.message_contexts):
            if n == 1:
                indent += self.nested_indent
            room = self.terminal_width - 1 - indent
            for line in self.diagnostic_lines(message_context, room):
                print(f'{" " * indent}{line}', file=self.stderr)
        for nested in diagnostic.nested_diagnostics:
            self.emit_recursive(nested, orig_indent + self.nested_indent)

    def diagnostic_lines(self, context, room):
        '''Generate all the lines to display for the diagnostic conext - one for the message, and
        perhaps several source lines and highlights.
        '''
        # The main diagnostic message, perhaps prefixed with file and line number, and
        # diagnostic severity, that informs the user know what the complaint is.
        yield from self.word_wrapped_lines(context.message_parts, room)

        if context.caret_highlight.start is not None:
            yield from self.show_highlighted_source(context, room)

    def word_wrapped_lines(self, parts, room):
        def accept_text(accepted_text, new_text, new_width, mid_line):
            if not mid_line:
                # Strip leading space from what will begin the next line
                count = 0
                while count < len(new_text) and new_text[count] == ' ':
                    count += 1
                new_width -= count
                new_text = new_text[count:]
            accepted_text += new_text
            return accepted_text, new_width

        def lines(parts, room, indent):
            # Return a sequence of parts to go on a line.  Subsequent lines are indented
            # by indent, and the first part of those lines consists of the spaces.
            room = max(room, indent + 2)
            result = []
            line = []
            accepted_parts = False
            accepted_width = 0
            for text, hint in parts:
                # Strip the text at the start of a line
                if not accepted_parts:
                    text = text.lstrip()
                if not text:
                    # Substitutions can also leave empty parts
                    continue
                char_widths = bytes(terminal_charwidth(ord(c)) for c in text)
                maybe_width = sum(char_widths)
                if accepted_width + maybe_width <= room:
                    line.append((text, hint))
                    accepted_width += maybe_width
                    accepted_parts = True
                    continue
                # If it can't be broken up, flush the current line if we have something and
                # start the next line with this part
                if hint != 'message':
                    if accepted_parts:
                        result.append(line)
                        line = [(' ' * indent, 'message'), (text, hint)]
                        accepted_width = indent + maybe_width
                    else:
                        line.append((text, hint))
                        accepted_width += maybe_width
                        accepted_parts = True
                    continue
                # Break the message up at spaces (or at CJK characters) over as many lines
                # as needed.  Last break point is the first text character which can be
                # pushed to the next line if a break is needed
                accepted_text = ''
                maybe_text = ''
                maybe_width = 0
                prev_char_width = 0
                for n, (c, char_width) in enumerate(zip(text, char_widths)):
                    # If this is a break position (before a CJK character or space, or
                    # after a CJK character), add maybe_text to accepted_text
                    if c == ' ' or char_width == 2 or prev_char_width == 2:
                        accepted_text, maybe_width = accept_text(accepted_text, maybe_text,
                                                                 maybe_width, accepted_parts)
                        accepted_width += maybe_width
                        accepted_parts = accepted_parts or maybe_width
                        maybe_text = ''
                        maybe_width = 0
                    prev_char_width = char_width
                    # Add this character to the text whose line we're not yet sure of
                    maybe_text += c
                    maybe_width += char_width
                    # If it fits or we've not yet accepted something, continue
                    if accepted_width + maybe_width <= room or not accepted_parts:
                        continue
                    # It doesn't fit and we have a non-empty line.  Flush the line and
                    # begin the next line with the indent.  accepted_text can be empty
                    # because of accepted parts before trying to split this 'message'
                    if accepted_text:
                        line.append((accepted_text, 'message'))
                    result.append(line)
                    line = [(' ' * indent, 'message')]
                    accepted_text = ''
                    accepted_width = indent
                    accepted_parts = False
                accepted_text, maybe_width = accept_text(accepted_text, maybe_text,
                                                         maybe_width, accepted_parts)
                accepted_width += maybe_width
                accepted_parts = accepted_parts or maybe_width
                if accepted_text:
                    line.append((accepted_text, 'message'))
            if line:
                result.append(line)
            return result

        def enhance_line(parts):
            '''Combine hints to enhance text in groups.'''
            hint = None
            combined_text = ''
            line = ''
            for text, text_hint in parts:
                if hint is None or text_hint == hint:
                    combined_text += text
                    hint = text_hint
                else:
                    line += self.enhance_text(combined_text, hint)
                    combined_text = text
                    hint = text_hint
            line += self.enhance_text(combined_text, hint)
            return line

        for line in lines(parts, room, 10):
            yield enhance_line(line)

    def show_highlighted_source(self, context, room):
        '''The first highlight lies in a single buffer and is the "centre" of the diagnostic where
        the caret is shown.  This function generates a sequence of text lines:

             printable source line 1
             highlights for line 1
             printable source line 2
             highlights for line 2
             ....

        The source lines are precisely those needed to show all of the first highlight.
        Other highlights appear only to the extent the intersect the first highlight's
        source lines.
        '''
        start, end = context.caret_highlight.start, context.caret_highlight.end
        lines = self.source_lines(start, end)

        # Margin book-keeping details
        max_line_number = start.presumed_line_number + len(lines) - 1
        line_number_width = max(5, len(str(max_line_number)))
        margin_suffix = ' | '
        margin_width = len(margin_suffix) + line_number_width
        room -= margin_width

        for line_number, line in enumerate(lines, start=start.presumed_line_number):
            margins = f'{line_number:{line_number_width}d}', ' ' * line_number_width
            texts = line.source_and_highlight_lines(context, room, self.enhance_text)
            for margin, text in zip(margins, texts):
                yield margin + margin_suffix + text

    def source_lines(self, start: PresumedLocation, end: PresumedLocation):
        '''Return a list of SourceLine objects, one for each line between start and end inclusive.
        Each contains printable text, with replacements for unprintable characters and bad
        encodings, and tabs have been replaced with spaces.
        '''
        # The range must be in a single buffer.
        assert start.buffer is end.buffer

        # Equality applies for zero-width end-of-source indicators
        assert (start.line_number, start.column_offset) <= (end.line_number, end.column_offset)

        # As the range does not include the final character, don't show a line if the
        # range ends at the first character of a line.  This currently can happen in some
        # raw string literal diagnostics.
        range_end = end.line_number + (end.column_offset != 0)
        return [SourceLine.from_buffer(start.buffer, line_number, self.tabstop)
                for line_number in range(start.line_number, range_end)]


@dataclass(slots=True)
class SourceLine:
    '''Encapsulates detailed information about a single line of source code in a buffer.

    from_buffer() takes the raw source line and stores it with replacements for
    unprintable text so that it can be printed to a unicode-enabled terminal.  Unprintable
    code points are replaced with <U+XXXX> sequences, and invalid UTF-8 encodings are
    replaced with <\xAB> hex sequences.  Finally tabs are expanded to spaces based on a
    tabstop.

    The stored text does not have a terminating newline.
    '''
    # See docstring above.
    text: str
    # in_widths and out_widths are byte arrays of the same length.  They hold the width,
    # in bytes, of each unicode character in the raw source line and in text.  For
    # example, if a NUL character is the Nth (0-based) character on the source line, then
    # src_widths[N] will be 1 - as the UTF-8 encoding of NUL is a single byte.
    # dst_widths[N] will be 8 because its representation "<U+0000>" in text occupies 8
    # columns on a terminal.  This representation makes it straight-forward to map physical
    # source bytes to positions in output text, and vice-versa.
    in_widths: bytearray
    out_widths: bytearray
    # The replacements from_buffer() made in order that they can be output as enhanced
    # text, in e.g. reverse video.  For that reason this list does not contain \t
    # replacements.  Each entry is an index into the in_widths / out_widths arrays.
    replacements: list
    # The buffer and line number of this line.
    buffer: Buffer
    line_number: int

    def char_width(self, out_column):
        '''Return the width of the character at out_column in the string.'''
        column = 0
        for char_width in self.out_widths:
            if column == out_column:
                return char_width
            column += char_width
        # End-of-line.  Return 1 for the caret.
        assert column == out_column
        return 1

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

    def convert_to_column_range(self, start: PresumedLocation, end: PresumedLocation):
        '''Given start and end coordinates, return a (start, end) pair of terminal columns based
        on where that range intersects this source line.  If it does not intersect this
        line then end == start == -1, otherwise end >= start.
        '''
        if start is None:
            return -1, -1
        if start.buffer is self.buffer is end.buffer:
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
        elif start.buffer is self.buffer and start.line_number == self.line_number:
            start = self.convert_column_offset(start.column_offset)
            end = sum(self.out_widths)
        elif end.buffer is self.buffer and end.line_number == self.line_number:
            start = 0
            end = self.convert_column_offset(end.column_offset)
        else:
            start = end = -1

        return start, end

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
        '''Construct a SourceLine object for the indicated source line, replacing tabs and
        unprintable characeters.
        '''
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

    def source_and_highlight_lines(self, context, room, enhance_text):
        '''Return a (source_line, highlight_line) pair of strings.  Each string contains SGR
        escape sequqences as specified by sgr_codes.
        '''
        highlights = [context.caret_highlight] + context.highlights
        col_ranges = [self.convert_to_column_range(highlight.start, highlight.end)
                      for highlight in highlights]

        # For long source lines we display a window of the line only.  Determine which
        # output column must be in that window.  We ensure the caret is displayed.
        required_column = col_ranges[0][0]
        assert required_column != -1

        # Truncate the line if necessary
        removed_chars, line = self.truncate(room, required_column)

        # Drop highlight ranges that didn't intersect the line.  Otherwise add ranges in
        # priority order in case of overlap and adjust column numbers to account for the
        # removed characters.
        char_ranges = []
        for n, (highlight, (start, end)) in enumerate(zip(highlights, col_ranges)):
            # Drop highlights that don't appear on this line
            if start == -1:
                continue
            start = max(0, min(start - removed_chars, room))
            end = max(0, min(end - removed_chars, room))
            # Special handling of caret range - the first character (including if wide) gets
            # the caret, the rest of the range gets the twiddles
            if n == 0:
                if highlight.start.line_number == self.line_number:
                    caret_end = start + line.char_width(start)
                    char_ranges.append((start, caret_end, 'caret'))
                char_ranges.append((start, end, 'locus'))
            else:
                char_ranges.append((start, end, 'range1' if n == 1 else 'range2'))

        def highlight_char(kind):
            return '^' if kind == 'caret' else '~'

        def highlight_parts(char_ranges):
            limit = max(end for start, end, kind in char_ranges)
            cursor = 0
            while cursor < limit:
                next_start = limit
                for start, end, kind in char_ranges:
                    end = min(end, next_start)
                    if start <= cursor < end:
                        highlight_text = highlight_char(kind) * (end - cursor)
                        yield enhance_text(highlight_text, kind)
                        cursor = end
                        break
                    elif start > cursor:
                        next_start = min(next_start, start)
                else:
                    yield ' ' * (next_start - cursor)
                    cursor = next_start

        def source_line_parts(line):
            cursor = 0
            for r in line.replacements:
                ncursor = sum(line.out_widths[:r])
                yield line.text[cursor: ncursor]
                cursor = ncursor + line.out_widths[r]
                replacement = line.text[ncursor:cursor]
                yield enhance_text(replacement, 'unprintable')
            yield line.text[cursor:]

        return ''.join(source_line_parts(line)), ''.join(highlight_parts(char_ranges))
