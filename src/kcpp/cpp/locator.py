# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The locator handles the details of token locations - whether they come from source files,
and if so which one, the include stack at the time of that location, the macro stack at the
time of expansion, etc.'''

from bisect import bisect_left
from dataclasses import dataclass
from enum import IntEnum, auto

from .lexer import Lexer
from ..core import Buffer, PresumedLocation
from ..diagnostics import (
    BufferRange, TokenRange, SpellingRange, DiagnosticContext, DID, RangeCoords,
    DiagnosticSeverity,
)


__all__ = ['Locator']


@dataclass(slots=True)
class LineRange:
    '''Gives a line number and filename to a location in a source file.  The BufferSpan
    constructor creates an initial instance.  Subsequent entries in the buffer span are
    created by #line directives.
    '''
    start: int         # The offset into the buffer that starts this range
    name: str          # A string literal
    line_adj: int      # Add this to the physical line number to get the presumed line number


class BufferSpan:
    '''Represents the span of locations used for a source file being processed.  If a source
    file is processed more than once (for example, if it is included twice), each usage
    gets its own span.

    An include stack is formed through parent_loc.  This points to the location of the include
    directive (the actual "include" token) so that an include stack can be produced.
    '''

    def __init__(self, buffer, start, name):
        self._buffer = buffer
        self.start = start
        # End is inclusive, so this permits an end-of-buffer location
        self.end = start + len(buffer.text)
        self.line_ranges = [LineRange(0, name, 0)]

    def buffer(self):
        return self._buffer

    def macro_parent_range(self, loc):
        assert self.start <= loc <= self.end
        # Buffers are terminal
        return None

    def spelling_loc(self, loc):
        return loc

    def add_line_range(self, start_loc, name, presumed_line_number):
        assert self.start <= start_loc <= self.end
        if name is Locator.prior_file_name:
            name = self.line_ranges[-1].name
        offset = start_loc - self.start
        _, line_number = self._buffer.offset_to_line_info(offset)
        self.line_ranges.append(LineRange(offset, name, presumed_line_number - line_number))

    def presumed_location(self, offset):
        '''Return a PresumedLocation instance.'''
        assert 0 <= offset <= self.end - self.start
        index = bisect_left(self.line_ranges, offset + 1, key=lambda lr: lr.start) - 1
        line_range = self.line_ranges[index]
        line_offset, line_number = self._buffer.offset_to_line_info(offset)
        presumed_line_number = line_number + line_range.line_adj

        return PresumedLocation(self._buffer, line_range.name, presumed_line_number,
                                line_number, offset - line_offset, line_offset)


class ScratchBufferSpan(Buffer):
    '''A scratch buffer holds the spelling of virtual tokens that are generated during
    preprocessing - including concatenated tokens, stringized tokens, _Pragma destringized
    strings, header names, builtin macro tokens, etc.

    The locator creates scratch buffers on demand; there may be several or none in
    existence.  A scratch buffer cannot change size once created as its span of locations
    is fixed and not unbounded.  The scratch buffer keeps track of the origin of the
    virtual tokens it creates so that the macro stack can be correctly reported.
    '''

    def __init__(self, start, end):
        '''Create a scratch buffer with the given size.'''
        super().__init__(bytearray(1))
        assert start <= end
        self.start = start
        self.end = end
        # Naturally sorted by offset.
        self.entries = []

    def buffer(self):
        return self

    def has_room(self, size):
        return len(self.text) + size + 1 <= self.end - self.start

    def add_spelling(self, spelling, parent_range, entry_kind):
        text = self.text
        # We place a newline character at the end of the spelling so it appears on its own
        # line in diagnostics.
        text.pop()  # Drop the NUL
        loc = self.start + len(text)
        text.extend(spelling)
        text.append(10)
        text.append(0)
        assert len(text) <= self.end - self.start
        self.entries.append(ScratchEntry(loc, parent_range, entry_kind))
        # Clear any cached line offsets
        self._sparse_line_offsets = None
        return loc

    def did_and_substitutions(self, pp, loc):
        entry = self.entry_for_loc(loc)
        if entry.kind == ScratchEntryKind.concatenate:
            return DID.in_token_concatenation, []
        elif entry.kind == ScratchEntryKind.stringize:
            return DID.in_argument_stringizing, []
        elif entry.kind == ScratchEntryKind.builtin:
            spelling = pp.token_spelling_at_loc(entry.parent_range.start)
            return DID.in_expansion_of_builtin, [spelling]
        elif entry.kind == ScratchEntryKind.header:
            return DID.from_formation_of_header_name, []
        elif entry.kind == ScratchEntryKind.pragma:
            return DID.from_Pragma, []
        raise RuntimeError('unknown ScratchEntryKind')

    def entry_for_loc(self, loc):
        '''Return the parent location (i.e. the location of the ## or # token) of a scratch buffer
        location.  loc is an offset into the scratch buffer.
        '''
        assert 0 <= loc - self.start < len(self.text)
        return self.entries[bisect_left(self.entries, loc + 1, key=lambda e: e.offset) - 1]

    def spelling_loc(self, loc):
        return loc

    def macro_parent_range(self, loc):
        return self.entry_for_loc(loc).parent_range

    def presumed_location(self, offset):
        '''Return a PresumedLocation instance.'''
        assert 0 <= offset <= self.end - self.start
        line_offset, line_number = self.offset_to_line_info(offset)
        # The filename must be a valid C string literal
        return PresumedLocation(self, '"<scratch>"', line_number, line_number,
                                offset - line_offset, line_offset)


class ScratchEntryKind(IntEnum):
    '''The reason a scratch entry was created.'''
    concatenate = auto()
    stringize = auto()
    builtin = auto()
    header = auto()
    pragma = auto()


@dataclass(slots=True)
class ScratchEntry:
    '''Describes an entry in the scratch buffer(s).'''
    offset: int
    parent_range: TokenRange
    kind: ScratchEntryKind


@dataclass(slots=True)
class MacroReplacementSpan:
    '''Represents the tokens of a macro's replacement list before parameter replacement takes
    place, in other words, as it appeared in the #define directive.  There is one location
    per token.
    '''
    macro: object
    invocation_range: TokenRange
    start: int
    end: int

    def spelling_loc(self, loc):
        token_index = loc - self.start
        return self.macro.replacement_list[token_index].loc

    def macro_parent_range(self, loc):
        return self.invocation_range

    def did_and_substitutions(self, pp, loc):
        return DID.in_expansion_of_macro, [self.macro.macro_name(pp)]


@dataclass(slots=True)
class MacroArgumentSpan:
    '''Represents the tokens that replaced a macro parameter in the replacement list.'''
    parameter_loc: int
    start: int
    end: int
    locations: list

    def spelling_loc(self, loc):
        assert self.start <= loc <= self.end
        return self.locations[loc - self.start]

    def macro_parent_range(self, loc):
        return TokenRange(self.parameter_loc, self.parameter_loc)


class Locator:
    '''Manages and supplies token locations.  A location is an integer but encodes the source
    of the token and its macro expansion history.
    '''

    FIRST_BUFFER_LOC = 1
    FIRST_MACRO_LOC = 1 << 40
    prior_file_name = str   # Used only as a sentinel

    def __init__(self, pp):
        self.pp = pp
        self.buffer_spans = []
        self.macro_spans = []
        self.scratch_range = None

    def next_buffer_span_start(self):
        if self.buffer_spans:
            return self.buffer_spans[-1].end + 1
        return self.FIRST_BUFFER_LOC

    def new_buffer_loc(self, buffer, name):
        start = self.next_buffer_span_start()
        self.buffer_spans.append(BufferSpan(buffer, start, name))
        # Create a scratch buffer after the initial buffer to keep first locations stable
        # if predefines change
        if not self.scratch_range:
            self.scratch_range = self.create_scratch_range(0)
        return start

    def primary_source_file_name(self):
        '''Return the string literal that is the name of the primary source file.'''
        if self.buffer_spans:
            return self.buffer_spans[0].line_ranges[0].name
        return None

    def next_macro_span_start(self):
        try:
            return self.macro_spans[-1].end + 1
        except IndexError:
            return self.FIRST_MACRO_LOC

    def macro_replacement_span(self, macro, invocation_range):
        assert isinstance(invocation_range, TokenRange)
        start = self.next_macro_span_start()
        end = start + len(macro.replacement_list) - 1
        self.macro_spans.append(MacroReplacementSpan(macro, invocation_range, start, end))
        return start

    def macro_argument_span(self, parameter_loc, locations):
        assert isinstance(parameter_loc, int)
        start = self.next_macro_span_start()
        end = start + len(locations) - 1
        self.macro_spans.append(MacroArgumentSpan(parameter_loc, start, end, locations))
        return start

    def create_scratch_range(self, min_size):
        start = self.next_buffer_span_start()
        size = max(min_size, 1_000)
        scratch_range = ScratchBufferSpan(start, start + size - 1)
        self.buffer_spans.append(scratch_range)
        return scratch_range

    def new_scratch_token(self, spelling, parent_range, entry_kind):
        assert isinstance(parent_range, TokenRange)
        size = len(spelling)
        if not self.scratch_range.has_room(size):
            self.scratch_range = self.create_scratch_range(size)
        return self.scratch_range.add_spelling(spelling, parent_range, entry_kind)

    def add_line_range(self, start_loc, name, line_number):
        span = self.lookup_span(start_loc)
        assert isinstance(span, BufferSpan)
        span.add_line_range(start_loc, name, line_number)

    def lookup_span(self, loc):
        if loc >= self.FIRST_MACRO_LOC:
            spans = self.macro_spans
        else:
            spans = self.buffer_spans
        n = bisect_left(spans, loc + 1, key=lambda lr: lr.start) - 1
        span = spans[n]
        assert span.start <= loc <= span.end, f'{span.start} <= {loc} <= {span.end} {span}'
        return span

    def spelling_span_and_offset(self, loc):
        '''Return a pair (span, offset) where span is a BufferSpan or ScratchBufferSpan
        instance.'''
        while True:
            span = self.lookup_span(loc)
            spelling_loc = span.spelling_loc(loc)
            if spelling_loc != loc:
                # Object-like macros need to recurse once.  Function-like macros may be
                # many times.
                loc = spelling_loc
                continue
            return span, loc - span.start

    def lexer_at_loc(self, loc):
        '''Return a new lexer ready to lex the spelling of the token at loc.'''
        span, offset = self.spelling_span_and_offset(loc)
        lexer = Lexer(self.pp, span.buffer().text, loc - offset, None)
        lexer.cursor = offset
        return lexer

    def derives_from_macro_expansion(self, loc):
        '''Return True if loc is from a macro expansion.'''
        return not isinstance(self.lookup_span(loc), BufferSpan)

    def spelling_coords(self, loc, token_end=False):
        '''Convert a location to a PresumedLocation instance.'''
        span, offset = self.spelling_span_and_offset(loc)
        if token_end:
            offset += self.token_length(span.start + offset)
        return span.presumed_location(offset)

    def token_length(self, loc):
        '''The length of the token in bytes in the source file.  This incldues, e.g., escaped
        newlines.  The result can be 0, for end-of-source indicator EOF.
        '''
        lexer = self.lexer_at_loc(loc)
        prior_cursor = lexer.cursor
        lexer.get_token_quietly()
        return lexer.cursor - prior_cursor

    def presumed_location(self, loc, force_outermost_context):
        '''Convert the location to a a PresumedLocation object.  The filename and line number are
        not necessarily physical, but affected by #line directives.

        If force_buffer_context is True, step through the parents of a location until a
        BufferSpan is reached.  The will be the original location if directly in a source
        file, otherwise the outermost macro invocation.  Otherwise use loc directly.
        '''
        if force_outermost_context:
            while True:
                span = self.lookup_span(loc)
                parent_range = span.macro_parent_range(loc)
                if parent_range is None:
                    break
                loc = parent_range.start
            offset = loc - span.start
        else:
            span, offset = self.spelling_span_and_offset(loc)
        return span.presumed_location(offset)

    def range_coords(self, source_range):
        if isinstance(source_range, SpellingRange):
            # Convert the SpellingRange to a BufferRange
            assert source_range.start < source_range.end
            lexer = self.lexer_at_loc(source_range.token_loc)
            cursor = lexer.cursor
            lexer.get_token_quietly()
            offsets = [source_range.start, source_range.end]
            # FIXME: this is ugly, find something better
            lexer.utf8_spelling(cursor, lexer.cursor, offsets)
            source_range = BufferRange(offsets[0], offsets[1])

        if isinstance(source_range, BufferRange):
            start = self.spelling_coords(source_range.start)
            end = self.spelling_coords(source_range.end)
            assert start.buffer is end.buffer
        elif isinstance(source_range, TokenRange):
            start = self.spelling_coords(source_range.start)
            end = self.spelling_coords(source_range.end, True)
        elif source_range is None:
            start = end = None
        else:
            raise RuntimeError(f'unhandled source range {source_range}')

        return RangeCoords(start, end)

    def spans_and_ranges(self, loc):
        '''Generates the spans of a location stepping up through the locations parents.

        The span of the location is generated first, then the span of its parent range,
        recursively.
        '''
        result = []
        range = TokenRange(loc, loc)
        while True:
            span = self.lookup_span(loc)
            if not span.start <= range.end <= span.end:
                range.end = span.end
            if isinstance(span, MacroArgumentSpan):
                macro_span = self.lookup_span(span.parameter_loc)
                result.append((macro_span, span.macro_parent_range(None)))
                loc = span.spelling_loc(loc)
                range = TokenRange(loc, loc)
            else:
                result.append((span, range))
                range = span.macro_parent_range(loc)
                if range is None:
                    return result
                loc = range.start

    def diagnostic_contexts(self, main_context):
        def intersections(spans, source_range):
            result = []
            if isinstance(source_range, BufferRange):
                start_spans_and_ranges = self.spans_and_ranges(source_range.start)
                for span in spans:
                    for our_span, our_range in start_spans_and_ranges:
                        if span is not our_span:
                            continue
                        if our_range.start == source_range.start:
                            item = source_range
                        else:
                            item = our_range
                        break
                    else:
                        item = None
                    result.append(item)
                return result

            if isinstance(source_range, SpellingRange):
                # A SpellingRange is a range of characters in a single token's spelling.
                # Remain a SpellingRange for the span with the token's spelling, otherwise
                # decay into a TokenRange for a single token.
                token_loc = source_range.token_loc
                spelling_span, _ = self.spelling_span_and_offset(token_loc)
                our_spans_and_ranges = self.spans_and_ranges(token_loc)
                for span in spans:
                    for our_span, our_range in our_spans_and_ranges:
                        if span is not our_span:
                            continue
                        if span is spelling_span:
                            item = SpellingRange(our_range.start, source_range.start,
                                                 source_range.end)
                        else:
                            item = our_range
                        break
                    else:
                        item = None
                    result.append(item)
                return result

            def token_intersection(span, token_spans_and_ranges, is_start):
                for token_span, token_range in token_spans_and_ranges:
                    if span is token_span:
                        loc = token_range.start if is_start else token_range.end
                        if span.start <= loc <= span.end:
                            return loc
                return None

            if isinstance(source_range, TokenRange):
                start_spans_and_ranges = self.spans_and_ranges(source_range.start)
                end_spans_and_ranges = self.spans_and_ranges(source_range.end)
                for span in spans:
                    start = token_intersection(span, start_spans_and_ranges, True)
                    end = token_intersection(span, end_spans_and_ranges, False)
                    if start is None:
                        if end is not None:
                            item = TokenRange(span.start, end)
                        else:
                            item = None
                    else:
                        if end is None:
                            item = TokenRange(start, span.end)
                        else:
                            item = TokenRange(start, end)
                    result.append(item)
                return result

            assert False

        # We are dealing with a diagnostic about source code.
        contexts = []
        # Caret range is an instance of BufferRange, SpellingRange or single token
        # location.  Highlights are token ranges.
        #
        # However, the generic case it is easy to handle, so in case future diagnostic
        # evolution would benefit, the code accepts and handles appropriately any of
        # Buffer Range, SpellingRange, TokenRange or single token locations for the caret
        # range and the highlights.
        caret_spans_and_ranges = self.spans_and_ranges(main_context.caret_range.caret_loc())
        caret_spans = [span for span, range in caret_spans_and_ranges]
        caret_ranges = intersections(caret_spans, main_context.caret_range)
        source_ranges_list = [intersections(caret_spans, source_range)
                              for source_range in main_context.source_ranges]

        for n, (span, caret_range) in enumerate(zip(caret_spans, caret_ranges)):
            if isinstance(span, BufferSpan):
                severity = main_context.severity
                did, substitutions = main_context.did, main_context.substitutions
            else:
                severity = DiagnosticSeverity.note
                did, substitutions = span.did_and_substitutions(self.pp, caret_range.caret_loc())

            source_ranges = [source_ranges_item[n] for source_ranges_item in source_ranges_list]

            # We finally have the new context; add it to the list.  Macro contexts are
            # always notes.
            contexts.append(DiagnosticContext(did, severity, substitutions,
                                              caret_range, source_ranges))

        contexts.reverse()
        return contexts
