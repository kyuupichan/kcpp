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

from ..diagnostics import (
    BufferRange, TokenRange, SpellingRange, DiagnosticContext, DID, location_none,
    RangeCoords,
)
from .basic import Buffer
from .lexer import Lexer


__all__ = ['Locator']


class RangeKind(IntEnum):
    '''Describes the type of a LocationRange instance.'''
    buffer = auto()     # A normal buffer
    scratch = auto()    # A scratch buffer
    macro = auto()      # A range of tokens from the macro expansion process


@dataclass(slots=True)
class LocationRange:
    '''Represents a contiguous location range.'''
    kind: RangeKind
    # Range is inclusive from start to end.
    start: int
    end: int
    # buffer: the Buffer object.  scratch: the ScratchBuffer object.  macro: a macro object
    origin: object

    def buffer_loc(self, loc):
        assert self.kind != RangeKind.buffer
        if self.kind == RangeKind.scratch:
            return loc
        return self.origin.buffer_loc(loc - self.start)

    def parent_loc(self, loc):
        assert self.kind != RangeKind.buffer
        return self.origin.parent_loc(loc - self.start)

    def did_and_substitutions(self, pp, loc):
        if self.kind == RangeKind.scratch:
            kind = self.origin.entry_for_loc(loc - self.start).kind
            if kind == ScratchEntryKind.concatenate:
                return DID.in_token_concatenation, []
            else:
                return DID.in_argument_stringizing, []
        elif self.kind == RangeKind.macro:
            return DID.in_expansion_of_macro, [self.origin.macro_name(pp)]
        assert False


@dataclass(slots=True)
class LineRange:
    '''Names a range of lines in a source file.  A new BufferRange entry creates an initial
    instance.  Subsequent entries are created by #line directives.'''
    start: int           # The first location in this range
    name: str            # Normally a file name, but can be e.g. <scratch>
    line_number: int


class BufferLocationRange:
    '''Represents the range of locations used for a source file being processed.  If a source
    file is processed more than once (for example, if it is included twice), each usage
    gets its own range.

    An include stack is formed through parent_loc.  This points to the location of the include
    directive (the actual "include" token) so that an include stack can be produced.
    '''

    def __init__(self, buffer, start, end, parent_loc, name):
        assert start <= end
        self.kind = RangeKind.buffer
        self.buffer = buffer
        self.start = start
        self.end = end
        self._parent_loc = parent_loc
        self.line_ranges = [LineRange(start, name, 1)]
        self.origin = buffer

    def parent_loc(self, loc):
        assert self.start <= loc <= self.end
        return -1

    def buffer_loc(self, loc):
        assert self.start <= loc <= self.end
        return loc

    def add_line_range(self, start, name, line_number):
        assert start < self.line_ranges[-1].start < start <= self.end
        self.line_ranges.append(LineRange(start, name, line_number))


@dataclass(slots=True)
class MacroContext:
    macro_loc: int
    loc_range: LocationRange


class ScratchEntryKind(IntEnum):
    concatenate = auto()
    stringize = auto()


@dataclass(slots=True)
class ScratchEntry:
    offset: int
    parent_loc: int
    kind: ScratchEntryKind


class ScratchBuffer(Buffer):
    '''A scratch buffer holds the spelling of virtual tokens that are generated by the macro
    expansion process - namely concatenated tokens and stringized tokens.

    The locator creates scratch buffers on demand; there may be several or none in
    existence.  A scratch buffer cannot change size once craeted, as the locator creates a
    fixed-size buffer range for it.  The scratch buffer keeps track of the origin of the
    virtual tokens it creates so that the macro stack can be correctly reported.'''

    def __init__(self, size):
        '''Create a scratch buffer with the given size.'''
        super().__init__(bytearray(), name='<scratch>')
        self.size = size
        # Naturally sorted by offset.
        self.entries = []

    def add_spelling(self, spelling, parent_loc, entry_kind):
        start = len(self.text)
        if start + len(spelling) + 1 < self.size:
            # Add the spelling and a newline character (so it appears on its own line in
            # diagnostics)
            self.text.extend(spelling)
            self.text.append(10)
            self.entries.append(ScratchEntry(start, parent_loc, entry_kind))
            # Clear the cached line offsets
            self._sparse_line_offsets = None
            return start
        return -1

    def entry_for_loc(self, loc):
        '''Return the parent location (i.e. the location of the ## or # token) of a scratch buffer
        location.  loc is an offset into the scratch buffer.
        '''
        assert 0 <= loc < len(self.text)
        return self.entries[bisect_left(self.entries, loc + 1, key=lambda e: e.offset) - 1]

    def parent_loc(self, loc):
        return self.entry_for_loc(loc).parent_loc


class Locator:
    '''Manages and supplies token locations.'''

    FIRST_BUFFER_LOC = 1
    FIRST_MACRO_LOC = 1 << 40

    def __init__(self, pp):
        self.pp = pp
        self.buffer_ranges = []
        self.macro_ranges = []
        self.scratch_buffer_range = None

    def new_buffer_range(self, size, buffer, kind, parent_loc, name):
        buffer_ranges = self.buffer_ranges
        if buffer_ranges:
            start = buffer_ranges[-1].end + 1
        else:
            start = self.FIRST_BUFFER_LOC

        if kind == RangeKind.scratch:
            buffer_range = LocationRange(kind, start, start + size - 1, buffer)
        else:
            buffer_range = BufferLocationRange(buffer, start, start + size - 1, parent_loc, name)
        buffer_ranges.append(buffer_range)
        return buffer_range

    def new_buffer_loc(self, buffer, name, parent_loc):
        # Allow a location for the buffer's EOF.
        size = len(buffer.text) + 1
        buffer_range = self.new_buffer_range(size, buffer, RangeKind.buffer, parent_loc, name)
        return buffer_range.start

    def new_scratch_buffer(self, size):
        buffer = ScratchBuffer(size)
        self.scratch_buffer_range = self.new_buffer_range(size, buffer, RangeKind.scratch, -1,
                                                          '<scratch>')

    def new_macro_range(self, count, origin):
        macro_ranges = self.macro_ranges
        if macro_ranges:
            start = macro_ranges[-1].end + 1
        else:
            start = self.FIRST_MACRO_LOC
        macro_ranges.append(LocationRange(RangeKind.macro, start, start + count - 1, origin))
        return start

    def new_scratch_token(self, spelling, parent_loc, entry_kind):
        def alloc_in_current(spelling):
            if self.scratch_buffer_range:
                scratch_buffer = self.scratch_buffer_range.origin
                result = scratch_buffer.add_spelling(spelling, parent_loc, entry_kind)
                if result != -1:
                    return self.scratch_buffer_range.start + result
            return -1

        assert isinstance(entry_kind, ScratchEntryKind)
        loc = alloc_in_current(spelling)
        if loc == -1:
            self.new_scratch_buffer(max(len(spelling), 1_000))
            loc = alloc_in_current(spelling)
            assert loc != -1
        return loc

    def lookup_range(self, loc):
        if loc >= self.FIRST_MACRO_LOC:
            loc_ranges = self.macro_ranges
        else:
            loc_ranges = self.buffer_ranges
        n = bisect_left(loc_ranges, loc + 1, key=lambda lr: lr.start) - 1
        loc_range = loc_ranges[n]
        assert loc_range.start <= loc <= loc_range.end
        return loc_range

    def loc_to_buffer_and_offset(self, loc):
        '''Return a pair (buffer, offset) where buffer is a Buffer or ScratchBuffer instance.'''
        loc_range = self.lookup_range(loc)
        if loc_range.kind == RangeKind.macro:
            loc = loc_range.buffer_loc(loc)
            loc_range = self.lookup_range(loc)
        assert loc_range.kind != RangeKind.macro
        return loc_range.origin, loc - loc_range.start

    def source_buffer_loc(self, loc):
        while True:
            loc_range = self.lookup_range(loc)
            if loc_range.kind != RangeKind.buffer:
                loc = loc_range.parent_loc(loc)
                continue
            return loc

    def source_file_coords(self, loc):
        return self.buffer_coords(self.source_buffer_loc(loc))

    def diagnostic_contexts_core(self, orig_context):
        def source_buffer_range(source_range):
            start = self.source_buffer_loc(source_range.start)
            end = self.source_buffer_loc(source_range.end)
            return TokenRange(start, end)

        def macro_context_stack(loc):
            contexts = []
            while True:
                loc_range = self.lookup_range(loc)
                if loc_range.kind != RangeKind.buffer:
                    contexts.append(MacroContext(loc, loc_range))
                    loc = loc_range.parent_loc(loc)
                    continue
                return contexts

        def range_contexts(token_range):
            start_contexts = macro_context_stack(token_range.start)
            if token_range.start == token_range.end:
                end_contexts = start_contexts
            else:
                end_contexts = macro_context_stack(token_range.end)
            return [start_contexts, end_contexts]

        def intersections(loc_range, highlight_contexts):
            ranges = []
            for start_contexts, end_contexts in highlight_contexts:
                start = None
                end = None
                for context in start_contexts:
                    if context.loc_range is loc_range:
                        start = context.macro_loc
                        break
                for context in end_contexts:
                    if context.loc_range is loc_range:
                        end = context.macro_loc
                        break
                if start is None:
                    if end is not None:
                        ranges.append((loc_range.start, end))
                elif end is None:
                    ranges.append((start, loc_range.end))
                else:
                    ranges.append((start, end))

            buffer_loc = loc_range.buffer_loc
            return [TokenRange(buffer_loc(start), buffer_loc(end)) for start, end in ranges]

        def caret_and_loc_ranges(orig_context):
            def caret_range_token_loc(source_range):
                if isinstance(source_range, BufferRange):
                    # A BufferRange can be into a standard or scratch buffer.
                    return source_range.start
                if isinstance(source_range, TokenRange):
                    assert source_range.start == source_range.end
                    return source_range.start
                return source_range.token_loc   # SpellingRange

            caret_token_loc = caret_range_token_loc(orig_context.caret_range)
            result = macro_context_stack(caret_token_loc)
            if result:
                for n, context in enumerate(result):
                    caret_loc = context.loc_range.buffer_loc(context.macro_loc)
                    if n == 0 and isinstance(orig_context.caret_range, SpellingRange):
                        caret_range = SpellingRange(caret_loc, orig_context.caret_range.start,
                                                    orig_context.caret_range.end)
                    else:
                        caret_range = TokenRange(caret_loc, caret_loc)
                    result[n] = (caret_loc, caret_range, context.loc_range)

                # Lower the orig_context caret range to a source file
                token_loc = self.source_buffer_loc(caret_token_loc)
                orig_context.caret_range = TokenRange(token_loc, token_loc)
            return result

        contexts = []
        caret_ranges = caret_and_loc_ranges(orig_context)

        if caret_ranges:
            highlight_contexts = [range_contexts(source_range)
                                  for source_range in orig_context.source_ranges]
            for caret_loc, caret_range, loc_range in caret_ranges:
                # Now add an extry for each source range that intersects this context level
                source_ranges = intersections(loc_range, highlight_contexts)
                if loc_range is None:
                    # Use the original context but replace its source ranges
                    orig_context.source_ranges = source_ranges
                    context = orig_context
                else:
                    did, substitutions = loc_range.did_and_substitutions(self.pp, caret_loc)
                    context = DiagnosticContext(did, substitutions, caret_loc,
                                                caret_range, source_ranges)
                contexts.append(context)

        # Lower the source ranges in the original context and make it the final context
        orig_context.source_ranges = [source_buffer_range(source_range)
                                      for source_range in orig_context.source_ranges]
        contexts.append(orig_context)
        contexts.reverse()
        return contexts

    def token_length(self, loc):
        '''The length of the token in bytes in the physical file.  This incldues, e.g., escaped
        newlines.  The result can be 0, for end-of-source indicator EOF.
        '''
        buffer, offset = self.loc_to_buffer_and_offset(loc)
        lexer = Lexer(self.pp, buffer.text, loc - offset)
        lexer.cursor = offset
        lexer.get_token_quietly()
        return lexer.cursor - offset

    def buffer_coords(self, loc):
        '''Convert a location to a BufferCoords instance.'''
        buffer, offset = self.loc_to_buffer_and_offset(loc)
        return buffer.offset_to_coords(offset)

    def range_coords(self, source_range):
        if isinstance(source_range, SpellingRange):
            # Convert the SpellingRange to a BufferRange
            assert source_range.start < source_range.end
            token_loc = source_range.token_loc
            buffer, offset = self.loc_to_buffer_and_offset(token_loc)
            lexer = Lexer(self.pp, buffer.text, token_loc - offset)
            lexer.cursor = offset
            lexer.get_token_quietly()
            offsets = [source_range.start, source_range.end]
            lexer.utf8_spelling(offset, lexer.cursor, offsets)
            source_range = BufferRange(offsets[0], offsets[1])

        if isinstance(source_range, BufferRange):
            start = self.buffer_coords(source_range.start)
            end = self.buffer_coords(source_range.end)
            assert start.buffer is end.buffer
        elif isinstance(source_range, TokenRange):
            if source_range.start <= location_none:
                start = end = None
            else:
                start = self.buffer_coords(source_range.start)
                if source_range.start == source_range.end:
                    end = start
                else:
                    end = self.buffer_coords(source_range.end)
                token_end = source_range.end + self.token_length(source_range.end)
                end = self.buffer_coords(token_end)
        else:
            raise RuntimeError(f'unhandled source range {source_range}')

        return RangeCoords(start, end)

    def diagnostic_contexts(self, context):
        '''Expand the diagnostic context stack for the given diagnostic context.'''
        # Apart from the caret range, all ranges must be TokenRange instances
        assert all(isinstance(source_range, TokenRange)
                   for source_range in context.source_ranges)

        # Special ranges don't have source text, and only a single location code
        if context.caret_range.start <= location_none:
            assert not context.source_ranges
            contexts = [context]
        else:
            contexts = self.diagnostic_contexts_core(context)

        for context in contexts:
            context.caret_range = self.range_coords(context.caret_range)
            context.source_ranges = [self.range_coords(source_range)
                                     for source_range in context.source_ranges]

        return contexts
