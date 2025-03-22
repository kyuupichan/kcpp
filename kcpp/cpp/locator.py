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
from .basic import Buffer, Token, BufferCoords
from .lexer import Lexer


__all__ = ['Locator']


@dataclass(slots=True)
class ElaboratedLocation:
    '''Detailed informatino about location within a buffer.'''
    # The original location
    loc: int
    # The location as coordinates.  None for diagnostics with special locations
    # (location_command_line, location_none).
    coords: BufferCoords


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

    def to_range_coords(self):
        return RangeCoords(self.start.coords, self.end.coords)


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
    parent: int   # A number > 0, or -1.   FIXME: this should probably go away.
    owner: any

    def parent_loc(self, loc):
        if self.kind == RangeKind.scratch:
            return self.owner.parent_loc(loc - self.start)
        return self.parent


@dataclass(slots=True)
class MacroContext:
    macro_loc: int
    loc_range: LocationRange


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
        # A sorted list of offsets into the scratch buffer, and corresponding parent locations.
        self.offsets = []
        self.parent_locs = []

    def add_spelling(self, spelling, parent_loc):
        start = len(self.text)
        if start + len(spelling) + 1 < self.size:
            # Add the spelling and a newline character (so it appears on its own line in
            # diagnostics)
            self.text.extend(spelling)
            self.text.append(10)
            self.offsets.append(start)
            self.parent_locs.append(parent_loc)
            # Clear the cached line offsets
            self._sparse_line_offsets = None
            return start
        return -1

    def buffer_loc(self, loc):
        '''Return the location of the spelling in the scratch buffer.  loc is a location, not an
        offset.  Return it unchanged as it is into this buffer.'''
        return loc

    def parent_loc(self, loc):
        '''Return the parent location (i.e. the location of the ## or # token) of a scratch buffer
        location.
        '''
        assert 0 <= loc < len(self.text)
        return self.parent_locs[bisect_left(self.offsets, loc + 1) - 1]

    def did_and_substitutions(self):
        return DID.in_token_concatenation, []


class Locator:
    '''Manages and supplies token locations.'''

    FIRST_BUFFER_LOC = 1
    FIRST_MACRO_LOC = 1 << 40

    def __init__(self, pp):
        self.pp = pp
        self.buffer_ranges = []
        self.macro_ranges = []
        self.scratch_buffer_range = None

    def new_buffer_range(self, parent_loc, size, buffer, kind):
        assert isinstance(parent_loc, int)
        assert parent_loc > 0 or parent_loc == -1
        buffer_ranges = self.buffer_ranges
        if buffer_ranges:
            start = buffer_ranges[-1].end + 1
        else:
            start = self.FIRST_BUFFER_LOC
        buffer_range = LocationRange(kind, start, start + size - 1, parent_loc, buffer)
        buffer_ranges.append(buffer_range)
        return buffer_range

    def new_buffer_loc(self, parent_loc, buffer_size, buffer):
        buffer_range = self.new_buffer_range(parent_loc, buffer_size, buffer, RangeKind.buffer)
        return buffer_range.start

    def new_scratch_buffer(self, size):
        buffer = ScratchBuffer(size)
        self.scratch_buffer_range = self.new_buffer_range(-1, size, buffer, RangeKind.scratch)

    def new_macro_range(self, parent_loc, count, owner):
        assert isinstance(parent_loc, int)
        assert parent_loc > 0
        macro_ranges = self.macro_ranges
        if macro_ranges:
            start = macro_ranges[-1].end + 1
        else:
            start = self.FIRST_MACRO_LOC
        macro_ranges.append(LocationRange(RangeKind.macro, start, start + count - 1,
                                          parent_loc, owner))
        return start

    def new_scratch_token(self, spelling, parent_loc):
        def alloc_in_current(spelling):
            if self.scratch_buffer_range:
                result = self.scratch_buffer_range.owner.add_spelling(spelling, parent_loc)
                if result != -1:
                    return self.scratch_buffer_range.start + result
            return -1

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
        '''Return a pair (buffer, offset).'''
        loc_range = self.lookup_range(loc)
        if loc_range.kind == RangeKind.macro:
            loc = loc_range.owner.buffer_loc(loc)
            loc_range = self.lookup_range(loc)
        assert loc_range.kind != RangeKind.macro
        return loc_range.owner, loc - loc_range.start

    def diagnostic_contexts_core(self, orig_context):
        def caret_range_token_loc(source_range):
            if isinstance(source_range, BufferRange):
                # A BufferRange can be into a standard or scratch buffer.
                return source_range.start
            if isinstance(source_range, TokenRange):
                assert source_range.start == source_range.end
                return source_range.start
            return source_range.token_loc   # SpellingRange

        def to_standard_buffer_loc(loc):
            while True:
                loc_range = self.lookup_range(loc)
                if loc_range.kind != RangeKind.buffer:
                    loc = loc_range.parent_loc(loc)
                    continue
                return loc

        def standard_buffer_range(source_range):
            start = to_standard_buffer_loc(source_range.start)
            end = to_standard_buffer_loc(source_range.end)
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

        def intersections(caret_context, highlight_contexts):
            def token_range(start, end):
                return TokenRange(owner.buffer_loc(start), owner.buffer_loc(end))

            loc_range = caret_context.loc_range
            owner = loc_range.owner
            # Start with the caret range for this context
            caret_range = token_range(caret_context.macro_loc, caret_context.macro_loc)
            source_ranges = []
            for start_contexts, end_contexts in highlight_contexts:
                start_loc = None
                end_loc = None
                for context in start_contexts:
                    if context.loc_range is loc_range:
                        start_loc = context.macro_loc
                        break
                for context in end_contexts:
                    if context.loc_range is loc_range:
                        end_loc = context.macro_loc
                        break
                if start_loc is None:
                    if end_loc is not None:
                        source_ranges.append(token_range(loc_range.start, end_loc))
                elif end_loc is None:
                    source_ranges.append(token_range(start_loc, loc_range.end))
                else:
                    source_ranges.append(token_range(start_loc, end_loc))
            return caret_range, source_ranges

        contexts = []
        caret_token_loc = caret_range_token_loc(orig_context.caret_range)
        # Do we require a context stack?
        if self.lookup_range(caret_token_loc).kind != RangeKind.buffer:
            caret_contexts = macro_context_stack(caret_token_loc)
            highlight_contexts = [range_contexts(source_range)
                                  for source_range in orig_context.source_ranges]

            for caret_context in caret_contexts:
                # Now add an extry for each source range that intersects this context level
                caret_range, source_ranges = intersections(caret_context, highlight_contexts)
                if caret_context.loc_range is None:
                    # Use the original context but replace its source ranges
                    orig_context.source_ranges = source_ranges
                    context = orig_context
                else:
                    did, substitutions = caret_context.loc_range.owner.did_and_substitutions()
                    context = DiagnosticContext(did, substitutions, caret_range, source_ranges)
                contexts.append(context)
            # Lower the caret range
            token_loc = to_standard_buffer_loc(caret_token_loc)
            orig_context.caret_range = TokenRange(token_loc, token_loc)

        # Lower the source ranges in the original context and make it the final context
        orig_context.source_ranges = [standard_buffer_range(source_range)
                                      for source_range in orig_context.source_ranges]
        contexts.append(orig_context)
        contexts.reverse()
        return contexts

    def token_spelling(self, loc):
        buffer, offset = self.loc_to_buffer_and_offset(loc)
        lexer = Lexer(self.pp, buffer.text, loc - offset)
        prior = self.pp.set_diagnostic_consumer(None)
        spelling = lexer.token_spelling(offset)
        self.pp.set_diagnostic_consumer(prior)
        return spelling

    def token_length(self, loc):
        '''The length of the token in bytes in the physical file.  This incldues, e.g., escaped
        newlines.  The result can be 0, for end-of-source indicator EOF.
        '''
        buffer, offset = self.loc_to_buffer_and_offset(loc)
        lexer = Lexer(self.pp, buffer.text, loc - offset)
        token = Token.create()
        lexer.cursor = offset
        prior = self.pp.set_diagnostic_consumer(None)
        lexer.get_token(token)
        self.pp.set_diagnostic_consumer(prior)
        return lexer.cursor - offset

    def elaborated_location(self, loc):
        '''Convert a location to an ElaboratedLocation.'''
        if loc <= location_none:
            return ElaboratedLocation(loc, None)
        buffer, offset = self.loc_to_buffer_and_offset(loc)
        coords = buffer.offset_to_coords(offset)
        return ElaboratedLocation(loc, coords)

    def elaborated_range(self, source_range):
        if isinstance(source_range, SpellingRange):
            # Convert the SpellingRange to a BufferRange
            assert source_range.start < source_range.end
            token_loc = source_range.token_loc
            buffer, offset = self.loc_to_buffer_and_offset(token_loc)
            lexer = Lexer(self.pp, buffer.text, token_loc - offset)
            token = Token.create()
            lexer.cursor = offset
            prior = self.pp.set_diagnostic_consumer(None)
            lexer.get_token(token)
            offsets = [source_range.start, source_range.end]
            lexer.utf8_spelling(offset, lexer.cursor, offsets)
            self.pp.set_diagnostic_consumer(prior)
            source_range = BufferRange(offsets[0], offsets[1])

        if isinstance(source_range, BufferRange):
            start = self.elaborated_location(source_range.start)
            end = self.elaborated_location(source_range.end)
            assert start.coords.buffer is end.coords.buffer
        elif isinstance(source_range, TokenRange):
            start = self.elaborated_location(source_range.start)
            if source_range.start == source_range.end:
                end = start
            else:
                end = self.elaborated_location(source_range.end)
            if source_range.start > location_none:
                token_end = source_range.end + self.token_length(end.loc)
                end = self.elaborated_location(token_end)
        else:
            raise RuntimeError(f'unhandled source range {source_range}')

        return ElaboratedRange(start, end)

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
            context.caret_range = self.elaborated_range(context.caret_range)
            context.source_ranges = [self.elaborated_range(source_range)
                                     for source_range in context.source_ranges]

        return contexts
