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

from ..diagnostics import BufferRange, TokenRange, SpellingRange

__all__ = ['Locator']


class LocationRangeKind(IntEnum):
    buffer = auto()
    macro = auto()


@dataclass(slots=True)
class LocationRange:
    '''Represents a contiguous location range.'''
    kind: LocationRangeKind
    first: int
    size: int
    parent: int   # A number > 0, or -1
    extra: any


@dataclass(slots=True)
class LocationContext:
    source_ranges: list
    macro: any


class Locator:
    '''Manages and supplies token locations.'''

    FIRST_BUFFER_LOC = 1
    FIRST_MACRO_LOC = 1 << 40

    def __init__(self):
        self.buffer_ranges = []
        self.macro_ranges = []

    def new_buffer_loc(self, parent_loc, buffer_size, extra):
        assert isinstance(parent_loc, int)
        assert parent_loc > 0 or parent_loc == -1
        buffer_ranges = self.buffer_ranges
        if buffer_ranges:
            first = buffer_ranges[-1].first + buffer_ranges[-1].size
        else:
            first = self.FIRST_BUFFER_LOC
        buffer_ranges.append(LocationRange(LocationRangeKind.buffer, first, buffer_size,
                                           parent_loc, extra))
        return first

    def new_macro_range(self, parent_loc, count, extra):
        assert isinstance(parent_loc, int)
        assert parent_loc > 0
        macro_ranges = self.macro_ranges
        if macro_ranges:
            first = macro_ranges[-1].first + macro_ranges[-1].size
        else:
            first = self.FIRST_MACRO_LOC
        macro_ranges.append(LocationRange(LocationRangeKind.macro, first, count,
                                          parent_loc, extra))
        return first

    def lookup_range(self, loc):
        if loc >= self.FIRST_MACRO_LOC:
            loc_ranges = self.macro_ranges
        else:
            loc_ranges = self.buffer_ranges
        n = bisect_left(loc_ranges, loc + 1, key=lambda lr: lr.first) - 1
        loc_range = loc_ranges[n]
        assert loc_range.first <= loc < loc_range.first + loc_range.size
        return loc_range

    def loc_to_buffer_and_offset(self, loc):
        '''Return a triple (buffer, buffer_start_loc, offset).'''
        loc_range = self.lookup_range(loc)
        if loc_range.kind == LocationRangeKind.macro:
            loc = loc_range.extra.token_loc(loc - loc_range.first)
            loc_range = self.lookup_range(loc)
        assert loc_range.kind == LocationRangeKind.buffer
        return loc_range.extra, loc - loc_range.first

    def loc_to_buffer_coords(self, loc):
        buffer, offset = self.loc_to_buffer_and_offset(loc)
        return buffer.offset_to_coords(offset)

    def location_stack(self, loc):
        locations = []
        while True:
            loc_range = self.lookup_range(loc)
            if loc_range.kind == LocationRangeKind.macro:
                locations.append(loc_range.extra.token_loc(loc - loc_range.first))
                loc = loc_range.parent
                continue
            locations.append(loc)
            return locations

    def locations_and_spans(self, loc):
        stack = []
        while True:
            loc_range = self.lookup_range(loc)
            if loc_range.kind == LocationRangeKind.macro:
                token_loc = loc_range.extra.token_loc(loc - loc_range.first)
                stack.append((token_loc, loc_range.extra, loc_range.first,
                              loc_range.first + loc_range.size - 1))
                loc = loc_range.parent
                continue
            stack.append((loc, None, -1, -1))
            return stack

    def locations_only(self, loc):
        stack = []
        while True:
            loc_range = self.lookup_range(loc)
            stack.append(loc)
            if loc_range.kind == LocationRangeKind.macro:
                loc = loc_range.parent
                continue
            return stack

    def macro_contexts(self, source_ranges):
        def is_a_buffer_range(source_range):
            if isinstance(source_range, BufferRange):
                return True
            if isinstance(source_range, TokenRange):
                assert source_range.start == source_range.end
                token_loc = source_range.start
            else:
                assert isinstance(source_range, SpellingRange)
                token_loc = source_range.token_loc
            return self.lookup_range(token_loc).kind is LocationRangeKind.buffer

        def lower_token_range(source_range):
            start = self.location_stack(source_range.start)[-1]
            end = self.location_stack(source_range.end)[-1]
            return TokenRange(start, end)

        def intersection(start, end, start_stack, end_stack):
            start_loc = None
            end_loc = None
            for loc in start_stack:
                if start <= loc <= end:
                    start_loc = loc
                    break
            for loc in end_stack:
                if start <= loc <= end:
                    end_loc = loc
                    break
            if start_loc is None:
                if end_loc is None:
                    return None
                return TokenRange(start, end_loc)
            if end_loc is None:
                return TokenRange(start_loc, end)
            return TokenRange(start_loc, end_loc)

        assert all(isinstance(source_range, TokenRange) for source_range in source_ranges[1:])
        caret_range = source_ranges[0]
        if is_a_buffer_range(caret_range):
            for n in range(1, len(source_ranges)):
                source_ranges[n] = lower_token_range(source_ranges[n])
            return [LocationContext(source_ranges, None)]

        # Get the location stack for the start and end of each source range.  The start
        # and end of a source range can have different depth stacks.
        start_stacks = [self.locations_only(sr.start) for sr in source_ranges[1:]]
        end_stacks = [self.locations_only(sr.end) for sr in source_ranges[1:]]

        print('Ranges:', source_ranges[1:])
        print('SS:', start_stacks)
        print('ES:', end_stacks)

        contexts = []
        for loc, macro, span_start, span_end in self.locations_and_spans(caret_range.start):
            # Start with the caret range for this level
            context_ranges = [TokenRange(loc, loc)]
            print('SPAN:', loc, span_start, span_end)
            # Now figure out where each source range intersects this context level, if any
            for start_stack, end_stack in zip(start_stacks, end_stacks):
                if span_start == -1:
                    if start_stack[-1] != loc:
                        context_ranges.append(TokenRange(start_stack[-1], end_stack[-1]))
                else:
                    context_range = intersection(span_start, span_end, start_stack, end_stack)
                    if context_range:
                        context_ranges.append(context_range)
            contexts.append(LocationContext(context_ranges, macro))

        return reversed(contexts)
