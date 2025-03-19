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

from ..diagnostics import BufferRange, TokenRange, SpellingRange, DiagnosticContext

__all__ = ['Locator']


class LocationRangeKind(IntEnum):
    buffer = auto()
    macro = auto()


@dataclass(slots=True)
class LocationRange:
    '''Represents a contiguous location range.'''
    kind: LocationRangeKind
    # Range is inclusive from start to end.
    start: int
    end: int
    parent: int   # A number > 0, or -1
    owner: any


@dataclass(slots=True)
class MacroContext:
    macro_loc: int
    loc_range: LocationRange


class Locator:
    '''Manages and supplies token locations.'''

    FIRST_BUFFER_LOC = 1
    FIRST_MACRO_LOC = 1 << 40

    def __init__(self):
        self.buffer_ranges = []
        self.macro_ranges = []

    def new_buffer_loc(self, parent_loc, buffer_size, buffer):
        assert isinstance(parent_loc, int)
        assert parent_loc > 0 or parent_loc == -1
        buffer_ranges = self.buffer_ranges
        if buffer_ranges:
            start = buffer_ranges[-1].end + 1
        else:
            start = self.FIRST_BUFFER_LOC
        buffer_ranges.append(LocationRange(LocationRangeKind.buffer, start,
                                           start + buffer_size - 1, parent_loc, buffer))
        return start

    def new_macro_range(self, parent_loc, count, owner):
        assert isinstance(parent_loc, int)
        assert parent_loc > 0
        macro_ranges = self.macro_ranges
        if macro_ranges:
            start = macro_ranges[-1].end + 1
        else:
            start = self.FIRST_MACRO_LOC
        macro_ranges.append(LocationRange(LocationRangeKind.macro, start, start + count - 1,
                                          parent_loc, owner))
        return start

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
        '''Return a triple (buffer, buffer_start_loc, offset).'''
        loc_range = self.lookup_range(loc)
        if loc_range.kind == LocationRangeKind.macro:
            loc = loc_range.owner.buffer_loc(loc)
            loc_range = self.lookup_range(loc)
        assert loc_range.kind == LocationRangeKind.buffer
        return loc_range.owner, loc - loc_range.start

    def loc_to_buffer_coords(self, loc):
        buffer, offset = self.loc_to_buffer_and_offset(loc)
        return buffer.offset_to_coords(offset)

    def macro_contexts(self, loc):
        contexts = []
        while True:
            loc_range = self.lookup_range(loc)
            if loc_range.kind == LocationRangeKind.macro:
                contexts.append(MacroContext(loc, loc_range))
                loc = loc_range.parent
                continue
            return contexts

    def ultimate_buffer_loc(self, loc):
        while True:
            loc_range = self.lookup_range(loc)
            if loc_range.kind == LocationRangeKind.macro:
                loc = loc_range.parent
                continue
            return loc

    def range_contexts(self, token_range):
        start_contexts = self.macro_contexts(token_range.start)
        if token_range.start == token_range.end:
            end_contexts = start_contexts
        else:
            end_contexts = self.macro_contexts(token_range.end)
        return [start_contexts, end_contexts]

    def diagnostic_contexts(self, pp, orig_context):
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
            start = self.ultimate_buffer_loc(source_range.start)
            end = self.ultimate_buffer_loc(source_range.end)
            return TokenRange(start, end)

        def intersections(caret_context, highlight_contexts):
            def token_range(start, end):
                return TokenRange(owner.buffer_loc(start), owner.buffer_loc(end))

            loc_range = caret_context.loc_range
            owner = loc_range.owner

            # Start with the caret range for this context
            yield token_range(caret_context.macro_loc, caret_context.macro_loc)
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
                        yield token_range(loc_range.start, end_loc)
                elif end_loc is None:
                    yield token_range(start_loc, loc_range.end)
                else:
                    yield token_range(start_loc, end_loc)

        caret_range = orig_context.source_ranges[0]
        other_ranges = orig_context.source_ranges[1:]
        contexts = []
        if not is_a_buffer_range(caret_range):
            caret_contexts = self.macro_contexts(caret_range.start)
            highlight_contexts = [self.range_contexts(source_range)
                                  for source_range in other_ranges]

            for caret_context in caret_contexts:
                # Now add an extry for each source range that intersects this context level
                source_ranges = list(intersections(caret_context, highlight_contexts))
                print(source_ranges)
                if caret_context.loc_range is None:
                    # Use the original context but replace its source ranges
                    orig_context.source_ranges = source_ranges
                    context = orig_context
                else:
                    did, substitutions = caret_context.loc_range.owner.did_and_substitutions()
                    context = DiagnosticContext(did, substitutions, source_ranges)
                contexts.append(context)

        # Use the original context but replace its source ranges
        orig_context.source_ranges[1:] = [lower_token_range(source_range)
                                          for source_range in other_ranges]
        contexts.append(orig_context)
        contexts.reverse()
        return contexts
