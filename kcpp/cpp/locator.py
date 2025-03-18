# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The locator handles the details of token locations - whether they come from source files,
and if so which one, the include stack at the time of that location, the macro stack at the
time of expansion, etc.'''

from bisect import bisect_right
from dataclasses import dataclass
from enum import IntEnum, auto

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
        n = bisect_right(loc_ranges, loc + 1, key=lambda bl: bl.first) - 1
        loc_range = loc_ranges[n]
        assert loc - loc_range.first < loc_range.size
        return loc_range

    def loc_to_buffer_and_offset(self, loc):
        '''Return a triple (buffer, buffer_start_loc, offset).'''
        loc_range = self.lookup_range(loc)
        if loc_range.kind == LocationRangeKind.macro:
            loc = loc_range.extra.spelling_loc(loc - loc_range.first)
            loc_range = self.lookup_range(loc)
        assert loc_range.kind == LocationRangeKind.buffer
        return loc_range.extra, loc - loc_range.first

    def loc_to_buffer_coords(self, loc):
        buffer, offset = self.loc_to_buffer_and_offset(loc)
        return buffer.offset_to_coords(offset)
