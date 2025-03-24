# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Basic types.  Should have no dependencies on external modules.'''


__all__ = ['Buffer', 'BufferCoords', 'BufferPosition']


from bisect import bisect_left
from dataclasses import dataclass
from enum import IntEnum, auto


def line_offsets_gen(raw):
    for n, c in enumerate(raw):
        if c == 10:  # '\n'
            yield n + 1
        elif c == 13:  # '\r'
            if n + 1 == len(raw) or raw[n + 1] != 10:
                yield n + 1


def sparse_line_offsets(raw, min_step):
    '''Return a sparse sorted list of (line_offset, line_number) pairs.  The list always
    starts with (0,1).
    '''
    result = [(0, 1)]

    beyond = min_step
    line_number = 1
    for line_number, offset in enumerate(line_offsets_gen(raw), start=2):
        if offset >= beyond:
            result.append((offset, line_number))
            beyond = offset + min_step

    return result


class Buffer:
    '''Represents a file being preprocessed.'''

    def __init__(self, text, *, name=None, sparsity=1_000):
        self.name = '<unnamed>' if name is None else name
        self.text = text
        # A sparse list of (offset, line_number) pairs to save memory
        self._sparse_line_offsets = None
        self.sparsity = sparsity

    def sparse_line_offsets(self):
        '''Calculate the sparse line offsets on demand, and cache them.'''
        if self._sparse_line_offsets is None:
            self._sparse_line_offsets = sparse_line_offsets(self.text, self.sparsity)
        return self._sparse_line_offsets

    def offset_to_line_info(self, offset):
        '''Convert an offset in the buffer to a (line_offset, line_number) pair, where line_offset
        is the start of the line.  The offset can range up to and including the buffer size.

        Line numbers are 1-based.
        '''
        if not 0 <= offset <= len(self.text):
            raise ValueError(f'offset {offset} out of range; max is {len(self.text)}')

        # Fix for wanting the position of '\n' in an '\r\n' sequence, as the '\r' would be
        # seen as ending and give a line number 1 too large.
        if self.text[offset - 1: offset + 1] == b'\r\n':
            offset -= 1

        line_offsets = self.sparse_line_offsets()

        pos = bisect_left(line_offsets, offset + 1, key=lambda pair: pair[0])
        line_offset, line_number = line_offsets[pos - 1]

        chunk = memoryview(self.text[line_offset: offset])
        chunk_offset = 0
        for chunk_offset in line_offsets_gen(chunk):
            line_number += 1

        return (line_offset + chunk_offset, line_number)

    def line_number_to_offset(self, line_number):
        '''Convert a line number to an offset in the buffer.'''
        if line_number < 1:
            raise ValueError('line number must be positive')
        line_offsets = self.sparse_line_offsets()
        pos = bisect_left(line_offsets, line_number + 1, key=lambda pair: pair[1])
        offset, src_line_number = line_offsets[pos - 1]

        if line_number == src_line_number:
            return offset

        chunk = memoryview(self.text[offset:])
        for chunk_offset in line_offsets_gen(chunk):
            src_line_number += 1
            if src_line_number == line_number:
                return offset + chunk_offset

        raise ValueError(f'buffer does not have {line_number} lines')

    def line_bytes(self, line_number):
        '''Returns a memoryview of the bytes of a raw line in the source; it does not include
        the newline, if any, at the end of the line.
        '''
        start = self.line_number_to_offset(line_number)
        text = self.text

        chunk = memoryview(text[start:])
        end = len(self.text)
        for offset in line_offsets_gen(chunk):
            end = start + offset
            break

        while end > start and text[end - 1] in (10, 13):
            end -= 1

        return memoryview(text[start:end])


class BufferPosition(IntEnum):
    '''Describes a position within a buffer.'''
    WITHIN_LINE = auto()
    END_OF_LINE = auto()
    END_OF_SOURCE = auto()


@dataclass(slots=True)
class BufferCoords:
    '''Represents a location in a buffer as a line number and column offset.'''
    # The buffer
    buffer: Buffer
    # The filename
    filename: str
    # Line number in the buffer (1-based)
    line_number: int
    # Byte offset of the location from the start of the line (0-based)
    column_offset: int
    # Byte offset of the start of the line in the buffer
    line_offset: int

    def buffer_offset(self):
        '''Convert the location as an offset in the buffer.'''
        return self.line_offset + self.column_offset

    def buffer_position(self):
        '''Return a BufferPosition describing the location in the buffer.'''
        text = self.buffer.text
        offset = self.buffer_offset()
        if offset == len(text):
            return BufferPosition.END_OF_SOURCE
        if text[offset] in {10, 13}:
            return BufferPosition.END_OF_LINE
        return BufferPosition.WITHIN_LINE
