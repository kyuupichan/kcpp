# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Target abstraction.'''

from dataclasses import dataclass

from ..unicode import Charset

from .types import IntegerKind

__all__ = ['Target']


@dataclass(slots=True)
class Target:
    '''Specification of a target machine.  Determines how numeric and character literals
    are interpreted.'''
    # If integers are stored little-endian
    is_little_endian: bool

    char_width: int
    short_width: int
    int_width: int
    long_width: int
    long_long_width: int

    char_kind: IntegerKind
    size_t_kind: IntegerKind
    wchar_t_kind: IntegerKind
    char16_t_kind: IntegerKind
    char32_t_kind: IntegerKind

    narrow_charset: Charset
    wide_charset: Charset

    def pp_arithmetic_width(self):
        return self.long_long_width

    def underlying_kind(self, kind):
        if kind == IntegerKind.char:
            return self.char_kind
        if kind == IntegerKind.char8_t:
            return IntegerKind.uchar
        if kind == IntegerKind.wchar_t:
            return self.wchar_t_kind
        if kind == IntegerKind.char16_t:
            return self.char16_t_kind
        if kind == IntegerKind.char32_t:
            return self.char32_t_kind
        return kind

    def is_unsigned(self, kind):
        ukind = self.underlying_kind(kind)
        if ukind in (IntegerKind.schar, IntegerKind.short, IntegerKind.int, IntegerKind.long,
                     IntegerKind.long_long):
            return False
        if ukind in (IntegerKind.uchar, IntegerKind.ushort, IntegerKind.uint, IntegerKind.ulong,
                     IntegerKind.ulong_long):
            return True
        raise RuntimeError(f'kind {kind} not handled in is_signed()')

    def integer_width(self, kind):
        kind = self.underlying_kind(kind)
        if kind in (IntegerKind.schar, IntegerKind.uchar):
            return self.char_width
        if kind in (IntegerKind.short, IntegerKind.ushort):
            return self.short_width
        if kind in (IntegerKind.int, IntegerKind.uint):
            return self.int_width
        if kind in (IntegerKind.long, IntegerKind.ulong):
            return self.long_width
        if kind in (IntegerKind.long_long, IntegerKind.ulong_long):
            return self.long_long_width
        raise RuntimeError(f'kind {kind} not handled in is_signed()')
