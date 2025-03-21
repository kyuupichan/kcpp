# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Macro expansion handling.'''

from dataclasses import dataclass
from enum import IntEnum

from ..diagnostics import DID
from .basic import Token, TokenSource, TokenKind
from .lexer import Lexer

__all__ = ['Macro', 'MacroFlags', 'ObjectLikeExpansion']


class MacroFlags(IntEnum):
    '''Flags on a Macro object.'''
    IS_FUNCTION_LIKE = 0x01
    IS_VARIADIC = 0x02
    IS_BUILTIN = 0x04
    IS_DISABLED = 0x08

    @staticmethod
    def from_param_count(count):
        '''Return the flag bits to represent the given parameter count.'''
        return count << 8


@dataclass(slots=True)
class Macro:
    '''Records the details of a macro definition.'''
    # Location of the macro name in the definition
    name_loc: int
    # See MacroFlags; additionally encodes a parameter count in the higher bits.
    flags: int
    # The replacement list of tokens.
    replacement_list: list
    # Space-separated parameter names.  Anonymous variable arguments apprear as __VA_ARGS__.
    param_names: str

    def disable(self):
        '''Mark this macro as not available for expansion.'''
        self.flags |= MacroFlags.IS_DISABLED

    def enable(self):
        '''Mark this macro as available for expansion (the default).'''
        self.flags &= ~MacroFlags.IS_DISABLED

    def is_disabled(self):
        '''Return true if the macro is disabled.'''
        return bool(self.flags & MacroFlags.IS_DISABLED)

    def is_variadic(self):
        '''Return true if the macro is variadic.'''
        return bool(self.flags & MacroFlags.IS_VARIADIC)

    def param_count(self):
        '''Return the parameter count (a variable argument is counted).'''
        return self.flags.value >> 8

    @classmethod
    def from_param_count(cls, count):
        '''Set the parameter count (including any variable argument).'''
        return count << 8


class ObjectLikeExpansion(TokenSource):
    '''A token source that returns tokens from the replacement list of an object-like
    macro.  Token concatenation via the ## operator is handled transparently.'''

    def __init__(self, pp, macro, token):
        assert token.kind == TokenKind.IDENTIFIER
        self.pp = pp
        self.macro = macro
        self.tokens = macro.replacement_list
        self.cursor = 0
        self.macro_token_flags = token.flags
        self.base_loc = pp.locator.new_macro_range(token.loc, len(self.tokens), self)

    def buffer_loc(self, loc):
        '''Return the buffer location of the token in our expansion with location loc.'''
        token_index = loc - self.base_loc
        return self.tokens[token_index].loc

    def did_and_substitutions(self):
        return DID.in_expansion_of_macro, [self.pp.token_spelling(self.macro.name_loc).decode()]

    def get_token(self, token):
        cursor = self.cursor
        tokens = self.tokens

        if cursor == len(tokens):
            self.macro.enable()
            self.pp.pop_source(token)
        else:
            token.set_to(tokens[cursor], self.base_loc + cursor)
            # Copy spacing flags of the invocation token.  Also disable the macro.
            if cursor == 0:
                token.copy_spacing_flags_from(self.macro_token_flags)
                self.macro.disable()
            cursor += 1

            if cursor != len(tokens) and tokens[cursor].kind == TokenKind.CONCAT:
                assert cursor + 1 < len(tokens)
                if self.concatenate_tokens(token, self.base_loc + cursor, tokens[cursor + 1]):
                    cursor += 2
                else:
                    cursor += 1

            self.cursor = cursor

    def concatenate_tokens(self, lhs, concat_loc, rhs):
        '''Concatenate LHS and RHS; RHS must not be modified.  Return True on success and replace
        LHS with the result, except LHS must retain its WS and BOL flags.  Return False on
        failure and leave LHS and RHS unmodified.

        If RHS is STRINGIZE, do that first.  concat_loc is the macro location of the ## or
        %:%: token.
        '''
        # FIXME: send the token; it can be faster if the spelling is attached
        lhs_spelling = self.pp.token_spelling(lhs.loc)
        rhs_spelling = self.pp.token_spelling(rhs.loc)
        spelling = lhs_spelling + rhs_spelling
        # FIXME: should not be quiet
        lexer = Lexer(self.pp, spelling, 0, quiet=True)
        token = Token.create()
        lexer.get_token(token)
        if token.kind == TokenKind.EOF or lexer.cursor != len(spelling):
            self.pp.diag(DID.token_concatenation_failed, concat_loc, [spelling])
            return False

        # Preserve the spacing flags
        token.copy_spacing_flags_from(lhs.flags)
        # Copy the token and create a location for it
        lhs.set_to(token, self.pp.locator.concatenated_token_loc(spelling, concat_loc))
        return True
