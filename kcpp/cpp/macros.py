# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Macro expansion handling.'''

from copy import copy
from dataclasses import dataclass
from enum import IntEnum

from ..diagnostics import DID
from .basic import Token, TokenSource, TokenKind, TokenFlags
from .lexer import Lexer

__all__ = ['Macro', 'MacroFlags', 'ObjectLikeExpansion', 'FunctionLikeExpansion']


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
class MacroArgument:
    # A list.  The first entry is the list of tokens forming the first argument, the
    # second entry is the list of tokens forming the second argument, etc.
    tokens: list
    # cache of expanded macro arguments, to save expanding arguments twice or more.
    # Initially None (to distinguish from empty); filled on-demand.
    expanded_tokens: object


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

    def buffer_loc(self, token_index):
        '''Return the location of the nth token in our replacement list.'''
        return self.replacement_list[token_index].loc

    def macro_name(self, pp):
        '''Return the macro name (as UTF-8).'''
        return pp.token_spelling_at_loc(self.name_loc)

    def collect_arguments(self, pp, name_token):
        paren_depth = 0
        get_token = pp.get_token
        token = Token.create()
        arguments = []
        tokens = []

        param_count = self.param_count()
        param_count_no_variadic = param_count - bool(self.is_variadic())

        # Eat the opening parenthesis that was already peeked
        get_token(token)
        assert token.kind == TokenKind.PAREN_OPEN

        # FIXME: handle newlines as whitespace
        while True:
            get_token(token)
            if paren_depth == 0:
                if token.kind == TokenKind.PAREN_CLOSE:
                    # This forms another argument.
                    arguments.append(MacroArgument(tokens, None))
                    # Check we have enough
                    if len(arguments) < param_count_no_variadic:
                        pp.diag(DID.too_few_macro_arguments, token.loc, [self.macro_name(pp)])
                        arguments = None
                        break
                    # Too many arguments have already been diagnosed at the comma that
                    # implicitly creates a new argument.  However we have to cover the
                    # no-comma case of a non-empty argument to a macro taking none.
                    if param_count_no_variadic == 0:
                        if self.is_variadic():
                            # This is fine
                            pass
                        elif tokens:
                            pp.diag(DID.too_many_macro_arguments, token.loc,
                                    [self.macro_name(pp)])
                            arguments = None
                        else:
                            arguments.pop()
                    break
                elif token.kind == TokenKind.COMMA:
                    # Does this finish an argument?  If so, save it and move on to the
                    # next one.
                    if len(arguments) + 1 < self.param_count:
                        arguments.append(MacroArgument(tokens, None))
                        tokens = []
                        continue
                    # It is impossible to have too many arguments to a variadic macro; the
                    # comma just becomes part of the current argument
                    if not self.is_variadic():
                        pp.diag(DID.too_many_macro_arguments, token.loc,
                                [self.macro_name(pp)])
                        arguments = None
                        break

            if token.kind == TokenKind.EOF:
                pp.diag(DID.unterminated_argument_list, token.loc, [self.macro_name(pp)])
                arguments = None
                break

            tokens.append(copy(token))
            if token.kind == TokenKind.PAREN_OPEN:
                paren_depth += 1
            elif token.kind == TokenKind.PAREN_CLOSE:
                paren_depth -= 1

        return arguments


class MacroExpansionBase(TokenSource):
    '''A token source that returns tokens from the replacement list of a macro.  Token
    concatenation via the ## operator is handled transparently.
    '''

    def __init__(self, pp, macro, token):
        assert token.kind == TokenKind.IDENTIFIER
        self.pp = pp
        self.macro = macro
        self.tokens = macro.replacement_list
        self.cursor = 0
        self.macro_token_flags = token.flags
        self.base_loc = pp.locator.new_macro_range(token.loc, len(self.tokens), macro)

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

    def peek_token_kind(self):
        if self.cursor == len(self.tokens):
            return TokenKind.PEEK_AGAIN
        return self.tokens[self.cursor].kind

    def concatenate_tokens(self, lhs, concat_loc, rhs):
        '''Concatenate LHS and RHS; RHS must not be modified.  Return True on success and replace
        LHS with the result, except LHS must retain its WS and BOL flags.  Return False on
        failure and leave LHS and RHS unmodified.

        If RHS is STRINGIZE, do that first.  concat_loc is the macro location of the ## or
        %:%: token.
        '''
        lhs_spelling = self.pp.token_spelling(lhs)
        rhs_spelling = self.pp.token_spelling(rhs)
        spelling = lhs_spelling + rhs_spelling

        # Get a scratch buffer location for the concatenated token
        scratch_loc = self.pp.locator.new_scratch_token(spelling, concat_loc)
        lexer = Lexer(self.pp, spelling, scratch_loc)

        token = Token.create()
        dfilter = DiagnosticFilter()
        dfilter.prior = self.pp.set_diagnostic_consumer(dfilter)
        lexer.get_token(token)
        # Remove the fake BOL flag
        token.flags &= ~TokenFlags.BOL
        self.pp.set_diagnostic_consumer(dfilter.prior)

        if token.kind == TokenKind.EOF or lexer.cursor != len(spelling):
            self.pp.diag(DID.token_concatenation_failed, concat_loc, [spelling])
            return False
        # Preserve the spacing flags and copy the token with its new macro location
        token.copy_spacing_flags_from(lhs.flags)
        lhs.set_to(token, token.loc)
        return True


class ObjectLikeExpansion(MacroExpansionBase):
    '''A token source that returns tokens from the replacement list of an object-like macro.
    Token concatenation via the ## operator is handled transparently.
    '''


class FunctionLikeExpansion(TokenSource):
    '''A token source that returns tokens from the replacement list of an function-like macro.
    Token concatenation via the ## operator, and stringizing via the # operator, are
    handled transparently.
    '''

    def __init__(self, pp, macro, token, arguments):
        super().__init__(pp, macro, token)
        self.arguments = arguments

    def get_token(self, token):
        super().get_token(token)


class DiagnosticFilter:
    '''A simple diagnostic consumer that simply collects the emitted diagnostics.'''

    def __init__(self):
        self.prior = None

    def emit(self, diagnostic):
        # These are diagnosed as invalid concatenations
        if diagnostic.did in (DID.unterminated_block_comment, DID.incomplete_UCN_as_tokens):
            pass
        else:
            self.prior.emit(diagnostic)
