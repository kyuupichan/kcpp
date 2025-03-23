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
from .locator import ScratchEntryKind

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

    @classmethod
    def from_tokens(cls, tokens):
        return cls(tokens, None if tokens else [])


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
        return self.flags >> 8

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

            if token.kind == TokenKind.EOF:
                pp.diag(DID.unterminated_argument_list, token.loc, [self.macro_name(pp)])
                arguments = None
                break
            if token.kind == TokenKind.PAREN_CLOSE:
                if paren_depth == 0:
                    break
                paren_depth -= 1
            elif paren_depth == 0 and arguments is not None:
                too_many = False
                if token.kind == TokenKind.COMMA:
                    # This completes an argument unless we are in the variable arguments.
                    if len(arguments) + 1 < param_count_no_variadic:
                        arguments.append(MacroArgument.from_tokens(tokens))
                        tokens = []
                        continue
                    # It is impossible to have too many arguments to a variadic macro; the
                    # comma just becomes part of the current variable argument.
                    too_many = not self.is_variadic()
                else:
                    too_many = param_count == 0
                if too_many:
                    pp.diag(DID.too_many_macro_arguments, token.loc, [self.macro_name(pp)])
                    arguments = None

            if token.kind == TokenKind.PAREN_OPEN:
                paren_depth += 1

            # Save the token and continue looking for the ')'.
            tokens.append(copy(token))

        if arguments is not None:
            # The ')' completed an argument unless there are no parameters at all.
            if token.kind == TokenKind.PAREN_CLOSE and param_count:
                arguments.append(MacroArgument.from_tokens(tokens))
                # Do we have enough arguments?
                if len(arguments) < param_count:
                    if len(arguments) < param_count_no_variadic:
                        pp.diag(DID.too_few_macro_arguments, token.loc, [self.macro_name(pp)])
                        arguments = None
                    else:
                        # Add an empty variable argument if none was given
                        arguments.append(MacroArgument.from_tokens([]))
            assert arguments is None or len(arguments) == param_count

        return arguments


class SimpleTokenList(TokenSource):
    '''A token source that returns tokens from a list, replacing their location with
    an offset from a base.'''

    def __init__(self, pp, parent_token, tokens, creator):
        self.pp = pp
        self.parent_flags = parent_token.flags
        self.tokens = tokens
        self.cursor = 0
        self.base_loc = pp.locator.new_macro_range(parent_token.loc, len(tokens), creator)

    def get_token(self, token):
        cursor = self.cursor
        tokens = self.tokens
        if cursor == len(tokens):
            self.pp.pop_source_and_get_token(token)
            self.on_popped()
        else:
            token.set_to(tokens[cursor], self.base_loc + cursor)
            if cursor == 0:
                # Copy spacing flags of the parent token
                token.copy_spacing_flags_from(self.parent_flags)
            self.cursor = cursor + 1
            self.on_token(token)

    def on_popped(self):
        pass

    def peek_token_kind(self):
        if self.cursor == len(self.tokens):
            return TokenKind.PEEK_AGAIN
        return self.tokens[self.cursor].kind

    def concatenate_tokens(self, lhs, concat_loc, rhs):
        '''Concatenate lhs and rhs; rhs must not be modified.  Return True on success and replace
        lhs with the result, except lhs must retain its WS and BOL flags.  Return False on
        failure and leave lhs and rhs unmodified.

        concat_loc is the macro location of the ## or %:%: token.
        '''
        lhs_spelling = self.pp.token_spelling(lhs)
        rhs_spelling = self.pp.token_spelling(rhs)
        spelling = lhs_spelling + rhs_spelling

        token, all_consumed = self.lex_from_scratch(spelling, concat_loc,
                                                    ScratchEntryKind.concatenate)
        if token.kind == TokenKind.EOF or not all_consumed:
            self.pp.diag(DID.token_concatenation_failed, concat_loc, [spelling])
            return False
        # Preserve the spacing flags and copy the token with its new macro location
        token.copy_spacing_flags_from(lhs.flags)
        lhs.set_to(token, token.loc)
        return True

    def lex_from_scratch(self, spelling, operator_loc, kind):
        '''Place the spelling in a scratch buffer and return a pair (token, all_consumed).
        all_consumed is True if lexing consumed the whole spelling.'''
        # Get a scratch buffer location for the new token
        scratch_loc = self.pp.locator.new_scratch_token(spelling, operator_loc, kind)
        lexer = Lexer(self.pp, spelling, scratch_loc)
        token = Token.create()
        dfilter = DiagnosticFilter()
        dfilter.prior = self.pp.set_diagnostic_consumer(dfilter)
        lexer.get_token(token)
        # Remove the fake BOL flag
        token.flags &= ~TokenFlags.BOL
        self.pp.set_diagnostic_consumer(dfilter.prior)
        return token, lexer.cursor == len(spelling)


class ObjectLikeExpansion(SimpleTokenList):
    '''A token source that returns tokens from the replacement list of an object-like macro.
    Token concatenation via the ## operator is handled by get_token().
    '''

    def __init__(self, pp, macro, parent_token):
        assert parent_token.kind == TokenKind.IDENTIFIER
        super().__init__(pp, parent_token, macro.replacement_list, macro)
        self.macro = macro

    def on_popped(self):
        self.macro.enable()

    def on_token(self, token):
        '''Handle token concatenation.'''
        cursor = self.cursor
        tokens = self.tokens
        if cursor != len(tokens) and tokens[cursor].kind == TokenKind.CONCAT:
            assert cursor + 1 < len(tokens)
            if self.concatenate_tokens(token, self.base_loc + cursor, tokens[cursor + 1]):
                cursor += 2
            else:
                cursor += 1
            self.cursor = cursor


class FunctionLikeExpansion(ObjectLikeExpansion):
    '''A token source that returns tokens from the replacement list of an function-like macro.
    Token concatenation via the ## operator, and stringizing via the # operator, are
    handled by get_token().
    '''

    def __init__(self, pp, macro, parent_token, arguments):
        super().__init__(pp, macro, parent_token)
        self.arguments = arguments

    def on_token(self, token):
        '''Handle argument replacement, stringizing and token concatenation.'''
        cursor = self.cursor
        tokens = self.tokens

        if token.kind == TokenKind.STRINGIZE:
            self.stringize_argument(token, self.tokens[cursor])
            cursor += 1

        next_is_concat = (cursor != len(tokens) and tokens[cursor].kind == TokenKind.CONCAT)
        if token.kind == TokenKind.MACRO_PARAM:
            if next_is_concat:
                # The unexpanded argument handles the concatenation.
                self.push_unexpanded_argument(token, UnexpandedArgument.left_of_concat)
            else:
                self.push_expanded_argment(token)
            # Set cursor now before getting the next token - it might be us
            self.cursor = cursor
            self.pp.get_token(token)
        elif next_is_concat:
            assert cursor + 1 < len(tokens)
            if self.concatenate_tokens(token, self.base_loc + cursor, tokens[cursor + 1]):
                self.cursor = cursor + 2
            else:
                self.cursor = cursor + 1
        else:
            self.cursor = cursor

    def push_expanded_argument(self, param_token):
        def collect_expanded_tokens(get_token):
            result = []
            while True:
                token = Token.create()
                get_token(token)
                if token.kind == TokenKind.EOF:
                    return result
                result.append(token)

        argument = self.check_argument(param_token)
        tokens = argument.expanded_tokens
        if tokens is None:
            self.push_unexpanded_argument(self, param_token, UnexpandedArgument.pre_expansion)
            tokens = argument.expanded_tokens = collect_expanded_tokens(self.pp.get_token)
            self.pp.pop_source()
        # Only push a source if it's non-empty
        if tokens:
            self.pp.push_source(ExpandedArgument(self.pp, param_token, tokens))

    def push_unexpanded_argument(self, param_token, reason):
        tokens = self.check_argument(param_token).tokens
        if not tokens:
            assert tokens is not None   # MacroArgument.from_tokens() set it to []
            # Create a placemarker token
            tokens = [self.placemarker_from_token(param_token)]
        self.pp.push_source(UnexpandedArgument(self.pp, param_token, tokens, reason))

    def placemarker_from_token(self, token):
        return Token(TokenKind.PLACEMARKER, token.flags & TokenFlags.WS, token.loc, None)

    def check_argument(self, param):
        assert param.kind == TokenKind.MACRO_PARAM
        assert 0 <= param.extra < len(self.arguments)
        return self.arguments[param.extra]

    def stringize_argument(self, lhs, rhs):
        '''lhs is a copy of the # operator.  rhs is the macro parameter token and must not be
        modified.

        Convert the argument tokens of rhs to a string and place it in lhs.  If
        strinization doesn't produce a valid string, set it to an empty string.
        lhs retains its WS and BOL flags.
        '''
        spelling = bytearray()

        # Build up the spelling of the stringized argument tokens.
        spelling.append(34)    # '"'
        for n, token in enumerate(self.check_argument(lhs).tokens):
            if n and token.flags & TokenFlags.WS:
                spelling.append(32)
            token_spelling = self.pp.token_spelling(token)
            if token.kind == TokenKind.STRING_LITERAL or token.kind == TokenKind.CHARACTER_LITERAL:
                for c in token_spelling:
                    if c == 34 or c == 92:   # '"' '\\'
                        spelling.append(92)
                    spelling.append(c)
            else:
                spelling.extend(token_spelling)
        spelling.append(34)    # '"'

        stringize_loc = lhs.loc
        token, all_consumed = self.lex_from_scratch(spelling, stringize_loc,
                                                    ScratchEntryKind.stringize)
        assert all_consumed

        if token.kind == TokenKind.ERROR:
            self.pp.diag(DID.stringize_failed, stringize_loc)
            # Replace with an empty string literal.  FIXME: imitate GCC and Clang?
            token = Token(TokenKind.STRING_LITERAL, 0, stringize_loc, (b'""', None))

        assert token.kind == TokenKind.STRING_LITERAL
        # Preserve the spacing flags and copy the token with its new macro location
        token.copy_spacing_flags_from(lhs.flags)
        lhs.set_to(token, token.loc)


class UnexpandedArgument(TokenSource):

    pre_expansion = 0
    left_of_concat = 1
    right_of_concat = 2

    def __init__(self, pp, param_token, tokens, reason):
        assert tokens
        self.pp = pp
        self.param_token_flags = param_token.flags
        self.tokens = tokens
        self.reason = reason
        self.cursor = 0

    def get_token(self, token):
        tokens = self.tokens
        cursor = self.cursor

        if self.reason == self.pre_expansion:
            if cursor == len(tokens):
                # Terminate the collect_expanded_tokens() loop.
                token.kind = TokenKind.EOF
            else:
                # Whitespace of first token is irrelevant;
                # ExpandedArgument.get_token() deals with it.
                token.set_to(tokens[cursor], token.loc)
                self.cursor = cursor + 1
        else:
            if cursor == len(tokens) - 1 and self.reason == self.left_of_concat:
                pass
            elif cursor == 0 and self.reason == self.right_of_concat:
                pass
            else:
                token.copy_spacing_flags_from(self.param_token_flags)
                token.set_to(tokens[cursor], token.loc)
                self.cursor = cursor + 1

    def peek_token_kind(self):
        if self.cursor == len(self.tokens):
            if self.reason == self.pre_expansion:
                return TokenKind.EOF
            return TokenKind.PEEK_AGAIN
        return self.tokens[self.cursor].kind


class ExpandedArgument(SimpleTokenList):

    def __init__(self, pp, parent_token, tokens):
        assert parent_token.kind == TokenKind.MACRO_PARAM
        super().__init__(pp, parent_token, tokens, self)


class DiagnosticFilter:
    '''A simple diagnostic consumer that simply collects the emitted diagnostics.'''

    def __init__(self):
        self.prior = None

    def emit(self, diagnostic):
        # These are diagnosed as invalid concatenations
        if diagnostic.did in (DID.unterminated_block_comment, DID.incomplete_UCN_as_tokens,
                              DID.unterminated_literal):
            pass
        else:
            self.prior.emit(diagnostic)
