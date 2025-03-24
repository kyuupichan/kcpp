# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Macro expansion handling.'''

from copy import copy
from dataclasses import dataclass
from enum import IntEnum

from ..diagnostics import DID, Diagnostic
from .basic import Token, TokenKind, TokenFlags
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
                macro_name = self.macro_name(pp)
                note = Diagnostic(DID.macro_defined_here, self.name_loc, [macro_name])
                pp.diag(DID.unterminated_argument_list, name_token.loc, [macro_name, note])
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
                        arguments.append(tokens)
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
                arguments.append(tokens)
                # Do we have enough arguments?
                if len(arguments) < param_count:
                    if len(arguments) < param_count_no_variadic:
                        pp.diag(DID.too_few_macro_arguments, token.loc, [self.macro_name(pp)])
                        arguments = None
                    else:
                        # Add an empty variable argument if none was given
                        arguments.append([])
            assert arguments is None or len(arguments) == param_count

        return arguments


class SimpleTokenList:
    '''Common functionaliy for various token lists.'''

    peek_end_kind = TokenKind.PEEK_AGAIN

    def peek_token_kind(self):
        if self.cursor == len(self.tokens):
            return self.peek_end_kind
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
        self.pp = pp
        self.macro = macro
        self.parent_flags = parent_token.flags
        self.tokens = macro.replacement_list
        self.cursor = 0
        self.base_loc = pp.locator.objlike_macro_replacement_span(macro, parent_token.loc)

    def get_token(self, token):
        '''Get the next replacement list token and handle token concatenation.'''
        cursor = self.cursor
        tokens = self.tokens
        if cursor == len(tokens):
            self.pp.pop_source_and_get_token(token)
            self.macro.enable()
            return

        token.set_to(tokens[cursor], self.base_loc + cursor)
        if cursor == 0:
            # Copy spacing flags of the parent token
            token.copy_spacing_flags_from(self.parent_flags)
        cursor += 1

        if cursor != len(tokens) and tokens[cursor].kind == TokenKind.CONCAT:
            assert cursor + 1 < len(tokens)
            if self.concatenate_tokens(token, self.base_loc + cursor, tokens[cursor + 1]):
                cursor += 2
            else:
                cursor += 1
        self.cursor = cursor


class FunctionLikeExpansion(SimpleTokenList):
    '''A token source that returns tokens from the replacement list of an function-like macro.
    Token concatenation via the ## operator, and stringizing via the # operator, are
    handled by get_token().
    '''

    def __init__(self, pp, macro, parent_token, arguments):
        assert parent_token.kind == TokenKind.IDENTIFIER
        self.pp = pp
        self.macro = macro
        self.parent_flags = parent_token.flags
        self.cursor = 0
        self.tokens = self.replace_arguments(macro.replacement_list, arguments, parent_token.loc)

    def replace_arguments(self, tokens, arguments, invocation_loc):
        def check_argument(param):
            assert param.kind == TokenKind.MACRO_PARAM
            assert 0 <= param.extra < len(arguments)
            return arguments[param.extra]

        result = []
        cursor = 0
        while cursor < len(tokens):
            token = tokens[cursor]
            cursor += 1

            if token.kind == TokenKind.STRINGIZE:
                string = self.stringize_argument(check_argument(tokens[cursor]), token.loc)
                # Preserve the spacing flags of # operator
                string.copy_spacing_flags_from(token.flags)
                result.append(string)
                cursor += 1
                continue

            if token.kind == TokenKind.MACRO_PARAM:
                argument_tokens = check_argument(token)
                lhs_concat = result and result[-1].kind == TokenKind.CONCAT
                rhs_concat = (cursor != len(tokens) and tokens[cursor].kind == TokenKind.CONCAT)
                if lhs_concat or rhs_concat:
                    # Replace empty argument with a placemarker
                    if argument_tokens:
                        argument_tokens = [copy(token) for token in argument_tokens]
                    else:
                        argument_tokens = [self.placemarker_from_token(token, token.loc)]
                else:
                    argument_tokens = self.expand_argument(argument_tokens)
                # Replace the first token with a copy and set its spacing flags
                argument_tokens[0].copy_spacing_flags_from(token.flags)
                result.extend(argument_tokens)
                continue

            result.append(copy(token))

        # Give the tokens their macro-expansion locations
        locations = [token.loc for token in result]
        base_loc = self.pp.locator.functionlike_macro_replacement_span(invocation_loc, locations)
        for loc, token in enumerate(result, start=base_loc):
            token.loc = loc

        return result

    def get_token(self, token):
        '''Handle argument replacement, stringizing and token concatenation.'''
        cursor = self.cursor
        tokens = self.tokens
        if cursor == len(tokens):
            self.pp.pop_source_and_get_token(token)
            self.macro.enable()
            return

        token.set_to(tokens[cursor], tokens[cursor].loc)
        if cursor == 0:
            # Copy spacing flags of the parent token
            token.copy_spacing_flags_from(self.parent_flags)
        cursor += 1

        if cursor != len(tokens) and tokens[cursor].kind == TokenKind.CONCAT:
            assert cursor + 1 < len(tokens)
            if self.concatenate_tokens(token, token.loc, tokens[cursor + 1]):
                cursor += 2
            else:
                cursor += 1
        self.cursor = cursor

    def expand_argument(self, argument_tokens):
        def collect_expanded_tokens(get_token):
            result = []
            while True:
                token = Token.create()
                get_token(token)
                if token.kind == TokenKind.EOF:
                    return result
                result.append(token)

        self.pp.push_source(UnexpandedArgument(argument_tokens))
        tokens = collect_expanded_tokens(self.pp.get_token)
        self.pp.pop_source()
        return tokens

    def placemarker_from_token(self, token):
        # FIXME: check this
        return Token(TokenKind.PLACEMARKER, token.flags & TokenFlags.WS, token.loc, None)

    def stringize_argument(self, argument_tokens, stringize_loc):
        '''Stringize the argument tokens.  Return a new token.  stringize_loc is the macro
        location of the # operator.

        Convert the argument tokens of rhs to a string and place it in the result.  If
        strinization doesn't produce a valid string, set it to an empty string.  The
        result retains the WS flag of lhs.
        '''
        spelling = bytearray()

        # Build up the spelling of the stringized argument tokens.
        spelling.append(34)    # '"'
        for n, token in enumerate(argument_tokens):
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

        token, all_consumed = self.lex_from_scratch(spelling, stringize_loc,
                                                    ScratchEntryKind.stringize)
        assert all_consumed

        if token.kind == TokenKind.ERROR:
            self.pp.diag(DID.stringize_failed, stringize_loc)
            # Replace with an empty string literal.  FIXME: imitate GCC and Clang?
            token = Token(TokenKind.STRING_LITERAL, 0, stringize_loc, (b'""', None))

        assert token.kind == TokenKind.STRING_LITERAL
        return token


class UnexpandedArgument(SimpleTokenList):

    peek_end_kind = TokenKind.EOF

    def __init__(self, tokens):
        assert tokens
        self.tokens = tokens
        self.cursor = 0

    def get_token(self, token):
        cursor = self.cursor
        tokens = self.tokens
        if cursor == len(tokens):
            # Terminate the collect_expanded_tokens() loop.
            token.kind = TokenKind.EOF
        else:
            # Don't care about the whitespace of the first token - it is set by
            # FunctionLikeExpansion.get_token().
            token.set_to(tokens[cursor], tokens[cursor].loc)
            self.cursor = cursor + 1


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
