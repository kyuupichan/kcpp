# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Macro expansion handling.'''

import itertools
from copy import copy
from dataclasses import dataclass
from datetime import datetime
from enum import IntEnum, auto

from ..diagnostics import DID, Diagnostic
from .basic import Token, TokenKind, TokenFlags
from .locator import ScratchEntryKind

__all__ = ['Macro', 'MacroFlags', 'ObjectLikeExpansion', 'FunctionLikeExpansion',
           'predefines']


class MacroFlags(IntEnum):
    '''Flags on a Macro object.'''
    IS_FUNCTION_LIKE = 0x01
    IS_VARIADIC = 0x02
    IS_PREDEFINED = 0x04
    IS_DISABLED = 0x08

    @staticmethod
    def from_param_count(count):
        '''Return the flag bits to represent the given parameter count.'''
        return count << 8


class BuiltinKind(IntEnum):
    '''The type of built-in dynamic macro.  There are only a handful.

    Predefined macros always have the same value no matter where expanded and across
    different invocations of the compiler with the same command line switches.  Built-in
    macros are dynamic - their expansion changes depending on location, or with each
    compilation.  For example, respectively, __LINE__ and __TIME__ are builtins.
    __cplusplus is not a builtin, it is a predefined macro.
    '''
    DATE = auto()
    TIME = auto()
    FILE = auto()
    LINE = auto()

    def is_predefined(self):
        return False

    def is_builtin(self):
        return True


@dataclass(slots=True)
class Macro:
    '''Records the details of a macro definition.'''
    # Location of the macro name in the definition
    name_loc: int
    # See MacroFlags; additionally encodes a parameter count in the higher bits.
    flags: int
    # The replacement list of tokens.
    replacement_list: list
    # ', '-joined parameter names.  Anonymous variable arguments appear as __VA_ARGS__.
    param_names: str

    def disable(self):
        '''Mark this macro as not available for expansion.'''
        self.flags |= MacroFlags.IS_DISABLED

    def enable(self):
        '''Mark this macro as available for expansion (the default).'''
        self.flags &= ~MacroFlags.IS_DISABLED

    def is_builtin(self):
        return False

    def is_disabled(self):
        '''Return true if the macro is disabled.'''
        return bool(self.flags & MacroFlags.IS_DISABLED)

    def is_predefined(self):
        '''Return true if the macro is variadic.'''
        return bool(self.flags & MacroFlags.IS_PREDEFINED)

    def is_variadic(self):
        '''Return true if the macro is variadic.'''
        return bool(self.flags & MacroFlags.IS_VARIADIC)

    def is_function_like(self):
        '''Return true if the macro is variadic.'''
        return bool(self.flags & MacroFlags.IS_FUNCTION_LIKE)

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

    def definition_text(self, pp):
        '''Return what appeared after the macro name (including the parameter list) in
        its definition.'''
        def parts(self, pp):
            if self.is_function_like():
                yield '('
                yield self.param_names.replace('__VA_ARGS__', '...')
                yield ')'
            for token in self.replacement_list:
                if token.flags & TokenFlags.WS:
                    yield ' '
                yield pp.token_spelling(token).decode()

        return ''.join(parts(self, pp))

    def collect_arguments(self, pp, name_token):
        paren_depth = 0
        get_token = pp.get_token
        token = Token.create()
        arguments = []
        tokens = []
        param_count = self.param_count()

        # Eat the opening parenthesis that was already peeked
        get_token(token)
        assert token.kind == TokenKind.PAREN_OPEN

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
                    if len(arguments) + 1 < param_count:
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
                    if len(arguments) < param_count - bool(self.is_variadic()):
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
        lhs with the result, except lhs must retain its WS flag.  Return False on failure
        and leave lhs and rhs unmodified.

        concat_loc is the macro location of the ## or %:%: token.
        '''
        lhs_spelling = self.pp.token_spelling(lhs)
        rhs_spelling = self.pp.token_spelling(rhs)
        spelling = lhs_spelling + rhs_spelling

        if not spelling:
            # Concatenation of two placemarkers - result is the left placemarker
            assert lhs.kind == rhs.kind == TokenKind.PLACEMARKER
            return True

        token, all_consumed = self.pp.lex_from_scratch(spelling, concat_loc,
                                                       ScratchEntryKind.concatenate)
        if token.kind == TokenKind.EOF or not all_consumed:
            self.pp.diag(DID.token_concatenation_failed, concat_loc, [spelling])
            return False
        # Do not produce concat operators through concatenation
        if token.kind == TokenKind.CONCAT:
            token.kind = TokenKind.OTHER
        if lhs.flags & TokenFlags.WS:
            token.flags |= TokenFlags.WS
        lhs.set_to(token, token.loc)
        return True


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
        self.base_loc = pp.locator.macro_replacement_span(macro, parent_token.loc)
        macro.disable()

    def get_token(self, token):
        '''Get the next replacement list token and handle token concatenation.'''
        cursor = self.cursor
        tokens = self.tokens
        if cursor == len(tokens):
            self.macro.enable()
            self.pp.pop_source_and_get_token(token)
            return

        token.set_to(tokens[cursor], self.base_loc + cursor)
        if cursor == 0:
            # Take whitespace from the invocation token
            if self.parent_flags & TokenFlags.WS:
                token.flags |= TokenFlags.WS
            else:
                token.flags &= ~TokenFlags.WS
        cursor += 1

        while cursor != len(tokens) and tokens[cursor].kind == TokenKind.CONCAT:
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
        self.cursor = 0
        self.trailing_ws = False
        tokens = macro.replacement_list
        base_loc = pp.locator.macro_replacement_span(macro, parent_token.loc)
        self.tokens = self.replace_arguments(tokens, arguments, base_loc, 0, len(tokens),
                                             parent_token.flags)
        macro.disable()

    def replace_arguments(self, tokens, arguments, base_loc, cursor, limit, parent_flags):
        def check_argument(param):
            assert param.kind == TokenKind.MACRO_PARAM
            if param.extra < 0:
                assert self.macro.is_variadic()
                # Pre-expand the variable argument to see if it's empty.
                va_tokens = self.expand_argument(arguments[-1])
                # FIXME: remove placemarkers
                if va_tokens:
                    token_count = -param.extra   # Includes the parentheses
                    va_opt_tokens = self.replace_arguments(
                        tokens, arguments, base_loc, cursor + 2, cursor + token_count,
                        param.flags)
                    return va_opt_tokens, token_count
                else:
                    return [self.placemarker_token()], -param.extra
            else:
                assert 0 <= param.extra < len(arguments)
                return arguments[param.extra], 0

        result = []
        loop_count = -1
        ws = parent_flags & TokenFlags.WS
        while cursor < limit:
            loop_count += 1
            token = tokens[cursor]
            new_token_loc = base_loc + cursor
            if loop_count and token.flags & TokenFlags.WS:
                ws = True

            if token.kind == TokenKind.STRINGIZE:
                cursor += 1
                argument_tokens, va_opt_count = check_argument(tokens[cursor])
                string = self.stringize_argument(argument_tokens, new_token_loc)
                if ws:
                    string.flags |= TokenFlags.WS
                    ws = False
                result.append(string)
                cursor += 1 + va_opt_count
                continue

            if token.kind == TokenKind.MACRO_PARAM:
                argument_tokens, token_count = check_argument(token)
                cursor += token_count + 1
                lhs_concat = result and result[-1].kind == TokenKind.CONCAT
                rhs_concat = (cursor != limit and tokens[cursor].kind == TokenKind.CONCAT)
                if lhs_concat or rhs_concat or token_count:
                    if argument_tokens:
                        argument_tokens = [copy(token) for token in argument_tokens]
                    # Replace empty argument with a placemarker
                    if not argument_tokens:
                        argument_tokens = [self.placemarker_token()]
                else:
                    argument_tokens = self.expand_argument(argument_tokens)

                # Set WS flag on the first token (if there is one)
                if argument_tokens:
                    if ws:
                        argument_tokens[0].flags |= TokenFlags.WS
                        ws = False
                    elif loop_count == 0:
                        argument_tokens[0].flags &= ~TokenFlags.WS
                # Give the tokens their macro-expansion locations
                locations = [token.loc for token in argument_tokens]
                first_loc = self.pp.locator.macro_argument_span(new_token_loc, locations)
                for loc, token in enumerate(argument_tokens, start=first_loc):
                    token.loc = loc

                result.extend(argument_tokens)
                continue

            token = copy(token)
            if ws:
                token.flags |= TokenFlags.WS
                ws = False
            elif loop_count == 0:
                token.flags &= ~TokenFlags.WS
            token.loc = new_token_loc
            result.append(token)
            cursor += 1

        result, result_ws = self.perform_concatenations(result)
        self.trailing_ws = ws or result_ws
        return result

    def perform_concatenations(self, tokens):
        result = []
        cursor = 0
        limit = len(tokens)
        ws = False
        while cursor < limit:
            token = tokens[cursor]
            if token.kind == TokenKind.CONCAT:
                assert cursor + 1 < len(tokens)
                if self.concatenate_tokens(result[-1], token.loc, tokens[cursor + 1]):
                    cursor += 2
                else:
                    cursor += 1
            else:
                result.append(token)
                cursor += 1
            if ws:
                result[-1].flags |= TokenFlags.WS
                ws = False

        return result, ws

    def get_token(self, token):
        '''Handle argument replacement, stringizing and token concatenation.'''
        cursor = self.cursor
        tokens = self.tokens
        if cursor == len(tokens):
            self.macro.enable()
            self.pp.pop_source_and_get_token(token)
            if self.trailing_ws:
                token.flags |= TokenFlags.WS
            return

        token.set_to(tokens[cursor], tokens[cursor].loc)
        self.cursor = cursor + 1

        # Don't let placemarker tokens leak out
        if token.kind == TokenKind.PLACEMARKER:
            ws = bool(token.flags & TokenFlags.WS)
            self.get_token(token)
            if ws:
                token.flags |= TokenFlags.WS

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

    def placemarker_token(self):
        return Token(TokenKind.PLACEMARKER, 0, 0, None)

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
        ws = False
        for n, token in enumerate(argument_tokens):
            if n and token.flags & TokenFlags.WS:
                ws = True
            if token.kind == TokenKind.PLACEMARKER:
                continue   # Placemarkers can only contribute to whitespace and not accumulate it
            if ws:
                spelling.append(32)
                ws = False
            token_spelling = self.pp.token_spelling(token)
            if token.kind == TokenKind.STRING_LITERAL or token.kind == TokenKind.CHARACTER_LITERAL:
                for c in token_spelling:
                    # Raw strings can have embedded newlines
                    if c == 10:
                        spelling.extend(b'\\n')
                        continue
                    if c == 34 or c == 92:   # '"' '\\'
                        spelling.append(92)
                    spelling.append(c)
            else:
                spelling.extend(token_spelling)
        spelling.append(34)    # '"'
        token, all_consumed = self.pp.lex_from_scratch(spelling, stringize_loc,
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


class BuiltinMacroExpansion(SimpleTokenList):

    def __init__(self, pp, parent_loc, kind):
        self.pp = pp
        self.parent_loc = parent_loc
        self.kind = kind

    def spelling(self):
        if self.kind == BuiltinKind.LINE or self.kind == BuiltinKind.FILE:
            location = self.pp.locator.presumed_location(self.parent_loc, True)
            if self.kind == BuiltinKind.LINE:
                return str(location.presumed_line_number)
            return location.presumed_filename

        if self.kind == BuiltinKind.TIME or self.kind == BuiltinKind.DATE:
            if self.pp.time_str is None:
                months = 'Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec'.split()
                today = datetime.today()
                self.pp.time_str = f'"{today.hour:02d}:{today.minute:02d}:{today.second:02d}"'
                self.pp.date_str = f'"{months[today.month - 1]} {today.day:2d} {today.year:4d}"'
            if self.kind == BuiltinKind.TIME:
                return self.pp.time_str
            return self.pp.date_str

        assert False

    def get_token(self, token):
        spelling = self.spelling()
        btoken, all_consumed = self.pp.lex_from_scratch(spelling.encode(), self.parent_loc,
                                                        ScratchEntryKind.builtin)
        assert all_consumed
        token.set_to(btoken, btoken.loc)
        self.pp.pop_source()


def predefines(pp):
    '''Return the predefined macros as a text file.'''
    all_macros = itertools.chain(
        toolchain_predefines(pp),
        standard_predefines(pp),
        target_predefines(pp),
    )
    return '\n'.join(f'#define {name} {expansion}' for name, expansion in all_macros)


def toolchain_predefines(pp):
    '''Yield the toolchain / implementation predefined macros as (name, expansion) pairs.'''
    from kcpp import _version, _version_str
    major, minor = _version[:2]

    # The predefines identifying the tool
    yield '__kcpp__', '1'
    yield '__kcpp_major__', str(major)
    yield '__kcpp_minor__', str(minor)
    yield '__kcpp_version__', f'kcpp {_version_str}'

    if pp.language.is_cxx():
        # The predefines expressing what the implementation supports.  In reality these
        # are a function of the tool, the library and perhaps the target machine as a
        # whole.
        yield '__STDCPP_BFLOAT16_T__', '1'
        yield '__STDCPP_FLOAT128_T__', '1'
        yield '__STDCPP_FLOAT16_T__', '1'
        yield '__STDCPP_FLOAT32_T__', '1'
        yield '__STDCPP_FLOAT64_T__', '1'


def standard_predefines(pp):
    '''Yield the predefined macros required by the user's selected standard as (name,
    expansion) pairs.
    '''
    if pp.language.is_cxx():
        standard_cxx_predefines(pp)

    yield '__STDC__', '1'

    # The values for C23
    yield '__STDC_EMBED_NOT_FOUND__', '0'
    yield '__STDC_EMBED_FOUND__', '1'
    yield '__STDC_EMBED_EMPTY__', '2'
    yield '__STDC_UTF_16__', '1'
    yield '__STDC_UTF_32__', '1'
    yield '__STDC_VERSION__', '202311L'
    # FIXME: __STDC_ISO_10646__


def standard_cxx_predefines(pp):
    # We do not define __STDC__ or __STDC_VERSION__ or __STDC_ISO_10646__ when compiling
    # C++.
    yield '__cplusplus', '202302L'

    # The values for C++23
    yield '__cpp_aggregate_bases', '201603L'
    yield '__cpp_aggregate_nsdmi', '201304L'
    yield '__cpp_aggregate_paren_init', '201902L'
    yield '__cpp_alias_templates', '200704L'
    yield '__cpp_aligned_new', '201606L'
    yield '__cpp_attributes', '200809L'
    yield '__cpp_auto_cast', '202110L'
    yield '__cpp_binary_literals', '201304L'
    yield '__cpp_capture_star_this', '201603L'
    yield '__cpp_char8_t', '202207L'
    yield '__cpp_concepts', '202002L'
    yield '__cpp_conditional_explicit', '201806L'
    yield '__cpp_constexpr', '202306L'
    yield '__cpp_constexpr_dynamic_alloc', '201907L'
    yield '__cpp_constexpr_in_decltype', '201711L'
    yield '__cpp_consteval', '202211L'
    yield '__cpp_constinit', '201907L'
    yield '__cpp_decltype', '200707L'
    yield '__cpp_decltype_auto', '201304L'
    yield '__cpp_deduction_guides', '201907L'
    yield '__cpp_delegating_constructors', '200604L'
    yield '__cpp_designated_initializers', '201707L'
    yield '__cpp_enumerator_attributes', '201411L'
    yield '__cpp_explicit_this_parameter', '202110L'
    yield '__cpp_fold_expressions', '201603L'
    yield '__cpp_generic_lambdas', '201707L'
    yield '__cpp_guaranteed_copy_elision', '201606L'
    yield '__cpp_hex_float', '201603L'
    yield '__cpp_if_consteval', '202106L'
    yield '__cpp_if_constexpr', '201606L'
    yield '__cpp_impl_coroutine', '201902L'
    yield '__cpp_impl_destroying_delete', '201806L'
    yield '__cpp_impl_three_way_comparison', '201907L'
    yield '__cpp_implicit_move', '202207L'
    yield '__cpp_inheriting_constructors', '201511L'
    yield '__cpp_init_captures', '201803L'
    yield '__cpp_initializer_lists', '200806L'
    yield '__cpp_inline_variables', '201606L'
    yield '__cpp_lambdas', '200907L'
    yield '__cpp_modules', '201907L'
    yield '__cpp_multidimensional_subscript', '202211L'
    yield '__cpp_named_character_escapes', '202207L'
    yield '__cpp_namespace_attributes', '201411L'
    yield '__cpp_noexcept_function_type', '201510L'
    yield '__cpp_nontype_template_args', '201911L'
    yield '__cpp_nontype_template_parameter_auto', '201606L'
    yield '__cpp_nsdmi', '200809L'
    yield '__cpp_pack_indexing', '202311L'
    yield '__cpp_placeholder_variables', '202306L'
    yield '__cpp_range_based_for', '202211L'
    yield '__cpp_raw_strings', '200710L'
    yield '__cpp_ref_qualifiers', '200710L'
    yield '__cpp_return_type_deduction', '201304L'
    yield '__cpp_rvalue_references', '200610L'
    yield '__cpp_size_t_suffix', '202011L'
    yield '__cpp_sized_deallocation', '201309L'
    yield '__cpp_static_assert', '202306L'
    yield '__cpp_static_call_operator', '202207L'
    yield '__cpp_structured_bindings', '201606L'
    yield '__cpp_template_template_args', '201611L'
    yield '__cpp_threadsafe_static_init', '200806L'
    yield '__cpp_unicode_characters', '200704L'
    yield '__cpp_unicode_literals', '200710L'
    yield '__cpp_user_defined_literals', '200809L'
    yield '__cpp_using_enum', '201907L'
    yield '__cpp_variable_templates', '201304L'
    yield '__cpp_variadic_templates', '200704L'
    yield '__cpp_variadic_using', '201611L'


def target_predefines(pp):
    '''Yield the target predefined macros as (name, expansion) pairs.'''
    if pp.language.is_cxx():
        yield '__STDCPP_DEFAULT_NEW_ALIGNMENT__', '16UZ'
        yield '__STDCPP_THREADS__', '1'

    yield '__STDC_HOSTED__', '1'

    # FIXME: __STDC_MB_MIGHT_NEQ_WC__
    # FIXME: __STDC_NO_THREADS__ etc
