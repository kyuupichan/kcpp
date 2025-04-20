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

from ..core import Token, TokenKind, TokenFlags
from ..diagnostics import DID, Diagnostic, TokenRange, BufferRange

from .locator import ScratchEntryKind

__all__ = ['Macro', 'MacroFlags', 'MacroExpansion', 'BuiltinKind', 'expand_builtin_macro',
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
    COUNTER = auto()

    # _Pragma unary operator
    Pragma = auto()

    # Pseudo function-like macros.  The do not necessarily collect arguments like a
    # function-like macro.  However they are considered defined, and must not be used in
    # contexts other than #if and #elif.
    has_c_attribute = auto()     # __has_c_attribute
    has_cpp_attribute = auto()   # __has_cpp_attribute
    has_embed = auto()           # __has_embed
    has_include = auto()         # __has_include

    first_has_attribute = has_c_attribute

    def is_predefined(self):
        return False

    def is_builtin(self):
        return True

    def is_has_feature(self):
        return self.value >= self.first_has_attribute


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
        '''Return true if the macro is function-like.'''
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

    def collect_arguments(self, pp, invocation_loc):
        '''Return the arguments to the macro.  This is a list of lists of tokens, once list per
        parameter (including any variable argument).
        '''
        paren_depth = 0
        get_token = pp.get_token
        arguments = []
        tokens = []
        param_count = self.param_count()
        too_many = False

        # Collect the arguments.  Macro expansion is disabled whilst doing this
        pp.expand_macros = False
        with pp.collect_arguments(self):
            while True:
                token = get_token()
                if token.kind == TokenKind.EOF:
                    macro_name = self.macro_name(pp)
                    note = Diagnostic(DID.macro_defined_here, self.name_loc, [macro_name])
                    pp.diag(DID.unterminated_argument_list, invocation_loc, [macro_name, note])
                    break
                if token.kind == TokenKind.PAREN_CLOSE:
                    if paren_depth == 0:
                        break
                    paren_depth -= 1
                elif paren_depth == 0 and not too_many:
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

                if token.kind == TokenKind.PAREN_OPEN:
                    paren_depth += 1
                # Save the token and continue looking for the ')'.  Remove leading WS from the
                # first token.
                if not tokens:
                    token.flags &= ~TokenFlags.WS
                tokens.append(token)

        pp.expand_macros = True
        # The ')', or EOF, completed an argument unless there are no parameters at all.
        if param_count:
            arguments.append(tokens)
            # Do we have enough arguments?
            if len(arguments) < param_count - bool(self.is_variadic()):
                pp.diag(DID.too_few_macro_arguments, token.loc, [self.macro_name(pp)])
            # Add empty arguments until we have the correct amount
            while len(arguments) < param_count:
                arguments.append([])

        assert len(arguments) == param_count
        return arguments, TokenRange(invocation_loc, token.loc)


class MacroExpansion:
    '''A token source that returns the expansion of a macro.'''

    def __init__(self, pp, macro, invocation_loc, token_ws_flag):
        self.pp = pp
        self.macro = macro
        self.cursor = 0
        tokens = macro.replacement_list
        if macro.is_function_like():
            arguments, invocation_range = macro.collect_arguments(pp, invocation_loc)
        else:
            invocation_range = TokenRange(invocation_loc, invocation_loc)
            arguments = None
        base_loc = pp.locator.macro_replacement_span(macro, invocation_range)
        # Small optimization: having checked the arguments, expand a function-like macro
        # taking no arguments as if it were object-like
        if arguments:
            self.tokens = self.replace_arguments(tokens, arguments, base_loc, 0, len(tokens),
                                                 token_ws_flag)
        else:
            self.tokens = self.objlike_replacement_tokens(tokens, base_loc, token_ws_flag)
        macro.disable()

    def get_token(self):
        '''Handle argument replacement, stringizing and token concatenation.'''
        cursor = self.cursor
        tokens = self.tokens
        if cursor == len(tokens):
            self.macro.enable()
            token = self.pp.pop_source_and_get_token()
            token.flags |= self.trailing_ws
            return token

        token = copy(tokens[cursor])
        self.cursor = cursor + 1
        return token

    def peek_token_kind(self):
        if self.cursor == len(self.tokens):
            return TokenKind.PEEK_AGAIN
        return self.tokens[self.cursor].kind

    def concatenate_tokens(self, tokens, concat_loc, rhs):
        '''Concatenate tokens[-1] and rhs.  On success replace tokens[-1] with the result and the
        WS flag of tokens[-1], otherwise append rhs to tokens.
        concat_loc is the macro location of the ## or %:%: token.
        '''
        lhs_spelling = self.pp.token_spelling(tokens[-1])
        rhs_spelling = self.pp.token_spelling(rhs)
        spelling = lhs_spelling + rhs_spelling

        if spelling:
            parent_range = TokenRange(tokens[-1].loc, rhs.loc)
            token, all_consumed = self.pp.lex_from_scratch(spelling, parent_range,
                                                           ScratchEntryKind.concatenate)
            if token.kind == TokenKind.EOF or not all_consumed:
                diag_loc = BufferRange(token.loc, token.loc + len(spelling))
                self.pp.diag(DID.token_concatenation_failed, diag_loc, [spelling])
                # Make a copy of RHS and give it the correct location
                rhs = copy(rhs)
                rhs.loc = concat_loc + 1
                tokens.append(rhs)
            else:
                if tokens[-1].flags & TokenFlags.WS:
                    token.flags |= TokenFlags.WS
                tokens[-1] = token
        else:  # Concatenation of two placemarkers - result is the left placemarker
            assert tokens[-1].kind == rhs.kind == TokenKind.PLACEMARKER

    def objlike_replacement_tokens(self, tokens, base_loc, ws):
        '''Copy the replacement list tokens, giving them their new locations.  Whilst doing so,
        perform any concatenations present.  Return a token list ready for stepping
        through with get_token().
        '''
        # Copy the replacement list tokens giving them their new in-expansion locations.
        result = []
        cursor, limit = 0, len(tokens)
        while cursor < limit:
            token = copy(tokens[cursor])
            token.loc = base_loc + cursor
            if token.kind == TokenKind.CONCAT:
                assert cursor + 1 < limit
                self.concatenate_tokens(result, token.loc, tokens[cursor + 1])
                cursor += 2
            else:
                result.append(token)
                cursor += 1

        # The first token acquires the WS flag of the invocation token.  If there is no
        # token, record any trailing whitepsace.
        if result:
            result[0].flags &= ~TokenFlags.WS
            result[0].flags |= ws
            ws = 0
        self.trailing_ws = ws
        return result

    def replace_arguments(self, tokens, arguments, base_loc, cursor, limit, ws):
        result = []
        first = cursor
        while cursor < limit:
            token = tokens[cursor]
            new_token_loc = base_loc + cursor
            if cursor != first:
                ws |= token.flags & TokenFlags.WS

            if token.kind != TokenKind.MACRO_PARAM:
                token = copy(token)
                token.loc = new_token_loc
                # Apply whitepace appropriately
                if cursor == first:
                    token.flags &= ~TokenFlags.WS
                token.flags |= ws
                ws = 0
                result.append(token)
                cursor += 1
                continue

            # Determine the argument
            if token.extra >= 0:
                argument_tokens = arguments[token.extra]
                va_opt_count = 0
            else:  # __VA_OPT__
                assert self.macro.is_variadic()
                # Fully expand the variable argument only to see if it's empty
                va_opt_count = -token.extra   # Includes the parentheses
                va_tokens = self.expand_argument(arguments[-1])
                if va_tokens:
                    argument_tokens = self.replace_arguments(
                        tokens, arguments, base_loc, cursor + 2, cursor + va_opt_count, 0)
                else:
                    argument_tokens = [self.placemarker_token()]
            cursor += 1 + va_opt_count

            # Stringify the argument?
            if result and result[-1].kind == TokenKind.STRINGIZE:
                stringize_range = TokenRange(result[-1].loc, new_token_loc)
                token = self.stringize_argument(argument_tokens, stringize_range)
                assert not (token.flags & TokenFlags.WS)
                token.flags |= result[-1].flags & TokenFlags.WS
                result[-1] = token
                ws = 0   # Don't carry the whitespace of the parameter
            else:
                lhs_concat = result and result[-1].kind == TokenKind.CONCAT
                rhs_concat = (cursor != limit and tokens[cursor].kind == TokenKind.CONCAT)
                if lhs_concat or rhs_concat or va_opt_count:
                    if argument_tokens:
                        argument_tokens = [copy(token) for token in argument_tokens]
                    else:
                        # Empty argument in a concatenation becomes a placemarker
                        argument_tokens = [self.placemarker_token()]
                else:
                    argument_tokens = self.expand_argument(argument_tokens)
                # Set WS flag on the first token if there is one
                if argument_tokens:
                    argument_tokens[0].flags |= ws
                    ws = 0
                # Give the argument tokens their special locations
                locations = [token.loc for token in argument_tokens]
                first_loc = self.pp.locator.macro_argument_span(new_token_loc, locations)
                for loc, token in enumerate(argument_tokens, start=first_loc):
                    token.loc = loc
                result.extend(argument_tokens)

        result, result_ws = self.perform_concatenations(result, first == 0)
        if first == 0:
            self.trailing_ws = ws | result_ws
        return result

    def perform_concatenations(self, tokens, remove_placemarkers):
        '''Perform concatenations for a function-like macro expansion before the rescan
        (get_token) step.  On leaving this function no placemarker tokens should remain.
        '''
        result = []
        cursor = 0
        limit = len(tokens)
        ws = 0
        while cursor < limit:
            token = tokens[cursor]
            if token.kind == TokenKind.CONCAT:
                assert cursor + 1 < limit
                self.concatenate_tokens(result, token.loc, tokens[cursor + 1])
                cursor += 2
            else:
                if remove_placemarkers and result and result[-1].kind == TokenKind.PLACEMARKER:
                    ws |= result[-1].flags & TokenFlags.WS
                    result.pop()
                result.append(token)
                cursor += 1
            result[-1].flags |= ws
            ws = 0

        if remove_placemarkers and result and result[-1].kind == TokenKind.PLACEMARKER:
            ws |= result[-1].flags & TokenFlags.WS
            result.pop()

        return result, ws

    def expand_argument(self, argument_tokens):
        def collect_expanded_tokens(get_token):
            result = []
            while True:
                token = get_token()
                if token.kind == TokenKind.EOF:
                    return result
                result.append(token)

        self.pp.push_source(UnexpandedArgument(argument_tokens))
        tokens = collect_expanded_tokens(self.pp.get_token)
        self.pp.pop_source()
        return tokens

    def placemarker_token(self):
        return Token(TokenKind.PLACEMARKER, 0, 0, None)

    def stringize_argument(self, argument_tokens, stringize_range):
        '''Stringize the argument tokens.  Return a new token.  stringize_loc is the macro
        location of the # operator.

        Convert the argument tokens of rhs to a string and place it in the result.  If
        strinization doesn't produce a valid string, set it to an empty string.  The
        result retains the WS flag of lhs.
        '''
        spelling = bytearray()

        # Build up the spelling of the stringized argument tokens.
        spelling.append(34)    # '"'
        ws = 0
        for n, token in enumerate(argument_tokens):
            if n:
                ws |= token.flags & TokenFlags.WS
            if token.kind == TokenKind.PLACEMARKER:
                continue   # Placemarkers can only contribute to whitespace and not accumulate it
            if ws:
                spelling.append(32)
                ws = 0
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
        token, all_consumed = self.pp.lex_from_scratch(spelling, stringize_range,
                                                       ScratchEntryKind.stringize)
        return token   # kind is either STRING_LITERAL or UNTERMINATED


class UnexpandedArgument:

    def __init__(self, tokens):
        self.tokens = tokens
        self.cursor = 0

    def peek_token_kind(self):
        if self.cursor == len(self.tokens):
            return TokenKind.EOF
        return self.tokens[self.cursor].kind

    def get_token(self):
        cursor = self.cursor
        tokens = self.tokens
        if cursor == len(tokens):
            # Terminate the collect_expanded_tokens() loop.
            return Token(TokenKind.EOF, 0, 0, None)

        # Don't care about the whitespace of the first token - it is set by
        # FunctionLikeExpansion.get_token().
        self.cursor = cursor + 1
        return tokens[cursor]


def expand_builtin_macro(pp, token):
    '''Token is a built-in macro like __LINE__.  Replace token with the result of lexing
    the spelling of the macro's expansion.
    '''
    kind = token.extra.macro
    if kind == BuiltinKind.LINE or kind == BuiltinKind.FILE:
        location = pp.locator.presumed_location(token.loc, True)
        if kind == BuiltinKind.LINE:
            spelling = str(location.presumed_line_number)
        else:
            spelling = location.presumed_filename
    elif kind == BuiltinKind.TIME or kind == BuiltinKind.DATE:
        # First time?
        if pp.time_str is None:
            epoch = pp.source_date_epoch
            if epoch is None:
                epoch = datetime.now(tz=pp.tz)
            else:
                epoch = datetime.fromtimestamp(epoch, tz=pp.tz)
            months = 'Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec'.split()
            pp.time_str = f'"{epoch.hour:02d}:{epoch.minute:02d}:{epoch.second:02d}"'
            pp.date_str = f'"{months[epoch.month - 1]} {epoch.day:2d} {epoch.year:4d}"'

        if kind == BuiltinKind.TIME:
            spelling = pp.time_str
        else:
            spelling = pp.date_str
    elif kind == BuiltinKind.COUNTER:
        spelling = str(pp.counter)
        pp.counter += 1
    else:
        pp.ice(token.loc, f'unhandled builtin kind {kind}')
        return token

    return lex_token_from_builtin_spelling(pp, token, spelling, TokenRange(token.loc, token.loc))


def lex_token_from_builtin_spelling(pp, token, spelling, parent_range):
    ws = token.flags & TokenFlags.WS
    assert spelling
    token, all_consumed = pp.lex_from_scratch(spelling.encode(), parent_range,
                                              ScratchEntryKind.builtin)
    assert all_consumed
    assert not (token.flags & TokenFlags.WS)
    token.flags |= ws
    return token


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
        yield from standard_cxx_predefines(pp)
    else:
        yield from standard_c_predefines(pp)


def standard_c_predefines(pp):
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
    yield '__cplusplus', '202302L'
    yield '__STDC__', '1'
    # We do not define __STDC_VERSION__ or __STDC_ISO_10646__ when compiling C++.

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
