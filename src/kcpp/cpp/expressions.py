# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''A fast preprocessor expression parser and evaluator with high-quality error recovery
and diagnostics.
'''

from dataclasses import dataclass
from enum import IntEnum, auto

from ..core import TokenKind, IntegerKind
from ..diagnostics import DID, TokenRange
from ..parsing import ParserState

from .literals import LiteralInterpreter
from .macros import BuiltinKind, lex_token_from_builtin_spelling

__all__ = ['ExprParser']


@dataclass(slots=True)
class ExprValue:
    '''Represents the value of an expression.  It is held as a positive number representing
    the bit-pattern on a 2's complement machine.
    '''
    value: int
    is_unsigned: bool
    is_erroneous: bool
    loc: TokenRange

    def get(self, mask):
        '''Return the value as a Python int.'''
        if self.is_unsigned or self.value <= (mask >> 1):
            return self.value
        return self.value - mask - 1

    def set(self, value, mask):
        '''Store the value from a Python int.  Returns True on overflow.'''
        if value < 0:
            value = -value - 1
            self.value = mask - (value & mask)
        else:
            self.value = value & mask
        return not self.is_unsigned and value > (mask >> 1)

    def set_boolean(self, value):
        self.value = int(value)
        self.is_unsigned = False


class BOP(IntEnum):
    '''Binary operator precedence.'''
    minimal = auto()            # Anything else.  Must be less than comma.
    comma = auto()              # ,
    invalid = auto()            # Invalid operators; maybe they could use conditional.
    conditional = auto()        # ? of ternary operator
    logical_or = auto()         # ||
    logical_and = auto()        # &&
    bitwise_or = auto()         # |
    bitwise_xor = auto()        # ^
    bitwise_and = auto()        # &
    equality = auto()           # == !=
    relational = auto()         # < > <= >=
    shift = auto()              # << >>
    additive = auto()           # + -
    multiplicative = auto()     # / % *


class ExprParser:
    '''A modified form of recursive descent based on operator precedence, which I believe is
    the best approach to the relatively simple expressions a preprocessor must accept.
    This parser is stateless and so can be re-used.
    '''
    def __init__(self, pp):
        '''Initialize an expression parser with a preprocessor object.'''
        self.pp = pp
        self.defined = pp.get_identifier(b'defined')
        self.false = pp.get_identifier(b'false')
        self.true = pp.get_identifier(b'true')
        self.width = pp.target.pp_arithmetic_width()
        self.mask = (1 << self.width) - 1
        self.literal_interpreter = LiteralInterpreter(pp, True)
        # Pass diagnostics on to the preprocessor.
        self.diag = pp.diag

    def parse_and_evaluate_constant_expr(self):
        '''The external interface - parse and evaluate a preprocessor expression.  Return a
        (value, token) pair.  The value is an ExprValue instance and token is the
        lookahead token.
        '''
        # As per the grammar, comma expressions are not acceptable at the top level.
        state = ParserState.from_pp(self.pp)
        return self.parse_conditional_expr(state, True), state.token

    def parse_expr(self, state, is_evaluated):
        '''Parse and evaluate an arbitrary expression (including comma expressions).'''
        return self.parse_binary_expr(state, BOP.minimal, is_evaluated)

    def parse_conditional_expr(self, state, is_evaluated):
        '''Parse and evaluate a conditional expression as per the grammar.'''
        return self.parse_binary_expr(state, BOP.comma, is_evaluated)

    def parse_binary_expr(self, state, reduce_precedence, is_evaluated):
        '''Parse and evaluate left-associative binary expressions using operator precedence, with
        a special case to get the branches of the conditional operator correct.
        '''
        # Start by parsing a unary expression.  Then, if the next binary operator in the
        # token sequence is of precedence less than or equal to reduce_precedence, then
        # perform (in LR parsing terminology) a reduction operation, otherwise perform a
        # shift operation.
        lhs = self.parse_unary_expr(state, is_evaluated)
        while True:
            token = state.get_token()
            precedence, evaluator = binary_ops.get(token.kind, (BOP.minimal, None))
            if precedence <= reduce_precedence:
                state.save_token(token)
                return lhs

            if precedence == BOP.conditional:
                lhs = self.parse_conditional_branches(state, token, lhs, is_evaluated)
            else:
                rhs_is_evaluated = is_evaluated
                if token.kind == TokenKind.LOGICAL_AND:
                    rhs_is_evaluated = rhs_is_evaluated and bool(lhs.value)
                elif token.kind == TokenKind.LOGICAL_OR:
                    rhs_is_evaluated = rhs_is_evaluated and not bool(lhs.value)
                rhs = self.parse_binary_expr(state, precedence, rhs_is_evaluated)
                if lhs.is_erroneous or rhs.is_erroneous:
                    lhs.is_erroneous = True
                else:
                    evaluator(self, lhs, rhs, token, is_evaluated)
                lhs.loc.end = rhs.loc.end

    def parse_conditional_branches(self, state, token, condition, is_evaluated):
        '''Parse and evaluate the branches of the conditional operator.'''
        condition_truth = bool(condition.value)
        state.enter_context(token.kind, token.loc)
        lhs = self.parse_expr(state, condition_truth and is_evaluated)
        colon = state.leave_context()
        if colon.kind != TokenKind.COLON:
            return lhs
        rhs = self.parse_conditional_expr(state, not condition_truth and is_evaluated)
        if not condition.is_erroneous:
            self.usual_arithmetic_conversions(lhs, rhs, colon, is_evaluated)
        result = lhs if condition_truth else rhs
        result.is_erroneous = condition.is_erroneous or lhs.is_erroneous or rhs.is_erroneous
        result.loc.start = lhs.loc.start
        result.loc.end = rhs.loc.end
        return result

    def parse_unary_expr(self, state, is_evaluated):
        '''Parse and evaluate a unary or primary expression.'''
        token = state.get_token()
        kind = token.kind

        # Primary expressions.
        if kind == TokenKind.IDENTIFIER:
            ident = token.extra
            if ident is self.defined:
                return self.parse_defined_macro_expr(state, token)
            if ident.macro and ident.macro.is_builtin():
                return self.parse_has_feature_expr(state, token, is_evaluated)
            else:
                return self.evaluate_identifier_expr(token, is_evaluated)

        if kind == TokenKind.NUMBER or kind == TokenKind.CHARACTER_LITERAL:
            return self.evaluate_literal(token)

        # Unary ops
        if kind in unary_ops:
            rhs = self.parse_unary_expr(state, is_evaluated)
            if not rhs.is_erroneous and is_evaluated:
                self.evaluate_unary_op(rhs, token)
            rhs.loc.start = token.loc
            return rhs

        # Parenthesized expressions
        if kind == TokenKind.PAREN_OPEN:
            return self.parse_parenthesized_expr(state, token, is_evaluated)

        if kind == TokenKind.STRING_LITERAL:
            self.diag(DID.string_invalid_in_pp_expression, token.loc)
        elif kind == TokenKind.UNTERMINATED:
            pass  # Already diagnosed
        else:
            self.diag(DID.expected_expression, token.loc)
            state.recover(token)

        return ExprValue(0, False, True, TokenRange(token.loc, token.loc))

    def parse_parenthesized_expr(self, state, paren_open, is_evaluated):
        '''Parse and evaluate a parenthesized expression.'''
        state.enter_context(paren_open.kind, paren_open.loc)
        expr = self.parse_expr(state, is_evaluated)
        token = state.leave_context()
        expr.loc.start = paren_open.loc
        expr.loc.end = token.loc
        return expr

    def parse_defined_macro_expr(self, state, defined):
        '''Parse a 'defined' macro expression.'''
        # Diagnose if "defined" came from a macro expansion
        if self.pp.locator.derives_from_macro_expansion(defined.loc):
            self.diag(DID.macro_produced_defined, defined.loc)
        self.pp.expand_macros = False
        paren = False
        token = state.get_token()
        if token.kind == TokenKind.PAREN_OPEN:
            state.enter_context(token.kind, token.loc)
            token = state.get_token()
            paren = True
        is_defined, is_macro_name = self.pp.is_defined(token)
        if not is_macro_name:
            token = state.recover(token)
        if paren:
            token = state.leave_context()
        self.pp.expand_macros = True
        return ExprValue(int(is_defined), False, not is_macro_name,
                         TokenRange(defined.loc, token.loc))

    def parse_has_feature_expr(self, state, macro_token, is_evaluated):
        '''Parse __has_include() __has_cpp_attribute() operators.'''
        token = state.get_token()
        if token.kind == TokenKind.PAREN_OPEN:
            state.enter_context(token.kind, token.loc)
            # Get the spelling that is the result of the __has() operator and lex it as a token.
            spelling = self.parse_body_method(macro_token.extra.macro)(state, is_evaluated)
            token = state.leave_context()
            if spelling is not None:
                token = lex_token_from_builtin_spelling(self.pp, macro_token, spelling,
                                                        TokenRange(macro_token.loc, token.loc))
                assert token.kind == TokenKind.NUMBER
                return self.evaluate_literal(token)
        else:
            self.diag(DID.expected_open_paren, token.loc)
            state.recover(token)
        # An erroneous result
        return ExprValue(0, False, True, TokenRange(macro_token.loc, token.loc))

    def parse_body_method(self, kind):
        '''Return the method that is responsible for parsing the part between parentheses.'''
        if kind is BuiltinKind.has_include:
            return self.parse_has_include_body
        elif kind is BuiltinKind.has_cpp_attribute:
            return self.parse_has_attribute_body
        else:
            assert False

    def expect_identifier(self, state):
        '''Read an identifier and return is IdentifierInfo.  If it is not an identifier, diagnose
        that and return None.
        '''
        token = state.get_token()
        if token.kind == TokenKind.IDENTIFIER:
            return token.extra
        self.diag(DID.expected_identifier, token.loc)
        state.recover(token)
        return None

    def parse_has_attribute_body(self, state, is_evaluated):
        '''Parse the parenthesized part of __has_cpp_attribute().  Return the spelling of the
        result, or None on error.'''
        # C and C++ require an attribute-token, which is either identifer or
        # identifier::identifier.
        scope_name = self.expect_identifier(state)
        if not scope_name:
            return None

        scope_spelling = scope_name.spelling
        if self.pp.peek_token_kind() == TokenKind.SCOPE:
            token = state.get_token()
            assert token.kind == TokenKind.SCOPE
            attrib_name = self.expect_identifier(state)
            if not attrib_name:
                return None
            attrib_spelling = attrib_name.spelling
        else:
            attrib_spelling = scope_spelling
            scope_spelling = b''
        return self.pp.has_attribute_spelling(scope_spelling, attrib_spelling)

    def parse_has_include_body(self, state, is_evaluated):
        '''Parse the parenthesized part of __has_include().  Return the spelling of the result,
        or None on error.'''
        header_token = self.pp.create_header_name(in__has_include=True)
        if header_token is None:
            return None

        # "The has-include-expression evaluates to 1 if the search for the source file
        # succeeds, and to 0 if the search fails."  It is not clear what constitutes a
        # search succeeding, but Clang and GCC diagnose the search as for an include
        # except that not-found is ignored.  In particular, an unreadable file is a
        # fatal error, but a directory is considered a header that is not found.  This
        # is reasonable so we do the same.
        if is_evaluated:
            file = self.pp.read_header_file(header_token, diagnose_if_not_found=False)
        else:
            file = None
        return '1' if file else '0'

    def overflow(self, lhs, op, args):
        '''Diagnose overflow of lhs at the operator 'op' with the given arguments.'''
        self.diag(DID.integer_overflow, op.loc, args)
        lhs.is_erroneous = True

    def evaluate_literal(self, token):
        '''Evaluate a character constant or number.  This needs to be done even in unevaluated
        contexts because the signedness of the result matters.'''
        literal = self.literal_interpreter.interpret(token)
        if literal.kind == IntegerKind.error:
            value, is_unsigned, is_erroneous = 0, False, True
        else:
            value, is_unsigned = literal.value, self.pp.target.is_unsigned(literal.kind)
            is_erroneous = False
        return ExprValue(value, is_unsigned, is_erroneous, TokenRange(token.loc, token.loc))

    def evaluate_identifier_expr(self, token, is_evaluated):
        '''Evaluate an identifier.  This could be a boolean literal or a random identifier.
        '''
        is_erroneous = False
        value = 0
        if self.pp.peek_token_kind() == TokenKind.PAREN_OPEN:
            # Idea from Clang; consider "#if foo(2)" where foo is undefined.
            self.diag(DID.function_like_macro_not_defined, token.loc,
                      [self.pp.token_spelling(token)])
            is_erroneous = True
        elif token.extra == self.false:
            pass
        elif token.extra == self.true:
            value = 1
        elif is_evaluated:
            self.diag(DID.undefined_identifier, token.loc, [self.pp.token_spelling(token)])
        return ExprValue(value, False, is_erroneous, TokenRange(token.loc, token.loc))

    def evaluate_unary_op(self, rhs, op):
        '''Evaluate a unary expression.'''
        kind = op.kind
        if kind == TokenKind.MINUS:
            if rhs.set(-rhs.get(self.mask), self.mask):
                self.overflow(rhs, op, [rhs.loc])
        elif kind == TokenKind.LOGICAL_NOT:
            rhs.set_boolean(not rhs.value)
        elif kind == TokenKind.TILDE:
            rhs.value = self.mask - rhs.value
        else:
            assert kind == TokenKind.PLUS

    def usual_arithmetic_conversions(self, lhs, rhs, op, is_evaluated):
        '''Perform the usual arithmetic conversions in-place on lhs and rhs.'''
        if lhs.is_unsigned != rhs.is_unsigned:
            # Find the side to convert to unsigned
            side = rhs if lhs.is_unsigned else lhs
            # Read its value before setting is_unsigned
            old_value = side.get(self.mask)
            side.is_unsigned = True
            if is_evaluated and old_value < 0:
                args = [side.loc, f'{old_value:,d}', f'{side.get(self.mask):,d}']
                self.diag(DID.value_changes_sign, op.loc, args)

    def evaluate_arithmetic(self, lhs, rhs, op, is_evaluated):
        '''Evaluate several kinds of binary expression.'''
        self.usual_arithmetic_conversions(lhs, rhs, op, is_evaluated)
        if not is_evaluated:
            return
        kind = op.kind
        lhs_value, rhs_value = lhs.get(self.mask), rhs.get(self.mask)
        if kind == TokenKind.PLUS:
            if lhs.set(lhs_value + rhs_value, self.mask):
                self.overflow(lhs, op, [lhs.loc, rhs.loc])
        elif kind == TokenKind.MINUS:
            if lhs.set(lhs_value - rhs_value, self.mask):
                self.overflow(lhs, op, [lhs.loc, rhs.loc])
        elif kind == TokenKind.MULTIPLY:
            if lhs.set(lhs_value * rhs_value, self.mask):
                self.overflow(lhs, op, [lhs.loc, rhs.loc])
        elif kind == TokenKind.LT:
            lhs.set_boolean(lhs_value < rhs_value)
        elif kind == TokenKind.GT:
            lhs.set_boolean(lhs_value > rhs_value)
        elif kind == TokenKind.LE:
            lhs.set_boolean(lhs_value <= rhs_value)
        elif kind == TokenKind.GE:
            lhs.set_boolean(lhs_value >= rhs_value)
        elif kind == TokenKind.DIVIDE:
            if rhs_value == 0:
                self.diag(DID.division_by_zero, op.loc, [0, rhs.loc])
                lhs.is_erroneous = True
            else:
                # Python is different to C when exactly one value is negative
                if (lhs_value < 0) ^ (rhs_value < 0):
                    result = -(-lhs_value // rhs_value)
                else:
                    result = lhs_value // rhs_value
                assert not lhs.set(result, self.mask)
        else:
            assert kind == TokenKind.MODULUS
            if rhs_value == 0:
                self.diag(DID.division_by_zero, op.loc, [1, rhs.loc])
                lhs.is_erroneous = True
            else:
                # Python is different to C when exactly one value is negative
                if (lhs_value < 0) ^ (rhs_value < 0):
                    result = -(-lhs_value % rhs_value)
                else:
                    result = lhs_value % rhs_value
                assert not lhs.set(result, self.mask)

    def evaluate_arithmetic_direct(self, lhs, rhs, op, is_evaluated):
        '''These operate directly on the value with no need for interpretation with get / set
        operations.'''
        self.usual_arithmetic_conversions(lhs, rhs, op, is_evaluated)
        if not is_evaluated:
            return
        kind = op.kind
        if kind == TokenKind.EQ:
            lhs.set_boolean(lhs.value == rhs.value)
        elif kind == TokenKind.NE:
            lhs.set_boolean(lhs.value != rhs.value)
        elif kind == TokenKind.BITWISE_OR:
            lhs.value |= rhs.value
        elif kind == TokenKind.BITWISE_XOR:
            lhs.value ^= rhs.value
        else:
            assert kind == TokenKind.BITWISE_AND
            lhs.value &= rhs.value

    def evaluate_shift(self, lhs, rhs, op, is_evaluated):
        '''Evaluate shift expressions.'''
        if not is_evaluated:
            return
        lhs_value, rhs_value = lhs.get(self.mask), rhs.get(self.mask)
        # Check negative or too large
        if rhs_value < 0:
            # Undefined behaviour - hard error
            self.diag(DID.shift_count_negative, op.loc, [rhs.loc])
            lhs.is_erroneous = True
            return
        if rhs_value >= self.width:
            # Undefined behaviour - we cap the shift
            self.diag(DID.shift_count_too_large, op.loc, [rhs.loc])
            rhs_value = self.width
        if op.kind == TokenKind.LSHIFT:
            # A masked logical bit-shift where the resulting bit-pattern is then
            # interpreted (in C++23).  In C, and earlier C++, the behaviour is more
            # subtle.
            value = lhs.value << rhs_value
            if not lhs.is_unsigned:
                # Undefined in C if lhs.value < 0 or if lhs * pow(2, rhs) cannot be
                # represented in the result's type.  We take the C++ value, but warn in
                # cases where it is undefined in C.
                if lhs_value < 0:
                    self.diag(DID.shift_of_negative_value, op.loc, [0, lhs.loc])
                elif value > (self.mask >> 1):
                    self.diag(DID.left_shift_overflows, op.loc, [lhs.loc, rhs.loc])
            lhs.value = value & self.mask
        else:
            assert op.kind == TokenKind.RSHIFT
            # In C++23, this is an arithmetic right shift preserving the sign (i.e.  a
            # division rounding to negative infinity).  It has an implementation-defined
            # value in C.
            if lhs_value < 0:
                # Shift the complement, and complement back.
                lhs.value = self.mask - ((self.mask - lhs.value) >> rhs_value)
                self.diag(DID.shift_of_negative_value, op.loc, [1, lhs.loc])
            else:
                lhs.value >>= rhs_value

    def evaluate_logical(self, lhs, rhs, op, is_evaluated):
        '''Evaluate the logical operators && and ||.'''
        if op.kind == TokenKind.LOGICAL_AND:
            lhs.set_boolean(bool(lhs.value) and bool(rhs.value))
        else:
            assert op.kind == TokenKind.LOGICAL_OR
            lhs.set_boolean(bool(lhs.value) or bool(rhs.value))

    def evaluate_comma(self, lhs, rhs, op, is_evaluated):
        '''Evaluate a comma expression.'''
        # C++11 started permitting commas in integer constant expressions.  Previously
        # they were not permitted, as in C.  It seems this was not intended to apply to
        # the preprocessor
        # (https://www.open-std.org/jtc1/sc22/wg21/docs/cwg_active.html#1436).  We treat
        # this as a defect in the standard and diagnose unconditionally but treat the
        # value appropriately.
        self.diag(DID.comma_in_pp_expression, op.loc)
        lhs.value = rhs.value
        lhs.is_unsigned = rhs.is_unsigned
        lhs.is_erroneous = rhs.is_erroneous
        lhs.loc = rhs.loc

    def invalid_op(self, _lhs, _rhs, op):
        '''Diagnose an operator that is invalid in preprocessor expressions.'''
        self.diag(DID.invalid_op_in_pp_expression, op.loc, [self.pp.token_spelling(op)])


unary_ops = {TokenKind.PLUS, TokenKind.MINUS, TokenKind.TILDE, TokenKind.LOGICAL_NOT}
binary_ops = {
    TokenKind.COMMA: (BOP.comma, ExprParser.evaluate_comma),
    TokenKind.QUESTION_MARK: (BOP.conditional, None),
    TokenKind.LOGICAL_OR: (BOP.logical_or, ExprParser.evaluate_logical),
    TokenKind.LOGICAL_AND: (BOP.logical_and, ExprParser.evaluate_logical),
    TokenKind.BITWISE_OR: (BOP.bitwise_or, ExprParser.evaluate_arithmetic_direct),
    TokenKind.BITWISE_XOR: (BOP.bitwise_xor, ExprParser.evaluate_arithmetic_direct),
    TokenKind.BITWISE_AND: (BOP.bitwise_and, ExprParser.evaluate_arithmetic_direct),
    TokenKind.EQ: (BOP.equality, ExprParser.evaluate_arithmetic_direct),
    TokenKind.NE: (BOP.equality, ExprParser.evaluate_arithmetic_direct),
    TokenKind.LT: (BOP.relational, ExprParser.evaluate_arithmetic),
    TokenKind.GT: (BOP.relational, ExprParser.evaluate_arithmetic),
    TokenKind.LE: (BOP.relational, ExprParser.evaluate_arithmetic),
    TokenKind.GE: (BOP.relational, ExprParser.evaluate_arithmetic),
    TokenKind.LSHIFT: (BOP.shift, ExprParser.evaluate_shift),
    TokenKind.RSHIFT: (BOP.shift, ExprParser.evaluate_shift),
    TokenKind.PLUS: (BOP.additive, ExprParser.evaluate_arithmetic),
    TokenKind.MINUS: (BOP.additive, ExprParser.evaluate_arithmetic),
    TokenKind.MULTIPLY: (BOP.multiplicative, ExprParser.evaluate_arithmetic),
    TokenKind.DIVIDE: (BOP.multiplicative, ExprParser.evaluate_arithmetic),
    TokenKind.MODULUS: (BOP.multiplicative, ExprParser.evaluate_arithmetic),
}

# This gives better diagnostics if these operators are encountered.
binary_ops.update({op: (BOP.invalid, ExprParser.invalid_op) for op in (
    TokenKind.DOT, TokenKind.DOT_STAR, TokenKind.DEREF, TokenKind.DEREF_STAR,
    TokenKind.ASSIGN, TokenKind.PLUS_ASSIGN, TokenKind.MINUS_ASSIGN, TokenKind.MULTIPLY_ASSIGN,
    TokenKind.DIVIDE_ASSIGN, TokenKind.MODULUS_ASSIGN, TokenKind.BITWISE_AND_ASSIGN,
    TokenKind.BITWISE_OR_ASSIGN, TokenKind.BITWISE_XOR_ASSIGN, TokenKind.LSHIFT_ASSIGN,
    TokenKind.RSHIFT_ASSIGN, TokenKind.LEG)})
