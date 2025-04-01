# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''A fast preprocessor expression parser and evaluator with high-quality error recovery
and diagnostics.
'''

from dataclasses import dataclass
from enum import IntEnum, auto

from ..diagnostics import Diagnostic, DID, TokenRange
from .basic import Token, TokenKind, IntegerKind
from .literals import LiteralInterpreter


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


@dataclass(slots=True)
class ContextMetadata:
    '''Static metadata about a grammatical context; see docstring for Context.'''
    want_kind: TokenKind
    did: DID
    open_punc: str


# Context metadata applicable to pp expressions.
ContextMetadata.kinds = {
    TokenKind.QUESTION_MARK: ContextMetadata(TokenKind.COLON, DID.expected_colon, '?'),
    TokenKind.PAREN_OPEN: ContextMetadata(TokenKind.PAREN_CLOSE, DID.expected_close_paren, '('),
}


@dataclass(slots=True)
class Context:
    '''A simple data structure used to aid parser recovery from syntax errors.  A context is
    generally a grammatic construct that, once entered, expects a later token to usually
    complete it.  For exanple, after '(' there must eventually be a ')'.  Similarly after
    '?' at some point we expect a ':' ,
    '''
    start_loc: int
    metadata: ContextMetadata


@dataclass(slots=True)
class ParserState:
    '''Parser state.  Separate from the parser so it is stateless and reusable.'''
    token: Token
    context_stack: list


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

    def get_token(self, state):
        '''Get the next token.  Use a lookahead token if there is one, otherwise ask the
        preprocessor.
        '''
        if state.token is None:
            token = Token.create()
            self.pp.get_token(token)
        else:
            token = state.token
            state.token = None
        return token

    def enter_context(self, state, kind, loc):
        '''Enter a grammatical context.'''
        state.context_stack.append(Context(loc, ContextMetadata.kinds[kind]))

    def leave_context(self, state):
        '''Leave a grammatical context.  Diagnoses if the next token is not of the expected
        kind.'''
        token = self.get_token(state)
        context = state.context_stack[-1]
        if token.kind != context.metadata.want_kind:
            note = Diagnostic(DID.prior_match, context.start_loc, [context.metadata.open_punc])
            self.diag(context.metadata.did, token.loc, [note])
            token = self.recover(state, token)
            if token.kind == context.metadata.want_kind:
                state.token = None
        state.context_stack.pop()
        return token

    def recover(self, state, token):
        '''Attempt to recover from a grammatical error in a smart way.  The goal is to not have a
        cascade of errors owing to this error, but also to continue parsing as early as
        possible so other genuine issues are still diagnosed.
        '''
        state.token = token
        stopping_tokens = {context.metadata.want_kind for context in state.context_stack}
        stopping_tokens.add(TokenKind.EOF)
        while True:
            token = self.get_token(state)
            if token.kind in stopping_tokens:
                state.token = token
                return token
            if token.kind in ContextMetadata.kinds:
                # Recurse
                self.enter_context(state, token.kind, token.loc)
                self.recover(state, None)
                self.leave_context(state)

    def parse_and_evaluate_constant_expr(self):
        '''The external interface - parse and evaluate a preprocessor expression.  Return a
        (value, token) pair.  The value is an ExprValue instance and token is the
        lookahead token.
        '''
        # As per the grammar, comma expressions are not acceptable at the top level.
        state = ParserState(None, [])
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
        # perform (in LR parsing terminology) a reduction operation, oterhwise perform a
        # shift operation.
        lhs = self.parse_unary_expr(state, is_evaluated)
        while True:
            token = self.get_token(state)
            precedence, evaluator = binary_ops.get(token.kind, (BOP.minimal, None))
            if precedence <= reduce_precedence:
                state.token = token
                return lhs

            if precedence == BOP.conditional:
                lhs = self.parse_conditional_branches(state, token, bool(lhs.value), is_evaluated)
            else:
                rhs_is_evaluated = is_evaluated
                if token.kind == TokenKind.LOGICAL_AND:
                    rhs_is_evaluated = rhs_is_evaluated and bool(lhs.value)
                elif token.kind == TokenKind.LOGICAL_OR:
                    rhs_is_evaluated = rhs_is_evaluated and not bool(lhs.value)
                rhs = self.parse_binary_expr(state, precedence, rhs_is_evaluated)
                if is_evaluated and not lhs.is_erroneous and not rhs.is_erroneous:
                    evaluator(self, lhs, rhs, token)
                lhs.loc.end = rhs.loc.end

    def parse_conditional_branches(self, state, token, condition_truth, is_evaluated):
        '''Parse and evaluate the branches of a conditional operator.'''
        self.enter_context(state, token.kind, token.loc)
        lhs = self.parse_expr(state, condition_truth and is_evaluated)
        colon = self.leave_context(state)
        if colon.kind != TokenKind.COLON:
            return lhs
        rhs = self.parse_conditional_expr(state, not condition_truth and is_evaluated)
        if is_evaluated and not (lhs.is_erroneous or rhs.is_erroneous):
            self.integer_promotions(lhs, rhs, colon)
        result = lhs if condition_truth else rhs
        result.loc.start = lhs.loc.start
        result.loc.end = rhs.loc.end
        return result

    def parse_unary_expr(self, state, is_evaluated):
        '''Parse and evaluate a unary or primary expression.'''
        token = self.get_token(state)
        kind = token.kind

        # Primary expressions
        if kind == TokenKind.IDENTIFIER:
            if token.extra == self.defined:
                return self.parse_defined_macro_expr(state, token)
            return self.evaluate_identifier_expr(token, is_evaluated)

        if kind == TokenKind.NUMBER or kind == TokenKind.CHARACTER_LITERAL:
            return self.evaluate_literal(token, is_evaluated)

        # Unary ops
        if kind in unary_ops:
            rhs = self.parse_unary_expr(state, is_evaluated)
            if is_evaluated and not rhs.is_erroneous:
                self.evaluate_unary_op(rhs, token)
            rhs.loc.start = token.loc
            return rhs

        # Parenthesized expressions
        if kind == TokenKind.PAREN_OPEN:
            return self.parse_parenthesized_expr(state, token, is_evaluated)

        if kind == TokenKind.STRING_LITERAL:
            self.diag(DID.string_invalid_in_pp_expression, token.loc)
        else:
            self.diag(DID.expected_expression, token.loc)

        return ExprValue(0, False, True, TokenRange(token.loc, token.loc))

    def parse_parenthesized_expr(self, state, paren_open, is_evaluated):
        '''Parse and evaluate a parenthesized expression.'''
        self.enter_context(state, paren_open.kind, paren_open.loc)
        expr = self.parse_expr(state, is_evaluated)
        token = self.leave_context(state)
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
        token = self.get_token(state)
        if token.kind == TokenKind.PAREN_OPEN:
            self.enter_context(state, token.kind, token.loc)
            token = self.get_token(state)
            paren = True
        is_defined, is_macro_name = self.pp.is_defined(token)
        if not is_macro_name:
            token = self.recover(state, token)
        if paren:
            token = self.leave_context(state)
        self.pp.expand_macros = True
        return ExprValue(int(is_defined), False, not is_macro_name,
                         TokenRange(defined.loc, token.loc))

    def overflow(self, lhs, op, args):
        '''Diagnose overflow of lhs at the operator 'op' with the given arguments.'''
        self.diag(DID.integer_overflow, op.loc, args)
        lhs.is_erroneous = True

    def evaluate_literal(self, token, is_evaluated):
        '''Evaluate a character constant or number.'''
        value, is_unsigned, is_erroneous = 0, False, False
        if is_evaluated:
            literal = self.literal_interpreter.interpret(token)
            if literal.kind == IntegerKind.error:
                is_erroneous = True
            else:
                value, is_unsigned = literal.value, self.pp.target.is_unsigned(literal.kind)
        return ExprValue(value, is_unsigned, is_erroneous, TokenRange(token.loc, token.loc))

    def evaluate_identifier_expr(self, token, is_evaluated):
        '''Evaluate an identifier.  This could be a boolean literal or a random identifier.
        '''
        value = 0
        if token.extra == self.false:
            pass
        elif token.extra == self.true:
            value = 1
        elif is_evaluated:
            self.diag(DID.identifier_in_pp_expr, token.loc, [self.pp.token_spelling(token)])
        return ExprValue(value, False, False, TokenRange(token.loc, token.loc))

    def evaluate_unary_op(self, rhs, op):
        '''Evaluate a unary expression.'''
        kind = op.kind
        if kind == TokenKind.PLUS:
            pass
        elif kind == TokenKind.MINUS:
            if rhs.set(-rhs.get(self.mask), self.mask):
                self.overflow(rhs, op, [rhs.loc])
        elif kind == TokenKind.LOGICAL_NOT:
            rhs.set_boolean(not rhs.value)
        elif kind == TokenKind.TILDE:
            rhs.value = self.mask - rhs.value

    def integer_promotions(self, lhs, rhs, op):
        '''Perform the usual arithmetic conversions on lhs and rhs.'''
        if lhs.is_unsigned != rhs.is_unsigned:
            # Find the side to convert to unsigned
            side = rhs if lhs.is_unsigned else lhs
            # Read its value before setting is_unsigned
            old_value = side.get(self.mask)
            side.is_unsigned = True
            if old_value < 0:
                args = [side.loc, f'{old_value:,d}', f'{side.get(self.mask):,d}']
                self.diag(DID.value_changes_sign, op.loc, args)

    def evaluate_arithmetic(self, lhs, rhs, op):
        '''Evaluate several kinds of binary expression.'''
        self.integer_promotions(lhs, rhs, op)
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

    def evaluate_arithmetic_direct(self, lhs, rhs, op):
        '''These operate directly on the value with no need for get / set operations.'''
        self.integer_promotions(lhs, rhs, op)
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

    def evaluate_shift(self, lhs, rhs, op):
        '''Evaluate shift expressions.'''
        lhs_value, rhs_value = lhs.get(self.mask), rhs.get(self.mask)
        # Check negative or too large
        if rhs_value < 0:
            # Undefined behaviour - error
            self.diag(DID.shift_count_negative, op.loc, [rhs.loc])
            lhs.is_erroneous = True
        elif rhs_value >= self.width:
            # Undefined behaviour - error
            self.diag(DID.shift_count_too_large, op.loc, [rhs.loc])
            lhs.is_erroneous = True
        elif op.kind == TokenKind.LSHIFT:
            # A masked logical bit-shift where the resulting bit-pattern is then
            # interpreted (in C++23).  In C, and earlier C++, the behaviour is more
            # subtle.
            value = lhs.value << rhs_value
            if not lhs.is_unsigned:
                # Undefined in C if lhs.value < 0 or if lhs * pow(2, rhs) cannot be
                # represented in the result's type.  We take the C++ value, but warn in
                # cases where it is undefined in C.
                if lhs_value < 0:
                    self.diag(DID.left_shift_of_negative_value, op.loc, [lhs.loc, 0])
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
                self.diag(DID.right_shift_of_negative_value, op.loc, [lhs.loc])
            else:
                lhs.value >>= rhs_value

    def evaluate_logical(self, lhs, rhs, op):
        '''Evaluate short-circuiting logical expressions (&& and ||).'''
        if op.kind == TokenKind.LOGICAL_AND:
            lhs.set_boolean(lhs.value and rhs.value)
        else:
            assert op.kind == TokenKind.LOGICAL_OR
            lhs.set_boolean(lhs.value or rhs.value)

    def evaluate_comma(self, lhs, rhs, _op):
        '''Evaluate a comma expression.'''
        # Fine from C++11.  Reject in C89, in C99 valid if unevaluated.  C++90?
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
    TokenKind.LOGICAL_OR: (BOP.logical_and, ExprParser.evaluate_logical),
    TokenKind.LOGICAL_AND: (BOP.logical_or, ExprParser.evaluate_logical),
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
