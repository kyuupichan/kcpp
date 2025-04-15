# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Parser state.  Enables high-quality error recovery and diagnostics.'''

from dataclasses import dataclass

from ..core import Token, TokenKind
from ..diagnostics import Diagnostic, DID


__all__ = ['ParserState']


@dataclass(slots=True)
class Metadata:
    '''Static metadata about a grammatical context; see docstring for Context.'''
    want_kind: TokenKind
    did: DID
    open_punc: str


# Context metadata applicable to pp expressions.
Metadata.kinds = {
    TokenKind.QUESTION_MARK: Metadata(TokenKind.COLON, DID.expected_colon, '?'),
    TokenKind.PAREN_OPEN: Metadata(TokenKind.PAREN_CLOSE, DID.expected_close_paren, '('),
}


@dataclass(slots=True)
class ParserContext:
    '''A simple data structure used to aid parser recovery from syntax errors.  A context is
    generally a grammatic construct that, once entered, expects a later token to usually
    complete it.  For exanple, after '(' there must eventually be a ')'.  Similarly, in
    expressions, after '?' at some point we expect a ':'.
    '''
    start_loc: int
    metadata: Metadata


@dataclass(slots=True)
class ParserState:
    '''Parser state.  Separate from the parser so it is stateless and reusable.'''
    pp: object
    token: Token
    context_stack: list

    @classmethod
    def from_pp(cls, pp):
        return cls(pp, None, [])

    def get_token(self):
        '''Get the next token.  Use a lookahead token if there is one, otherwise ask the
        preprocessor.
        '''
        if self.token is None:
            return self.pp.get_token()

        result = self.token
        self.token = None
        return result

    def save_token(self, token):
        assert self.token is None
        self.token = token

    def enter_context(self, kind, loc):
        '''Enter a grammatical context.'''
        self.context_stack.append(ParserContext(loc, Metadata.kinds[kind]))

    def leave_context(self):
        '''Leave a grammatical context.  Diagnoses if the next token is not of the expected
        kind.'''
        token = self.get_token()
        context = self.context_stack[-1]
        if token.kind != context.metadata.want_kind:
            note = Diagnostic(DID.prior_match, context.start_loc, [context.metadata.open_punc])
            self.pp.diag(context.metadata.did, token.loc, [note])
            token = self.recover(token)
            if token.kind == context.metadata.want_kind:
                self.token = None
        self.context_stack.pop()
        return token

    def recover(self, token):
        '''Attempt to recover from a grammatical error in a smart way.  The goal is to not have a
        cascade of errors owing to this error, but also to continue parsing as early as
        possible so other genuine issues are still diagnosed.
        '''
        self.save_token(token)
        stopping_tokens = {context.metadata.want_kind for context in self.context_stack}
        stopping_tokens.add(TokenKind.EOF)
        while True:
            token = self.get_token()
            if token.kind in stopping_tokens:
                self.save_token(token)
                return token
            if token.kind in Metadata.kinds:
                # Recurse
                self.enter_context(token.kind, token.loc)
                self.recover(None)
                self.leave_context()
