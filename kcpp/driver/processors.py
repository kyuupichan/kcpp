# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Preprocessed output.'''

import sys

from ..cpp import Token, TokenKind, TokenFlags


__all__ = ['PreprocessedOutput']


class PreprocessedOutput:
    '''Consume tokens from the preprocessor and output the preprocessed source.'''

    def run(self, pp):
        # FIXME: this needs a lot of work; it's currently used for simple debugging.
        token = Token.create()
        write = sys.stdout.buffer.write

        line_number = 1
        count = 0
        while True:
            pp.get_token(token)
            if token.kind == TokenKind.EOF:
                break
            count += 1
            if token.flags & TokenFlags.BOL:
                eloc = pp.elaborated_location(token.loc)
                if eloc.line_number != line_number:
                    write(b'\n' * (eloc.line_number - line_number))
                    line_number = eloc.line_number
                    if eloc.column_offset > 1:
                        assert token.flags & TokenFlags.WS
                        # One will be done below for WS flag
                        write(b' ' * (eloc.column_offset - 1))
            if token.flags & TokenFlags.WS:
                write(b' ')
            write(pp.token_spelling(token.loc))

        write(b'\n')


class FrontEnd:
    '''Simulate a compiler front end.  For now, all it does is output consumed tokens, and the
    interpretation of literals.
    '''

    def run(self, pp):
        '''Act like a front-end, consuming tokens and evaluating literals.  At present
        this is used for debugging purposes.'''
        token = Token.create()
        pp.get_token(token)
        while True:
            if token.kind == TokenKind.EOF:
                return
            if token.is_literal():
                result = pp.interpret_literal(token)
                print(result)

            # Consume the token.  String literal concatenation has already consumed all
            # adjacent string literals.
            if token.kind != TokenKind.STRING_LITERAL:
                pp.get_token(token)
