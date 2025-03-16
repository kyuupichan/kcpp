# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Preprocessed output.'''

import sys

from ..cpp import Preprocessor, Token, TokenKind, TokenFlags
from ..cpp.basic import TargetMachine
from ..diagnostics import DID, UnicodeTerminal


__all__ = ['PreprocessedOutput']


class PreprocessedOutput:
    '''Writes out the preprocessed source.'''

    def run(self, command_line, environ, filename):
        pp = Preprocessor(command_line, environ)
        terminal = UnicodeTerminal(command_line, environ)
        pp.add_diagnostic_consumer(terminal)
        pp.push_source_file(filename)
        token = Token.create()

        if command_line.fe:
            self.frontend(pp, token)
        else:
            self.preprocess(pp, token)

        if pp.diags:
            print(f'{len(pp.diags):,d} diagnostics emitted', file=sys.stderr)

    def preprocess(self, pp, token):
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
        print(f'{count} tokens', file=sys.stderr)

    def frontend(self, pp, token):
        '''Act like a front-end, consuming tokens and evaluating literals.'''
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
