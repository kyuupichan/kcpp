# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Preprocessed output.'''

import sys
from abc import ABC, abstractmethod

from kcpp.cpp import Token, TokenKind, TokenFlags, Preprocessor
from kcpp.diagnostics import UnicodeTerminal


__all__ = ['PreprocessedOutput', 'ProcessorBase']


class ProcessorBase(ABC):

    def __init__(self):
        pass

    def diagnostic_consumer(self, pp, env):
        return UnicodeTerminal(pp, env)

    def sources(self, env):
        return env.command_line.files

    @abstractmethod
    def process_source(self, source):
        pass

    def run(self, source, env):
        pp = Preprocessor(env)

        # Get a diagnostic consumer
        consumer = self.diagnostic_consumer(pp, env)
        pp.set_diagnostic_consumer(consumer)

        # Emit diagnostics from processing the command line
        for diagnostic in env.diagnostics:
            pp.emit(diagnostic)
        # Process the source if no error
        if not consumer.error_count:
            self.process_source(pp, source)
        # Emit the error summary
        consumer.emit_error_count()


class PreprocessedOutput(ProcessorBase):
    '''Consume tokens from the preprocessor and output the preprocessed source.'''

    def process_source(self, pp, source):
        # FIXME: this needs a lot of work; it's currently used for simple debugging.
        pp.push_source_file(source)
        token = Token.create()
        write = sys.stdout.buffer.write

        line_number = 1
        count = 0
        while True:
            pp.get_token(token)
            print(token.to_text())
            if token.kind == TokenKind.EOF:
                break
            count += 1
            if token.flags & TokenFlags.BOL:
                # FIXME: this isn't right any more
                coords = pp.elaborated_location(token.loc).coords
                if coords.line_number != line_number:
                    write(b'\n' * (coords.line_number - line_number))
                    line_number = coords.line_number
                    if coords.column_offset > 1:
                        # FIXME assert token.flags & TokenFlags.WS
                        # One will be done below for WS flag
                        write(b' ' * (coords.column_offset - 1))
            if token.flags & TokenFlags.WS:
                write(b' ')
            write(pp.token_spelling(token.loc))

        write(b'\n')


class FrontEnd(ProcessorBase):
    '''Simulate a compiler front end.  For now, all it does is output consumed tokens, and the
    interpretation of literals.
    '''

    def process_source(self, pp, source):
        '''Act like a front-end, consuming tokens and evaluating literals.  At present
        this is used for debugging purposes.'''
        pp.push_source_file(source)

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
