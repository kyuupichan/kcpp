# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Preprocessed output.'''

import sys
from abc import ABC, abstractmethod

from kcpp.cpp import (
    Token, TokenKind, TokenFlags, Preprocessor, quoted_string, PreprocessorActions,
)
from kcpp.diagnostics import UnicodeTerminal, DiagnosticPrinter


__all__ = ['PreprocessedOutput', 'ProcessorBase', 'FrontEnd']


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

    def push_source(self, pp, filename):
        '''Split out so it can be overridden.'''
        pp.push_source_file(filename)

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

    def __init__(self):
        self.at_bol = True
        self.write = sys.stdout.write
        self.line_number = -1
        self.filename = None
        self.pp = None

    def write_line_marker(self):
        '''Write a line marker.  On return self.at_bol is True.'''
        if not self.at_bol:
            self.write('\n')
        self.write(f'#line {self.line_number} {quoted_string(self.filename)}\n')
        self.at_bol = True

    def source_file_changed(self, loc, reason):
        location = self.pp.locator.presumed_location(loc, True)
        self.line_number = location.line_number
        self.filename = location.filename
        self.write_line_marker()

    def move_to_line_number(self, line_number):
        count = line_number - self.line_number
        self.line_number = line_number

        assert count >= 0
        if count < 8:
            self.write('\n' * count)
        else:
            self.write_line_marker()
        self.at_bol = True

    def process_source(self, pp, filename):
        self.pp = pp
        pp.actions = PreprocessorActions()
        pp.actions.source_file_changed = self.source_file_changed
        self.push_source(pp, filename)

        token = Token.create()
        write = self.write
        locator = pp.locator
        while True:
            pp.get_token(token)
            if token.kind == TokenKind.EOF:
                break

            location = locator.presumed_location(token.loc, True)
            if location.line_number != self.line_number:
                self.move_to_line_number(location.line_number)
                if location.column_offset > 1:
                    write(' ' * location.column_offset)
            elif token.flags & TokenFlags.WS:
                write(' ')
            write(pp.token_spelling(token).decode())
            self.at_bol = False

        write('\n')


class FrontEnd(ProcessorBase):
    '''Simulate a compiler front end.  For now, all it does is output consumed tokens, and the
    interpretation of literals.
    '''

    def diagnostic_consumer(self, pp, env):
        return DiagnosticPrinter()

    def process_source(self, pp, filename):
        '''Act like a front-end, consuming tokens and evaluating literals.  At present
        this is used for debugging purposes.'''
        self.push_source(pp, filename)

        token = Token.create()
        consume = True
        while True:
            # The literal interpreter concatenates string literals, and to do so, it reads
            # the first token after the last string literal so we don't need to fetch
            # another token
            if consume:
                pp.get_token(token)
            consume = token.kind != TokenKind.STRING_LITERAL

            if token.kind == TokenKind.EOF:
                return
            print(token.to_short_text())
            if token.is_literal():
                result = pp.interpret_literal(token)
                print(result.to_short_text())
