# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Preprocessor frontends.'''

import sys
from abc import ABC, abstractmethod

from kcpp.cpp import (
    Token, TokenKind, TokenFlags, Preprocessor, PreprocessorActions,
)
from kcpp.diagnostics import UnicodeTerminal, DiagnosticPrinter


__all__ = ['PreprocessedOutput', 'FrontEndBase', 'FrontEnd']


class FrontEndBase(ABC):

    def __init__(self, env):
        super().__init__()
        self.pp = None

    @classmethod
    def update_argument_parser(cls, parser):
        pass

    def diagnostic_consumer(self, env):
        return UnicodeTerminal(self.pp, env)

    def push_source(self, filename):
        return self.pp.push_main_source_file(filename)

    def process_source(self, filename):
        self.pp.actions = self.preprocessor_actions()
        if self.push_source(filename):
            self.process()

    def preprocessor_actions(self):
        return None

    @abstractmethod
    def process(self):
        pass

    def run(self, source, env):
        self.pp = Preprocessor(env)

        # Get a diagnostic consumer
        consumer = self.diagnostic_consumer(env)
        self.pp.set_diagnostic_consumer(consumer)

        # Emit diagnostics from processing the command line
        for diagnostic in env.diagnostics:
            self.pp.emit(diagnostic)
        # Process the source if no error
        if not consumer.error_count:
            self.process_source(source)
        # Emit the error summary
        consumer.emit_error_count()


class PreprocessedOutput(FrontEndBase, PreprocessorActions):
    '''Consume tokens from the preprocessor and output the preprocessed source.'''

    def __init__(self, env):
        super().__init__(env)
        self.at_bol = True
        self.write = sys.stdout.write
        self.line_number = -1   # Presumed line number
        self.filename = None
        self.suppress_linemarkers = env.command_line.P
        self.list_macros = env.command_line.list_macros

    @classmethod
    def update_argument_parser(cls, parser):
        group = parser.add_argument_group(title='preprocessed output')
        group.add_argument('-P', help='suppress generation of linemarkers', action='store_true',
                           default=False)
        group.add_argument('--list-macros', help='output macro definitions', action='store_true',
                           default=False)

    def finish_line(self):
        if not self.at_bol:
            self.write('\n')
            self.line_number += 1
        self.at_bol = True

    def write_line_marker(self):
        '''Write a line marker.  On return self.at_bol is True.'''
        if not self.suppress_linemarkers:
            self.write(f'#line {self.line_number} {self.filename}\n')

    def on_source_file_change(self, loc, reason):
        self.finish_line()
        location = self.pp.locator.presumed_location(loc, True)
        self.line_number = location.presumed_line_number
        self.filename = location.presumed_filename
        self.write_line_marker()

    def on_macro_defined(self, macro):
        if not self.list_macros:
            return
        self.finish_line()
        macro_name = macro.macro_name(self.pp).decode()
        self.write(f'#define {macro_name}{macro.definition_text(self.pp)}\n')
        self.line_number += 1
        self.at_bol = True

    def move_to_line_number(self, line_number):
        self.finish_line()
        count = line_number - self.line_number
        assert count >= 0
        self.line_number = line_number
        if not self.suppress_linemarkers:
            if count < 8:
                self.write('\n' * count)
            else:
                self.write_line_marker()

    def process(self):
        pp = self.pp
        pp.actions = self
        token = Token.create()
        write = self.write
        locator = pp.locator
        while True:
            pp.get_token(token)
            if token.kind == TokenKind.EOF:
                break

            location = locator.presumed_location(token.loc, True)
            if location.presumed_line_number != self.line_number:
                self.move_to_line_number(location.presumed_line_number)
            if self.at_bol and location.column_offset > 1:
                write(' ' * location.column_offset)
            elif token.flags & TokenFlags.WS:
                write(' ')
            write(pp.token_spelling(token).decode())
            self.at_bol = False

        self.finish_line()


class FrontEnd(FrontEndBase):
    '''Simulate a compiler front end.  For now, all it does is output consumed tokens, and the
    interpretation of literals.
    '''

    def diagnostic_consumer(self, env):
        return DiagnosticPrinter()

    def process(self):
        '''Act like a front-end, consuming tokens and evaluating literals.  At present
        this is used for debugging purposes.'''
        pp = self.pp
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
