# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Preprocessor frontends.'''

import sys
from abc import ABC, abstractmethod

from kcpp.cpp import (
    Token, TokenKind, TokenFlags, Preprocessor, PreprocessorActions, Lexer,
)
from kcpp.diagnostics import UnicodeTerminal, DiagnosticPrinter


__all__ = ['PreprocessedOutput', 'FrontEndBase', 'FrontEnd']


class FrontEndBase(ABC):

    def __init__(self):
        super().__init__()
        self.pp = Preprocessor()

    def diagnostic_consumer(self, env):
        return UnicodeTerminal(self.pp, env)

    def push_source(self, filename):
        return self.pp.push_main_source_file(filename)

    @abstractmethod
    def process(self):
        pass

    def customize(self, env):
        self.env = env

    def run(self, source):
        # Set up the diagnostic consumer before processing the command line
        self.pp.set_diagnostic_consumer(self.diagnostic_consumer(self.env))
        self.pp.initialize(self.env)

        # Process the source
        if self.push_source(source):
            self.process()
        # Tidy up
        return self.pp.finish()


class PreprocessedOutput(FrontEndBase, PreprocessorActions):
    '''Consume tokens from the preprocessor and output the preprocessed source.'''

    def __init__(self):
        super().__init__()
        self.at_bol = False
        self.write = None
        self.line_number = -1   # Presumed line number
        self.filename = None
        # Controlled from the command line
        self.suppress_linemarkers = False
        self.list_macros = False

    def customize(self, env):
        super().customize(env)
        self.suppress_linemarkers = env.command_line.P
        self.list_macros = env.command_line.list_macros

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
        self.write = sys.stdout.write
        self.at_bol = True

        pp = self.pp
        pp.actions = self
        token = Token.create()
        write = self.write
        locator = pp.locator
        prior_loc = None
        prior_spelling = None

        while True:
            pp.get_token(token)
            if token.kind == TokenKind.EOF:
                break

            location = locator.presumed_location(token.loc, True)
            if location.presumed_line_number != self.line_number:
                self.move_to_line_number(location.presumed_line_number)
            if self.at_bol:
                if location.column_offset > 1:
                    write(' ' * location.column_offset)
            elif token.flags & TokenFlags.WS:
                write(' ')
            elif self.separate_tokens(prior_loc, prior_spelling, token):
                write(' ')

            prior_loc = token.loc
            prior_spelling = pp.token_spelling(token)
            write(prior_spelling.decode())
            self.at_bol = False

        self.finish_line()

    def separate_tokens(self, lhs_loc, lhs_spelling, rhs):
        '''Return True if a space should be output to separate two tokens.'''
        # We must separate the tokens if:
        # 1) spellings that lex to a different token to LHS (or start a comment)
        # 2) spellings that lex to LHS but could become part of a longer token if more
        #    were concatenated
        #
        # Many casees for 1): // /* += --
        # Three cases for 2):  ..  %:% <NUMBER><CHARACTER_LITERAL>

        # If they were adjacent in the source code, no space is needed
        lhs_span, lhs_offset = self.pp.locator.spelling_span_and_offset(lhs_loc)
        rhs_span, rhs_offset = self.pp.locator.spelling_span_and_offset(rhs.loc)
        if lhs_span == rhs_span and rhs_offset == lhs_offset + len(lhs_spelling):
            return False

        rhs_spelling = self.pp.token_spelling(rhs)
        spelling = lhs_spelling + rhs_spelling
        lexer = Lexer(self.pp, spelling + b'\0', 1)
        token = Token.create()
        prior = self.pp.set_diagnostic_consumer(None)
        lexer.get_token(token)
        self.pp.set_diagnostic_consumer(prior)

        # Case 1: if it formed a different token we need a space
        if lexer.cursor != len(lhs_spelling):
            return True

        # Case 2 above
        return (lhs_spelling == b'.' and rhs.kind == TokenKind.DOT
                or (lhs_spelling == b'%:' and rhs.kind == TokenKind.MODULUS)
                or (token.kind == TokenKind.NUMBER and rhs.kind == TokenKind.CHARACTER_LITERAL))


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
