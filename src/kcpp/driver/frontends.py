# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Preprocessor frontends.'''

from abc import ABC

from kcpp.core import TokenKind, TokenFlags
from kcpp.cpp import PreprocessorActions
from kcpp.diagnostics import UnicodeTerminal


__all__ = ['PreprocessedOutput', 'FrontEndBase', 'FrontEnd']


class FrontEndBase(ABC):

    help_group_name = 'frontend'
    diagnostic_class = UnicodeTerminal

    def __init__(self, pp):
        super().__init__()
        self.pp = pp

    def process(self, source, multiple):
        '''Front ends customize how they handle the token stream here.'''
        self.pp.push_main_source_file(source, multiple)


class PreprocessedOutput(FrontEndBase, PreprocessorActions):
    '''Consume tokens from the preprocessor and output the preprocessed source.'''

    help_group_name = 'preprocessed output'

    def __init__(self, pp):
        super().__init__(pp)
        self.at_bol = True
        self.write = None
        self.line_number = -1   # Presumed line number
        self.filename = None
        # Controlled from the command line
        self.suppress_linemarkers = False
        self.list_macros = False
        pp.actions = self

    def maybe_write_newline(self):
        if not self.at_bol:
            self.write('\n')
            self.line_number += 1
        self.at_bol = True

    def start_new_line(self, loc, *, force):
        '''Start a new line if loc is on a different line to the current one.'''
        location = self.pp.locator.presumed_location(loc, True)
        line_number = location.presumed_line_number
        if line_number != self.line_number or force:
            self.maybe_write_newline()
            count = line_number - self.line_number
            self.line_number = line_number
            if not self.suppress_linemarkers:
                if 0 <= count < 8:
                    self.write('\n' * count)
                else:
                    self.write_line_marker(False)
        return location

    def write_line_marker(self, write_filename):
        '''Write a line marker.  On return self.at_bol is True.'''
        if not self.suppress_linemarkers:
            if write_filename:
                self.write(f'#line {self.line_number} {self.filename}\n')
            else:
                self.write(f'#line {self.line_number}\n')

    def write_line(self, text_line, loc):
        '''Start a new line and output a text line that may contain embedded newlines.'''
        self.start_new_line(loc, force=True)
        self.write(text_line)
        self.line_number += text_line.count('\n')

    def on_source_file_change(self, loc, reason):
        self.maybe_write_newline()
        location = self.pp.locator.presumed_location(loc, True)
        file_name_changed = self.filename != location.presumed_filename
        self.line_number = location.presumed_line_number
        self.filename = location.presumed_filename
        self.write_line_marker(file_name_changed)

    def on_define(self, macro):
        if not self.list_macros:
            return
        name = macro.macro_name(self.pp).decode()
        self.write_line(f'#define {name}{macro.definition_text(self.pp)}\n', macro.name_loc)

    def on_undef(self, token):
        if not self.list_macros:
            return
        name = token.extra.spelling.decode()
        self.write_line(f'#undef {name}\n', token.loc)

    def on_pragma(self, token):
        def parts(token):
            pp = self.pp
            yield '#pragma '
            not_first = False
            while token.kind != TokenKind.EOF:
                if not_first and token.flags & TokenFlags.WS:
                    yield ' '
                yield pp.token_spelling(token).decode()
                token = pp.get_token()
                not_first = True
            yield '\n'

        self.write_line(''.join(parts(token)), token.loc)
        return False

    def process(self, source, multiple):
        # Set self.write first as we will immediately get on_source_file_change() callback
        write = self.write = self.pp.stdout.write
        super().process(source, multiple)
        pp = self.pp
        loc = None
        spelling = None

        while True:
            token = pp.get_token()
            if token.kind == TokenKind.EOF:
                break

            location = self.start_new_line(token.loc, force=False)
            if self.at_bol:
                if location.column_offset > 1:
                    write(' ' * location.column_offset)
            elif token.flags & TokenFlags.WS:
                write(' ')
            elif self.separate_tokens(loc, spelling, token):
                write(' ')

            loc = token.loc
            spelling = pp.token_spelling(token)
            write(spelling.decode())
            self.line_number += spelling.count(b'\n')
            self.at_bol = False

        self.maybe_write_newline()

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
        token, consumed = self.pp.lex_spelling_quietly(spelling)

        # Case 1: if it formed a different token we need a space
        if consumed != len(lhs_spelling):
            return True

        # Case 2 above
        return (lhs_spelling == b'.' and rhs.kind == TokenKind.DOT
                or (lhs_spelling == b'%:' and rhs.kind == TokenKind.MODULUS)
                or (token.kind == TokenKind.NUMBER and rhs.kind == TokenKind.CHARACTER_LITERAL))


class FrontEnd(FrontEndBase):
    '''Simulate a compiler front end.  For now, all it does is output consumed tokens, and the
    interpretation of literals.
    '''

    help_group_name = 'token dumper'

    def process(self, source, multiple):
        '''Act like a front-end, consuming tokens and evaluating literals.  At present
        this is used for debugging purposes.'''
        super().process(source, multiple)
        get_token = self.pp.get_token
        interpreter = self.pp.literal_interpreter
        write = self.pp.stdout.write
        token = get_token()
        while token.kind != TokenKind.EOF:
            write(token.to_short_text())
            write('\n')
            if token.kind == TokenKind.STRING_LITERAL:
                literal, token = interpreter.concatenate_strings(token)
                write(literal.to_short_text())
                write('\n')
            else:
                if token.kind == TokenKind.CHARACTER_LITERAL or token.kind == TokenKind.NUMBER:
                    literal = interpreter.interpret(token)
                    write(literal.to_short_text())
                    write('\n')
                token = get_token()
