# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The compiler driver.'''

import argparse
import sys

from cpp import Preprocessor, Token, TokenKind, TokenFlags
from cpp.basic import TargetMachine
from diagnostics import DID, UnicodeTerminal


def preprocess(pp, token):
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
                    write(b' ' * (eloc.column_offset - 1))  # One will be done below for WS flag
        if token.flags & TokenFlags.WS:
            write(b' ')
        write(pp.token_spelling(token.loc))

    write(b'\n')
    print(f'{count} tokens', file=sys.stderr)


def frontend(pp, token):
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


def process(filename, args):
    target = TargetMachine.default()
    target.set_narrow_encoding(args.exec_charset)
    target.set_wide_encoding(args.wide_exec_charset)

    pp = Preprocessor(target)
    pp.add_diagnostic_consumer(UnicodeTerminal(tabstop=args.tabstop, colours=args.colours))
    try:
        with open(filename, 'rb') as f:
            raw = f.read()
    except OSError as e:
        print(f'error: unable to open {filename}: {e}', file=sys.stderr)
        return

    pp.push_buffer(raw, name=filename)
    token = Token.create()

    if args.fe:
        frontend(pp, token)
    else:
        preprocess(pp, token)

    if pp.diags:
        print(f'{len(pp.diags):,d} diagnostics emitted', file=sys.stderr)



class Driver:

    def __init__(self):
        pass

    def run(self, argv, environ):
        parser = argparse.ArgumentParser(
            prog='cpp',
        )

        parser.add_argument('files', metavar='files', nargs='*', default=[sys.stdin],
                            help='files to preprocess')
        parser.add_argument('--fe', action='store_true')
        parser.add_argument('-exec-charset', type=str)
        parser.add_argument('-wide-exec-charset', type=str)
        parser.add_argument('--tabstop', nargs='?', default=8, type=int)
        parser.add_argument('--colours', action=argparse.BooleanOptionalAction, default=True)
        args = parser.parse_args(argv)

        for filename in args.files:
            process(filename, args)
