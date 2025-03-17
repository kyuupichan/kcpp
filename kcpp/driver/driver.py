# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The compiler driver.'''

import argparse
import os
import sys

from .processors import PreprocessedOutput, FrontEnd
from kcpp.cpp import Preprocessor
from kcpp.diagnostics import UnicodeTerminal


__all__ = ['Driver', 'main_cli']


class Driver:

    def __init__(self):
        parser = argparse.ArgumentParser(
            prog='kcpp',
            description = 'A preprocessor for C++23 writen in Python',
        )
        parser.add_argument('files', metavar='files', nargs='*', default=['-'],
                            help='files to preprocess')
        parser.add_argument('--fe', help='emulate a front end', action='store_true')
        group = parser.add_argument_group(title='preprocessor')
        Preprocessor.add_arguments(group)
        group = parser.add_argument_group(title='diagnostics')
        UnicodeTerminal.add_arguments(group)
        self.parser = parser

    def run(self, argv=None, environ=None):
        if environ is None:
            environ = os.environ

        command_line = self.parser.parse_args(argv)
        if command_line.fe:
            processor = PreprocessedOutput()
        else:
            processor = FrontEnd()
        terminal = UnicodeTerminal(command_line, environ)

        for filename in command_line.files:
            pp = Preprocessor(command_line, environ)
            pp.add_diagnostic_consumer(terminal)
            pp.push_source_file(filename)
            processor.run(pp)
            # FIXME: put this somewhere more appropriate
            if pp.diags:
                print(f'{len(pp.diags):,d} diagnostics emitted', file=sys.stderr)


def main_cli():
    driver = Driver()
    driver.run()
