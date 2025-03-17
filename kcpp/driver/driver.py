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
from kcpp.cpp.basic import Environment
from kcpp.diagnostics import UnicodeTerminal


__all__ = ['Driver', 'main_cli']


class Driver:

    def __init__(self):
        parser = argparse.ArgumentParser(
            prog='kcpp',
            description='A preprocessor for C++23 writen in Python',
        )
        parser.add_argument('files', metavar='files', nargs='*', default=['-'],
                            help='files to preprocess')
        parser.add_argument('--fe', help='emulate a front end', action='store_true')
        pp_group = parser.add_argument_group(title='preprocessor')
        diag_group = parser.add_argument_group(title='diagnostics')
        Preprocessor.add_arguments(pp_group, diag_group)
        UnicodeTerminal.add_arguments(diag_group)
        self.parser = parser

    def run(self, argv=None, environ=None):
        command_line = self.parser.parse_args(argv)
        environ = os.environ if environ is None else environ
        environment = Environment(command_line, environ, [])
        terminal = UnicodeTerminal(environment)

        if command_line.fe:
            processor = PreprocessedOutput()
        else:
            processor = FrontEnd()

        for filename in command_line.files:
            pp = Preprocessor(environment)
            pp.add_diagnostic_consumer(terminal)
            pp.diagnostic_engine.emit(environment.diagnostics)
            pp.push_source_file(filename)
            processor.run(pp)
            # FIXME: put this somewhere more appropriate
            if pp.diags:
                print(f'{len(pp.diags):,d} diagnostics emitted', file=sys.stderr)


def main_cli():
    driver = Driver()
    driver.run()
