# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The compiler driver.'''

import argparse
import os

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

    def environment(self, argv=None, environ=None):
        command_line = self.parser.parse_args(argv)
        environ = os.environ if environ is None else environ
        return Environment(command_line, environ, [])

    def run(self, argv=None, environ=None):
        env = self.environment(argv, environ)
        terminal = UnicodeTerminal(env)

        if env.command_line.fe:
            processor = PreprocessedOutput()
        else:
            processor = FrontEnd()

        for filename in env.command_line.files:
            pp = Preprocessor(env)
            pp.add_diagnostic_consumer(terminal)
            pp.diagnostic_engine.emit(env.diagnostics)
            if pp.diagnostic_engine.error_count == 0:
                pp.push_source_file(filename)
                processor.run(pp)
            pp.diagnostic_engine.emit_error_count()


def main_cli():
    driver = Driver()
    driver.run()
