# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The compiler driver.'''

import argparse
import os
import sys
import shlex

from kcpp.cpp import Preprocessor, Environment
from kcpp.diagnostics import UnicodeTerminal

from .frontends import PreprocessedOutput, FrontEnd


__all__ = ['Driver', 'main_cli']


class Driver:

    def frontend_class(self, argv):
        try:
            argv.remove('--tokens')
            result = FrontEnd
        except ValueError:
            result = PreprocessedOutput
        return result, argv

    def parser(self, frontend):
        parser = argparse.ArgumentParser(
            prog='kcpp',
            description='A preprocessor for C++23 writen in Python',
        )

        parser.add_argument('files', metavar='files', nargs='*', default=['-'],
                            help='files to preprocess')
        frontend.update_argument_parser(parser)
        pp_group = parser.add_argument_group(title='preprocessor')
        diag_group = parser.add_argument_group(title='diagnostics')
        Preprocessor.add_arguments(pp_group, diag_group)
        UnicodeTerminal.add_arguments(diag_group)
        return parser

    def environment(self, argv, environ, frontend):
        parser = self.parser(frontend)
        command_line = parser.parse_args(argv)
        environ = os.environ if environ is None else environ
        return Environment(command_line, environ, [])

    def run(self, argv=None, environ=None, frontend_class=None):
        assert isinstance(argv, (str, list, type(None)))
        if isinstance(argv, str):
            argv = shlex.split(argv)
        else:
            argv = sys.argv[1:]
        if frontend_class is None:
            frontend_class, argv = self.frontend_class(argv)
        frontend = frontend_class()
        env = self.environment(argv, environ, frontend)
        frontend.customize(env)

        sources = env.command_line.files
        exit_code = 0
        for source in sources:
            exit_code = max(exit_code, frontend.run(source))
        return exit_code


def main_cli():
    driver = Driver()
    sys.exit(driver.run())
