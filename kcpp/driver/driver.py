# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The compiler driver.'''

import argparse
import os
import shlex

from kcpp.cpp import Preprocessor, Environment
from kcpp.diagnostics import UnicodeTerminal

from .frontends import PreprocessedOutput, FrontEnd


__all__ = ['Driver', 'main_cli']


class Driver:

    def frontend_class(self, argv):
        # Use a basic parser to figure out what frontend has been selected.  Then we can
        # get a more accurate set of arguments to add
        parser = argparse.ArgumentParser(add_help=False)
        parser.add_argument('--fe', help='emulate a front end', action='store_true')
        namespace, argv = parser.parse_known_args(argv)
        return FrontEnd if namespace.fe else PreprocessedOutput

    def parser(self, frontend_class):
        parser = argparse.ArgumentParser(
            prog='kcpp',
            description='A preprocessor for C++23 writen in Python',
        )

        parser.add_argument('files', metavar='files', nargs='*', default=['-'],
                            help='files to preprocess')
        frontend_class.update_argument_parser(parser)
        pp_group = parser.add_argument_group(title='preprocessor')
        diag_group = parser.add_argument_group(title='diagnostics')
        Preprocessor.add_arguments(pp_group, diag_group)
        UnicodeTerminal.add_arguments(diag_group)
        return parser

    def frontend_class_and_environment(self, argv=None, environ=None, frontend_class=None):
        assert isinstance(argv, (str, list, type(None)))
        if isinstance(argv, str):
            argv = shlex.split(argv)
        if not frontend_class:
            frontend_class = self.frontend_class(argv)
        parser = self.parser(frontend_class)
        command_line = parser.parse_args(argv)
        environ = os.environ if environ is None else environ
        env = Environment(command_line, environ, [])
        frontend = frontend_class(env)
        return frontend, env

    def run(self, argv=None, environ=None, frontend_class=None):
        frontend, env = self.frontend_class_and_environment(argv, environ, frontend_class)
        sources = env.command_line.files
        for source in sources:
            frontend.run(source, env)


def main_cli():
    driver = Driver()
    driver.run()
