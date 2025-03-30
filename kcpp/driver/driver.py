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
        assert isinstance(argv, (str, list, type(None)))
        if isinstance(argv, str):
            argv = shlex.split(argv)
        command_line = self.parser.parse_args(argv)
        environ = os.environ if environ is None else environ
        return Environment(command_line, environ, [])

    def frontend(self, env):
        return FrontEnd() if env.command_line.fe else PreprocessedOutput()

    def run(self, argv=None, environ=None, frontend=None):
        env = self.environment(argv, environ)
        frontend = frontend or self.frontend(env)
        for source in env.command_line.files:
            frontend.run(source, env)


def main_cli():
    driver = Driver()
    driver.run()
