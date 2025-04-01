# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Driver skins.'''

import argparse

from kcpp.cpp import Environment, Preprocessor
from kcpp.diagnostics import UnicodeTerminal

from .frontends import PreprocessedOutput, FrontEnd


__all__ = ['KCPP']


class KCPP:

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

    def frontend_and_sources(self, argv, environ, frontend_class):
        if frontend_class is None:
            try:
                argv.remove('--tokens')
                frontend_class = FrontEnd
            except ValueError:
                frontend_class = PreprocessedOutput

        frontend = frontend_class()
        parser = self.parser(frontend)

        command_line = parser.parse_args(argv)
        env = Environment(command_line, environ)
        frontend.customize(env)
        sources = env.command_line.files

        return frontend, sources
