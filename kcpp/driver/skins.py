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


class Skin:

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

    def parser(self, frontend):
        parser = argparse.ArgumentParser(
            prog='kcpp',
            description='A preprocessor for C++23 writen in Python',
        )

        parser.add_argument('files', metavar='files', nargs='*', default=['-'],
                            help='files to preprocess')

        if isinstance(frontend, PreprocessedOutput):
            group = parser.add_argument_group(title='preprocessed output')
            self.add_preprocessed_output_commands(group)
        elif isinstance(frontend, FrontEnd):
            group = parser.add_argument_group(title='C++ frontend')
            self.add_frontend_commands(group)

        pp_group = parser.add_argument_group(title='preprocessor')
        self.add_preprocessor_commands(pp_group)

        diag_group = parser.add_argument_group(title='diagnostics')
        self.add_diagnostic_commands(diag_group)

        Preprocessor.add_arguments(pp_group, diag_group)
        UnicodeTerminal.add_arguments(diag_group)
        return parser


class KCPP(Skin):

    def add_preprocessed_output_commands(self, group):
        group.add_argument('-P', help='suppress generation of linemarkers', action='store_true',
                           default=False)
        group.add_argument('--list-macros', help='output macro definitions', action='store_true',
                           default=False)

    def add_frontend_commands(self, group):
        pass

    def add_preprocessor_commands(self, group):
        pass

    def add_diagnostic_commands(self, group):
        pass
