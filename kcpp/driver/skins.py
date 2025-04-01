# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Driver skins.'''

import argparse

from kcpp.cpp import Environment

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

        return parser


class KCPP(Skin):

    def add_preprocessed_output_commands(self, group):
        group.add_argument('-P', help='suppress generation of linemarkers', action='store_true',
                           default=False)
        group.add_argument('--list-macros', help='output macro definitions', action='store_true',
                           default=False)

    def add_frontend_commands(self, group):
        # There are none
        pass

    def add_preprocessor_commands(self, group):
        group.add_argument('-exec-charset', type=str,
                           help='set the narrow execution character set')
        group.add_argument('-wide-exec-charset', type=str,
                           help='set the wide execution character set')
        group.add_argument('-D', '--define-macro', action='append', default=[],
                           help='''In -D name[(param-list)][=def], define macro 'name' as
                           'def'.  If 'def' is omitted 'name' is defined to 1.  Function-like
                           macros can be defined by specifying a parameter list.''')
        group.add_argument('-U', '--undefine-macro', action='append', default=[],
                           help='''Remove the definition of a macro.
                           -U options are processed after all -D options.''')

    def add_diagnostic_commands(self, group):
        group.add_argument('--tabstop', nargs='?', default=8, type=int)
        group.add_argument('--colours', action=argparse.BooleanOptionalAction, default=True)
