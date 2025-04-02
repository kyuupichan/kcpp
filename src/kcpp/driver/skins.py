# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Driver skins.'''

import argparse

from kcpp.cpp import Preprocessor
from kcpp.diagnostics import UnicodeTerminal

from .frontends import PreprocessedOutput, FrontEnd


__all__ = ['KCPP']


class Skin:

    c_suffixes = ['.c']

    def __init__(self):
        self.command_line = None
        self.environ = None
        self.frontend_class = None

    def parser(self, frontend_class):
        parser = argparse.ArgumentParser(
            prog='kcpp',
            description='A preprocessor for C++23 writen in Python',
        )

        parser.add_argument('files', metavar='files', nargs='*', default=['-'],
                            help='files to preprocess')

        group = parser.add_argument_group(frontend_class.help_group_name)
        self.add_frontend_commands(group, frontend_class)

        pp_group = parser.add_argument_group(title='preprocessor')
        self.add_preprocessor_commands(pp_group)

        diag_group = parser.add_argument_group(title='diagnostics')
        self.add_diagnostic_commands(diag_group)

        return parser

    @classmethod
    def skin(cls, argv, environ):
        '''Determine the skin to use from the command line / environment.'''
        return KCPP()

    def sources_to_run(self, argv, environ, frontend_class):
        if frontend_class is None:
            try:
                argv.remove('--tokens')
                frontend_class = FrontEnd
            except ValueError:
                frontend_class = PreprocessedOutput

        parser = self.parser(frontend_class)
        self.command_line = parser.parse_args(argv)
        self.environ = environ
        self.frontend_class = frontend_class
        return self.command_line.files

    def run(self, source):
        pp = Preprocessor()
        # Set up diagnostics first so that they are customized as early as possible.
        frontend = self.frontend_class(pp)
        consumer = frontend.diagnostic_class(pp)
        pp.set_diagnostic_consumer(consumer)
        self.customize_diagnostics(consumer, pp.host)

        # Next customize the preprocessor and then initialize it
        self.customize_and_initialize_preprocessor(pp, source)

        # Finally customize the front end
        self.customize_frontend(frontend)

        # Process the source
        frontend.process_source(source)

        # Tidy up
        return pp.finish()


class KCPP(Skin):

    COLOURS_ENVVAR = 'KCPP_COLOURS'
    DEFAULT_COLOURS = (
        'error=1;31:warning=1;35:note=1;36:remark=1;34:'
        'path=1:caret=1;32:locus=1;32:range1=34:range2=34:quote=1:unprintable=7'
    )

    def add_frontend_commands(self, group, frontend_class):
        if issubclass(frontend_class, PreprocessedOutput):
            group.add_argument('-P', help='suppress generation of linemarkers',
                               action='store_true', default=False)
            group.add_argument('--list-macros', help='output macro definitions',
                               action='store_true', default=False)

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

    def customize_and_initialize_preprocessor(self, pp, source):
        if any(source.endswith(suffix) for suffix in self.c_suffixes):
            pp.language.kind = 'C'
        pp.set_command_line_macros(self.command_line.define_macro,
                                   self.command_line.undefine_macro)
        pp.initialize(exec_charset=self.command_line.exec_charset,
                      wide_exec_charset=self.command_line.wide_exec_charset)

    def customize_frontend(self, frontend):
        if isinstance(frontend, PreprocessedOutput):
            frontend.suppress_linemarkers = self.command_line.P
            frontend.list_macros = self.command_line.list_macros

    def customize_diagnostics(self, consumer, host):
        if isinstance(consumer, UnicodeTerminal):
            consumer.tabstop = self.command_line.tabstop
            if self.command_line.colours and host.terminal_supports_colours(self.environ):
                colour_string = self.environ.get(self.COLOURS_ENVVAR, self.DEFAULT_COLOURS)
                consumer.set_sgr_code_assignments(colour_string)
            # FIXME: the stderr redirected file
            if host.is_a_tty(consumer.file):
                consumer.terminal_width = host.terminal_width(consumer.file)
