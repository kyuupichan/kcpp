# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Driver skins.'''

import argparse

from kcpp.cpp import Preprocessor, Config, Language
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

    def run(self, source, multiple):
        pp = Preprocessor()
        if multiple:
            pp.starting_compilation(source)

        # Create the front end, customize diagnostics, then initialize the preprocessor.
        # Finally customize the front end.
        frontend = self.frontend_class(pp)
        consumer = self.customize_diagnostics(frontend, pp)
        pp.initialize(self.preprocessor_configuration(consumer, source))
        self.customize_frontend(frontend, pp)

        # Now do the work and tidy up
        frontend.process_source(source)
        return pp.finish()


class KCPP(Skin):

    COLOURS_ENVVAR = 'KCPP_COLOURS'
    DEFAULT_COLOURS = (
        'error=1;31:warning=1;35:note=1;36:remark=1;34:'
        'path=1:caret=1;32:locus=1;32:range1=34:range2=34:quote=1:unprintable=7'
    )
    SOURCE_DATE_EPOCH_ENVVAR = 'SOURCE_DATE_EPOCH'

    def add_frontend_commands(self, group, frontend_class):
        group.add_argument('-o', '--output', metavar='FILENAME', default='',
                           help='compilation output is written to FILENAME instaed of stdout')
        if issubclass(frontend_class, PreprocessedOutput):
            group.add_argument('-P', help='suppress generation of linemarkers',
                               action='store_true', default=False)
            group.add_argument('--list-macros', help='output macro definitions',
                               action='store_true', default=False)

    def add_preprocessor_commands(self, group):
        group.add_argument('-target', type=str, metavar='TARGET', default='',
                           help='select the target machine')
        group.add_argument('-exec-charset', type=str, metavar='CHARSET',
                           help='set the narrow execution character set')
        group.add_argument('-wide-exec-charset', type=str, metavar='CHARSET',
                           help='set the wide execution character set')
        group.add_argument('--max-include-depth', type=int, default=-1, metavar='DEPTH',
                           help='set the maximum depth of nested source file inclusion')
        group.add_argument('-D', '--define-macro', action='append', default=[],
                           metavar='NAME[(PARAM-LIST)][=DEF]',
                           help='''define macro NAME as DEF.  If DEF is omitted NAME is defined
                           to 1.  Function-like macros can be defined by specifying a
                           parameter list''')
        group.add_argument('-U', '--undefine-macro', action='append', default=[], metavar='NAME',
                           help='''Remove the definition of a macro.
                           -U options are processed after all -D options.''')
        group.add_argument('--quoted-dir', action='append', default=[], metavar='DIR',
                           help='''add a directory to the list of directories searched for ""
                           includes and before the -I directories''')
        group.add_argument('-I', '--angled-dir', action='append', default=[], metavar='DIR',
                           help='''add a directory to the list of directories searched for <>
                           includes before the system directories''')
        group.add_argument('--system-dir', action='append', default=[], metavar='DIR',
                           help='''add a directory to the list of directories searched for <>
                           includes before the standard directories but after -I directories''')
        group.add_argument('--include', action='append', default=[], metavar='FILENAME',
                           help='''process FILENAME as if #include "FILENAME" as the first line
                           of the primary source file.  This happens after -D and -U options
                           are processed.''')

    def add_diagnostic_commands(self, group):
        group.add_argument('--error-output', metavar='FILENAME', default='',
                           help='diagnostic output is written to FILENAME instaed of stderr')
        group.add_argument('--tabstop', nargs='?', default=8, type=int)
        group.add_argument('--colours', action=argparse.BooleanOptionalAction, default=True)

    def preprocessor_configuration(self, consumer, source):
        config = Config.default()
        config.diagnostic_consumer = consumer
        config.error_output = self.command_line.error_output
        config.output = self.command_line.output
        if any(source.endswith(suffix) for suffix in self.c_suffixes):
            config.language = Language('C', 2023)
        config.target_name = self.command_line.target
        config.narrow_exec_charset = self.command_line.exec_charset
        config.wide_exec_charset = self.command_line.wide_exec_charset
        config.source_date_epoch = self.environ.get(self.SOURCE_DATE_EPOCH_ENVVAR, '')
        config.max_include_depth = self.command_line.max_include_depth
        config.defines = self.command_line.define_macro
        config.undefines = self.command_line.undefine_macro
        config.includes = self.command_line.include
        config.quoted_dirs = self.command_line.quoted_dir
        config.angled_dirs = self.command_line.angled_dir
        config.system_dirs = self.command_line.system_dir
        return config

    def customize_frontend(self, frontend, pp):
        if isinstance(frontend, PreprocessedOutput):
            frontend.suppress_linemarkers = self.command_line.P
            frontend.list_macros = self.command_line.list_macros

    def customize_diagnostics(self, frontend, pp):
        consumer = frontend.diagnostic_class(pp)
        if isinstance(consumer, UnicodeTerminal):
            consumer.tabstop = self.command_line.tabstop
            if self.command_line.colours and pp.host.terminal_supports_colours(self.environ):
                colour_string = self.environ.get(self.COLOURS_ENVVAR, self.DEFAULT_COLOURS)
                consumer.set_sgr_code_assignments(colour_string)
            if pp.host.is_a_tty(pp.stderr):
                consumer.terminal_width = pp.host.terminal_width(pp.stderr)
        return consumer
