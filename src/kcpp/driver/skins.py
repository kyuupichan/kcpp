# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Driver skins.'''

import argparse

from kcpp.cpp import Preprocessor, Config, Language
from kcpp.diagnostics import DiagnosticConfig, UnicodeTerminal

from .frontends import PreprocessedOutput, FrontEnd


__all__ = ['KCPP']


class Skin:

    c_suffixes = ['.c']
    COLOURS_ENVVAR = None
    DEFAULT_COLOURS = (
        'error=1;31:warning=1;35:note=1;36:remark=1;34:'
        'path=1:caret=1;32:locus=1;32:range1=34:range2=34:quote=1:unprintable=7'
    )
    SOURCE_DATE_EPOCH_ENVVAR = 'SOURCE_DATE_EPOCH'

    def __init__(self):
        self.command_line = None
        self.environ = None
        self.frontend_class = None

    @classmethod
    def skin(cls, argv, environ, frontend_class):
        '''Determine the skin to use from the command line / environment.'''
        parser = argparse.ArgumentParser(
            prog='kcpp',
            description='A preprocessor for C++23 writen in Python',
        )
        parser.add_argument('--skin', choices=['gcc', 'kcpp'], type=str.lower, default='kcpp')
        parser.add_argument('files', metavar='files', nargs='*', default=['-'],
                            help='files to preprocess')

        ns, _ = parser.parse_known_args(argv)
        if ns.skin == 'gcc':
            skin = GCC()
        else:
            skin = KCPP()

        if frontend_class is None:
            try:
                argv.remove('--tokens')
                frontend_class = FrontEnd
            except ValueError:
                frontend_class = PreprocessedOutput

        group = parser.add_argument_group(frontend_class.help_group_name)
        skin.add_frontend_commands(group, frontend_class)

        pp_group = parser.add_argument_group(title='preprocessor')
        skin.add_preprocessor_commands(pp_group)

        diag_group = parser.add_argument_group(title='diagnostics')
        skin.add_diagnostic_commands(diag_group)

        skin.command_line = parser.parse_args(argv)
        skin.environ = environ
        skin.frontend_class = frontend_class

        return skin, skin.command_line.files

    def run(self, source, multiple):
        # Create and initialize a preprocessor; if that succeeds create the frontend and
        # process the source file.
        pp = Preprocessor()
        if pp.initialize(self.preprocessor_configuration(pp, source)):
            frontend = self.create_frontend(pp)
            frontend.process(source, multiple)

        return pp.finish(source)


class KCPP(Skin):

    COLOURS_ENVVAR = 'KCPP_COLOURS'

    def add_frontend_commands(self, group, frontend_class):
        group.add_argument('-o', '--output', metavar='FILENAME', default='',
                           help='compilation output is written to FILENAME instaed of stdout')
        if issubclass(frontend_class, PreprocessedOutput):
            group.add_argument('-P', help='suppress generation of linemarkers',
                               action='store_true', default=False)
            group.add_argument('--list-macros', action='store_true', default=False,
                               help='output macro definitions with preprocessed source')

    def add_preprocessor_commands(self, group):
        group.add_argument('--target', type=str, metavar='TARGET', default='',
                           help='select the target machine')
        group.add_argument('--exec-charset', type=str, metavar='CHARSET',
                           help='set the narrow execution character set')
        group.add_argument('--wide-exec-charset', type=str, metavar='CHARSET',
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
        group.add_argument('--tabstop', metavar='WIDTH', nargs='?', default=8, type=int)
        group.add_argument('--colours', action=argparse.BooleanOptionalAction, default=True)

    def preprocessor_configuration(self, pp, source):
        config = Config.default(self.diagnostic_configuation(pp))
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

    def create_frontend(self, pp):
        frontend = self.frontend_class(pp)
        if isinstance(frontend, PreprocessedOutput):
            frontend.suppress_linemarkers = self.command_line.P
            frontend.list_macros = self.command_line.list_macros
        return frontend

    def diagnostic_configuation(self, pp):
        config = DiagnosticConfig.default()
        config.consumer = self.frontend_class.diagnostic_class()
        config.error_output = self.command_line.error_output
        config.worded_locations = True
        config.show_columns = False
        consumer = config.consumer
        if isinstance(consumer, UnicodeTerminal):
            consumer.tabstop = self.command_line.tabstop
            if self.command_line.colours and pp.host.terminal_supports_colours(self.environ):
                colour_string = self.environ.get(self.COLOURS_ENVVAR, self.DEFAULT_COLOURS)
                consumer.set_sgr_code_assignments(colour_string)
#            if pp.host.is_a_tty(pp.stderr):
#                consumer.terminal_width = pp.host.terminal_width(pp.stderr)
        return config


class GCC(Skin):

    COLOURS_ENVVAR = 'GCC_COLORS'
    DEFAULT_COLOURS = (
        'error=01;31:warning=01;35:note=01;36:range1=32:range2=34:locus=01:'
        'quote=01:path=01:fixit-insert=32:fixit-delete=31:'
        'diff-filename=01:diff-hunk=32:diff-delete=31:diff-insert=32:'
        'type-diff=01;32:fnname=01;32:targs=35:valid=01;31:invalid=01;32'
        'highlight-a=01;32:highlight-b=01;34'
    )

    def add_frontend_commands(self, group, frontend_class):
        if issubclass(frontend_class, PreprocessedOutput):
            group.add_argument('-P', help='suppress generation of linemarkers',
                               action='store_true', default=False)
            group.add_argument('-dD', help='output macro definitions with preprocessed source',
                               action='store_true', default=False)

    def add_preprocessor_commands(self, group):
        group.add_argument('-fexec-charset', type=str, metavar='CHARSET',
                           help='set the narrow execution character set')
        group.add_argument('-fwide-exec-charset', type=str, metavar='CHARSET',
                           help='set the wide execution character set')
        group.add_argument('-fmax-include-depth', type=int, default=-1, metavar='DEPTH',
                           help='set the maximum depth of nested source file inclusion')
        group.add_argument('-D', action='append', default=[],
                           metavar='NAME[(PARAM-LIST)][=DEF]',
                           help='''define macro NAME as DEF.  If DEF is omitted NAME is defined
                           to 1.  Function-like macros can be defined by specifying a
                           parameter list''')
        group.add_argument('-U', action='append', default=[], metavar='NAME',
                           help='''Remove the definition of a macro.
                           -U options are processed after all -D options.''')
        group.add_argument('-iquote', action='append', default=[], metavar='DIR',
                           help='''add a directory to the list of directories searched for ""
                           includes and before the -I directories''')
        group.add_argument('-I', action='append', default=[], metavar='DIR',
                           help='''add a directory to the list of directories searched for <>
                           includes before the system directories''')
        group.add_argument('-isystem', action='append', default=[], metavar='DIR',
                           help='''add a directory to the list of directories searched for <>
                           includes before the standard directories but after -I directories''')
        group.add_argument('-include', action='append', default=[], metavar='FILENAME',
                           help='''process FILENAME as if #include "FILENAME" as the first line
                           of the primary source file.  This happens after -D and -U options
                           are processed.''')

    def add_diagnostic_commands(self, group):
        group.add_argument('-ftabstop', metavar='WIDTH', nargs='?', default=8, type=int)
        group.add_argument('-fdiagnostics-color', action=argparse.BooleanOptionalAction,
                           default=True)

    def preprocessor_configuration(self, pp, source):
        config = Config.default()
        config.diagnostic_consumer = self.diagnostic_consumer(pp)
        if any(source.endswith(suffix) for suffix in self.c_suffixes):
            config.language = Language('C', 2023)
        config.narrow_exec_charset = self.command_line.fexec_charset
        config.wide_exec_charset = self.command_line.fwide_exec_charset
        config.source_date_epoch = self.environ.get(self.SOURCE_DATE_EPOCH_ENVVAR, '')
        config.max_include_depth = self.command_line.fmax_include_depth
        config.defines = self.command_line.D
        config.undefines = self.command_line.U
        config.includes = self.command_line.include
        config.quoted_dirs = self.command_line.iquote
        config.angled_dirs = self.command_line.I
        config.system_dirs = self.command_line.isystem
        return config

    def create_frontend(self, pp):
        frontend = self.frontend_class(pp)
        if isinstance(frontend, PreprocessedOutput):
            frontend.suppress_linemarkers = self.command_line.P
            frontend.list_macros = self.command_line.dD
        return frontend

    def diagnostic_configuation(self, pp):
        config = DiagnosticConfig.default()
        config.consumer = self.frontend_class.diagnostic_class()
        config.worded_locations = False
        config.show_columns = True
        consumer = config.consumer
        if isinstance(consumer, UnicodeTerminal):
            consumer.tabstop = self.command_line.ftabstop
            if self.command_line.fdiagnostics_color and pp.host.terminal_supports_colours(
                    self.environ):
                colour_string = self.environ.get(self.COLOURS_ENVVAR, self.DEFAULT_COLOURS)
                consumer.set_sgr_code_assignments(colour_string)
#            if pp.host.is_a_tty(pp.stderr):
#                consumer.terminal_width = pp.host.terminal_width(pp.stderr)
        return config
