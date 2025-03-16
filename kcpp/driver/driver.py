# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The compiler driver.'''

import argparse
import sys

from .pp_output import PreprocessedOutput


class Driver:

    def __init__(self):
        self.parser = self.default_argument_parser()

    def default_argument_parser(self):
        parser = argparse.ArgumentParser(
            prog='kcpp.py',
        )
        parser.add_argument('files', metavar='files', nargs='*', default=[sys.stdin],
                            help='files to preprocess')
        parser.add_argument('--fe', action='store_true')
        parser.add_argument('-exec-charset', type=str)
        parser.add_argument('-wide-exec-charset', type=str)
        parser.add_argument('--tabstop', nargs='?', default=8, type=int)
        parser.add_argument('--colours', action=argparse.BooleanOptionalAction, default=True)
        return parser

    def run(self, argv, environ):
        command_line = self.parser.parse_args(argv)
        for filename in command_line.files:
            processor = PreprocessedOutput()
            processor.run(command_line, environ, filename)
