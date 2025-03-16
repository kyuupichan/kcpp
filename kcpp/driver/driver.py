# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The compiler driver.'''

import os
import sys

from .processors import PreprocessedOutput, FrontEnd
from kcpp.cpp import Preprocessor
from kcpp.diagnostics import UnicodeTerminal


class Driver:

    def __init__(self):
        self.parser = Preprocessor.argument_parser()

    def run(self, argv=None, environ=None):
        if environ is None:
            environ = os.environ
        command_line = self.parser.parse_args(argv)
        if command_line.fe:
            processor = PreprocessedOutput(command_line, environ)
        else:
            processor = FrontEnd(command_line, environ)
        terminal = UnicodeTerminal(command_line, environ)

        for filename in command_line.files:
            pp = Preprocessor(command_line, environ)
            pp.add_diagnostic_consumer(terminal)
            pp.push_source_file(filename)
            processor.run(pp)
            # FIXME: put this somewhere more appropriate
            if pp.diags:
                print(f'{len(pp.diags):,d} diagnostics emitted', file=sys.stderr)
