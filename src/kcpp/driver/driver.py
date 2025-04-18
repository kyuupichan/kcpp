# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The compiler driver.'''

import os
import sys
import shlex

from .frontends import PreprocessedOutput
from .skins import Skin


__all__ = ['Driver', 'cpp_cli', 'cc_cli']


class Driver:

    def __init__(self, default_frontend_class):
        self.default_frontend_class = default_frontend_class

    def run(self, argv=None, environ=None, frontend_class=None):
        assert isinstance(argv, (str, list, type(None)))
        if isinstance(argv, str):
            argv = shlex.split(argv)
        else:
            argv = sys.argv[1:]
        environ = os.environ if environ is None else environ
        frontend_class = frontend_class or self.default_frontend_class
        skin, sources = Skin.skin(argv, environ, frontend_class)

        exit_code = 0
        for source in sources:
            exit_code = max(exit_code, skin.run(source, len(sources) > 1))
        return exit_code


def cpp_cli():
    driver = Driver(PreprocessedOutput)
    sys.exit(driver.run())


def cc_cli():
    from kcpp.cc import Compiler
    driver = Driver(Compiler)
    sys.exit(driver.run())
