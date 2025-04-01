# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The compiler driver.'''

import os
import sys
import shlex

from .skins import KCPP


__all__ = ['Driver', 'main_cli']


class Driver:

    def run(self, argv=None, environ=None, frontend_class=None):
        assert isinstance(argv, (str, list, type(None)))
        if isinstance(argv, str):
            argv = shlex.split(argv)
        else:
            argv = sys.argv[1:]
        environ = os.environ if environ is None else environ

        skin = KCPP()
        frontend, sources = skin.frontend_and_sources(argv, environ, frontend_class)
        exit_code = 0
        for source in sources:
            exit_code = max(exit_code, frontend.run(source))
        return exit_code


def main_cli():
    driver = Driver()
    sys.exit(driver.run())
