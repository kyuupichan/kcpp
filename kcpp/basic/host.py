# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Host abstraction.  For a Python implementation Python essentially abstracts the host so
most logic here appears in HostBase.  However for a C or C++ implementation there should
be an abstraction layer.
'''

import abc
import os

__all__ = ['Host']


class Host(abc.ABC):
    '''Base class of the host abstraction.'''

    @staticmethod
    def host():
        '''Return an instance of Host.'''
        if os.name == 'posix':
            return HostPosix()
        elif os.name == 'nt':
            return HostWindows()
        else:
            raise RuntimeError('unsupported host')

    def is_a_tty(self, file):
        '''Return True if file is connected to a terminal device.'''
        return file.isatty()

    def terminal_width(self, file):
        '''Return the terminal width.'''
        return os.get_terminal_size(file.fileno()).columns

    def terminal_supports_colours(self, variables):
        '''Return True if the environment variables (a dict) indicate that the terminal
        supports colours.
        '''
        term = variables.get('TERM', '')
        if term in 'ansi cygwin linux'.split():
            return True
        if any(term.startswith(prefix) for prefix in 'screen xterm vt100 rxvt'.split()):
            return True
        return term.endswith('color')


class HostPosix(Host):
    pass


class HostWindows(Host):
    pass
