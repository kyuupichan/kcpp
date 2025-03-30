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
import stat

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

    def path_dirname(self, path):
        return os.path.dirname(path)

    def path_is_absolute(self, path):
        return os.path.isabs(path)

    def path_join(self, lhs, rhs):
        return os.path.join(lhs, rhs)

    def path_splitext(self, path):
        return os.path.splitext(path)

    def stat(self, path):
        try:
            return os.stat(path, follow_symlinks=True)
        except OSError:
            return None

    def fstat(self, fileno):
        return os.fstat(fileno)

    def stat_is_directory(self, stat_result):
        return stat.S_ISDIR(stat_result.mode)

    def stat_is_regular_file(self, stat_result):
        return stat.S_ISREG(stat_result.mode)

    def stat_mtime_ns(self, stat_result):
        return stat_result.mtime_ns

    def stat_file_size(self, stat_result):
        return stat_result.st_size

    def read_file_contents(self, path):
        try:
            with open(path, 'rb') as f:
                stat_result = self.fstat(f)
                if not self.stat_is_regular_file(stat_result):
                    return (DID.cannot_open_file,
                # TODO: memory mapped files
                if stat_result.st_size > 16_384:
                    pass
                return f.read()
        except OSError as e:
            return (DID.cannot_open_file, str(e))


class HostPosix(Host):
    pass


class HostWindows(Host):
    pass
