# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Host abstraction.  For a Python implementation Python essentially abstracts the host so
most logic here appears in HostBase.  However for a C or C++ implementation there should
sbe an abstraction layer.
'''

import abc
import os
import platform
import stat

__all__ = ['host']


class Host(abc.ABC):
    '''Base class of the host abstraction.'''

    @staticmethod
    def host():
        '''Return an instance of Host.'''
        if os.name == 'posix':
            if platform.system() == 'Darwin':
                return MacOSX()
            return Posix()
        elif os.name == 'nt':
            return Windows()
        else:
            raise RuntimeError('unsupported host')

    def standard_search_paths(self, is_cplusplus):
        '''This is really a function of the target, but we currently don't have a proper
        target abstraction.'''
        return []

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

    def path_split(self, path):
        return os.path.split(path)

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
        return stat.S_ISDIR(stat_result.st_mode)

    def stat_is_regular_file(self, stat_result):
        return stat.S_ISREG(stat_result.st_mode)

    def stat_mtime_ns(self, stat_result):
        return stat_result.st_mtime_ns

    def stat_file_size(self, stat_result):
        return stat_result.st_size

    def read_file(self, path, *, nul_terminate):
        '''Python returns an OSError if reading a directory.  A C or C++ implementation would
        have to do it itself.'''
        try:
            with open(path, 'rb') as f:
                result = f.read()
            if nul_terminate:
                result += b'\0'
            return result
        except OSError as e:
            return e.strerror

    def open_file_for_writing(self, path):
        '''Open a file for writing.  Return a file object, or an error string.'''
        try:
            return open(path, 'w')
        except OSError as e:
            return e.strerror


class Posix(Host):

    def standard_search_paths(self, is_cplusplus):
        return ['/usr/include']


class MacOSX(Host):

    def standard_search_paths(self, is_cplusplus):
        # This is what I can glean from Clang
        prefix = '/Applications/Xcode.app/Contents/Developer/'
        suffixes = [
            # This first one is only present if compiling C++
            'Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/c++/v1',
            'Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/17/include',
            'Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include',
            'Toolchains/XcodeDefault.xctoolchain/usr/include',
            # We don't support frameworks (yet)
            # 'Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/Frameworks',
        ]

        start = 0 if is_cplusplus else 1
        return [prefix + suffix for suffix in suffixes[start:]]


class Windows(Host):
    pass


host = Host.host()
