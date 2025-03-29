# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The file manager.'''

import os
from dataclasses import dataclass

__all__ = ['FileManager']


@dataclass(slots=True)
class IncludeDirectory:
    path: str
    is_system_dir: bool
    non_existent: bool


class FileManager:
    '''The file manager caches the content of files, manages the include paths, and looks up
    included files on the search path.
    '''

    def __init__(self, host):
        self.host = host
        # Lists of include directories
        self.quote_include_dirs = []
        self.angle_include_dirs = []

    def add_include_dir(self, path, is_quote, is_system_dir):
        '''Add path to the include search path.  is_quote is True to add it to the search path for
        quoted file names, otherwise it is added to the search path for angled file names.
        is_system_dir is True if it is to be treated as a system directory (which may
        suppress some diagnostics in headers found under that path).
        '''
        dirs = self.quote_include_dirs if is_quote else self.angle_include_dirs
        non_existent = not self.host.is_directory(path)
        dirs.append(IncludeDirectory(path, is_system_dir, non_existent))

    def header_exists(self, header_name):
        stat = self.host.file_stat(header_name)
        return stat

    def lookup_in_include_dir(self, header_name, include_dir):
        if include_dir.non_existent:
            return None
        return self.lookup_file(os.path.join(include_dir.path, header_name))

    def lookup_in_include_dirs(self, header_name, include_dirs):
        for include_dir in include_dirs:
            result = self.lookup_in_include_dir(header_name, include_dir)
            if result:
                return result
        return None

    def lookup_header(self, header_name, is_quote):
        if is_quote:
            result = self.lookup_in_include_dirs(header_name, self.quote_include_dirs)
            if result:
                return result
        return self.lookup_in_include_dirs(header_name, self.angle_include_dirs)

    def read_file(self):
        pass
