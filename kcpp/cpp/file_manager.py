# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The file manager.'''

from dataclasses import dataclass
from enum import IntEnum, auto


__all__ = ['FileManager']


class DirectoryKind(IntEnum):
    quoted = auto()
    angled = auto()
    system = auto()
    standard = auto()
    final = auto()


@dataclass(slots=True)
class IncludeDirectory:
    '''This describes a directory on a search path.'''
    path: str
    kind: DirectoryKind
    exists: bool

    def is_system(self):
        return self.kind in (DirectoryKind.system, DirectoryKind.standard, DirectoryKind.final)


@dataclass(slots=True)
class SearchResult:
    '''This describes the result of a header search.'''
    # The include directory it was found in.  None for absolute header names.
    directory: IncludeDirectory
    # The path of the header file found.  This will begin with directory.path, then the
    # header name, and finally any suffix that was used.
    path: str


@dataclass(slots=True)
class FileContents:
    # The contents of the file
    raw: bytes
    # The most recent content modification of the file
    mtime_ns: int



class FileManager:
    '''The file manager caches the content of files, maintains the include file search paths,
    and looks up header names on the search path.  It also keeps track of the include
    stack.
    '''
    def __init__(self, host):
        self.host = host
        self.file_stack = []        # a list of SearchResult objects
        self.current_file_search = True
        # Lists of include directories
        self.user_quoted = []       # -iquote.   for "" searches only
        self.user_angled = []       # -I.  <> searches start here
        self.user_system = []       # -isystem
        self.standard = []          # a function of the target
        self.user_final = []        # -idirafter
        # Each suffix is appended to suffix-less header names and tried in turn
        self.suffixes = ['']
        # Cache.  Map from a SearchKey instance to a SearchResult
        self.cache = {}

    def add_search_path(self, path, kind):
        '''Add path to the include search path.  is_quote is True to add it to the search path for
        quoted file names, otherwise it is added to the search path for angled file names.
        is_system_dir is True if it is to be treated as a system directory (which may
        suppress some diagnostics in headers found under that path).
        '''
        if kind is DirectoryKind.quoted:
            dir_list = self.user_quoted
        elif kind is DirectoryKind.angled:
            dir_list = self.user_angled
        elif kind is DirectoryKind.system:
            dir_list = self.user_system
        elif kind is DirectoryKind.standard:
            dir_list = self.standard
        elif kind is DirectoryKind.final:
            dir_list = self.user_final

        stat_result = self.host.stat(path)
        exists = self.host.stat_is_directory(stat_result)
        dir_list.append(IncludeDirectory(path, kind, exists))

    def cache_lookup(self, search_key, search_cache_miss):
        result = self.cache.get(search_key, False)
        if result is False:
            result = search_cache_miss(search_key[0])
            self.cache[search_key] = result
        return result

    def lookup_in_directory(self, header_name, directory):
        if directory:
            path = self.host.path_join(directory.path, header_name)
        else:
            path = header_name

        # Only accept regular files
        stat_result = self.host.stat(path)
        if not stat_result:
            return None
        if not self.host.stat_is_regular_file(stat_result):
            return None
        return SearchResult(directory, path)

    def search_directory(self, header_name, directory):
        if directory and not directory.exists:
            return None

        return self.cache_lookup((header_name, dire
        root, suffix = self.host.path_splitext(header_name)
        if suffix:
            return self.lookup_in_directory(header_name, directory)
        for suffix in self.suffix_list:
            result = self.lookup_in_directory(header_name + suffix, directory)
            if result:
                return result
        return None

    def search_directory_lists(self, header_name, dir_lists):
        for dir_list in dir_lists:
            for directory in dir_list:
                result = self.search_directory(header_name, directory)
                if result:
                    return result
        return None

    def search_quoted_dirlists_cache_miss(self, header_name):
        # Try quoted header directory list
        result = self.search_directory_lists(header_name, [self.user_quoted])
        if result:
            return result
        # Finally, search as an angled header
        return self.search_angled_header(self, header_name)

    def search_quoted_header(self, header_name):
        if self.host.path_is_absolute(header_name):
            return self.search_absolute(header_name)

        def search_quoted_cache_miss(self, header_name):
            if self.current_file_search:
                is_system = entry.directory.is_system() if entry.directory else False
                directory = IncludeDirectory(dirname, is_system, True)
                result = self.search_directory(header_name, directory)
                if result:
                    return result
            return self.cache_lookup((header_name, 1), self.search_quoted_dirlists_cache_miss)

        # Search in the directory of the current file
        entry = self.file_stack[-1]
        dirname = self.host.path_dirname(entry.path)
        return self.cache_lookup((header_name, 1, dirname), search_quoted_cache_miss)

    #
    # Absolute filenames
    #

    def search_absolute_cache_miss(self, header_name):
        return self.search_directory(header_name, None)

    def search_absolute(self, header_name):
        return self.cache_lookup(header_name, self.search_absolute_cache_miss)

    #
    # Angled headers
    #

    def search_angled_cache_miss(self, header_name):
        # Search in the standard and final
        angled_lists = [self.user_angled, self.user_system, self.standard, self.user_final]
        return self.search_directory_lists(header_name, angled_lists)

    def search_angled_not_absolute(self, header_name):
        return self.cache_lookup((header_name, 0), self.search_angled_cache_miss)

    def search_angled_header(self, header_name):
        if self.host.path_is_absolute(header_name):
            return self.search_absolute(header_name)
        return self.search_angled_not_absolute(header_name)

    #


    def read_main_file(self, filename):
        search_result = self.lookup_in_directory(filename, None)
        if search_result:
            return self.read_and_push_search_result(search_result)
        return None

    def read_and_push_search_result(self, search_result):
        return self.host.read_file(search_result.path)
