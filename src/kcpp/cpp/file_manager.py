# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''The file manager.'''

from dataclasses import dataclass
from enum import IntEnum, auto

from ..core import host


__all__ = ['FileManager']


class DirectoryKind(IntEnum):
    none = auto()          # For e.g. the directory of the main file
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
class FileContents:
    # The raw bytes of the file, or a strerror if reading failed, or None if it has not
    # been read yet
    raw: object
    # Virtual files do not exist on the filesystem.  Examples: <stdin>, <predefines>.
    is_virtual: bool


@dataclass(slots=True)
class File:
    '''Represents a file in the file system.'''
    # The include directory it was found in.  None for absolute header names.
    directory: IncludeDirectory
    # The path of the file.  If directory is not None, this will begin with
    # directory.path, then the header name, and finally any suffix that was used.
    path: str
    # The contents of the file
    contents: FileContents
    # If the file is a virtual file
    is_virtual: bool

    def nul_terminated_contents(self):
        result = self.contents.raw
        assert isinstance(result, (bytes, str))
        if isinstance(result, str):
            return b'\0'
        return result


class FileManager:
    '''The file manager caches the content of files, maintains the include file search paths,
    and looks up header names on the search path.  It also keeps track of the include
    stack.
    '''
    def __init__(self):
        self.file_stack = []        # a list of File objects
        self.current_file_search = True
        # Lists of include directories
        self.user_quoted = []       # --quoted-dir.  for "" searches only
        self.user_angled = []       # -I --angled-dir <> searches start here
        self.user_system = []       # --system-dir
        self.standard = []          # a function of the target
        self.user_final = []        # -idirafter
        # Each suffix is appended to suffix-less header names and tried in turn
        self.suffixes = ['']

    def add_search_paths(self, paths, kind):
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

        for path in paths:
            stat_result = host.stat(path)
            exists = stat_result is not None and host.stat_is_directory(stat_result)
            dir_list.append(IncludeDirectory(path, kind, exists))

    def add_standard_search_paths(self, paths):
        self.add_search_paths(paths, DirectoryKind.standard)

    def lookup_in_directory(self, header_name, directory):
        if directory:
            path = host.path_join(directory.path, header_name)
        else:
            path = header_name

        # Only accept regular files
        stat_result = host.stat(path)
        if not stat_result:
            return None
        if not host.stat_is_regular_file(stat_result):
            return None
        return File(directory, path, FileContents(None, False), False)

    def search_directory(self, header_name, directory):
        if directory and not directory.exists:
            return None

        root, suffix = host.path_splitext(header_name)
        if suffix:
            return self.lookup_in_directory(header_name, directory)
        for suffix in self.suffixes:
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

    #
    # Absolute filenames
    #

    def search_absolute(self, header_name):
        return self.search_directory(header_name, None)

    #
    # Quoted headers
    #

    def search_quoted_header(self, header_name):
        if host.path_is_absolute(header_name):
            return self.search_absolute(header_name)

        # Search in the directory of the current file
        if self.current_file_search:
            entry = self.file_stack[-1]
            # create a new directory entry if it is not a simple filename
            dirname, filename = host.path_split(entry.path)
            if dirname == '' or dirname == '.':
                directory = entry.directory
            else:
                # Relative directory searches acquite the kind of what they are relative
                # to; this also ensures they inherit systemheader-ness
                directory = IncludeDirectory(dirname, entry.directory.kind, True)
            result = self.search_directory(header_name, directory)
            if result:
                return result

        # Try quoted header directory list
        result = self.search_directory_lists(header_name, [self.user_quoted])
        if result:
            return result

        # Finally, search as an angled header
        return self.search_angled_header(header_name)

    #
    # Angled headers
    #

    def search_angled_header(self, header_name):
        if host.path_is_absolute(header_name):
            return self.search_absolute(header_name)

        # Search in four directory lists.
        angled_lists = [self.user_angled, self.user_system, self.standard, self.user_final]
        return self.search_directory_lists(header_name, angled_lists)

    def virtual_file(self, filename, raw):
        '''Return a File object for a virtual file with the given contents.  It is given an empty
        dirname, so current-file relative "" include lookups are done relative to the
        current working directory of the process.
        '''
        directory = IncludeDirectory('', DirectoryKind.none, True)
        contents = FileContents(raw + b'\0', is_virtual=True)
        return File(directory, filename, contents, True)

    def file_for_path(self, path):
        dirname = host.path_dirname(path)
        directory = IncludeDirectory(dirname, DirectoryKind.none, True)
        contents = FileContents(None, is_virtual=False)
        return File(directory, path, contents, False)

    def read_file(self, file):
        if file.contents.raw is None:
            file.contents.raw = host.read_file(file.path, nul_terminate=True)
        return file.contents.raw

    def enter_file(self, search_result):
        self.file_stack.append(search_result)

    def leave_file(self):
        self.file_stack.pop()

    def include_depth(self):
        # Don't count the primary source file.
        return len(self.file_stack) - 1

    def in_system_header(self):
        '''Return true if we're in a system header.'''
        return self.file_stack and self.file_stack[-1].directory.is_system()
