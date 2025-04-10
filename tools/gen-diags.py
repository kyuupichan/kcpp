import argparse
import itertools
import os
import sys
import yaml


template = """#
# This file is generated by gen-diags - do not edit!
#

from dataclasses import dataclass
from enum import IntEnum, auto


__all__ = [
    'DID', 'DiagnosticGroup', 'DiagnosticSeverity', 'DiagnosticDefinition',
    'diagnostic_definitions',
]


class DiagnosticSeverity(IntEnum):
    '''The severity of a diagnostic.'''
    none = auto()        # Source file locations, compilation summaries, etc.
    note = auto()        # Notes are emitted nested inside another diagnostic
    ignored = auto()     # Indicates this diagnostic is suppressed and to be ignored
    remark = auto()      # A diagnostic milder than a warning; suppressed by default
    warning = auto()     # Does not increase the error count
    error = auto()       # Increases the error count, halts compilation if limit reached
    fatal = auto()       # A fatal error terminates compilation immediately


class DiagnosticGroup(IntEnum):
    '''All diagnostics whose severity can be controlled by the user must belong to a group.
    Therefore all ignored, warning and remark diagnostics have an associated group.  Errors
    that belong to a group can have their severity downgraded.'''
    none = auto()
<GROUPS>


class DID(IntEnum):
    '''The diagnostic identifier.'''
<DIDS>

    def __repr__(self):
        return f'DID.{self.name}'


@dataclass
class DiagnosticDefinition:
    '''Defines a diagnostic; sourced from a .yaml file.'''
    did: DID
    severity: DiagnosticSeverity
    group: DiagnosticGroup
    text: str


diagnostic_definitions = {
<DEFNS>
}"""


def read_defns(entry):
    with open(entry.path, 'r') as f:
        doc = f.read()
    result = yaml.safe_load(doc)
    print(f'read {entry.path}', file=sys.stderr)
    for key, defn in result.items():
        severity = defn.get('severity')
        if severity not in ('none', 'note', 'ignored', 'remark', 'warning', 'error',
                            'fatal', 'ice'):
            print(f'WARNING: diagnostic {key} has invalid severity', file=sys.stderr)
            defn['severity'] = 'fatal'
        group = defn.get('group')
        if severity in ('ignored', 'warning', 'remark'):
            if not group:
                print(f'WARNING: diagnostic {key} must have a group', file=sys.stderr)
                defn['group'] = 'no_group'
        elif group is not None and severity != 'error':
            print(f'WARNING: diagnostic {key} must not have a group', file=sys.stderr)
            del defn['group']
    return result


def did_lines(defns):
    return '\n'.join(f'    {did} = auto()' for did in sorted(defns))


def group_lines(defns):
    groups = set(defn.get('group') for defn in defns.values())
    groups.discard(None)

    return '\n'.join(f'    {group} = auto()'
                     for group in sorted(groups))


def chunks(items, size):
    '''Break up items, an iterable, into chunks of length size.'''
    assert size > 0
    for i in range(0, len(items), size):
        yield items[i: i + size]


def defns_lines(defns):
    def key_text(key):
        defn = defns[key]
        group = defn.get('group', 'none')
        if ' ' in group:
            print(f'ERROR: bad group name: {group}', file=sys.stderr)
        text_parts = list(chunks(defn["text"], 84))
        yield f'    DID.{key}: DiagnosticDefinition('
        yield f'        DID.{key},'
        yield f'        DiagnosticSeverity.{defn["severity"]},'
        yield f'        DiagnosticGroup.{group},'
        for n, part in enumerate(text_parts, start=1):
            yield f'        {part!r}' + (',' if n == len(text_parts) else '')
        yield '    ),'

    return '\n'.join(itertools.chain(*(key_text(key) for key in sorted(defns))))


def main():
    parser = argparse.ArgumentParser(
        'gen-diags',
        description='Generate diagnostic definitions from .yaml files.'
    )

    parser.add_argument('dir', help='Diagnostic definitions directory')
    args = parser.parse_args()

    defns = {}
    directory = args.dir
    for entry in os.scandir(directory):
        if entry.is_file() and entry.name.endswith('.yaml'):
            file_defns = read_defns(entry)
            dups = set(defns).intersection(file_defns)
            if dups:
                dups = ', '.join(dups)
                print(f'ERROR: duplicate definitions: {dups}', file=sys.stderr)
                exit(1)
            spaces = [defn for defn in file_defns if ' ' in defn]
            if spaces:
                dups = ', '.join(spaces)
                print(f'ERROR: definitions with spaces: {spaces}', file=sys.stderr)
                exit(1)
            defns.update(file_defns)

    body = template.replace('<GROUPS>', group_lines(defns))
    body = body.replace('<DIDS>', did_lines(defns))
    body = body.replace('<DEFNS>', defns_lines(defns))
    print(body)


if __name__ == '__main__':
    main()
