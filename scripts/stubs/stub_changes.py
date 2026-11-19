#!/usr/bin/env python3
"""Checks that a merge keeps the stub changes of the base (the build job of .github/workflows/gradle.yml).

The stub version of .erl files is the number of entries of ENTRIES in
src/org/intellij/erlang/stubs/ErlangStubChanges.java. Entries are only ever appended. Two pull requests that each
append one conflict on purpose, and the resolution keeps both entries, the base's first. This check fails when a
resolution dropped, reordered or edited an entry of the base, or put a new one before them: the entries of the base
must be a prefix of the entries of the merge result (equal is fine).

Usage: stub_changes.py BASE_FILE HEAD_FILE
  BASE_FILE is ErlangStubChanges.java of the base. If it does not exist, the base predates the file and the check
  passes. HEAD_FILE is the same file in the merge result and must exist.

Exit codes: 0 if the check passes, 1 if it fails, 2 if a file cannot be read or parsed.

Python 3 standard library only.
"""

import os
import re
import sys

START = re.compile(r'^\s*public static final String\[\] ENTRIES = \{\s*$')
ENTRY = re.compile(r'^\s*"((?:[^"\\]|\\.)*)",\s*$')
END = re.compile(r'^\s*\};\s*$')


class ParseError(Exception):
    pass


def parse_entries(text):
    """The entries of ENTRIES in the text of ErlangStubChanges.java, in order.

    Every line between the opening and the closing line of the array must be one entry ending with a comma.
    """
    lines = text.splitlines()
    starts = [i for i, line in enumerate(lines) if START.match(line)]
    if len(starts) != 1:
        raise ParseError('expected one line "public static final String[] ENTRIES = {", found %d' % len(starts))
    entries = []
    for number in range(starts[0] + 1, len(lines)):
        line = lines[number]
        if END.match(line):
            return entries
        match = ENTRY.match(line)
        if not match:
            raise ParseError('line %d is not an entry of the form "text",: %s' % (number + 1, line.strip()))
        entries.append(match.group(1))
    raise ParseError('ENTRIES is not closed with "};"')


def check(base_entries, head_entries):
    """None if the entries of the base are a prefix of the entries of the head, otherwise the problem."""
    for index, entry in enumerate(base_entries):
        if index >= len(head_entries):
            return 'the merge result has %d entries, the base %d; entry %d of the base is missing: %s' % (
                len(head_entries), len(base_entries), index + 1, entry)
        if head_entries[index] != entry:
            return 'entry %d of the base is %r, in the merge result it is %r' % (index + 1, entry, head_entries[index])
    return None


def read(path):
    with open(path, encoding='utf-8') as f:
        return f.read()


def main(argv):
    if len(argv) != 3:
        print(__doc__.strip(), file=sys.stderr)
        return 2
    base_path, head_path = argv[1], argv[2]
    try:
        head_entries = parse_entries(read(head_path))
        if not os.path.exists(base_path):
            print('The base has no ErlangStubChanges.java; nothing to compare. The merge result has %d entries.'
                  % len(head_entries))
            return 0
        base_entries = parse_entries(read(base_path))
    except (OSError, UnicodeDecodeError, ParseError) as e:
        print('::error::Cannot read the stub changes: %s' % e)
        return 2
    problem = check(base_entries, head_entries)
    if problem:
        print('::error::The entries of ErlangStubChanges.java of the base must stay, unchanged and in order, at the '
              'start of ENTRIES. Keep every entry of master and append new ones after them: %s' % problem)
        return 1
    print('The %d entries of the base are kept; the merge result has %d (stub version %d).'
          % (len(base_entries), len(head_entries), len(head_entries)))
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))
