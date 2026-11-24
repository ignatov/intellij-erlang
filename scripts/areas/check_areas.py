#!/usr/bin/env python3
"""The code areas of the plugin (areas.yml beside this file): the check of the map, and the area of a file.

  python3 scripts/areas/check_areas.py [check]          every tracked file of the mapped roots is in exactly one
                                                         area, and every glob matches a file; exit 1 when not
  python3 scripts/areas/check_areas.py which PATH...    the area of each path ("-" when it has none)
  python3 scripts/areas/check_areas.py touched [PATH...]  the areas that the paths touch, one per line; without
                                                         paths it reads them from stdin, one per line:
                                                         git diff --name-only origin/master... | check_areas.py touched

--labels prints the labels (area:<name>) instead of the names, --json prints JSON, --map FILE reads another map.
As a library: load() or parse(text) give an AreaMap with area_of(path), classify(path) and touched(paths).
Only the standard library is used.
"""

import argparse
import json
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
DEFAULT_MAP = os.path.join(HERE, 'areas.yml')
REPOSITORY = os.path.dirname(os.path.dirname(HERE))
ROOTS = ('src', 'grammars', 'gen', 'resources', 'jps-plugin', 'tests', 'testData')
LABEL_PREFIX = 'area:'
NAME_RE = re.compile(r'[a-z][a-z0-9]*(?:-[a-z0-9]+)*$')
BAD_GLOB_CHARS = set('?[]{}\\')

AREA_RE = re.compile(r'- name: (\S+)$')
FIELD_RE = re.compile(r'  (label|description): (.*)$')
PATHS_RE = re.compile(r'  paths:$')
PATH_RE = re.compile(r'    - "([^"]+)"$')


class MapError(ValueError):
    """A map that does not follow the format of areas.yml."""


def unquote(value):
    value = value.strip()
    if len(value) >= 2 and value[0] == value[-1] == '"':
        return value[1:-1]
    return value


def glob_regex(glob):
    out, i = [], 0
    while i < len(glob):
        if glob.startswith('**', i):
            out.append('.*')
            i += 2
        elif glob[i] == '*':
            out.append('[^/]*')
            i += 1
        else:
            out.append(re.escape(glob[i]))
            i += 1
    return re.compile(''.join(out) + r'\Z')


def specificity(glob):
    """The length of the text before the first "*"; a glob without "*" names one file and wins over any other."""
    star = glob.find('*')
    return len(glob) + 1000000 if star < 0 else star


def normalize(path):
    path = path.strip().replace(os.sep, '/')
    while path.startswith('./'):
        path = path[2:]
    return path


class Area(object):
    def __init__(self, name, label=None, description=None, paths=None, line=0):
        self.name, self.label, self.description = name, label, description
        self.paths = paths if paths is not None else []
        self.line = line

    def as_dict(self):
        return {'name': self.name, 'label': self.label, 'description': self.description, 'paths': list(self.paths)}


class AreaMap(object):
    def __init__(self, areas):
        self.areas = list(areas)
        self.by_name = {a.name: a for a in self.areas}
        self.globs = [(a.name, g, glob_regex(g), specificity(g)) for a in self.areas for g in a.paths]

    def matches(self, path):
        """[(area, glob, specificity)] of every glob that matches path, in the order of the map."""
        path = normalize(path)
        return [(name, glob, spec) for name, glob, rx, spec in self.globs if rx.match(path)]

    def classify(self, path):
        """The areas of the most specific matching globs, in the order of the map: [] when no glob matches, one
        name normally, several when the map has a tie for this path (check reports it)."""
        found = self.matches(path)
        if not found:
            return []
        best = max(spec for _, _, spec in found)
        out = []
        for name, _, spec in found:
            if spec == best and name not in out:
                out.append(name)
        return out

    def area_of(self, path):
        """The name of the area of path, or None when no area or a tie of the map."""
        names = self.classify(path)
        return names[0] if len(names) == 1 else None

    def touched(self, paths):
        """The areas that the paths touch, in the order of the map; paths without an area add nothing."""
        hit = set()
        for path in paths:
            hit.update(self.classify(path))
        return [a.name for a in self.areas if a.name in hit]

    def label(self, name):
        return self.by_name[name].label


def parse(text):
    """An AreaMap from the text of areas.yml. Raises MapError with the line number on anything else."""
    areas, cur, in_paths = [], None, False
    for number, raw in enumerate(text.splitlines(), 1):
        line = raw.rstrip()
        if not line.strip() or line.lstrip().startswith('#'):
            continue
        m = AREA_RE.match(line)
        if m:
            cur, in_paths = Area(m.group(1), line=number), False
            areas.append(cur)
            continue
        if cur is None:
            raise MapError('line %d: expected "- name: <area>": %s' % (number, line))
        m = FIELD_RE.match(line)
        if m:
            key = m.group(1)
            if getattr(cur, key) is not None:
                raise MapError('line %d: a second %s in area %s' % (number, key, cur.name))
            setattr(cur, key, unquote(m.group(2)))
            in_paths = False
            continue
        if PATHS_RE.match(line):
            in_paths = True
            continue
        m = PATH_RE.match(line)
        if m and in_paths:
            cur.paths.append(m.group(1))
            continue
        raise MapError('line %d: not in the format of areas.yml: %s' % (number, line))
    problems = validate(areas)
    if problems:
        raise MapError('; '.join(problems))
    return AreaMap(areas)


def validate(areas):
    problems, seen = [], set()
    if not areas:
        problems.append('the map has no area')
    for a in areas:
        where = 'area %s (line %d)' % (a.name, a.line)
        if not NAME_RE.match(a.name):
            problems.append('%s: the name must be lower case words joined by "-"' % where)
        if a.name in seen:
            problems.append('%s: a second area of this name' % where)
        seen.add(a.name)
        if a.label != LABEL_PREFIX + a.name:
            problems.append('%s: the label must be %s%s' % (where, LABEL_PREFIX, a.name))
        if not a.description:
            problems.append('%s: no description' % where)
        if not a.paths:
            problems.append('%s: no paths' % where)
        for g in a.paths:
            if g.startswith('/') or g.startswith('./') or '//' in g or BAD_GLOB_CHARS & set(g):
                problems.append('%s: the glob %s must be a plain path from the root, with only * and **' % (where, g))
            elif g.split('/')[0] not in ROOTS:
                problems.append('%s: the glob %s is outside %s' % (where, g, ', '.join(ROOTS)))
        dupes = sorted({g for g in a.paths if a.paths.count(g) > 1})
        for g in dupes:
            problems.append('%s: the glob %s is listed twice' % (where, g))
    return problems


def load(path=DEFAULT_MAP):
    with open(path, encoding='utf-8') as f:
        return parse(f.read())


def tracked_files(repository=REPOSITORY):
    out = subprocess.run(['git', 'ls-files', '-z', '--'] + list(ROOTS), cwd=repository, check=True,
                         stdout=subprocess.PIPE).stdout.decode('utf-8')
    return [f for f in out.split('\0') if f]


def check(area_map, files):
    """The problems of the map for the files: a file with no area, a file with a tie, a glob that matches no file."""
    problems, used = [], set()
    for f in files:
        found = area_map.matches(f)
        used.update((name, glob) for name, glob, _ in found)
        names = area_map.classify(f)
        if not names:
            problems.append('no area: %s' % f)
        elif len(names) > 1:
            problems.append('two or more areas with equally specific globs (%s): %s' % (', '.join(names), f))
    for a in area_map.areas:
        for g in a.paths:
            if (a.name, g) not in used:
                problems.append('the glob %s of area %s matches no file' % (g, a.name))
    return problems


def main(argv=None):
    p = argparse.ArgumentParser(description='The code areas of the plugin (scripts/areas/areas.yml).')
    p.add_argument('command', nargs='?', default='check', choices=('check', 'which', 'touched'))
    p.add_argument('paths', nargs='*')
    p.add_argument('--map', default=DEFAULT_MAP)
    p.add_argument('--labels', action='store_true', help='print area:<name> instead of <name>')
    p.add_argument('--json', action='store_true')
    args = p.parse_intermixed_args(argv)
    try:
        area_map = load(args.map)
    except (MapError, OSError) as e:
        print('error: %s: %s' % (args.map, e), file=sys.stderr)
        return 2

    def shown(name):
        return area_map.label(name) if args.labels else name

    if args.command == 'check':
        if args.paths:
            p.error('check takes no paths')
        files = tracked_files()
        problems = check(area_map, files)
        if args.json:
            print(json.dumps({'files': len(files), 'areas': len(area_map.areas), 'problems': problems}, indent=1))
        else:
            for line in problems:
                print(line)
            print('%d files, %d areas, %d problems' % (len(files), len(area_map.areas), len(problems)))
        return 1 if problems else 0
    paths = args.paths or [normalize(l) for l in sys.stdin.read().splitlines() if l.strip()]
    if args.command == 'which':
        result = {normalize(f): [shown(n) for n in area_map.classify(f)] for f in paths}
        if args.json:
            print(json.dumps(result, indent=1))
        else:
            for f, names in result.items():
                print('%s\t%s' % (f, ','.join(names) or '-'))
        return 0
    names = [shown(n) for n in area_map.touched(paths)]
    if args.json:
        print(json.dumps(names))
    elif names:
        print('\n'.join(names))
    return 0


if __name__ == '__main__':
    sys.exit(main())
