"""Which changed lines of src/ the tests of a pull request execute, from a JaCoCo XML report.

    python3 scripts/coverage/prcoverage.py plan --diff changes.diff
    python3 scripts/coverage/prcoverage.py report --diff changes.diff --report coverageReport.xml --plan plan.json

`changes.diff` is `git diff --find-renames -U0 <merge base> <head>`. `plan` names the test classes of the pull request
that `test` can run, and why the others are left out. `report` prints a short text for people and, with --json, writes
the same as JSON for programs. scripts/coverage/pr-coverage.sh runs both with Gradle in between.

Only Java files of src/ and jps-plugin/src/ count; gen/, tests/, testData/ and other files are ignored. A line counts
when the report has it, which means it holds code; comments, blank lines and declarations without code do not count.
"Executed" means that a test ran the line, not that a test checked what it does.
"""

import argparse
import json
import os
import re
import sys
import xml.etree.ElementTree as ET

REPOSITORY = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..')
SOURCE_ROOTS = ['src/', 'jps-plugin/src/']
TEST_ROOTS = ['tests/', 'jps-plugin/tests/']
# Tests that only testCompilation runs (the `test` task excludes them, see build.gradle).
COMPILATION_TESTS = ['tests/org/intellij/erlang/build/', 'tests/org/intellij/erlang/jps/', 'jps-plugin/tests/']
# Code that mainly the compilation tests run. A line here that a run without testCompilation did not execute is
# "not measured by this run", not "not executed".
COMPILATION_CODE = ['jps-plugin/src/', 'src/org/intellij/erlang/compilation/',
                    'src/org/intellij/erlang/eunit/ErlangUnitRunningState.java',
                    'src/org/intellij/erlang/rebar/runner/RebarEunitRunningState.java']
# How many file names the text lists before it says "and N more".
MAX_LISTED = 12


QUOTED = r'"(?:[^"\\]|\\.)*"'
ESCAPES = {'a': 7, 'b': 8, 't': 9, 'n': 10, 'v': 11, 'f': 12, 'r': 13, '"': 34, '\\': 92}


def unquote(name):
    """A path as git prints it: in double quotes with C escapes (\\303\\251 for é) when it has unusual bytes."""
    if len(name) < 2 or not (name.startswith('"') and name.endswith('"')):
        return name
    body, out, i = name[1:-1], bytearray(), 0
    while i < len(body):
        if body[i] == '\\' and re.match(r'[0-7]{3}', body[i + 1:i + 4]):
            out.append(int(body[i + 1:i + 4], 8))
            i += 4
        elif body[i] == '\\' and body[i + 1:i + 2] in ESCAPES:
            out.append(ESCAPES[body[i + 1]])
            i += 2
        else:
            out.extend(body[i].encode('utf-8'))
            i += 1
    return out.decode('utf-8', errors='replace')


def header_path(name):
    """The path of a ---/+++ line without a/ or b/, None for /dev/null. Git ends a name with a space with a tab."""
    if name.endswith('\t'):
        name = name[:-1]
    if name == '/dev/null':
        return None
    name = unquote(name)
    return name[2:] if name[:2] in ('a/', 'b/') else name


def git_header_paths(rest):
    """(old, new) of the part after "diff --git ", or None when it cannot be split."""
    m = re.match(r'(%s) (%s)$' % (QUOTED, QUOTED), rest)
    if m:
        return header_path(m.group(1)), header_path(m.group(2))
    half = (len(rest) - 1) // 2
    if rest[:2] == 'a/' and rest[half:half + 3] == ' b/' and rest[2:half] == rest[half + 3:]:
        return rest[2:half], rest[half + 3:]
    m = re.match(r'a/(.*) b/(.*)$', rest)
    return (m.group(1), m.group(2)) if m else None


def parse_diff(text):
    """[{"path", "old_path", "status", "added": [line numbers in the new file]}] of a git diff with -U0.

    status is "added", "deleted", "renamed" or "modified"; path is the new path (the old one for a deleted file).
    Header lines are read only before the first hunk of a file: in a hunk, "+++ x" is an added line "++ x"."""
    files, current, in_hunks = [], None, False
    for line in text.splitlines():
        if line.startswith('diff --git '):
            current, in_hunks = {'path': None, 'old_path': None, 'status': 'modified', 'added': []}, False
            files.append(current)
            paths = git_header_paths(line[len('diff --git '):])
            if paths:
                current['old_path'], current['path'] = paths
        elif current is None:
            continue
        elif in_hunks and not line.startswith('@@'):
            continue
        elif line.startswith('new file mode'):
            current['status'] = 'added'
        elif line.startswith('deleted file mode'):
            current['status'] = 'deleted'
        elif line.startswith('rename from '):
            current['status'] = 'renamed'
            current['old_path'] = unquote(line[len('rename from '):])
        elif line.startswith('rename to '):
            current['path'] = unquote(line[len('rename to '):])
        elif line.startswith('--- '):
            path = header_path(line[len('--- '):])
            if path is not None:
                current['old_path'] = path
        elif line.startswith('+++ '):
            path = header_path(line[len('+++ '):])
            current['path'] = current['old_path'] if path is None else path
        elif line.startswith('@@'):
            in_hunks = True
            m = re.match(r'@@ -\d+(?:,\d+)? \+(\d+)(?:,(\d+))? @@', line)
            if m:
                start, count = int(m.group(1)), int(m.group(2) if m.group(2) is not None else 1)
                current['added'].extend(range(start, start + count))
    return [f for f in files if f['path']]


def is_source(path):
    return path.endswith('.java') and any(path.startswith(root) for root in SOURCE_ROOTS)


def is_compilation_code(path):
    return any(path == p or (p.endswith('/') and path.startswith(p)) for p in COMPILATION_CODE)


def class_name(path):
    """org.intellij.erlang.X of tests/org/intellij/erlang/X.java or jps-plugin/tests/org/intellij/erlang/X.java."""
    root = next(r for r in TEST_ROOTS if path.startswith(r))
    return path[len(root):-len('.java')].replace('/', '.')


def has_changed_source_lines(files):
    return any(is_source(f['path']) and f['status'] != 'deleted' and f['added'] for f in files)


def plan(files, repository):
    """{"run": [test classes for `test`], "left_out": [{"class", "why"}], "notes": [...]} for a pull request.

    Without a changed line in the Java files of src/ or jps-plugin/src/ nothing is run: there is nothing to measure."""
    run, left_out, notes = [], [], []
    measurable = has_changed_source_lines(files)
    tests = [f for f in files if f['path'].endswith('.java') and any(f['path'].startswith(r) for r in TEST_ROOTS)]
    for f in tests:
        path = f['path']
        name = class_name(path)
        if f['status'] == 'deleted':
            left_out.append({'class': name, 'why': 'deleted'})
            continue
        if any(path.startswith(p) for p in COMPILATION_TESTS):
            left_out.append({'class': name, 'why': 'a compilation test: only testCompilation runs it'})
            continue
        try:
            with open(os.path.join(repository, path), encoding='utf-8') as source:
                text = source.read()
        except OSError:
            left_out.append({'class': name, 'why': 'not found in the checkout'})
            continue
        simple = os.path.basename(path)[:-len('.java')]
        if re.search(r'\babstract\s+class\s+' + re.escape(simple) + r'\b', text):
            left_out.append({'class': name, 'why': 'an abstract base class: its subclasses are not run'})
        elif not (simple.endswith('Test') or re.search(r'\bpublic\s+void\s+test\w*\s*\(', text)):
            left_out.append({'class': name, 'why': 'not a test class (no name ending with Test, no test method)'})
        elif not measurable:
            left_out.append({'class': name, 'why': 'no changed line in the Java files of src/ or jps-plugin/src/ to '
                                                   'measure'})
        else:
            run.append(name)
    if not tests:
        if any(f['path'].startswith('testData/') for f in files):
            notes.append('This pull request changes test data but no test class, so no test ran.')
        else:
            notes.append('This pull request changes no test class, so no test ran.')
    return {'run': sorted(set(run)), 'left_out': left_out, 'notes': notes}


def report_lines(report_path):
    """{"<package>/<file>": {line number: executed}} of the lines that hold code, from a JaCoCo XML report."""
    result = {}
    for package in ET.parse(report_path).getroot().findall('package'):
        for source in package.findall('sourcefile'):
            lines = {}
            for line in source.findall('line'):
                lines[int(line.get('nr'))] = int(line.get('ci', '0')) > 0
            result[package.get('name') + '/' + source.get('name')] = lines
    return result


def key_of(path):
    for root in SOURCE_ROOTS:
        if path.startswith(root):
            return path[len(root):]
    return None


def ranges(numbers):
    """[[start, end]] of sorted line numbers."""
    out = []
    for n in sorted(numbers):
        if out and n == out[-1][1] + 1:
            out[-1][1] = n
        else:
            out.append([n, n])
    return out


def places(entries):
    """"A.java:3-5, B.java:7" of [(path, [[start, end]])], with the file name only when it is unique."""
    names = [os.path.basename(p) for p, _ in entries]
    out = []
    for path, rs in entries:
        name = os.path.basename(path) if names.count(os.path.basename(path)) == 1 else path
        out.extend('%s:%d' % (name, a) if a == b else '%s:%d-%d' % (name, a, b) for a, b in rs)
    if len(out) > MAX_LISTED:
        out = out[:MAX_LISTED] + ['and %d more' % (len(out) - MAX_LISTED)]
    return ', '.join(out)


def report(files, lines, the_plan, with_compilation, gradle_status=None, failed_tests=None):
    """The result as a dict; its "text" is what people read."""
    sources = [f for f in files if is_source(f['path'])]
    deleted = [f['path'] for f in sources if f['status'] == 'deleted']
    changed = [f for f in sources if f['status'] != 'deleted' and f['added']]
    result = {'measured': ['test', 'testCompilation'] if with_compilation else ['test'],
              'planned': the_plan.get('run', []),
              'tests_run': the_plan.get('run', []) if lines is not None else [],
              'tests_left_out': the_plan.get('left_out', []), 'notes': list(the_plan.get('notes', [])),
              'deleted_files': deleted, 'files': [], 'gradle_status': gradle_status,
              'failed_tests': failed_tests or []}
    totals = {'changed': 0, 'executed': 0, 'not_executed': 0, 'not_measured': 0, 'unknown': 0}
    for f in changed:
        entry = {'path': f['path'], 'status': f['status'], 'changed': 0, 'executed': 0,
                 'not_executed': [], 'not_measured': [], 'unknown': []}
        file_lines = None if lines is None else lines.get(key_of(f['path']))
        if file_lines is None:
            entry['unknown'] = ranges(f['added'])
            totals['unknown'] += len(f['added'])
        else:
            code = [n for n in f['added'] if n in file_lines]
            missed = [n for n in code if not file_lines[n]]
            entry['changed'], entry['executed'] = len(code), len(code) - len(missed)
            if missed and not with_compilation and is_compilation_code(f['path']):
                entry['not_measured'] = ranges(missed)
                totals['not_measured'] += len(missed)
            else:
                entry['not_executed'] = ranges(missed)
                totals['not_executed'] += len(missed)
            totals['changed'] += entry['changed']
            totals['executed'] += entry['executed']
        result['files'].append(entry)
    result['totals'] = totals
    result['text'] = text_of(result, lines is not None)
    return result


def text_of(result, measured):
    t, out = result['totals'], []
    if not result['files']:
        out.append('changed lines: none in the Java files of src/ or jps-plugin/src/; nothing to measure.')
    elif not measured and result['planned']:
        out.append('changed lines: not measured, because the run of the tests with coverage wrote no report'
                   + (' (Gradle ended with status %d).' % result['gradle_status']
                      if result['gradle_status'] not in (None, 0) else '.'))
    elif not measured:
        out.append('changed lines: not measured, because no test of this pull request ran with coverage.')
    else:
        line = 'changed lines: %d of %d executed' % (t['executed'], t['changed'])
        not_executed = [(f['path'], f['not_executed']) for f in result['files'] if f['not_executed']]
        if not_executed:
            line += '; not executed: ' + places(not_executed)
        out.append(line + '.')
        not_measured = [(f['path'], f['not_measured']) for f in result['files'] if f['not_measured']]
        if not_measured:
            out.append('Not measured by this run (only testCompilation runs this code, and it did not run): '
                       + places(not_measured) + '.')
        unknown = [(f['path'], f['unknown']) for f in result['files'] if f['unknown']]
        if unknown:
            out.append('Not in the coverage report: ' + places(unknown) + '.')
    if result['tests_run']:
        out.append('Tests run with coverage: ' + ', '.join(n.rsplit('.', 1)[-1] for n in result['tests_run']) + '.')
    for left in result['tests_left_out']:
        out.append('Not run: %s (%s).' % (left['class'].rsplit('.', 1)[-1], left['why']))
    if result['failed_tests']:
        out.append('Failed in this run: ' + ', '.join(result['failed_tests']) + '.')
    if result['gradle_status'] not in (None, 0) and measured:
        out.append('Gradle ended with status %d.' % result['gradle_status'])
    if result['deleted_files']:
        out.append('Deleted source files (nothing to measure): ' + ', '.join(
            os.path.basename(p) for p in result['deleted_files']) + '.')
    out.extend(result['notes'])
    out.append('"Executed" means a test ran the line; it does not mean a test checked the result.')
    return '\n'.join(out)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    sub = parser.add_subparsers(dest='command', required=True)
    p = sub.add_parser('plan', help='the test classes of the pull request that `test` can run')
    p.add_argument('--diff', required=True)
    p.add_argument('--repository', default=REPOSITORY)
    p.add_argument('--json', help='write the plan here')
    r = sub.add_parser('report', help='which changed lines the tests executed')
    r.add_argument('--diff', required=True)
    r.add_argument('--report', help='the JaCoCo XML report; left out or missing: nothing was measured')
    r.add_argument('--plan', help='the JSON that `plan` wrote')
    r.add_argument('--with-test-compilation', action='store_true', help='the run included testCompilation')
    r.add_argument('--gradle-status', type=int)
    r.add_argument('--failed-test', action='append', default=[], help='a test class that failed in the run')
    r.add_argument('--json', help='write the result here')
    args = parser.parse_args(argv)
    with open(args.diff, encoding='utf-8', errors='replace') as f:
        files = parse_diff(f.read())
    if args.command == 'plan':
        result = plan(files, args.repository)
        if args.json:
            with open(args.json, 'w', encoding='utf-8') as f:
                json.dump(result, f, indent=2)
        for name in result['run']:
            print(name)
        return 0
    the_plan = {}
    if args.plan and os.path.isfile(args.plan):
        with open(args.plan, encoding='utf-8') as f:
            the_plan = json.load(f)
    lines = report_lines(args.report) if args.report and os.path.isfile(args.report) else None
    result = report(files, lines, the_plan, args.with_test_compilation, args.gradle_status, args.failed_test)
    if args.json:
        with open(args.json, 'w', encoding='utf-8') as f:
            json.dump(result, f, indent=2)
    print(result['text'])
    return 0


if __name__ == '__main__':
    sys.exit(main())
