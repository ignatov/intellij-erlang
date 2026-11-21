"""Coverage totals of a JaCoCo XML report for src/ (without gen/), gen/ and jps-plugin/.

    python3 scripts/coverage/totals.py build/reports/jacoco/coverageReport/coverageReport.xml

Prints a Markdown table. The totals are sums over source files, which count a line once. Sums over the classes of the
CSV report count a line again for every class that has code on it (an anonymous class, a lambda) and are larger.
"""

import argparse
import os
import sys
import xml.etree.ElementTree as ET

REPOSITORY = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..')
# Each source file of the report belongs to the first root that has it.
PARTS = [('src/ (without gen/)', 'src'), ('gen/', 'gen'), ('jps-plugin/', 'jps-plugin/src')]
OTHER = 'other'


def counter(element, kind):
    """(missed, covered) of the counter of that kind, (0, 0) when the element has none."""
    for c in element.findall('counter'):
        if c.get('type') == kind:
            return int(c.get('missed')), int(c.get('covered'))
    return 0, 0


def parse(path):
    """The root element of a JaCoCo XML report. The report names a DTD that is never read."""
    return ET.parse(path).getroot()


def part_of(package, source_file, repository):
    for name, root in PARTS:
        if os.path.isfile(os.path.join(repository, root, package, source_file)):
            return name
    return OTHER


def totals(report, repository):
    """{part: [line missed, line covered, branch missed, branch covered, files]} over the source files."""
    result = {name: [0, 0, 0, 0, 0] for name, _ in PARTS}
    for package in report.findall('package'):
        for source in package.findall('sourcefile'):
            name = part_of(package.get('name'), source.get('name'), repository)
            t = result.setdefault(name, [0, 0, 0, 0, 0])
            lines, branches = counter(source, 'LINE'), counter(source, 'BRANCH')
            t[0] += lines[0]
            t[1] += lines[1]
            t[2] += branches[0]
            t[3] += branches[1]
            t[4] += 1
    return result


def percent(missed, covered):
    total = missed + covered
    return '%.1f%%' % (100.0 * covered / total) if total else '-'


def table(result):
    rows = ['| Part | Files | Lines covered | Line % | Branches covered | Branch % |',
            '|---|---|---|---|---|---|']
    for name, t in result.items():
        if name == OTHER and not t[4]:
            continue
        rows.append('| %s | %d | %d of %d | %s | %d of %d | %s |' % (
            name, t[4], t[1], t[0] + t[1], percent(t[0], t[1]), t[3], t[2] + t[3], percent(t[2], t[3])))
    return '\n'.join(rows)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument('report', help='the JaCoCo XML report')
    parser.add_argument('--repository', default=REPOSITORY, help='the checkout the report was made from')
    args = parser.parse_args(argv)
    print(table(totals(parse(args.report), args.repository)))
    return 0


if __name__ == '__main__':
    sys.exit(main())
