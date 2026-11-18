"""Unit tests of stub_changes.py: python3 -m unittest discover -s scripts/stubs -p 'test_*.py' -v"""

import contextlib
import io
import os
import tempfile
import unittest

import stub_changes

REPOSITORY = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
SOURCE = os.path.join(REPOSITORY, 'src', 'org', 'intellij', 'erlang', 'stubs', 'ErlangStubChanges.java')


def source(*entries):
    lines = ['package org.intellij.erlang.stubs;', '', 'public final class ErlangStubChanges {',
             '  public static final String[] ENTRIES = {']
    lines += ['    "%s",' % entry for entry in entries]
    lines += ['  };', '', '  private ErlangStubChanges() {', '  }', '}', '']
    return '\n'.join(lines)


class ParseTest(unittest.TestCase):
    def test_entries_in_order(self):
        self.assertEqual(['#1 a', '#2 b \\"c\\"'], stub_changes.parse_entries(source('#1 a', '#2 b \\"c\\"')))

    def test_no_entries(self):
        self.assertEqual([], stub_changes.parse_entries(source()))

    def test_repository_file(self):
        entries = stub_changes.parse_entries(stub_changes.read(SOURCE))
        self.assertGreaterEqual(len(entries), 7)
        self.assertEqual(len(entries), len(set(entries)))

    def test_entry_without_comma(self):
        with self.assertRaisesRegex(stub_changes.ParseError, 'line 6'):
            stub_changes.parse_entries(source('#1 a').replace('"#1 a",', '"#1 a",\n    "#2 b"'))

    def test_comment_in_entries(self):
        with self.assertRaises(stub_changes.ParseError):
            stub_changes.parse_entries(source('#1 a').replace('"#1 a",', '"#1 a",\n    // "#2 b",'))

    def test_conflict_markers(self):
        text = source('#1 a').replace('"#1 a",', '"#1 a",\n<<<<<<< HEAD\n    "#2 b",\n=======\n    "#3 c",\n>>>>>>> x')
        with self.assertRaises(stub_changes.ParseError):
            stub_changes.parse_entries(text)

    def test_no_array(self):
        with self.assertRaises(stub_changes.ParseError):
            stub_changes.parse_entries('class ErlangStubChanges {}\n')

    def test_unclosed_array(self):
        with self.assertRaises(stub_changes.ParseError):
            stub_changes.parse_entries(source('#1 a').replace('  };', ''))


class CheckTest(unittest.TestCase):
    BASE = ['#1 a', '#2 b', '#3 c']

    def test_equal(self):
        self.assertIsNone(stub_changes.check(self.BASE, list(self.BASE)))

    def test_appended(self):
        self.assertIsNone(stub_changes.check(self.BASE, self.BASE + ['#4 d']))

    def test_appended_two(self):
        self.assertIsNone(stub_changes.check(self.BASE, self.BASE + ['#4 d', '#5 e']))

    def test_dropped(self):
        self.assertIn('missing', stub_changes.check(self.BASE, ['#1 a', '#2 b']))

    def test_dropped_in_the_middle(self):
        self.assertIn('entry 2', stub_changes.check(self.BASE, ['#1 a', '#3 c', '#4 d']))

    def test_reordered(self):
        self.assertIn('entry 2', stub_changes.check(self.BASE, ['#1 a', '#3 c', '#2 b']))

    def test_inserted_in_the_middle(self):
        self.assertIn('entry 2', stub_changes.check(self.BASE, ['#1 a', '#4 d', '#2 b', '#3 c']))

    def test_edited(self):
        self.assertIn('entry 3', stub_changes.check(self.BASE, ['#1 a', '#2 b', '#3 C']))


class MainTest(unittest.TestCase):
    def run_main(self, base, head):
        with tempfile.TemporaryDirectory() as directory:
            paths = []
            for name, text in (('base.java', base), ('head.java', head)):
                path = os.path.join(directory, name)
                if text is not None:
                    with open(path, 'w', encoding='utf-8') as f:
                        f.write(text)
                paths.append(path)
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                code = stub_changes.main(['stub_changes.py'] + paths)
            return code, out.getvalue()

    def test_equal_passes(self):
        self.assertEqual(0, self.run_main(source('#1 a', '#2 b'), source('#1 a', '#2 b'))[0])

    def test_appended_passes(self):
        self.assertEqual(0, self.run_main(source('#1 a'), source('#1 a', '#2 b'))[0])

    def test_dropped_fails(self):
        code, out = self.run_main(source('#1 a', '#2 b'), source('#1 a'))
        self.assertEqual(1, code)
        self.assertIn('::error::', out)

    def test_reordered_fails(self):
        self.assertEqual(1, self.run_main(source('#1 a', '#2 b'), source('#2 b', '#1 a'))[0])

    def test_inserted_in_the_middle_fails(self):
        self.assertEqual(1, self.run_main(source('#1 a', '#2 b'), source('#1 a', '#3 c', '#2 b'))[0])

    def test_no_base_passes(self):
        self.assertEqual(0, self.run_main(None, source('#1 a'))[0])

    def test_no_head_is_an_error(self):
        self.assertEqual(2, self.run_main(source('#1 a'), None)[0])

    def test_unparsable_head_is_an_error(self):
        self.assertEqual(2, self.run_main(source('#1 a'), 'class ErlangStubChanges {}\n')[0])

    def test_wrong_arguments(self):
        with contextlib.redirect_stderr(io.StringIO()):
            self.assertEqual(2, stub_changes.main(['stub_changes.py', 'base.java']))


if __name__ == '__main__':
    unittest.main()
