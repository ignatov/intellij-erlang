"""Unit tests of check_areas.py: python3 -m unittest discover -s scripts/areas -p 'test_*.py' -v"""

import contextlib
import io
import json
import os
import tempfile
import unittest

import check_areas

MAP = '''# a comment
- name: parser
  label: area:parser
  description: "Lexer and grammar."
  paths:
    - "grammars/**"
    - "src/org/intellij/erlang/parser/**"
- name: macro
  label: area:macro
  description: "Macro layer."
  paths:
    - "src/org/intellij/erlang/parser/macro/**"
- name: plugin-core
  label: area:plugin-core
  description: "Shared utilities."
  paths:
    - "src/org/intellij/erlang/utils/**"
    - "testData/mockSdk-*/**"
- name: rebar-tools
  label: area:rebar-tools
  description: "External tools."
  paths:
    - "src/org/intellij/erlang/utils/ExtProcessUtil.java"
'''


def area(name, *paths):
    return '- name: %s\n  label: area:%s\n  description: "d"\n  paths:\n%s' % (
        name, name, ''.join('    - "%s"\n' % p for p in paths))


class ParseTest(unittest.TestCase):
    def test_fields(self):
        m = check_areas.parse(MAP)
        self.assertEqual(['parser', 'macro', 'plugin-core', 'rebar-tools'], [a.name for a in m.areas])
        self.assertEqual('area:macro', m.label('macro'))
        self.assertEqual('Lexer and grammar.', m.by_name['parser'].description)
        self.assertEqual(['grammars/**', 'src/org/intellij/erlang/parser/**'], m.by_name['parser'].paths)

    def test_unknown_line(self):
        with self.assertRaisesRegex(check_areas.MapError, 'line 6'):
            check_areas.parse(area('parser', 'grammars/**') + '  owner: me\n')

    def test_path_outside_paths(self):
        with self.assertRaises(check_areas.MapError):
            check_areas.parse('- name: parser\n  label: area:parser\n    - "grammars/**"\n')

    def test_text_before_first_area(self):
        with self.assertRaisesRegex(check_areas.MapError, 'line 1'):
            check_areas.parse('areas:\n' + area('parser', 'grammars/**'))

    def test_label_must_follow_name(self):
        with self.assertRaisesRegex(check_areas.MapError, 'label must be area:parser'):
            check_areas.parse(area('parser', 'grammars/**').replace('area:parser', 'area:lexer'))

    def test_duplicate_area(self):
        with self.assertRaisesRegex(check_areas.MapError, 'second area'):
            check_areas.parse(area('parser', 'grammars/**') + area('parser', 'gen/**'))

    def test_bad_globs(self):
        for glob in ('/src/**', 'src/a?.java', 'build.gradle', 'scripts/**', 'src//a'):
            with self.assertRaises(check_areas.MapError, msg=glob):
                check_areas.parse(area('parser', glob))

    def test_missing_description(self):
        with self.assertRaisesRegex(check_areas.MapError, 'no description'):
            check_areas.parse('- name: parser\n  label: area:parser\n  paths:\n    - "gen/**"\n')


class ClassifyTest(unittest.TestCase):
    def setUp(self):
        self.m = check_areas.parse(MAP)

    def test_star_does_not_cross_a_slash(self):
        self.assertEqual('plugin-core', self.m.area_of('testData/mockSdk-R27/lib/x.erl'))
        self.assertIsNone(self.m.area_of('testData/mockSdk/x.erl'))

    def test_double_star_crosses_slashes(self):
        self.assertEqual('parser', self.m.area_of('grammars/a/b/erlang.bnf'))

    def test_more_specific_glob_wins(self):
        self.assertEqual('macro', self.m.area_of('src/org/intellij/erlang/parser/macro/ErlangMacroExpander.java'))
        self.assertEqual('parser', self.m.area_of('src/org/intellij/erlang/parser/Erlang.flex'))

    def test_file_glob_wins_over_package(self):
        self.assertEqual('rebar-tools', self.m.area_of('src/org/intellij/erlang/utils/ExtProcessUtil.java'))
        self.assertEqual('plugin-core', self.m.area_of('src/org/intellij/erlang/utils/NewUtil.java'))

    def test_no_area(self):
        self.assertIsNone(self.m.area_of('build.gradle'))
        self.assertEqual([], self.m.classify('src/org/intellij/erlang/newpkg/A.java'))

    def test_normalized_path(self):
        self.assertEqual('parser', self.m.area_of('./grammars/erlang.bnf'))

    def test_tie(self):
        m = check_areas.parse(area('a', 'src/x/**') + area('b', 'src/x/**'))
        self.assertEqual(['a', 'b'], m.classify('src/x/y.java'))
        self.assertIsNone(m.area_of('src/x/y.java'))

    def test_touched_in_map_order(self):
        paths = ['src/org/intellij/erlang/utils/ExtProcessUtil.java', 'build.gradle', 'grammars/erlang.bnf',
                 'src/org/intellij/erlang/parser/Erlang.flex']
        self.assertEqual(['parser', 'rebar-tools'], self.m.touched(paths))
        self.assertEqual([], self.m.touched([]))


class CheckTest(unittest.TestCase):
    def test_clean(self):
        m = check_areas.parse(MAP)
        files = ['grammars/erlang.bnf', 'src/org/intellij/erlang/parser/macro/M.java',
                 'src/org/intellij/erlang/utils/ExtProcessUtil.java', 'testData/mockSdk-R27/a.erl']
        self.assertEqual([], check_areas.check(m, files))

    def test_problems(self):
        m = check_areas.parse(MAP + area('copy', 'grammars/**'))
        problems = check_areas.check(m, ['grammars/erlang.bnf', 'src/org/intellij/erlang/newpkg/A.java'])
        self.assertIn('no area: src/org/intellij/erlang/newpkg/A.java', problems)
        self.assertIn('two or more areas with equally specific globs (parser, copy): grammars/erlang.bnf', problems)
        self.assertIn('the glob testData/mockSdk-*/** of area plugin-core matches no file', problems)


class CliTest(unittest.TestCase):
    def setUp(self):
        fd, self.path = tempfile.mkstemp(suffix='.yml')
        with os.fdopen(fd, 'w') as f:
            f.write(MAP)

    def tearDown(self):
        os.remove(self.path)

    def run_main(self, *argv):
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            code = check_areas.main(list(argv) + ['--map', self.path])
        return code, out.getvalue()

    def test_which(self):
        code, out = self.run_main('which', 'grammars/erlang.bnf', 'build.gradle')
        self.assertEqual(0, code)
        self.assertEqual('grammars/erlang.bnf\tparser\nbuild.gradle\t-\n', out)

    def test_touched_labels(self):
        code, out = self.run_main('touched', '--labels', 'src/org/intellij/erlang/parser/macro/M.java',
                                  'grammars/erlang.bnf')
        self.assertEqual((0, 'area:parser\narea:macro\n'), (code, out))

    def test_touched_json(self):
        code, out = self.run_main('touched', '--json', 'build.gradle')
        self.assertEqual([], json.loads(out))

    def test_broken_map(self):
        with open(self.path, 'a') as f:
            f.write('nonsense\n')
        err = io.StringIO()
        with contextlib.redirect_stderr(err):
            code, _ = self.run_main('which', 'gen/a')
        self.assertEqual(2, code)
        self.assertIn('not in the format', err.getvalue())


class RepositoryMapTest(unittest.TestCase):
    """The committed map against the files of this checkout: what the check command prints."""

    def test_committed_map_covers_the_repository(self):
        m = check_areas.load()
        self.assertEqual([], check_areas.check(m, check_areas.tracked_files()))

    def test_every_area_has_a_label(self):
        for a in check_areas.load().areas:
            self.assertEqual('area:' + a.name, a.label)


if __name__ == '__main__':
    unittest.main()
