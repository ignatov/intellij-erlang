"""Unit tests of prcoverage.py: python3 -m unittest discover -s scripts/coverage -p 'test_*.py' -v"""

import contextlib
import io
import json
import os
import tempfile
import textwrap
import unittest

import prcoverage


def diff(*parts):
    return '\n'.join(textwrap.dedent(p).strip('\n') for p in parts) + '\n'


MODIFIED_SDK = """
    diff --git a/src/org/intellij/erlang/sdk/ErlangSdkType.java b/src/org/intellij/erlang/sdk/ErlangSdkType.java
    index 1111111..2222222 100644
    --- a/src/org/intellij/erlang/sdk/ErlangSdkType.java
    +++ b/src/org/intellij/erlang/sdk/ErlangSdkType.java
    @@ -10,0 +11,3 @@ class ErlangSdkType {
    +  a
    +  b
    +  c
    @@ -40 +43 @@ class ErlangSdkType {
    -  old
    +  new
    @@ -50,2 +53,0 @@ class ErlangSdkType {
    -  gone
    -  gone
    """
ADDED_TEST = """
    diff --git a/tests/org/intellij/erlang/sdk/ErlangSdkTypeTest.java b/tests/org/intellij/erlang/sdk/ErlangSdkTypeTest.java
    new file mode 100644
    index 0000000..3333333
    --- /dev/null
    +++ b/tests/org/intellij/erlang/sdk/ErlangSdkTypeTest.java
    @@ -0,0 +1,2 @@
    +class ErlangSdkTypeTest {
    +}
    """
DELETED_SOURCE = """
    diff --git a/src/org/intellij/erlang/Old.java b/src/org/intellij/erlang/Old.java
    deleted file mode 100644
    index 4444444..0000000
    --- a/src/org/intellij/erlang/Old.java
    +++ /dev/null
    @@ -1,2 +0,0 @@
    -class Old {
    -}
    """
RENAMED_SOURCE = """
    diff --git a/src/org/intellij/erlang/A.java b/src/org/intellij/erlang/editor/B.java
    similarity index 90%
    rename from src/org/intellij/erlang/A.java
    rename to src/org/intellij/erlang/editor/B.java
    index 5555555..6666666 100644
    --- a/src/org/intellij/erlang/A.java
    +++ b/src/org/intellij/erlang/editor/B.java
    @@ -5 +5 @@ class B {
    -  x
    +  y
    """
PURE_RENAME = """
    diff --git a/src/org/intellij/erlang/C.java b/src/org/intellij/erlang/D.java
    similarity index 100%
    rename from src/org/intellij/erlang/C.java
    rename to src/org/intellij/erlang/D.java
    """
GEN_AND_DATA = """
    diff --git a/gen/org/intellij/erlang/parser/ErlangParser.java b/gen/org/intellij/erlang/parser/ErlangParser.java
    index 7777777..8888888 100644
    --- a/gen/org/intellij/erlang/parser/ErlangParser.java
    +++ b/gen/org/intellij/erlang/parser/ErlangParser.java
    @@ -1 +1 @@
    -a
    +b
    diff --git a/grammars/erlang.bnf b/grammars/erlang.bnf
    index 9999999..aaaaaaa 100644
    --- a/grammars/erlang.bnf
    +++ b/grammars/erlang.bnf
    @@ -1 +1 @@
    -a
    +b
    diff --git a/testData/parser/X.txt b/testData/parser/X.txt
    index bbbbbbb..ccccccc 100644
    --- a/testData/parser/X.txt
    +++ b/testData/parser/X.txt
    @@ -1 +1 @@
    -a
    +b
    """
JPS_CHANGE = """
    diff --git a/jps-plugin/src/org/intellij/erlang/jps/builder/ErlangBuilder.java b/jps-plugin/src/org/intellij/erlang/jps/builder/ErlangBuilder.java
    index ddddddd..eeeeeee 100644
    --- a/jps-plugin/src/org/intellij/erlang/jps/builder/ErlangBuilder.java
    +++ b/jps-plugin/src/org/intellij/erlang/jps/builder/ErlangBuilder.java
    @@ -7,0 +8,2 @@ class ErlangBuilder {
    +  p
    +  q
    """

REPORT = """\
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<!DOCTYPE report PUBLIC "-//JACOCO//DTD Report 1.1//EN" "report.dtd">
<report name="intellij-erlang">
  <package name="org/intellij/erlang/sdk">
    <sourcefile name="ErlangSdkType.java">
      <line nr="11" mi="0" ci="3" mb="0" cb="0"/>
      <line nr="12" mi="2" ci="0" mb="0" cb="0"/>
      <line nr="43" mi="4" ci="0" mb="2" cb="0"/>
    </sourcefile>
  </package>
  <package name="org/intellij/erlang/editor">
    <sourcefile name="B.java">
      <line nr="5" mi="0" ci="1" mb="0" cb="0"/>
    </sourcefile>
  </package>
  <package name="org/intellij/erlang/jps/builder">
    <sourcefile name="ErlangBuilder.java">
      <line nr="8" mi="3" ci="0" mb="0" cb="0"/>
      <line nr="9" mi="3" ci="0" mb="0" cb="0"/>
    </sourcefile>
  </package>
</report>
"""


class ParseDiffTest(unittest.TestCase):
    def test_added_lines_of_a_modified_file(self):
        [f] = prcoverage.parse_diff(diff(MODIFIED_SDK))
        self.assertEqual('src/org/intellij/erlang/sdk/ErlangSdkType.java', f['path'])
        self.assertEqual('modified', f['status'])
        # A hunk that only deletes (+53,0) adds no line.
        self.assertEqual([11, 12, 13, 43], f['added'])

    def test_added_deleted_and_renamed_files(self):
        files = prcoverage.parse_diff(diff(ADDED_TEST, DELETED_SOURCE, RENAMED_SOURCE, PURE_RENAME))
        self.assertEqual([('tests/org/intellij/erlang/sdk/ErlangSdkTypeTest.java', 'added', [1, 2]),
                          ('src/org/intellij/erlang/Old.java', 'deleted', []),
                          ('src/org/intellij/erlang/editor/B.java', 'renamed', [5]),
                          ('src/org/intellij/erlang/D.java', 'renamed', [])],
                         [(f['path'], f['status'], f['added']) for f in files])


class Checkout(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.dir.cleanup)
        self.repository = self.dir.name

    def write(self, path, text=''):
        full = os.path.join(self.repository, path)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, 'w', encoding='utf-8') as f:
            f.write(textwrap.dedent(text))
        return full


class PlanTest(Checkout):
    def change(self, path, status='modified'):
        return {'path': path, 'old_path': path, 'status': status, 'added': [1]}

    def test_runs_test_classes_and_says_why_others_are_left_out(self):
        self.write('tests/org/intellij/erlang/sdk/ErlangSdkTypeTest.java', 'public class ErlangSdkTypeTest {}')
        self.write('tests/org/intellij/erlang/SmallIdeVariant.java', """
            public class SmallIdeVariant extends Base {
              public void testOne() {}
            }""")
        self.write('tests/org/intellij/erlang/utils/ErlangTestBase.java', 'public abstract class ErlangTestBase {}')
        self.write('tests/org/intellij/erlang/utils/Helper.java', 'class Helper { void help() {} }')
        files = [self.change('tests/org/intellij/erlang/sdk/ErlangSdkTypeTest.java'),
                 self.change('tests/org/intellij/erlang/SmallIdeVariant.java'),
                 self.change('tests/org/intellij/erlang/utils/ErlangTestBase.java'),
                 self.change('tests/org/intellij/erlang/utils/Helper.java'),
                 self.change('tests/org/intellij/erlang/build/ErlangRebarBuildTest.java'),
                 self.change('jps-plugin/tests/org/intellij/erlang/jps/ErlangBuilderTest.java'),
                 self.change('tests/org/intellij/erlang/GoneTest.java', 'deleted'),
                 self.change('src/org/intellij/erlang/sdk/ErlangSdkType.java')]
        result = prcoverage.plan(files, self.repository)
        self.assertEqual(['org.intellij.erlang.SmallIdeVariant', 'org.intellij.erlang.sdk.ErlangSdkTypeTest'],
                         result['run'])
        self.assertEqual({'org.intellij.erlang.utils.ErlangTestBase': 'an abstract base class: its subclasses are not run',
                          'org.intellij.erlang.utils.Helper':
                              'not a test class (no name ending with Test, no test method)',
                          'org.intellij.erlang.build.ErlangRebarBuildTest':
                              'a compilation test: only testCompilation runs it',
                          'jps-plugin/tests/org/intellij/erlang/jps/ErlangBuilderTest.java':
                              'a compilation test: only testCompilation runs it',
                          'org.intellij.erlang.GoneTest': 'deleted'},
                         {x['class']: x['why'] for x in result['left_out']})
        self.assertEqual([], result['notes'])

    def test_no_test_change(self):
        result = prcoverage.plan([self.change('src/org/intellij/erlang/sdk/ErlangSdkType.java')], self.repository)
        self.assertEqual([], result['run'])
        self.assertEqual(['This pull request changes no test class, so no test ran.'], result['notes'])

    def test_only_test_data(self):
        result = prcoverage.plan([self.change('testData/parser/X.txt')], self.repository)
        self.assertEqual(['This pull request changes test data but no test class, so no test ran.'], result['notes'])


class ReportTest(Checkout):
    def setUp(self):
        super().setUp()
        self.lines = prcoverage.report_lines(self.write('report.xml', REPORT))
        self.plan = {'run': ['org.intellij.erlang.sdk.ErlangSdkTypeTest'], 'left_out': [], 'notes': []}

    def test_executed_and_not_executed_lines(self):
        files = prcoverage.parse_diff(diff(MODIFIED_SDK, RENAMED_SOURCE, DELETED_SOURCE, ADDED_TEST))
        result = prcoverage.report(files, self.lines, self.plan, False)
        # Line 13 holds no code; 11 and B.java:5 ran; 12 and 43 did not.
        self.assertEqual({'changed': 4, 'executed': 2, 'not_executed': 2, 'not_measured': 0, 'unknown': 0},
                         result['totals'])
        self.assertEqual(textwrap.dedent("""\
            changed lines: 2 of 4 executed; not executed: ErlangSdkType.java:12, ErlangSdkType.java:43.
            Tests run with coverage: ErlangSdkTypeTest.
            Deleted source files (nothing to measure): Old.java.
            "Executed" means a test ran the line; it does not mean a test checked the result."""), result['text'])

    def test_compilation_code_is_not_measured_without_test_compilation(self):
        files = prcoverage.parse_diff(diff(JPS_CHANGE))
        result = prcoverage.report(files, self.lines, self.plan, False)
        self.assertEqual(2, result['totals']['not_measured'])
        self.assertIn('changed lines: 0 of 2 executed.\nNot measured by this run (only testCompilation runs this code,'
                      ' and it did not run): ErlangBuilder.java:8-9.', result['text'])
        with_compilation = prcoverage.report(files, self.lines, self.plan, True)
        self.assertIn('changed lines: 0 of 2 executed; not executed: ErlangBuilder.java:8-9.', with_compilation['text'])

    def test_only_gen_grammar_and_test_data(self):
        files = prcoverage.parse_diff(diff(GEN_AND_DATA))
        result = prcoverage.report(files, None, prcoverage.plan(files, self.repository), False)
        self.assertEqual(textwrap.dedent("""\
            changed lines: none in the Java files of src/ or jps-plugin/src/; nothing to measure.
            This pull request changes test data but no test class, so no test ran.
            "Executed" means a test ran the line; it does not mean a test checked the result."""), result['text'])

    def test_no_test_ran(self):
        files = prcoverage.parse_diff(diff(MODIFIED_SDK))
        result = prcoverage.report(files, None, prcoverage.plan(files, self.repository), False)
        self.assertTrue(result['text'].startswith(
            'changed lines: not measured, because no test of this pull request ran with coverage.\n'
            'This pull request changes no test class, so no test ran.'), result['text'])

    def test_run_that_wrote_no_report(self):
        files = prcoverage.parse_diff(diff(MODIFIED_SDK, ADDED_TEST))
        result = prcoverage.report(files, None, self.plan, False, gradle_status=1)
        self.assertTrue(result['text'].startswith('changed lines: not measured, because the run of the tests with '
                                                  'coverage wrote no report (Gradle ended with status 1).'))
        self.assertEqual([], result['tests_run'])

    def test_failed_tests_and_a_file_missing_from_the_report(self):
        files = prcoverage.parse_diff(diff(MODIFIED_SDK, PURE_RENAME, """
            diff --git a/src/org/intellij/erlang/New.java b/src/org/intellij/erlang/New.java
            new file mode 100644
            --- /dev/null
            +++ b/src/org/intellij/erlang/New.java
            @@ -0,0 +1,3 @@
            +a
            +b
            +c
            """))
        result = prcoverage.report(files, self.lines, self.plan, False, 1, ['ErlangSdkTypeTest'])
        self.assertIn('Not in the coverage report: New.java:1-3.', result['text'])
        self.assertIn('Failed in this run: ErlangSdkTypeTest.', result['text'])
        self.assertIn('Gradle ended with status 1.', result['text'])
        # A pure rename adds no line and is not listed.
        self.assertNotIn('D.java', result['text'])

    def test_many_places_are_cut(self):
        entries = [('src/a/F%d.java' % i, [[1, 1]]) for i in range(20)]
        self.assertTrue(prcoverage.places(entries).endswith('F11.java:1, and 8 more'))

    def test_same_file_name_in_two_packages(self):
        self.assertEqual('src/a/X.java:1, src/b/X.java:2-3',
                         prcoverage.places([('src/a/X.java', [[1, 1]]), ('src/b/X.java', [[2, 3]])]))


class MainTest(Checkout):
    def test_plan_and_report_commands(self):
        self.write('tests/org/intellij/erlang/sdk/ErlangSdkTypeTest.java', 'public class ErlangSdkTypeTest {}')
        changes = self.write('changes.diff', diff(MODIFIED_SDK, ADDED_TEST))
        report = self.write('report.xml', REPORT)
        plan_file, result_file = os.path.join(self.repository, 'plan.json'), os.path.join(self.repository, 'r.json')
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            prcoverage.main(['plan', '--diff', changes, '--repository', self.repository, '--json', plan_file])
        self.assertEqual('org.intellij.erlang.sdk.ErlangSdkTypeTest\n', out.getvalue())
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            prcoverage.main(['report', '--diff', changes, '--plan', plan_file, '--report', report,
                             '--json', result_file])
        self.assertTrue(out.getvalue().startswith('changed lines: 1 of 3 executed; not executed: '
                                                  'ErlangSdkType.java:12, ErlangSdkType.java:43.\n'))
        with open(result_file, encoding='utf-8') as f:
            self.assertEqual(1, json.load(f)['totals']['executed'])

    def test_missing_report_file_means_nothing_measured(self):
        changes = self.write('changes.diff', diff(MODIFIED_SDK))
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            prcoverage.main(['report', '--diff', changes, '--report', os.path.join(self.repository, 'none.xml')])
        self.assertTrue(out.getvalue().startswith('changed lines: not measured'))


if __name__ == '__main__':
    unittest.main()
