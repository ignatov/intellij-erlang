"""Unit tests of totals.py: python3 -m unittest discover -s scripts/coverage -p 'test_*.py' -v"""

import contextlib
import io
import os
import tempfile
import textwrap
import unittest

import totals

REPORT = """\
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<!DOCTYPE report PUBLIC "-//JACOCO//DTD Report 1.1//EN" "report.dtd">
<report name="intellij-erlang">
  <package name="org/intellij/erlang/sdk">
    <class name="org/intellij/erlang/sdk/A" sourcefilename="A.java">
      <counter type="LINE" missed="3" covered="7"/>
    </class>
    <class name="org/intellij/erlang/sdk/A$1" sourcefilename="A.java">
      <counter type="LINE" missed="0" covered="2"/>
    </class>
    <sourcefile name="A.java">
      <counter type="LINE" missed="3" covered="8"/>
      <counter type="BRANCH" missed="1" covered="3"/>
    </sourcefile>
  </package>
  <package name="org/intellij/erlang/parser">
    <sourcefile name="ErlangParser.java">
      <counter type="LINE" missed="1" covered="9"/>
      <counter type="BRANCH" missed="2" covered="2"/>
    </sourcefile>
    <sourcefile name="ErlangParserUtil.java">
      <counter type="LINE" missed="0" covered="5"/>
    </sourcefile>
  </package>
  <package name="org/intellij/erlang/jps/builder">
    <sourcefile name="ErlangBuilder.java">
      <counter type="LINE" missed="4" covered="0"/>
      <counter type="BRANCH" missed="2" covered="0"/>
    </sourcefile>
  </package>
  <package name="org/intellij/erlang/elsewhere">
    <sourcefile name="Gone.java">
      <counter type="LINE" missed="1" covered="1"/>
    </sourcefile>
  </package>
</report>
"""


class TotalsTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.dir.cleanup)
        self.repository = os.path.join(self.dir.name, 'repo')
        for path in ['src/org/intellij/erlang/sdk/A.java', 'gen/org/intellij/erlang/parser/ErlangParser.java',
                     'src/org/intellij/erlang/parser/ErlangParserUtil.java',
                     'jps-plugin/src/org/intellij/erlang/jps/builder/ErlangBuilder.java']:
            full = os.path.join(self.repository, path)
            os.makedirs(os.path.dirname(full), exist_ok=True)
            open(full, 'w').close()
        self.report = os.path.join(self.dir.name, 'report.xml')
        with open(self.report, 'w', encoding='utf-8') as f:
            f.write(REPORT)

    def test_sums_source_files_by_root(self):
        result = totals.totals(totals.parse(self.report), self.repository)
        # A line of A.java that both A and A$1 have counts once: 8 covered, not 9.
        self.assertEqual([3, 13, 1, 3, 2], result['src/ (without gen/)'])
        self.assertEqual([1, 9, 2, 2, 1], result['gen/'])
        self.assertEqual([4, 0, 2, 0, 1], result['jps-plugin/'])
        self.assertEqual([1, 1, 0, 0, 1], result[totals.OTHER])

    def test_table(self):
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            totals.main([self.report, '--repository', self.repository])
        self.assertEqual(textwrap.dedent("""\
            | Part | Files | Lines covered | Line % | Branches covered | Branch % |
            |---|---|---|---|---|---|
            | src/ (without gen/) | 2 | 13 of 16 | 81.2% | 3 of 4 | 75.0% |
            | gen/ | 1 | 9 of 10 | 90.0% | 2 of 4 | 50.0% |
            | jps-plugin/ | 1 | 0 of 4 | 0.0% | 0 of 2 | 0.0% |
            | other | 1 | 1 of 2 | 50.0% | 0 of 0 | - |
            """), out.getvalue())

    def test_no_other_row_when_every_file_has_a_root(self):
        result = totals.totals(totals.parse(self.report), self.repository)
        result[totals.OTHER] = [0, 0, 0, 0, 0]
        self.assertNotIn('| other |', totals.table(result))


if __name__ == '__main__':
    unittest.main()
