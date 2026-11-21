"""Tests of pr-coverage.sh in a scratch repository with a fake gradlew:
python3 -m unittest discover -s scripts/coverage -p 'test_*.py' -v"""

import json
import os
import shutil
import subprocess
import tempfile
import textwrap
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
# The fake gradlew records its arguments and runs fake-gradle.sh of the scratch repository, when a test wrote one.
FAKE_GRADLEW = """\
#!/usr/bin/env bash
echo "$*" >> gradlew.calls
if [[ -f fake-gradle.sh ]]; then
  . ./fake-gradle.sh
fi
exit 0
"""


class WrapperTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.dir.cleanup)
        self.repo = self.dir.name
        self.env = dict(os.environ, GIT_CONFIG_NOSYSTEM='1', GIT_CONFIG_GLOBAL=os.devnull,
                        GIT_AUTHOR_NAME='t', GIT_AUTHOR_EMAIL='t@example.com',
                        GIT_COMMITTER_NAME='t', GIT_COMMITTER_EMAIL='t@example.com')
        os.makedirs(os.path.join(self.repo, 'scripts', 'coverage'))
        for name in ('pr-coverage.sh', 'prcoverage.py'):
            shutil.copy(os.path.join(HERE, name), os.path.join(self.repo, 'scripts', 'coverage', name))
        self.write('gradlew', FAKE_GRADLEW)
        os.chmod(os.path.join(self.repo, 'gradlew'), 0o755)
        self.write('.gitignore', 'build/\ngradlew.calls\nfake-gradle.sh\n')
        self.git('init', '-q')
        self.write('src/org/intellij/erlang/A.java', 'class A {\n}\n')
        self.write('gen/org/intellij/erlang/parser/ErlangParser.java', 'class ErlangParser {\n}\n')
        self.commit('base')

    def write(self, path, text):
        full = os.path.join(self.repo, path)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, 'w', encoding='utf-8') as f:
            f.write(textwrap.dedent(text))

    def read(self, path):
        with open(os.path.join(self.repo, path), encoding='utf-8') as f:
            return f.read()

    def git(self, *args):
        subprocess.run(['git', *args], cwd=self.repo, env=self.env, check=True, capture_output=True)

    def commit(self, message):
        self.git('add', '-A')
        self.git('commit', '-q', '--no-verify', '-m', message)

    def run_script(self, *args):
        done = subprocess.run(['bash', 'scripts/coverage/pr-coverage.sh', *args], cwd=self.repo, env=self.env,
                              capture_output=True, text=True, timeout=60)
        self.assertEqual(0, done.returncode, done.stderr)
        return done.stdout

    def gradle_calls(self):
        path = os.path.join(self.repo, 'gradlew.calls')
        return self.read('gradlew.calls').splitlines() if os.path.exists(path) else []

    def test_parser_fix_without_a_source_line_starts_no_gradle(self):
        self.write('gen/org/intellij/erlang/parser/ErlangParser.java', 'class ErlangParser {\n  int x;\n}\n')
        self.write('testData/parser/X.txt', 'tree\n')
        self.write('tests/org/intellij/erlang/parser/ErlangParserTest.java',
                   'public class ErlangParserTest {\n  public void testX() {}\n}\n')
        self.commit('parser fix')
        out = self.run_script('HEAD^')
        self.assertEqual([], self.gradle_calls())
        self.assertTrue(out.startswith('changed lines: none in the Java files of src/ or jps-plugin/src/; '
                                       'nothing to measure.\n'), out)
        self.assertIn('Not run: ErlangParserTest (no changed line in the Java files of src/ or jps-plugin/src/ to '
                      'measure).', out)
        self.assertEqual(out, self.read('build/coverage-pr/result.txt'))

    def test_what_ran_comes_from_the_test_results(self):
        self.write('src/org/intellij/erlang/A.java', 'class A {\n  int a;\n}\n')
        for name in ('bif/ErlangBifParser', 'sdk/ATest'):
            self.write('tests/org/intellij/erlang/%s.java' % name,
                       'public class %s {\n  public void testX() {}\n}\n' % name.split('/')[1])
        self.commit('change')
        # The fake run writes a report and the results: ATest ran and failed, ErlangBifParser is @Ignore'd.
        self.write('fake-gradle.sh', """\
            mkdir -p build/reports/jacoco/coverageReport build/test-results/test
            cat > build/reports/jacoco/coverageReport/coverageReport.xml <<'XML'
            <report name="r"><package name="org/intellij/erlang"><sourcefile name="A.java">
            <line nr="2" mi="0" ci="1" mb="0" cb="0"/></sourcefile></package></report>
            XML
            cat > build/test-results/test/TEST-org.intellij.erlang.sdk.ATest.xml <<'XML'
            <testsuite name="org.intellij.erlang.sdk.ATest"><testcase name="testX"><failure>boom</failure></testcase>
            </testsuite>
            XML
            cat > build/test-results/test/TEST-org.intellij.erlang.bif.ErlangBifParser.xml <<'XML'
            <testsuite name="org.intellij.erlang.bif.ErlangBifParser"><testcase name="testX"><skipped/></testcase>
            </testsuite>
            XML
            """)
        out = self.run_script('HEAD^')
        self.assertEqual(1, len(self.gradle_calls()))
        self.assertIn('changed lines: 1 of 1 executed.\n', out)
        self.assertIn('Tests run with coverage: ATest.\nPlanned, but no test of it ran: ErlangBifParser.\n', out)
        self.assertIn('Failed in this run: ATest.\n', out)

    def test_unknown_base_replaces_the_files_of_an_earlier_run(self):
        for name in ('result.txt', 'result.json', 'plan.json'):
            self.write('build/coverage-pr/' + name, '{"text": "changed lines: 3 of 3 executed."}')
        out = self.run_script('no-such-commit')
        text = "changed lines: not measured, because the base commit 'no-such-commit' is not known."
        self.assertEqual(text + '\n', out)
        self.assertEqual(out, self.read('build/coverage-pr/result.txt'))
        self.assertEqual({'text': text}, json.loads(self.read('build/coverage-pr/result.json')))
        self.assertFalse(os.path.exists(os.path.join(self.repo, 'build/coverage-pr/plan.json')))
        self.assertEqual([], self.gradle_calls())


if __name__ == '__main__':
    unittest.main()
