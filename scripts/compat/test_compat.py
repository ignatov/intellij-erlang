"""Unit tests of compat.py: python3 -m unittest discover -s scripts/compat -p 'test_*.py' -v"""

import argparse
import contextlib
import io
import json
import os
import tempfile
import textwrap
import unittest
import unittest.mock

import compat

try:
    import yaml
except ImportError:  # PyYAML is on the GitHub runners; without it only the workflow tests are skipped.
    yaml = None

REPOSITORY = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..')
# The files of this repository, outside scripts/compat, that these tests read. The workflow's checkout of the
# compatibility check must contain them (WorkflowTest.test_tool_checkout_has_what_the_tests_read).
REPOSITORY_PATHS = [
    '.github/workflows/gradle.yml',
    '.github/workflows/compat.yml',
    'src/org/intellij/erlang/sdk/ErlangSdkRelease.java',
    'testData/mockSdk-R27',
]


def write(path, content):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, 'w', encoding='utf-8') as f:
        f.write(textwrap.dedent(content).lstrip('\n') if isinstance(content, str) else content)


JAVAC_LOG = """
> Task :compileJava
/repo/src/org/intellij/erlang/Foo.java:12: error: cannot find symbol
    Bar.baz();
       ^
  symbol:   method baz()
  location: class Bar
/repo/src/org/intellij/erlang/Qux.java:3: error: package com.intellij.gone does not exist
import com.intellij.gone.Thing;
                        ^
error: warnings found and -Werror specified
2 errors

FAILURE: Build failed with an exception.
"""

JUNIT_XML = """
<?xml version="1.0" encoding="UTF-8"?>
<testsuite name="org.intellij.erlang.FooTest" tests="4" skipped="1" failures="1" errors="1">
  <testcase name="testOk" classname="org.intellij.erlang.FooTest" time="0.1"/>
  <testcase name="testFails" classname="org.intellij.erlang.FooTest" time="0.1">
    <failure message="expected:&lt;1&gt; but was:&lt;2&gt;" type="junit.framework.AssertionFailedError">trace</failure>
  </testcase>
  <testcase name="testErrors" classname="org.intellij.erlang.FooTest" time="0.1">
    <error type="java.lang.NullPointerException">java.lang.NullPointerException: boom
	at Foo</error>
  </testcase>
  <testcase name="testSkipped" classname="org.intellij.erlang.FooTest" time="0.0">
    <skipped/>
  </testcase>
</testsuite>
"""

PASSING_XML = """
<?xml version="1.0" encoding="UTF-8"?>
<testsuite name="org.intellij.erlang.BarTest" tests="1" skipped="0" failures="0" errors="0">
  <testcase name="testOk" classname="org.intellij.erlang.BarTest" time="0.1"/>
</testsuite>
"""

COMPATIBLE_VERDICT = ('Compatible. 11 usages of scheduled for removal API and 49 usages of deprecated API. '
                      '7 usages of experimental API. 8 usages of internal API. 1 non-extendable API usage violation')
BROKEN_VERDICT = '2 compatibility problems. 3 usages of deprecated API'

IDE_INFO = {'name': 'IntelliJ IDEA', 'productCode': 'IU', 'version': '2026.3', 'versionSuffix': None,
            'buildNumber': '263.7307.200', 'platformPath': '/x'}


class Fixture(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = os.path.join(self.temp.name, 'repo')
        self.logs = os.path.join(self.temp.name, 'logs')
        os.makedirs(self.root)
        os.makedirs(self.logs)

    def tearDown(self):
        self.temp.cleanup()

    def check_ran(self, name, exit_code, log=''):
        write(os.path.join(self.logs, name + '.log'), log)
        write(os.path.join(self.logs, name + '.exit'), '%d\n' % exit_code)

    def verdict(self, reports_dir, verdict, problems=None, ide='IU-263.7307.200'):
        plugin = os.path.join(self.root, reports_dir, ide, 'plugins', 'org.jetbrains.erlang', '0.11.1302')
        write(os.path.join(plugin, 'verification-verdict.txt'), verdict + '\n')
        write(os.path.join(plugin, 'dependencies.txt'), 'a huge dependency tree\n')
        for name, lines in (problems or {}).items():
            write(os.path.join(plugin, name + '.txt'), '\n'.join(lines) + '\n')


class TrimTest(unittest.TestCase):
    def test_trimmed(self):
        self.assertEqual({'total': 2, 'trimmed': False, 'items': [1, 2]}, compat.trimmed([1, 2]))
        self.assertEqual({'total': 5, 'trimmed': True, 'items': [0, 1]}, compat.trimmed(range(5), 2))

    def test_text(self):
        self.assertEqual('abc', compat.text('abc', 3))
        self.assertEqual('a' * 6 + '... [trimmed]', compat.text('a' * 30, 19))
        self.assertIsNone(compat.text(None))

    def test_text_stays_within_the_limit(self):
        self.assertEqual(compat.TEXT_LIMIT, len(compat.text('x' * 5000)))

    def test_log_tail_stays_within_the_limit_and_keeps_the_end(self):
        log = '\n'.join('line %d %s' % (i, 'x' * 200) for i in range(100)) + '\n'
        tail = compat.log_tail(log)
        self.assertLessEqual(len(tail), compat.TEXT_LIMIT)
        self.assertTrue(tail.endswith('line 99 ' + 'x' * 200), tail[-50:])
        self.assertTrue(tail.startswith('... [trimmed]'))
        self.assertEqual('a\nb', compat.log_tail('a\nb\n'))


class ParserTest(unittest.TestCase):
    def test_javac_errors(self):
        errors = compat.parse_javac_errors(JAVAC_LOG, '/repo')
        self.assertEqual([
            {'file': 'src/org/intellij/erlang/Foo.java', 'line': 12,
             'message': 'cannot find symbol; symbol:   method baz(); location: class Bar'},
            {'file': 'src/org/intellij/erlang/Qux.java', 'line': 3,
             'message': 'package com.intellij.gone does not exist'},
            {'file': None, 'line': None, 'message': 'warnings found and -Werror specified'},
        ], errors)

    def test_verdict_levels(self):
        self.assertEqual(['DEPRECATED_API_USAGES', 'SCHEDULED_FOR_REMOVAL_API_USAGES', 'EXPERIMENTAL_API_USAGES',
                          'INTERNAL_API_USAGES', 'NON_EXTENDABLE_API_USAGES'], compat.verdict_levels(COMPATIBLE_VERDICT))
        self.assertEqual(['COMPATIBILITY_PROBLEMS', 'DEPRECATED_API_USAGES'], compat.verdict_levels(BROKEN_VERDICT))

    def test_pins(self):
        with tempfile.TemporaryDirectory() as temp:
            path = os.path.join(temp, 'gradle.yml')
            write(path, """
                env:
                  OTP_VERSION: '27.3.4.9'
                  REBAR3_VERSION: '3.28.0'
                  REBAR3_SHA256: 5a28b1cca082d3b023ec0f5df9d9f9d52557036c6f2a21c1ffa11b93a984dd42
                """)
            self.assertEqual({'OTP_VERSION': '27.3.4.9', 'REBAR3_VERSION': '3.28.0',
                              'REBAR3_SHA256': '5a28b1cca082d3b023ec0f5df9d9f9d52557036c6f2a21c1ffa11b93a984dd42'},
                             compat.read_pins(path))

    def test_pins_of_this_repository(self):
        pins = compat.validate_pins(compat.read_pins(os.path.join(REPOSITORY, '.github/workflows/gradle.yml')))
        self.assertRegex(pins['OTP_VERSION'], r'^\d+(\.\d+)*$')
        self.assertRegex(pins['REBAR3_SHA256'], r'^[0-9a-f]{64}$')

    def test_known_releases_of_this_repository(self):
        root = REPOSITORY
        check = compat.known_release_check(root, '27')
        self.assertEqual('passed', check['status'])
        self.assertEqual('V_27_0', check['sdkReleaseConstant'])
        self.assertEqual('testData/mockSdk-R27', check['mockSdk'])
        self.assertIn('R15B02', check['knownReleases'])
        unknown = compat.known_release_check(root, '99')
        self.assertEqual('failed', unknown['status'])
        self.assertIsNone(unknown['sdkReleaseConstant'])
        self.assertIsNone(unknown['mockSdk'])


class IdeMatchTest(unittest.TestCase):
    def test_forms(self):
        info = dict(IDE_INFO)
        self.assertIsNone(compat.ide_matches('', info))
        self.assertTrue(compat.ide_matches('2026.3', info))
        self.assertFalse(compat.ide_matches('2026.3.1', info))
        self.assertTrue(compat.ide_matches('263.7307.200', info))
        self.assertTrue(compat.ide_matches('IU-263.7307.200', info))
        self.assertFalse(compat.ide_matches('263.7307.112', info))
        self.assertTrue(compat.ide_matches('263-EAP-SNAPSHOT', info))
        self.assertTrue(compat.ide_matches('263.7307.200-EAP-SNAPSHOT', info))
        self.assertTrue(compat.ide_matches('LATEST-EAP-SNAPSHOT', info))
        self.assertFalse(compat.ide_matches('271-EAP-SNAPSHOT', info))
        self.assertTrue(compat.ide_matches('latest', info))


class IdeResultTest(Fixture):
    def args(self, ide=''):
        info = os.path.join(self.root, 'build/compat/ide.json')
        write(info, json.dumps(IDE_INFO))
        return argparse.Namespace(
            root=self.root, logs=self.logs, commit='abc', ref='', ide=ide, ide_info=info,
            out=os.path.join(self.temp.name, 'compat-ide.json'), summary=None)

    def test_passed(self):
        self.check_ran('compile', 0, 'BUILD SUCCESSFUL')
        self.check_ran('test', 0)
        write(os.path.join(self.root, 'build/test-results/test/TEST-Bar.xml'), PASSING_XML)
        self.check_ran('verify', 0)
        self.verdict('build/reports/pluginVerifier', COMPATIBLE_VERDICT,
                     {'deprecated-usages': ['Deprecated method a() is invoked in B.c()']})
        self.check_ran('released', 0)
        self.verdict('build/compat/released-verifier', COMPATIBLE_VERDICT)
        write(os.path.join(self.logs, 'released.json'), '{"tag": "v0.11.1302", "asset": "intellij-erlang-0.11.1302.zip"}')
        result = compat.ide_result(self.args())
        self.assertEqual('passed', result['status'])
        self.assertEqual({'commit': 'abc', 'ide': {
            'name': 'IntelliJ IDEA', 'productCode': 'IU', 'version': '2026.3', 'versionSuffix': None,
            'buildNumber': '263.7307.200', 'build': 'IU-263.7307.200', 'matchesRequest': None}}, result['resolved'])
        self.assertEqual(['compile', 'test', 'verifyPlugin', 'releasedPlugin'], list(result['checks']))
        verify = result['checks']['verifyPlugin']
        self.assertEqual('passed', verify['status'])
        self.assertEqual(1, len(verify['reports']))
        report = verify['reports'][0]
        self.assertEqual('IU-263.7307.200', report['ide'])
        self.assertEqual('0.11.1302', report['pluginVersion'])
        self.assertEqual([], report['failingLevels'])
        self.assertEqual(['deprecated-usages'], list(report['problems']))
        released = result['checks']['releasedPlugin']
        self.assertEqual('passed', released['status'])
        self.assertEqual('v0.11.1302', released['release'])
        json.dumps(result)

    def test_released_plugin_not_checked_is_incomplete(self):
        # Finding 7: the question "is the published plugin broken in this IDE" was not answered.
        self.check_ran('compile', 0)
        self.check_ran('test', 0)
        write(os.path.join(self.root, 'build/test-results/test/TEST-Bar.xml'), PASSING_XML)
        self.check_ran('verify', 0)
        write(os.path.join(self.logs, 'released.json'), '{"error": "cannot list the releases: HTTP 403 rate limit"}')
        result = compat.ide_result(self.args())
        released = result['checks']['releasedPlugin']
        self.assertEqual('not-run', released['status'])
        self.assertEqual('cannot list the releases: HTTP 403 rate limit', released['reason'])
        self.assertEqual('incomplete', result['status'])
        out = os.path.join(self.temp.name, 'compat-ide.json')
        write(out, json.dumps(result))
        for event in ('pull_request', 'workflow_dispatch'):
            with contextlib.redirect_stdout(io.StringIO()) as printed:
                self.assertEqual(1, compat.conclusion(out, event))
            self.assertIn('releasedPlugin (cannot list the releases', printed.getvalue())

    def test_compile_failure_is_a_finding(self):
        self.check_ran('compile', 1, JAVAC_LOG.replace('/repo', self.root))
        result = compat.ide_result(self.args())
        self.assertEqual('failed', result['status'])
        compile_check = result['checks']['compile']
        self.assertEqual('failed', compile_check['status'])
        self.assertEqual(3, compile_check['errors']['total'])
        self.assertEqual('src/org/intellij/erlang/Foo.java', compile_check['errors']['items'][0]['file'])
        self.assertEqual('not-run', result['checks']['test']['status'])
        self.assertEqual('the plugin does not compile against this IDE', result['checks']['test']['reason'])

    def test_failed_tests_and_verifier_problems(self):
        self.check_ran('compile', 0)
        self.check_ran('test', 1, 'There were failing tests')
        write(os.path.join(self.root, 'build/test-results/test/TEST-Foo.xml'), JUNIT_XML)
        self.check_ran('verify', 1, 'Plugin verification failed')
        self.verdict('build/reports/pluginVerifier', BROKEN_VERDICT,
                     {'compatibility-problems': ['Invocation of unresolved method a()', 'Missing class B']})
        self.check_ran('released', 1)
        self.verdict('build/compat/released-verifier', BROKEN_VERDICT, {'compatibility-problems': ['Missing class B']})
        write(os.path.join(self.logs, 'released.json'), '{"tag": "v0.11.1302", "asset": "intellij-erlang-0.11.1302.zip"}')
        result = compat.ide_result(self.args('263.7307.200'))
        self.assertEqual('failed', result['status'])
        self.assertTrue(result['resolved']['ide']['matchesRequest'])
        test = result['checks']['test']
        self.assertEqual('failed', test['status'])
        self.assertEqual((4, 1, 1, 1), (test['tests'], test['failures'], test['errors'], test['skipped']))
        self.assertEqual([
            {'class': 'org.intellij.erlang.FooTest', 'name': 'testFails', 'kind': 'failure',
             'message': 'expected:<1> but was:<2>'},
            {'class': 'org.intellij.erlang.FooTest', 'name': 'testErrors', 'kind': 'error',
             'message': 'java.lang.NullPointerException: boom'},
        ], test['failedTests']['items'])
        verify = result['checks']['verifyPlugin']
        self.assertEqual('failed', verify['status'])
        self.assertEqual(['COMPATIBILITY_PROBLEMS'], verify['reports'][0]['failingLevels'])
        self.assertEqual(2, verify['reports'][0]['problems']['compatibility-problems']['total'])
        released = result['checks']['releasedPlugin']
        self.assertEqual('failed', released['status'])
        self.assertEqual('v0.11.1302', released['release'])
        summary = compat.summary(result)
        self.assertIn('| releasedPlugin | failed |', summary)
        self.assertIn('compatibility-problems: Missing class B', summary)

    def test_download_failure_after_the_ide_is_resolved_is_a_finding(self):
        self.check_ran('compile', 1, "> Could not resolve all files for configuration ':intellijPlatformDependency'.")
        check = compat.ide_result(self.args())['checks']['compile']
        self.assertEqual('failed', check['status'])
        self.assertIn("possible download or environment failure: > Could not resolve all files", check['reason'])

    def test_missing_bundled_plugin_is_a_finding(self):
        # Finding 4: the new IDE no longer has a bundled plugin that build.gradle depends on.
        self.check_ran('compile', 1, textwrap.dedent("""
            FAILURE: Build failed with an exception.

            * What went wrong:
            Execution failed for task ':compileJava'.
            > Could not resolve all dependencies for configuration ':compileClasspath'.
               > Could not find bundled plugin with ID: 'tanvd.grazi.removed'
            """))
        result = compat.ide_result(self.args())
        self.assertEqual('failed', result['status'])
        check = result['checks']['compile']
        self.assertEqual('failed', check['status'])
        self.assertIn("Could not find bundled plugin with ID: 'tanvd.grazi.removed'", check['logTail'])
        self.assertEqual('the plugin does not compile against this IDE', result['checks']['test']['reason'])

    def test_download_failure_of_one_check_keeps_the_findings_of_the_others(self):
        # Finding 5, case A: a failed test and a verifier whose log shows a download failure.
        self.check_ran('compile', 0)
        self.check_ran('test', 1, 'There were failing tests')
        write(os.path.join(self.root, 'build/test-results/test/TEST-Foo.xml'), JUNIT_XML)
        self.check_ran('verify', 1, "> Could not resolve all files for configuration ':intellijPluginVerifier'.")
        result = compat.ide_result(self.args())
        self.assertEqual('failed', result['status'])
        self.assertEqual(2, result['checks']['test']['failedTests']['total'])
        verify = result['checks']['verifyPlugin']
        self.assertEqual('failed', verify['status'])
        self.assertIn(':intellijPluginVerifier', verify['reason'])

    def test_crashed_test_jvm_with_connection_refused_in_its_output_is_a_finding(self):
        # Finding 5, case C: the test JVM crashed, no failed test case, and a test printed a connection error.
        self.check_ran('compile', 0)
        self.check_ran('test', 1, 'java.net.ConnectException: Connection refused\n'
                                  'Process \'Gradle Test Executor 1\' finished with non-zero exit value 134')
        write(os.path.join(self.root, 'build/test-results/test/TEST-Bar.xml'), PASSING_XML)
        result = compat.ide_result(self.args())
        test = result['checks']['test']
        self.assertEqual('failed', test['status'])
        self.assertIn('non-zero exit value 134', test['logTail'])
        self.assertEqual('passed', result['checks']['compile']['status'])

    def test_unfinished_check_is_failed_with_a_reason(self):
        # Decision 7: the step of the check timed out, so run-check.sh wrote NAME.started and a partial log only.
        self.check_ran('compile', 0)
        write(os.path.join(self.logs, 'test.started'), '')
        write(os.path.join(self.logs, 'test.log'), '> Task :test\nFooTest > testHangs STARTED\n')
        result = compat.ide_result(self.args())
        test = result['checks']['test']
        self.assertEqual('failed', test['status'])
        self.assertIsNone(test['exitCode'])
        self.assertIn('did not finish', test['reason'])
        self.assertIn('testHangs', test['logTail'])
        self.assertEqual('failed', result['status'])

    def test_unfinished_compile_check(self):
        write(os.path.join(self.logs, 'compile.started'), '')
        result = compat.ide_result(self.args())
        self.assertEqual('failed', result['checks']['compile']['status'])
        self.assertEqual('the compile check did not finish', result['checks']['test']['reason'])

    def test_unresolved_ide_is_infrastructure(self):
        args = self.args()
        os.remove(args.ide_info)
        self.check_ran('compile', 1, 'FAILURE')
        with self.assertRaises(compat.InfrastructureError):
            compat.ide_result(args)

    def test_unexplained_gradle_failure_is_a_finding(self):
        self.check_ran('compile', 1, 'FAILURE: Build failed with an exception.\n* What went wrong: something')
        check = compat.ide_result(self.args())['checks']['compile']
        self.assertEqual('failed', check['status'])
        self.assertIn('something', check['logTail'])

    def test_another_ide_is_infrastructure(self):
        with self.assertRaises(compat.InfrastructureError):
            compat.ide_result(self.args('2026.3.1'))


class OtpResultTest(Fixture):
    def args(self, version='27.3.4.9'):
        otp_root = os.path.join(self.temp.name, 'erlang')
        write(os.path.join(otp_root, 'releases', '27', 'OTP_VERSION'), version + '\n')
        source = os.path.join(self.root, 'src/org/intellij/erlang/sdk/ErlangSdkRelease.java')
        write(source, '''
            public static final ErlangSdkRelease V_R15B02 = new ErlangSdkRelease("R15B02", "5.9.2");
            public static final ErlangSdkRelease V_27_0 = new ErlangSdkRelease("27", "15.0");
            ''')
        os.makedirs(os.path.join(self.root, 'testData/mockSdk-R27'))
        return argparse.Namespace(
            root=self.root, logs=self.logs, commit='abc', ref='master', otp='', otp_version=version,
            otp_root=otp_root, rebar3_version='3.28.0',
            corpus_report=os.path.join(self.temp.name, 'corpus.json'),
            out=os.path.join(self.temp.name, 'compat-otp.json'), summary=None)

    def corpus(self, args, **report):
        content = {'corpusDir': '/erl', 'sdkRelease': '29', 'files': 3, 'filesWithErrors': 0, 'errorElements': 0,
                   'slowFileMillis': 10000, 'errors': [], 'exceptions': [], 'slowFiles': []}
        content.update(report)
        write(args.corpus_report, json.dumps(content))
        self.check_ran('corpus', 0)

    def test_passed(self):
        args = self.args()
        self.check_ran('compilation', 0)
        write(os.path.join(self.root, 'build/test-results/testCompilation/TEST-Bar.xml'), PASSING_XML)
        self.corpus(args)
        result = compat.otp_result(args)
        self.assertEqual('passed', result['status'])
        self.assertEqual({'release': '27', 'version': '27.3.4.9', 'root': args.otp_root, 'requested': '27.3.4.9',
                          'matchesRequest': True}, result['resolved']['otp'])
        self.assertEqual(['compilationTests', 'corpus', 'knownRelease'], list(result['checks']))
        self.assertEqual('V_27_0', result['checks']['knownRelease']['sdkReleaseConstant'])
        self.assertEqual(['R15B02', '27'], result['checks']['knownRelease']['knownReleases'])
        self.assertEqual(3, result['checks']['corpus']['files'])

    def test_skipped_compilation_tests_fail(self):
        args = self.args()
        self.check_ran('compilation', 1)
        write(os.path.join(self.root, 'jps-plugin/build/test-results/testCompilation/TEST-Foo.xml'), JUNIT_XML)
        result = compat.otp_result(args)
        check = result['checks']['compilationTests']
        self.assertEqual('failed', check['status'])
        self.assertEqual('tests were skipped', check['reason'])
        self.assertEqual([{'class': 'org.intellij.erlang.FooTest', 'name': 'testSkipped'}], check['skippedTests']['items'])
        self.assertEqual('not-run', result['checks']['corpus']['status'])

    def test_no_compilation_tests_fail(self):
        args = self.args()
        self.check_ran('compilation', 0)
        self.assertEqual('no test ran', compat.otp_result(args)['checks']['compilationTests']['reason'])

    def test_corpus_findings(self):
        args = self.args()
        errors = [{'file': 'lib/a/src/a%d.erl' % i, 'line': i, 'column': 1, 'description': '<expression> expected'}
                  for i in range(150)]
        self.corpus(args, filesWithErrors=150, errorElements=150, errors=errors,
                    exceptions=[{'file': 'b.erl', 'exception': 'java.lang.StackOverflowError'}],
                    slowFiles=[{'file': 'c.erl', 'millis': 12000}])
        check = compat.otp_result(args)['checks']['corpus']
        self.assertEqual('failed', check['status'])
        self.assertEqual({'total': 150, 'trimmed': True}, {k: check['errors'][k] for k in ('total', 'trimmed')})
        self.assertEqual(compat.LIST_LIMIT, len(check['errors']['items']))
        self.assertEqual(1, check['exceptions']['total'])
        self.assertEqual(1, check['slowFiles']['total'])

    def test_empty_corpus_fails(self):
        # Finding 6: an installation without sources, or a corpus root that was not walked, parsed nothing.
        args = self.args()
        self.check_ran('compilation', 0)
        write(os.path.join(self.root, 'build/test-results/testCompilation/TEST-Bar.xml'), PASSING_XML)
        self.corpus(args, files=0)
        result = compat.otp_result(args)
        corpus = result['checks']['corpus']
        self.assertEqual('failed', corpus['status'])
        self.assertIn('no .erl or .hrl file', corpus['reason'])
        self.assertEqual('failed', result['status'])

    def test_rebar3_that_does_not_run_is_a_finding(self):
        # Decision 6: the pinned rebar3 does not run on the requested Erlang/OTP.
        args = self.args()
        self.check_ran('rebar3', 1, 'escript: exception error: undefined function rebar3:main/1\nmore\n')
        self.check_ran('compilation', 0)
        write(os.path.join(self.root, 'build/test-results/testCompilation/TEST-Bar.xml'), PASSING_XML)
        self.corpus(args)
        check = compat.otp_result(args)['checks']['compilationTests']
        self.assertEqual('failed', check['status'])
        self.assertEqual('rebar3 3.28.0 does not run on Erlang/OTP 27.3.4.9: `escript rebar3 version` exited with 1: '
                         'escript: exception error: undefined function rebar3:main/1', check['reason'])

    def test_rebar3_that_runs(self):
        args = self.args()
        self.check_ran('rebar3', 0, 'rebar 3.28.0 on Erlang/OTP 27 Erts 15.2.7')
        self.check_ran('compilation', 0)
        write(os.path.join(self.root, 'build/test-results/testCompilation/TEST-Bar.xml'), PASSING_XML)
        self.corpus(args)
        self.assertEqual('passed', compat.otp_result(args)['checks']['compilationTests']['status'])

    def test_unknown_release(self):
        args = self.args('30.0-rc1')
        os.rename(os.path.join(args.otp_root, 'releases', '27'), os.path.join(args.otp_root, 'releases', '30'))
        result = compat.otp_result(args)
        self.assertEqual('30', result['resolved']['otp']['release'])
        self.assertEqual('failed', result['checks']['knownRelease']['status'])
        self.assertEqual('failed', result['status'])

    def test_wrong_release_is_infrastructure(self):
        args = self.args()
        args.otp_version = '28.0'
        with self.assertRaises(compat.InfrastructureError):
            compat.otp_result(args)


class MainTest(Fixture):
    def run_main(self, *argv):
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            code = compat.main(list(argv))
        return code, out.getvalue()

    def test_conclusion(self):
        path = os.path.join(self.temp.name, 'result.json')
        write(path, json.dumps({'job': 'otp', 'status': 'failed',
                                'checks': {'corpus': {'status': 'failed'}, 'knownRelease': {'status': 'passed'}}}))
        code, out = self.run_main('conclusion', path)
        self.assertEqual(1, code)
        self.assertIn('corpus', out)
        write(path, json.dumps({'job': 'otp', 'status': 'passed', 'checks': {}}))
        self.assertEqual(0, self.run_main('conclusion', path)[0])

    def test_infrastructure_failure_writes_no_result(self):
        out = os.path.join(self.temp.name, 'compat-ide.json')
        summary = os.path.join(self.temp.name, 'summary.md')
        code, _ = self.run_main('ide-result', '--root', self.root, '--logs', self.logs, '--commit', 'abc',
                                '--out', out, '--summary', summary, '--ide-info', os.path.join(self.root, 'none.json'))
        self.assertEqual(2, code)
        self.assertFalse(os.path.exists(out))
        with open(summary, encoding='utf-8') as f:
            self.assertIn('infrastructure failure', f.read())

    def test_result_and_summary(self):
        write(os.path.join(self.root, 'build/compat/ide.json'), json.dumps(IDE_INFO))
        self.check_ran('compile', 0)
        out = os.path.join(self.temp.name, 'compat-ide.json')
        summary = os.path.join(self.temp.name, 'summary.md')
        code, _ = self.run_main('ide-result', '--root', self.root, '--logs', self.logs, '--commit', 'abc',
                                '--out', out, '--summary', summary,
                                '--ide-info', os.path.join(self.root, 'build/compat/ide.json'))
        self.assertEqual(0, code)
        with open(out, encoding='utf-8') as f:
            result = json.load(f)
        self.assertEqual(compat.SCHEMA_VERSION, result['schemaVersion'])
        self.assertEqual({'listItems': 100, 'textChars': 2000}, result['limits'])
        with open(summary, encoding='utf-8') as f:
            self.assertIn('IDE: IntelliJ IDEA 2026.3 (IU-263.7307.200)', f.read())


EVIL_OTP = '27.3.4.9\nREBAR3_VERSION=../../../../evil/repo/releases/download/v1\nREBAR3_SHA256=' + 'e' * 64
DEFAULT_SHA = 'a' * 40


class FakeApi:
    """The GitHub REST API of a repository with branch master, tag v0.11.1302 and a fork commit."""
    def __init__(self):
        self.calls = []

    def __call__(self, path):
        self.calls.append(path)
        responses = {
            'repos/o/r/commits/heads/master': {'sha': DEFAULT_SHA},
            'repos/o/r/commits/heads/feature/x': {'sha': 'b' * 40},
            'repos/o/r/commits/tags/v0.11.1302': {'sha': 'c' * 40},
            'repos/o/r/compare/master...1234567': {'status': 'behind', 'merge_base_commit': {'sha': '1234567' + '0' * 33}},
            'repos/o/r/compare/master...' + 'f' * 40: {'status': 'diverged', 'merge_base_commit': {'sha': DEFAULT_SHA}},
        }
        return responses.get(path)


class InputsTest(Fixture):
    def test_documented_forms(self):
        for ide in ('', '2026.3', '2026.3.1', '263.7307.44', '263.7307', 'latest', '263-EAP-SNAPSHOT',
                    '263.7307.44-EAP-SNAPSHOT', 'LATEST-EAP-SNAPSHOT'):
            self.assertEqual(ide, compat.validate_input('ide', ide))
        for otp in ('', '27.3.4.9', '29.1', '30.0-rc1', '28'):
            self.assertEqual(otp, compat.validate_input('otp', otp))
        for ref in ('', 'master', 'feature/x', 'v0.11.1302', 'refs/heads/master', 'refs/tags/v0.11.1302', 'a1b2c3d'):
            self.assertEqual(ref, compat.validate_input('ref', ref))

    def test_other_values_are_rejected(self):
        # Decision 4 for ide; finding 2 for otp; finding 3 for ref.
        rejected = {
            'ide': ['-Pfoo=bar', '--init-script=x', 'no-such-ide-999', '../../../evil/x-SNAPSHOT', '2026.3\nX=1',
                    'LATEST', '263-CANDIDATE-SNAPSHOT', ' 2026.3'],
            'otp': [EVIL_OTP, '27.3.4.9\n', '-rc1', '27.3;rm', 'OTP-27.3', '27..3'],
            'ref': ['refs/pull/5/head', 'refs/remotes/origin/x', '-x', '--upload-pack=x', 'a..b', 'a//b', 'a/',
                    'x.lock', 'a\nb', 'a b', 'HEAD@{1}', 'x' * 201],
        }
        for name, values in rejected.items():
            for value in values:
                with self.assertRaises(compat.InputError, msg='%s=%r' % (name, value)):
                    compat.validate_input(name, value)

    def test_evil_otp_writes_no_output(self):
        # Finding 2: a line break in otp used to add REBAR3_VERSION and REBAR3_SHA256 lines after the pinned ones.
        gradle_yml = os.path.join(REPOSITORY, '.github/workflows/gradle.yml')
        output = os.path.join(self.temp.name, 'output')
        write(output, '')
        env = {'INPUT_OTP': EVIL_OTP, 'PR_HEAD_SHA': DEFAULT_SHA, 'GITHUB_EVENT_NAME': 'pull_request'}
        out = io.StringIO()
        with unittest.mock.patch.dict(os.environ, env), contextlib.redirect_stdout(out):
            code = compat.main(['inputs', '--job', 'otp', '--gradle-yml', gradle_yml, '--output', output])
        self.assertEqual(1, code)
        self.assertIn('::error::the input otp=', out.getvalue())
        with open(output, encoding='utf-8') as f:
            self.assertEqual('', f.read())

    def test_otp_inputs_of_a_pull_request(self):
        gradle_yml = os.path.join(REPOSITORY, '.github/workflows/gradle.yml')
        output = os.path.join(self.temp.name, 'output')
        env = {'PR_HEAD_SHA': DEFAULT_SHA, 'GITHUB_EVENT_NAME': 'pull_request'}
        with unittest.mock.patch.dict(os.environ, env), contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(0, compat.main(['inputs', '--job', 'otp', '--gradle-yml', gradle_yml, '--output', output]))
        with open(output, encoding='utf-8') as f:
            lines = dict(line.split('=', 1) for line in f.read().splitlines())
        pins = compat.read_pins(gradle_yml)
        self.assertEqual({'ide': '', 'otp': pins['OTP_VERSION'], 'ref': '', 'commit': DEFAULT_SHA,
                          'OTP_VERSION': pins['OTP_VERSION'], 'REBAR3_VERSION': pins['REBAR3_VERSION'],
                          'REBAR3_SHA256': pins['REBAR3_SHA256']}, lines)

    def test_invalid_pins_are_rejected(self):
        with self.assertRaises(compat.InputError):
            compat.validate_pins({'REBAR3_VERSION': '../evil'})
        with self.assertRaises(compat.InputError):
            compat.validate_pins({'REBAR3_SHA256': 'abc'})

    def test_write_outputs_refuses_line_breaks(self):
        with self.assertRaises(compat.InputError):
            compat.write_outputs({'otp': EVIL_OTP}, os.path.join(self.temp.name, 'output'))

    def test_ref_of_this_repository(self):
        api = FakeApi()
        self.assertEqual(DEFAULT_SHA, compat.resolve_ref('', 'o/r', 'master', api))
        self.assertEqual(DEFAULT_SHA, compat.resolve_ref('refs/heads/master', 'o/r', 'master', api))
        self.assertEqual('b' * 40, compat.resolve_ref('feature/x', 'o/r', 'master', api))
        self.assertEqual('c' * 40, compat.resolve_ref('v0.11.1302', 'o/r', 'master', api))
        self.assertEqual('c' * 40, compat.resolve_ref('refs/tags/v0.11.1302', 'o/r', 'master', api))
        self.assertEqual('1234567' + '0' * 33, compat.resolve_ref('1234567', 'o/r', 'master', api))

    def test_ref_from_elsewhere_is_rejected(self):
        # Finding 3: a commit that is not on the default branch (a fork's) or not a branch or tag of this repository.
        api = FakeApi()
        for ref in ('f' * 40, 'no-such-branch', 'refs/tags/master'):
            with self.assertRaises(compat.InputError, msg=ref):
                compat.resolve_ref(ref, 'o/r', 'master', api)
        with self.assertRaises(compat.InputError):
            compat.check_inputs('ide', 'workflow_dispatch', {'ref': 'refs/pull/5/head'}, None, 'o/r', 'master', api=api)

    def test_dispatch_resolves_the_ref(self):
        values = compat.check_inputs('ide', 'workflow_dispatch', {'ide': '263-EAP-SNAPSHOT', 'ref': 'feature/x'},
                                     None, 'o/r', 'master', api=FakeApi())
        self.assertEqual({'ide': '263-EAP-SNAPSHOT', 'otp': '', 'ref': 'feature/x', 'commit': 'b' * 40}, values)


class ReleaseTagTest(unittest.TestCase):
    def test_highest_build_of_the_version(self):
        # Decision 2: v0.11.1268 was published after v0.11.1301, and GitHub's "latest" is not the highest build.
        releases = [{'tagName': 'v0.11.1268'}, {'tagName': 'v0.11.1302'}, {'tagName': 'v0.11.1301'},
                    {'tagName': 'v0.11.999'}, {'tagName': 'v0.12.5'}, {'tagName': 'v0.11.1400', 'isDraft': True},
                    {'tagName': 'v0.11.1500-rc'}, {'tagName': 'v0x11.1600'}]
        self.assertEqual('v0.11.1302', compat.release_tag(releases, '0.11'))

    def test_no_release_of_the_version(self):
        with self.assertRaises(compat.InputError):
            compat.release_tag([{'tagName': 'v0.10.900'}], '0.11')

    def test_command(self):
        with tempfile.TemporaryDirectory() as temp:
            properties = os.path.join(temp, 'gradle.properties')
            write(properties, 'javaVersion = 25\nversion = 0.11\nbuildNumber = SNAPSHOT\n')
            out = io.StringIO()
            stdin = io.StringIO(json.dumps([{'tagName': 'v0.11.1268'}, {'tagName': 'v0.11.1302'}]))
            with unittest.mock.patch('sys.stdin', stdin), contextlib.redirect_stdout(out):
                self.assertEqual(0, compat.main(['release-tag', '--gradle-properties', properties]))
            self.assertEqual('v0.11.1302\n', out.getvalue())


class ConclusionTest(Fixture):
    def conclude(self, event, checks):
        path = os.path.join(self.temp.name, 'result.json')
        result = {'job': 'otp', 'status': compat.overall(checks), 'checks': checks}
        write(path, json.dumps(result))
        with contextlib.redirect_stdout(io.StringIO()):
            return compat.conclusion(path, event)

    def test_findings_are_red_on_dispatch_only(self):
        # Decision 1.
        checks = {'corpus': {'status': 'failed'}, 'knownRelease': {'status': 'passed'}}
        self.assertEqual(0, self.conclude('pull_request', checks))
        self.assertEqual(1, self.conclude('workflow_dispatch', checks))

    def test_a_check_that_did_not_run_is_red_on_both(self):
        checks = {'corpus': {'status': 'not-run', 'reason': 'not run'}, 'knownRelease': {'status': 'passed'}}
        self.assertEqual('incomplete', compat.overall(checks))
        self.assertEqual(1, self.conclude('pull_request', checks))
        self.assertEqual(1, self.conclude('workflow_dispatch', checks))

    def test_no_result_is_red_on_both(self):
        for event in ('pull_request', 'workflow_dispatch'):
            with contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(1, compat.conclusion(os.path.join(self.temp.name, 'none.json'), event))

    def test_passed(self):
        self.assertEqual(0, self.conclude('workflow_dispatch', {'corpus': {'status': 'passed'}}))


@unittest.skipIf(yaml is None, 'PyYAML is not installed')
class WorkflowTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        with open(os.path.join(REPOSITORY, '.github/workflows/compat.yml'), encoding='utf-8') as f:
            cls.workflow = yaml.safe_load(f)
        cls.jobs = cls.workflow['jobs']

    def steps(self, job):
        return self.jobs[job]['steps']

    def step(self, job, name):
        return next(step for step in self.steps(job) if step.get('name') == name)

    def test_tool_checkout_has_what_the_tests_read(self):
        # Finding 1: the unit tests run in the sparse checkout of the compatibility check, in cone mode.
        for job in self.jobs:
            checkout = self.step(job, 'Check out the compatibility check')['with']
            self.assertNotIn('sparse-checkout-cone-mode', checkout)
            directories = checkout['sparse-checkout'].split()
            for path in REPOSITORY_PATHS + ['scripts/compat']:
                self.assertTrue(any(path == d or path.startswith(d + '/') for d in directories), '%s: %s' % (job, path))
                self.assertTrue(os.path.exists(os.path.join(REPOSITORY, path)), path)

    def test_inputs_are_checked_before_they_are_used(self):
        # Finding 2 and decision 4: before the step "Check the inputs", no step reads an input.
        for job in self.jobs:
            names = [step.get('name') for step in self.steps(job)]
            check = names.index('Check the inputs')
            for step in self.steps(job)[:check]:
                self.assertNotIn('inputs.', json.dumps(step), '%s: %s' % (job, step.get('name')))
            for step in self.steps(job):
                self.assertNotIn('${{', step.get('run', ''), '%s: %s' % (job, step.get('name')))
            env = self.step(job, 'Check the inputs')['env']
            self.assertEqual({'INPUT_IDE', 'INPUT_OTP', 'INPUT_REF'}, {k for k in env if k.startswith('INPUT_')})
            self.assertEqual('${{ steps.inputs.outputs.commit }}', self.step(job, 'Check out the plugin')['with']['ref'])

    def test_code_of_ref_gets_no_credentials_and_saves_no_cache(self):
        # Finding 3.
        for job in self.jobs:
            steps = self.steps(job)
            for step in steps:
                if step.get('uses', '').startswith('actions/checkout@'):
                    self.assertIs(False, step['with'].get('persist-credentials'), '%s: %s' % (job, step['name']))
                if step.get('uses', '').startswith('actions/setup-java@'):
                    self.assertNotIn('cache', step['with'], job)
                self.assertFalse(step.get('uses', '').startswith('actions/cache'), job)
            first_build = min(i for i, step in enumerate(steps) if 'gradle.sh' in step.get('run', ''))
            for i, step in enumerate(steps):
                if 'github.token' in json.dumps(step) or 'secrets.' in json.dumps(step):
                    self.assertLess(i, first_build, '%s: %s has a token after the build ran' % (job, step['name']))

    def test_a_hanging_check_still_leaves_a_result(self):
        # Decision 7: every long step has a timeout, their sum stays below the job's, and the result is still written.
        for job in self.jobs:
            steps = self.steps(job)
            long_steps = [step for step in steps
                          if any(tool in step.get('run', '') for tool in ('gradle.sh', 'install-'))]
            for step in long_steps:
                self.assertIn('timeout-minutes', step, '%s: %s' % (job, step['name']))
            self.assertLessEqual(sum(step['timeout-minutes'] for step in long_steps) + 20,
                                 self.jobs[job]['timeout-minutes'], job)
            for name in ('Write the result', 'Conclusion'):
                self.assertIn('!cancelled()', self.step(job, name)['if'], '%s: %s' % (job, name))
            for name in ('Upload the result', 'Upload the logs'):
                self.assertEqual('always()', self.step(job, name)['if'])


if __name__ == '__main__':
    unittest.main()
