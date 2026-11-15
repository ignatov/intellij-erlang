"""Unit tests of compat.py: python3 -m unittest discover -s scripts/compat -p 'test_*.py' -v"""

import argparse
import contextlib
import io
import json
import os
import tempfile
import textwrap
import unittest

import compat


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
        self.assertEqual('ab... [trimmed]', compat.text('abc', 2))
        self.assertIsNone(compat.text(None))


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
        gradle_yml = os.path.join(os.path.dirname(__file__), '..', '..', '.github', 'workflows', 'gradle.yml')
        pins = compat.read_pins(gradle_yml)
        self.assertRegex(pins['OTP_VERSION'], r'^\d+(\.\d+)*$')
        self.assertRegex(pins['REBAR3_SHA256'], r'^[0-9a-f]{64}$')

    def test_known_releases_of_this_repository(self):
        root = os.path.join(os.path.dirname(__file__), '..', '..')
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
        write(os.path.join(self.logs, 'released.json'), '{"error": "no latest release"}')
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
        self.assertEqual('not-run', released['status'])
        self.assertEqual('no latest release', released['reason'])
        json.dumps(result)

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

    def test_download_failure_is_infrastructure(self):
        self.check_ran('compile', 1, "> Could not resolve all files for configuration ':intellijPlatformDependency'.")
        with self.assertRaises(compat.InfrastructureError):
            compat.ide_result(self.args())

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


if __name__ == '__main__':
    unittest.main()
