"""Unit tests of smoke.py: python3 -m unittest discover -s scripts/smoke -p 'test_*.py'"""

import io
import json
import os
import sys
import tempfile
import unittest
import zipfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import smoke  # noqa: E402

PLUGIN_XML = '''<idea-plugin>
  <id>org.jetbrains.erlang</id>
  <version>0.11.SNAPSHOT-7</version>
  <idea-version since-build="262" until-build="263.*"/>
</idea-plugin>'''
ERLANG_PLUGIN_XML = '''<idea-plugin>
  <extensions defaultExtensionNs="com.intellij">
    <localInspection language="Erlang" shortName="ErlangUnusedFunction" displayName="Unused function"
                     groupName="Erlang" enabledByDefault="true" level="WARNING" implementationClass="A"/>
    <localInspection language="Erlang" shortName="ErlangSingleClauseCase" enabledByDefault="false"
                     level="WEAK WARNING" implementationClass="B"/>
    <globalInspection shortName="ErlangGlobal" implementationClass="C"/>
  </extensions>
</idea-plugin>'''
BEAMS = ['debugger/beams/otp_27_plus/debugnode.beam', 'eunit/beams/otp_27_plus/eunit_teamcity.beam']


def jar_bytes(entries):
    buffer = io.BytesIO()
    with zipfile.ZipFile(buffer, 'w') as jar:
        for name, content in entries.items():
            jar.writestr(name, content)
    return buffer.getvalue()


class ArchiveTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.repo = self.temp.name
        for beam in BEAMS:
            path = os.path.join(self.repo, 'resources', beam)
            os.makedirs(os.path.dirname(path))
            with open(path, 'wb') as f:
                f.write(b'beam ' + beam.encode())

    def tearDown(self):
        self.temp.cleanup()

    def archive(self, plugin_xml=PLUGIN_XML, skip=(), beam_content=None):
        entries = {'META-INF/plugin.xml': plugin_xml, 'META-INF/ErlangPlugin.xml': ERLANG_PLUGIN_XML,
                   'META-INF/java-deps.xml': '<idea-plugin/>'}
        for beam in BEAMS:
            entries[beam] = beam_content or b'beam ' + beam.encode()
        entries = {k: v for k, v in entries.items() if k not in skip}
        path = os.path.join(self.repo, 'plugin.zip')
        jars = {'intellij-erlang/lib/intellij-erlang.jar': jar_bytes(entries),
                'intellij-erlang/lib/intellij-erlang.jps-plugin.jar': b'jar',
                'intellij-erlang/lib/OtpErlang.jar': b'jar',
                'intellij-erlang/lib/intellij-erlang-0.11.SNAPSHOT-7-searchableOptions.jar': b'jar'}
        with zipfile.ZipFile(path, 'w') as archive:
            for name, content in jars.items():
                if name not in skip:
                    archive.writestr(name, content)
        return path

    def test_good_archive_passes(self):
        failures, info = smoke.check_archive(self.archive(), self.repo, '0.11.SNAPSHOT-7')
        self.assertEqual([], failures)
        self.assertEqual('0.11.SNAPSHOT-7', info['version'])
        self.assertEqual(BEAMS, info['beams'])

    def test_wrong_version_and_range_fail(self):
        xml = PLUGIN_XML.replace('until-build="263.*"', 'until-build="264.*"')
        failures, _ = smoke.check_archive(self.archive(plugin_xml=xml), self.repo, '0.11.1304')
        self.assertEqual(['version 0.11.SNAPSHOT-7, expected 0.11.1304', 'until-build 264.*, expected 263.*'], failures)

    def test_missing_parts_fail(self):
        path = self.archive(skip=('META-INF/java-deps.xml', BEAMS[1], 'intellij-erlang/lib/OtpErlang.jar',
                                  'intellij-erlang/lib/intellij-erlang-0.11.SNAPSHOT-7-searchableOptions.jar'))
        failures, _ = smoke.check_archive(path, self.repo, '0.11.SNAPSHOT-7')
        self.assertEqual(['no lib/OtpErlang.jar in the archive', 'no searchable options jar in the archive',
                          'no META-INF/java-deps.xml in lib/intellij-erlang.jar',
                          'no eunit/beams/otp_27_plus/eunit_teamcity.beam in lib/intellij-erlang.jar'], failures)

    def test_changed_beam_fails(self):
        failures, _ = smoke.check_archive(self.archive(beam_content=b'other'), self.repo, '0.11.SNAPSHOT-7')
        self.assertEqual(2, len(failures))
        self.assertIn('differs from resources/debugger/beams/otp_27_plus/debugnode.beam', failures[0])

    def test_profile_turns_on_every_erlang_inspection(self):
        tools = smoke.erlang_inspections(self.archive())
        self.assertEqual([('ErlangUnusedFunction', 'WARNING'), ('ErlangSingleClauseCase', 'WEAK WARNING'),
                          ('ErlangGlobal', 'WARNING')], tools)
        profile = smoke.inspection_profile(tools)
        self.assertIn('<inspection_tool class="ErlangSingleClauseCase" enabled="true" level="WEAK WARNING"', profile)
        self.assertIn('<inspection_tool class="Annotator" enabled="false"', profile)


LOG = '''2026-11-20 04:40:11,915 [    783]   INFO - #c.i.p.i.b.AppStarter - Loaded custom plugins: Erlang (org.jetbrains.erlang, 0.11.SNAPSHOT-7)
2026-11-20 04:40:21,087 [   9955]   INFO - #c.i.o.p.MergingQueueGuiExecutor - Running task: (dumb mode task) UnindexedFilesIndexer[proj, reasons: [On project open]]
2026-11-20 04:40:46,677 [  35545]   INFO - #c.i.u.i.UnindexedFilesIndexer - Finished for proj. Unindexed files update took 25548ms; general responsiveness: ok
2026-11-20 04:40:46,677 [  35545]   INFO - #c.i.o.p.MergingQueueGuiExecutor - Task finished: (dumb mode task) UnindexedFilesIndexer[proj, reasons: [On project open]]
2026-11-20 04:40:50,727 [  39595] SEVERE - #c.i.c.InspectionsResultUtil - Descriptions are missed for tools: MongoJSResolveInspection
2026-11-20 04:40:50,727 [  39595] SEVERE - #c.i.c.InspectionsResultUtil - IntelliJ IDEA 2026.3  Build #IU-263.7307.200
'''
PLUGIN_EXCEPTION = '''2026-11-19 22:04:25,796 [ 131388] SEVERE - #c.i.c.e.GlobalInspectionContextImpl - In file: /p/erl_lint.erl
java.lang.NullPointerException: Cannot invoke "com.intellij.psi.PsiElement.getText()"
\tat org.intellij.erlang.inspection.ErlangIoFormatInspection$1.visitGlobalFunctionCallExpression(ErlangIoFormatInspection.java:72)
\tat com.intellij.codeInspection.InspectionEngine.lambda$inspectElements$3(InspectionEngine.java:388)
'''


class LogTest(unittest.TestCase):
    def test_clean_log(self):
        info = smoke.analyze_log(LOG, 'Done.\n')
        self.assertEqual('0.11.SNAPSHOT-7', info['pluginVersion'])
        self.assertEqual(2, info['logErrors'])
        self.assertEqual(0, info['pluginExceptions'])
        self.assertEqual(1, info['dumbTasks'])
        self.assertEqual([], info['unfinishedDumbTasks'])
        self.assertEqual(1, info['indexingFinished'])
        self.assertEqual([], smoke.inspect_failures('cowboy', 0, False, '0.11.SNAPSHOT-7', info, 3, set()))

    def test_plugin_version_of_2026_2(self):
        info = smoke.analyze_log('x - Loaded custom plugins: Other (1.0), Erlang (0.11.1303)\n', '')
        self.assertEqual('0.11.1303', info['pluginVersion'])

    def test_plugin_exception_fails(self):
        info = smoke.analyze_log(LOG + PLUGIN_EXCEPTION, '')
        self.assertEqual(1, info['pluginExceptions'])
        self.assertIn('ErlangIoFormatInspection.java:72', info['pluginExceptionTexts'][0])
        self.assertEqual(['1 exceptions of the plugin in the IDE log'],
                         smoke.inspect_failures('cowboy', 0, False, '0.11.SNAPSHOT-7', info, 3, set()))

    def test_plugin_frames_on_the_console_fail(self):
        console = 'Exception in thread "x" java.lang.IllegalStateException\n\tat org.intellij.erlang.psi.X.y(X.java:1)\n'
        info = smoke.analyze_log(LOG, console)
        self.assertEqual(['1 stack frames of the plugin on the console'],
                         smoke.inspect_failures('cowboy', 0, False, '0.11.SNAPSHOT-7', info, 3, set()))

    def test_unfinished_indexing_and_missing_plugin_fail(self):
        log = '\n'.join(l for l in LOG.split('\n') if 'Task finished' not in l and 'Loaded custom' not in l)
        info = smoke.analyze_log(log, '')
        self.assertEqual(['UnindexedFilesIndexer[proj, reasons: [On project open]]'], info['unfinishedDumbTasks'])
        self.assertEqual(['the inspect run exited with 3', 'the IDE did not load the plugin',
                          'indexing did not finish: UnindexedFilesIndexer[proj, reasons: [On project open]]'],
                         smoke.inspect_failures('cowboy', 3, False, '0.11.SNAPSHOT-7', info, 3, set()))

    def test_timeout_other_version_no_problems_and_missing_canary_fail(self):
        info = smoke.analyze_log(LOG.replace('SNAPSHOT-7', 'SNAPSHOT-6'), '')
        self.assertEqual(['the inspect run did not end in time; thread dump in log/threaddump-timeout.txt',
                          'the IDE loaded plugin version 0.11.SNAPSHOT-6, expected 0.11.SNAPSHOT-7',
                          'no Erlang problem found: the Erlang inspections did not run',
                          'no ErlangUnusedFunction problem in src/smoke_canary.erl'],
                         smoke.inspect_failures('smoke_app', -15, True, '0.11.SNAPSHOT-7', info, 0, set()))

    def test_problems_read_from_the_results(self):
        with tempfile.TemporaryDirectory() as res:
            with open(os.path.join(res, 'ErlangUnusedFunction.xml'), 'w') as f:
                f.write('<problems><problem><file>file://$PROJECT_DIR$/src/smoke_canary.erl</file>'
                        '<problem_class severity="WARNING">Unused</problem_class></problem></problems>')
            with open(os.path.join(res, 'JsonStandardCompliance.xml'), 'w') as f:
                f.write('<problems><problem><file>x</file></problem></problems>')
            with open(os.path.join(res, '.descriptions.xml'), 'w') as f:
                f.write('<inspections/>')
            by_inspection, by_severity, found = smoke.problems(res, {'ErlangUnusedFunction', 'ErlangOther'})
        self.assertEqual({'ErlangUnusedFunction': 1}, by_inspection)
        self.assertEqual({'WARNING': 1}, by_severity)
        self.assertEqual({('ErlangUnusedFunction', 'src/smoke_canary.erl')}, found)


class LaunchTest(unittest.TestCase):
    def test_linux_layout(self):
        info = {'launch': [{'os': 'Linux', 'arch': 'amd64', 'javaExecutablePath': 'jbr/bin/java',
                            'vmOptionsFilePath': 'bin/idea64.vmoptions', 'bootClassPathJarNames': ['a.jar', 'b.jar'],
                            'additionalJvmArguments': ['-Dx=$IDE_HOME/plugins'], 'mainClass': 'com.intellij.idea.Main'}]}
        launch = smoke.launch_record(info, 'Linux', 'amd64')
        command = smoke.launch_command('/ide', '/ide/product-info.json', launch, ['-Xmx3g'], '/s/idea.properties',
                                       ['inspect', '/p'])
        self.assertEqual(['/ide/jbr/bin/java', '-Xmx3g', '-Dx=/ide/plugins', '-Didea.home.path=/ide',
                          '-Didea.properties.file=/s/idea.properties', '-Djava.class.path=/ide/lib/a.jar:/ide/lib/b.jar',
                          'com.intellij.idea.Main', 'inspect', '/p'], command)

    def test_macos_layout(self):
        info = {'launch': [{'os': 'macOS', 'arch': 'aarch64', 'javaExecutablePath': '../jbr/Contents/Home/bin/java',
                            'vmOptionsFilePath': '../bin/idea.vmoptions', 'bootClassPathJarNames': ['a.jar'],
                            'additionalJvmArguments': ['-Dx=$APP_PACKAGE/Contents/lib'], 'mainClass': 'M'}]}
        launch = smoke.launch_record(info, 'macOS', 'aarch64')
        command = smoke.launch_command('/ide', '/ide/Resources/product-info.json', launch, [], '/p', [])
        self.assertEqual('/ide/jbr/Contents/Home/bin/java', command[0])
        self.assertIn('-Dx=/ide/lib', command)
        with self.assertRaises(smoke.SmokeError):
            smoke.launch_record(info, 'Linux', 'amd64')

    def test_watchdog_stops_a_hanging_run(self):
        with tempfile.TemporaryDirectory() as d:
            dump = os.path.join(d, 'dump.txt')
            rc, timed_out = smoke.run_with_watchdog(['sleep', '30'], os.path.join(d, 'out.txt'), 1, dump,
                                                    os.path.join(d, 'no-jcmd'))
            self.assertTrue(timed_out)
            self.assertNotEqual(0, rc)
            with open(dump) as f:
                self.assertIn('no thread dump', f.read())
        with tempfile.TemporaryDirectory() as d:
            self.assertEqual((3, False), smoke.run_with_watchdog(['sh', '-c', 'exit 3'], os.path.join(d, 'out.txt'), 10,
                                                                 os.path.join(d, 'dump.txt'), 'jcmd'))

    def test_sdk_release_of_an_otp_root(self):
        with tempfile.TemporaryDirectory() as otp:
            os.makedirs(os.path.join(otp, 'releases', '27'))
            os.makedirs(os.path.join(otp, 'erts-15.2.7.6'))
            with open(os.path.join(otp, 'releases', '27', 'OTP_VERSION'), 'w') as f:
                f.write('27.3.4.9\n')
            self.assertEqual(('27.3.4.9', 'OTP 27.3.4.9, erts-15.2.7.6'), smoke.sdk_release(otp))
            table, name = smoke.jdk_table(otp)
            self.assertEqual('Erlang 27.3.4.9', name)
            self.assertIn('<version value="OTP 27.3.4.9, erts-15.2.7.6" />', table)


class ParserTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.baseline = os.path.join(self.temp.name, 'baseline.txt')
        smoke.write_baseline(self.baseline, 'cowboy 1, Erlang/OTP 27', 10, {'otp/a.erl': 2, 'cowboy/b.erl': 1})

    def tearDown(self):
        self.temp.cleanup()

    def report(self, errors, files=10, exceptions=()):
        path = os.path.join(self.temp.name, 'report.json')
        with open(path, 'w') as f:
            json.dump({'files': files, 'errors': [{'file': e, 'line': 3, 'description': 'x expected'} for e in errors],
                       'exceptions': list(exceptions)}, f)
        return smoke.read_corpus_report(path)

    def test_baseline_round_trip(self):
        baseline = smoke.read_baseline(self.baseline)
        self.assertEqual({'corpus': 'cowboy 1, Erlang/OTP 27', 'files': 10,
                          'counts': {'otp/a.erl': 2, 'cowboy/b.erl': 1}}, baseline)
        with open(self.baseline) as f:
            self.assertIn('scripts/smoke/run.sh inputs parser-baseline', f.read())

    def test_same_and_fewer_errors_pass(self):
        files, counts, first, exceptions = self.report(['otp/a.erl'])
        failures, worse, better = smoke.compare_with_baseline(smoke.read_baseline(self.baseline),
                                                              'cowboy 1, Erlang/OTP 27', files, counts, first, exceptions)
        self.assertEqual(([], []), (failures, worse))
        self.assertEqual([{'file': 'otp/a.erl', 'baseline': 2, 'now': 1}, {'file': 'cowboy/b.erl', 'baseline': 1, 'now': 0}],
                         sorted(better, key=lambda b: b['file'], reverse=True))

    def test_more_errors_fail(self):
        files, counts, first, exceptions = self.report(['otp/a.erl'] * 3 + ['smoke_app/c.erl'])
        failures, worse, _ = smoke.compare_with_baseline(smoke.read_baseline(self.baseline),
                                                         'cowboy 1, Erlang/OTP 27', files, counts, first, exceptions)
        self.assertEqual(['otp/a.erl', 'smoke_app/c.erl'], [w['file'] for w in worse])
        self.assertEqual('otp/a.erl: 3 error elements, the baseline has 2 (line 3: x expected)', failures[0])

    def test_other_corpus_and_exceptions_fail(self):
        files, counts, first, exceptions = self.report([], files=9, exceptions=[{'file': 'x.erl', 'exception': 'E'}])
        failures, _, _ = smoke.compare_with_baseline(smoke.read_baseline(self.baseline), 'cowboy 2, Erlang/OTP 27',
                                                     files, counts, first, exceptions)
        self.assertEqual(['the baseline is for the corpus "cowboy 1, Erlang/OTP 27", this corpus is '
                          '"cowboy 2, Erlang/OTP 27"; refresh the baseline',
                          'the test parsed 9 files, the baseline has 10', 'the parser threw on x.erl: E'], failures)

    def test_malformed_baseline_is_an_error(self):
        with open(self.baseline, 'a') as f:
            f.write('two otp/x.erl\n')
        with self.assertRaises(smoke.SmokeError):
            smoke.read_baseline(self.baseline)


if __name__ == '__main__':
    unittest.main()
