"""Unit tests of smoke.py: python3 -m unittest discover -s scripts/smoke -p 'test_*.py'"""

import contextlib
import io
import json
import os
import sys
import tempfile
import unittest
import zipfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import smoke  # noqa: E402

PLUGIN_XML = '''<idea-plugin xmlns:xi="http://www.w3.org/2001/XInclude">
  <id>org.jetbrains.erlang</id>
  <version>0.11.SNAPSHOT-7</version>
  <idea-version since-build="262" until-build="263.*"/>
  <depends optional="true" config-file="java-deps.xml">com.intellij.modules.java</depends>
  <xi:include href="ErlangPlugin.xml" xpointer="xpointer(/idea-plugin/*)">
    <xi:fallback><xi:include href="/META-INF/ErlangPlugin.xml" xpointer="xpointer(/idea-plugin/*)"/></xi:fallback>
  </xi:include>
  <extensionPoints>
    <extensionPoint name="sdkProvider" interface="x.Y"/>
  </extensionPoints>
  <extensions defaultExtensionNs="org.jetbrains.erlang">
    <sdkProvider implementation="x.Z"/>
  </extensions>
</idea-plugin>'''
ERLANG_PLUGIN_XML = '''<idea-plugin>
  <extensions defaultExtensionNs="com.intellij">
    <localInspection language="Erlang" shortName="ErlangUnusedFunction" displayName="Unused function"
                     groupName="Erlang" enabledByDefault="true" level="WARNING" implementationClass="A"/>
    <localInspection language="Erlang" shortName="ErlangSingleClauseCase" enabledByDefault="false"
                     level="WEAK WARNING" implementationClass="B"/>
    <globalInspection shortName="ErlangGlobal" implementationClass="C"/>
    <annotator language="Erlang" implementationClass="D"/>
    <codeInsight.lineMarkerProvider language="Erlang" implementationClass="E"/>
  </extensions>
</idea-plugin>'''
JAVA_DEPS_XML = '<idea-plugin><extensions defaultExtensionNs="com.intellij"><programPatcher implementation="F"/>' \
                '</extensions></idea-plugin>'
BEAMS = ['debugger/beams/otp_27_plus/debugnode.beam', 'eunit/beams/otp_27_plus/eunit_teamcity.beam']
IDE_DESCRIPTOR = b'''<idea-plugin><id>com.intellij</id><extensionPoints>
  <extensionPoint name="localInspection" beanClass="x"/>
  <extensionPoint name="globalInspection" beanClass="x"/>
  <extensionPoint name="codeInsight.lineMarkerProvider" beanClass="x"/>
  <extensionPoint qualifiedName="com.intellij.annotator" beanClass="x"/>
  <extensionPoint qualifiedName="com.intellij.programPatcher" interface="x"/>
</extensionPoints></idea-plugin>'''


def jar_bytes(entries):
    buffer = io.BytesIO()
    with zipfile.ZipFile(buffer, 'w') as jar:
        for name, content in entries.items():
            jar.writestr(name, content)
    return buffer.getvalue()


def make_archive(directory, plugin_xml=PLUGIN_XML, erlang_plugin_xml=ERLANG_PLUGIN_XML, skip=(), beam_content=None):
    entries = {'META-INF/plugin.xml': plugin_xml, 'META-INF/ErlangPlugin.xml': erlang_plugin_xml,
               'META-INF/java-deps.xml': JAVA_DEPS_XML, 'META-INF/MANIFEST.MF': 'Manifest-Version: 1.0\n'}
    for beam in BEAMS:
        entries[beam] = beam_content or b'beam ' + beam.encode()
    entries = {k: v for k, v in entries.items() if k not in skip}
    path = os.path.join(directory, 'plugin.zip')
    jars = {'intellij-erlang/lib/intellij-erlang.jar': jar_bytes(entries),
            'intellij-erlang/lib/intellij-erlang.jps-plugin.jar': b'jar',
            'intellij-erlang/lib/OtpErlang.jar': b'jar',
            'intellij-erlang/lib/intellij-erlang-0.11.SNAPSHOT-7-searchableOptions.jar': b'jar'}
    with zipfile.ZipFile(path, 'w') as archive:
        for name, content in jars.items():
            if name not in skip:
                archive.writestr(name, content)
    return path


def quiet(function, *args):
    """Calls `function` with stdout captured; returns (its value, the output)."""
    out = io.StringIO()
    with contextlib.redirect_stdout(out):
        value = function(*args)
    return value, out.getvalue()


class TempTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.dir = self.temp.name

    def tearDown(self):
        self.temp.cleanup()

    def path(self, *parts):
        return os.path.join(self.dir, *parts)

    def result(self, name):
        with open(self.path(name)) as f:
            return json.load(f)


class ArchiveTest(TempTest):
    def setUp(self):
        super().setUp()
        for beam in BEAMS:
            path = self.path('resources', beam)
            os.makedirs(os.path.dirname(path))
            with open(path, 'wb') as f:
                f.write(b'beam ' + beam.encode())

    def test_good_archive_passes(self):
        failures, info = smoke.check_archive(make_archive(self.dir), self.dir, '0.11.SNAPSHOT-7')
        self.assertEqual([], failures)
        self.assertEqual('0.11.SNAPSHOT-7', info['version'])
        self.assertEqual(BEAMS, info['beams'])
        self.assertEqual(['META-INF/ErlangPlugin.xml', 'META-INF/java-deps.xml'], info['descriptors'])

    def test_wrong_version_and_range_fail(self):
        xml = PLUGIN_XML.replace('until-build="263.*"', 'until-build="264.*"')
        failures, _ = smoke.check_archive(make_archive(self.dir, plugin_xml=xml), self.dir, '0.11.1304')
        self.assertEqual(['version 0.11.SNAPSHOT-7, expected 0.11.1304', 'until-build 264.*, expected 263.*'], failures)

    def test_missing_parts_fail(self):
        path = make_archive(self.dir, skip=('META-INF/java-deps.xml', BEAMS[1], 'intellij-erlang/lib/OtpErlang.jar',
                                            'intellij-erlang/lib/intellij-erlang-0.11.SNAPSHOT-7-searchableOptions.jar'))
        failures, _ = smoke.check_archive(path, self.dir, '0.11.SNAPSHOT-7')
        self.assertEqual(['no lib/OtpErlang.jar in the archive', 'no searchable options jar in the archive',
                          'no META-INF/java-deps.xml in lib/intellij-erlang.jar',
                          'plugin.xml names META-INF/java-deps.xml, which is not in lib/intellij-erlang.jar',
                          'no eunit/beams/otp_27_plus/eunit_teamcity.beam in lib/intellij-erlang.jar'], failures)

    def test_config_file_that_is_not_in_the_jar_fails(self):
        xml = PLUGIN_XML.replace('<extensionPoints>', '<depends optional="true" config-file="nope-missing.xml">'
                                 'com.intellij.modules.platform</depends>\n  <extensionPoints>')
        failures, _ = smoke.check_archive(make_archive(self.dir, plugin_xml=xml), self.dir, '0.11.SNAPSHOT-7')
        self.assertEqual(['plugin.xml names META-INF/nope-missing.xml, which is not in lib/intellij-erlang.jar'],
                         failures)

    def test_changed_beam_fails(self):
        failures, _ = smoke.check_archive(make_archive(self.dir, beam_content=b'other'), self.dir, '0.11.SNAPSHOT-7')
        self.assertEqual(2, len(failures))
        self.assertIn('differs from resources/debugger/beams/otp_27_plus/debugnode.beam', failures[0])

    def test_profile_turns_on_every_erlang_inspection_and_the_annotators(self):
        tools = smoke.erlang_inspections(make_archive(self.dir))
        self.assertEqual([('ErlangUnusedFunction', 'WARNING'), ('ErlangSingleClauseCase', 'WEAK WARNING'),
                          ('ErlangGlobal', 'WARNING')], tools)
        profile = smoke.inspection_profile(tools)
        self.assertIn('<inspection_tool class="ErlangSingleClauseCase" enabled="true" level="WEAK WARNING"', profile)
        self.assertIn('<inspection_tool class="Annotator" enabled="true" level="ERROR"', profile)
        self.assertNotIn('class="Annotator" enabled="false"', profile)


class ExtensionsTest(TempTest):
    def fake_ide(self, descriptor=IDE_DESCRIPTOR, copies=1):
        os.makedirs(self.path('ide', 'lib'))
        for i in range(copies):
            with open(self.path('ide', 'lib', 'app%d.jar' % i), 'wb') as f:
                f.write(jar_bytes({'META-INF/plugin.xml': descriptor, 'META-INF/other.txt': b'<extensionPoint name="no"/>',
                                   'deep/dir/x.xml': b'<extensionPoint name="notScanned"/>'}))
        with open(self.path('ide.json'), 'w') as f:
            json.dump({'platformPath': self.path('ide'), 'version': '2026.3', 'buildNumber': '263.1'}, f)

    def test_extensions_of_the_descriptors(self):
        used, declared = smoke.used_extensions(smoke.plugin_descriptors(make_archive(self.dir)))
        self.assertEqual({('org.jetbrains.erlang', 'sdkProvider'), ('com.intellij', 'localInspection'),
                          ('com.intellij', 'globalInspection'), ('com.intellij', 'annotator'),
                          ('com.intellij', 'codeInsight.lineMarkerProvider'), ('com.intellij', 'programPatcher')}, used)
        self.assertEqual({'org.jetbrains.erlang.sdkProvider'}, declared)

    def test_every_extension_point_is_known(self):
        self.fake_ide()
        used, declared = smoke.used_extensions(smoke.plugin_descriptors(make_archive(self.dir)))
        qualified, short = smoke.ide_extension_points(self.path('ide'))
        self.assertEqual({'com.intellij.annotator', 'com.intellij.programPatcher'}, qualified)
        self.assertEqual({'localInspection', 'globalInspection', 'codeInsight.lineMarkerProvider'}, short)
        self.assertEqual([], smoke.unknown_extensions(used, declared, qualified, short))

    def test_unknown_extension_point_fails(self):
        self.fake_ide()
        xml = ERLANG_PLUGIN_XML.replace('<annotator ', '<fooBarUnknownEp implementation="x"/>\n    <annotator ')
        used, declared = smoke.used_extensions(smoke.plugin_descriptors(make_archive(self.dir, erlang_plugin_xml=xml)))
        qualified, short = smoke.ide_extension_points(self.path('ide'))
        self.assertEqual(['com.intellij.fooBarUnknownEp'], smoke.unknown_extensions(used, declared, qualified, short))

    def test_command_fails_on_an_unknown_extension_point_and_on_a_scan_that_finds_nothing(self):
        self.fake_ide(copies=1)
        xml = ERLANG_PLUGIN_XML.replace('<annotator ', '<fooBarUnknownEp implementation="x"/>\n    <annotator ')
        args = ['extensions', '--ide', self.path('ide.json'), '--zip', make_archive(self.dir, erlang_plugin_xml=xml),
                '--result', self.path('r.json')]
        code, _ = quiet(smoke.main, args)
        self.assertEqual(1, code)
        failures = self.result('r.json')['failures']
        self.assertEqual('IntelliJ IDEA 2026.3 declares no extension point com.intellij.fooBarUnknownEp', failures[0])
        self.assertIn('the scan does not work', failures[1])


LOG = '''2026-11-20 04:40:11,915 [    783]   INFO - #c.i.p.i.b.AppStarter - Loaded custom plugins: Erlang (org.jetbrains.erlang, 0.11.SNAPSHOT-7)
2026-11-20 04:40:21,087 [   9955]   INFO - #c.i.o.p.MergingQueueGuiExecutor - Running task: (dumb mode task) UnindexedFilesIndexer[proj, reasons: [On project open]]
2026-11-20 04:40:46,677 [  35545]   INFO - #c.i.u.i.UnindexedFilesIndexer - Finished for proj. Unindexed files update took 25548ms; general responsiveness: ok
2026-11-20 04:40:46,677 [  35545]   INFO - #c.i.o.p.MergingQueueGuiExecutor - Task finished: (dumb mode task) UnindexedFilesIndexer[proj, reasons: [On project open]]
2026-11-20 04:40:50,727 [  39595] SEVERE - #c.i.c.InspectionsResultUtil - Descriptions are missed for tools: MongoJSResolveInspection, MsBuiltinInspection
2026-11-20 04:40:50,727 [  39595] SEVERE - #c.i.c.InspectionsResultUtil - IntelliJ IDEA 2026.3  Build #IU-263.7307.200
2026-11-20 04:40:51,000 [  39600]   WARN - #c.i.o.v.n.p.FSRecordsImpl - Slow operation in com.intellij.foo
'''
PLUGIN_EXCEPTION = '''2026-11-19 22:04:25,796 [ 131388] SEVERE - #c.i.c.e.GlobalInspectionContextImpl - In file: /p/erl_lint.erl
java.lang.NullPointerException: Cannot invoke "com.intellij.psi.PsiElement.getText()"
\tat org.intellij.erlang.inspection.ErlangIoFormatInspection$1.visitGlobalFunctionCallExpression(ErlangIoFormatInspection.java:72)
\tat com.intellij.codeInspection.InspectionEngine.lambda$inspectElements$3(InspectionEngine.java:388)
'''
PLUGIN_ID_ONLY = '''2026-11-20 13:12:14,441 [  46166] SEVERE - #c.i.c.e.GlobalInspectionContextBase - Cannot start inspection 'X'. [Plugin: org.jetbrains.erlang]
com.intellij.diagnostic.PluginException: Cannot start inspection 'X'. [Plugin: org.jetbrains.erlang]
\tat com.intellij.codeInspection.ex.GlobalInspectionContextBase.run(GlobalInspectionContextBase.java:1)
'''
BLAMED = '''2026-11-20 13:12:15,000 [  46200]  ERROR - #c.i.o.u.ObjectTree - Memory leak detected
java.lang.Throwable: Memory leak detected
\tat com.intellij.openapi.util.ObjectTree.assertIsEmpty(ObjectTree.java:1)
2026-11-20 13:12:15,001 [  46201]  ERROR - #c.i.o.u.ObjectTree - Plugin to blame: Erlang version: 0.11.SNAPSHOT-7
'''
ERROR_LEVEL = '''2026-11-20 13:12:16,000 [  46300]  ERROR - #o.i.e.s.ErlangSdkType - failed
java.lang.IllegalStateException: failed
\tat org.intellij.erlang.sdk.ErlangSdkType.detect(ErlangSdkType.java:301)
'''
WARN_WITH_FRAME = '''2026-11-20 13:12:17,000 [  46400]   WARN - #c.i.u.i.FileBasedIndexImpl - indexer failed
java.lang.RuntimeException: boom
\tat org.intellij.erlang.index.ErlangAtomIndex.map(ErlangAtomIndex.java:5)
'''
BIND_EXCEPTION = '''2026-11-20 04:40:22,467 [  11335] SEVERE - #c.i.o.a.i.ExceptionsKt - Unhandled exception in [CoroutineName(com.intellij.mcpserver.impl.McpServerService)]
java.net.BindException: Address already in use
\tat java.base/sun.nio.ch.Net.bind0(Native Method)
'''


def clean_failures(info, problems=3, rc=0, timed_out=False, version='0.11.SNAPSHOT-7'):
    return smoke.inspect_failures(rc, timed_out, version, info, problems)


class LogTest(unittest.TestCase):
    def test_clean_log(self):
        info = smoke.analyze_log(LOG, 'Done.\n', {'ErlangUnusedFunction'})
        self.assertEqual('0.11.SNAPSHOT-7', info['pluginVersion'])
        self.assertEqual(2, info['logErrors'])
        self.assertEqual(0, info['pluginExceptions'])
        self.assertEqual([], info['missingDescriptions'])
        self.assertEqual(1, info['dumbTasks'])
        self.assertEqual([], info['unfinishedDumbTasks'])
        self.assertEqual(1, info['indexingFinished'])
        self.assertEqual([], clean_failures(info))

    def test_plugin_version_of_2026_2(self):
        info = smoke.analyze_log('x - Loaded custom plugins: Other (1.0), Erlang (0.11.1303)\n', '')
        self.assertEqual('0.11.1303', info['pluginVersion'])

    def test_plugin_exception_fails(self):
        info = smoke.analyze_log(LOG + PLUGIN_EXCEPTION, '')
        self.assertEqual(1, info['pluginExceptions'])
        self.assertIn('ErlangIoFormatInspection.java:72', info['pluginExceptionTexts'][0])
        self.assertEqual(['1 exceptions of the plugin in the IDE log'], clean_failures(info))

    def test_entry_with_only_the_plugin_id_counts(self):
        info = smoke.analyze_log(LOG + PLUGIN_ID_ONLY, '')
        self.assertEqual(1, info['pluginExceptions'])

    def test_entry_that_blames_the_plugin_counts(self):
        info = smoke.analyze_log(LOG + BLAMED, '')
        self.assertEqual(1, info['pluginExceptions'])
        self.assertIn('Plugin to blame: Erlang', info['pluginExceptionTexts'][0])

    def test_entry_at_error_level_counts(self):
        info = smoke.analyze_log(LOG + ERROR_LEVEL, '')
        self.assertEqual(3, info['logErrors'])
        self.assertEqual(1, info['pluginExceptions'])

    def test_warn_entry_counts_only_with_a_frame_of_the_plugin(self):
        info = smoke.analyze_log(LOG + WARN_WITH_FRAME, '')
        self.assertEqual(1, info['pluginExceptions'])
        self.assertEqual(2, info['logErrors'])
        self.assertEqual(0, smoke.analyze_log(LOG.replace('com.intellij.foo', 'org.intellij.erlang.Foo'), '')
                         ['pluginExceptions'])

    def test_platform_bind_exception_neither_counts_nor_hides_one_of_the_plugin(self):
        info = smoke.analyze_log(LOG + BIND_EXCEPTION + PLUGIN_EXCEPTION, '')
        self.assertEqual(1, info['pluginExceptions'])
        self.assertIn('ErlangIoFormatInspection', info['pluginExceptionTexts'][0])
        self.assertEqual(0, smoke.analyze_log(LOG + BIND_EXCEPTION, '')['pluginExceptions'])

    def test_missing_description_of_an_erlang_inspection_fails(self):
        log = LOG.replace('MongoJSResolveInspection, MsBuiltinInspection',
                          'MongoJSResolveInspection, ErlangUnusedFunction, MsBuiltinInspection')
        info = smoke.analyze_log(log, '', {'ErlangUnusedFunction', 'ErlangOther'})
        self.assertEqual(['ErlangUnusedFunction'], info['missingDescriptions'])
        self.assertEqual(['the IDE found no description of ErlangUnusedFunction'], clean_failures(info))

    def test_plugin_frames_on_the_console_fail(self):
        console = 'Exception in thread "x" java.lang.IllegalStateException\n\tat org.intellij.erlang.psi.X.y(X.java:1)\n'
        info = smoke.analyze_log(LOG, console)
        self.assertEqual(['1 stack frames of the plugin on the console'], clean_failures(info))

    def test_unfinished_indexing_and_missing_plugin_fail(self):
        log = '\n'.join(l for l in LOG.split('\n') if 'Task finished' not in l and 'Loaded custom' not in l)
        info = smoke.analyze_log(log, '')
        self.assertEqual(['UnindexedFilesIndexer[proj, reasons: [On project open]]'], info['unfinishedDumbTasks'])
        self.assertEqual(['the inspect run exited with 3', 'the IDE did not load the plugin',
                          'indexing did not finish: UnindexedFilesIndexer[proj, reasons: [On project open]]'],
                         clean_failures(info, rc=3))

    def test_no_finished_indexing_fails(self):
        log = '\n'.join(l for l in LOG.split('\n') if 'UnindexedFilesIndexer - Finished' not in l)
        info = smoke.analyze_log(log, '')
        self.assertEqual([], info['unfinishedDumbTasks'])
        self.assertEqual(['the IDE log shows no finished indexing'], clean_failures(info))

    def test_timeout_other_version_and_no_problems_fail(self):
        info = smoke.analyze_log(LOG.replace('SNAPSHOT-7', 'SNAPSHOT-6'), '')
        self.assertEqual(['the inspect run did not end in time; thread dump in log/threaddump-timeout.txt',
                          'the IDE loaded plugin version 0.11.SNAPSHOT-6, expected 0.11.SNAPSHOT-7',
                          'no Erlang problem found: the Erlang inspections did not run'],
                         clean_failures(info, problems=0, rc=-15, timed_out=True))

    def test_problems_read_from_the_results(self):
        with tempfile.TemporaryDirectory() as res:
            with open(os.path.join(res, 'ErlangUnusedFunction.xml'), 'w') as f:
                problem = ('<problem><file>file://$PROJECT_DIR$/src/smoke_canary.erl</file><line>%d</line>'
                           '<problem_class severity="WARNING">Unused</problem_class></problem>')
                # The second problem at line 30 is a duplicate of the first one.
                f.write('<problems>%s%s%s</problems>' % (problem % 30, problem % 31, problem % 30))
            with open(os.path.join(res, 'JsonStandardCompliance.xml'), 'w') as f:
                f.write('<problems><problem><file>x</file></problem></problems>')
            with open(os.path.join(res, '.descriptions.xml'), 'w') as f:
                f.write('<inspections/>')
            by_inspection, by_severity, by_file, duplicates = smoke.problems(res, {'ErlangUnusedFunction', 'ErlangOther'})
        self.assertEqual({'ErlangUnusedFunction': 2}, by_inspection)
        self.assertEqual({'WARNING': 2}, by_severity)
        self.assertEqual({('ErlangUnusedFunction', 'src/smoke_canary.erl'): 2}, dict(by_file))
        self.assertEqual(1, duplicates)


INPUTS = 'cowboy 2.19.0 79e3fb02b31d, Erlang/OTP 27.3.4.9'


class ExpectationsTest(TempTest):
    def test_exact_round_trip_and_match(self):
        path = self.path('expected.txt')
        smoke.write_expectations(path, 'smoke_app', smoke.EXACT, INPUTS,
                                 {'ErlangUnusedFunction src/smoke_canary.erl': 1, 'ErlangSingleClauseCase src/smoke_canary.erl': 1})
        expected = smoke.read_expectations(path)
        self.assertEqual(smoke.EXACT, expected['mode'])
        with open(path) as f:
            self.assertIn(smoke.REFRESH_EXPECTED, f.read())
        found = {('ErlangUnusedFunction', 'src/smoke_canary.erl'): 1, ('ErlangSingleClauseCase', 'src/smoke_canary.erl'): 1}
        self.assertEqual(([], []), smoke.compare_expectations(expected, INPUTS, found))

    def test_exact_fails_on_an_extra_and_a_missing_problem(self):
        path = self.path('expected.txt')
        smoke.write_expectations(path, 'smoke_app', smoke.EXACT, INPUTS, {'ErlangUnusedFunction src/smoke_canary.erl': 1})
        found = {('ErlangUnresolvedFunction', 'src/resolve_smoke.erl'): 2}
        failures, _ = smoke.compare_expectations(smoke.read_expectations(path), INPUTS, found)
        self.assertEqual(['ErlangUnresolvedFunction src/resolve_smoke.erl: 2 problems, expected 0',
                          'ErlangUnusedFunction src/smoke_canary.erl: 0 problems, expected 1'], failures)

    def test_limit_fails_above_and_reports_below(self):
        path = self.path('expected.txt')
        smoke.write_expectations(path, 'cowboy', smoke.LIMIT, INPUTS, {'ErlangUnusedVariable': 10, 'ErlangUnboundVariable': 5})
        found = {('ErlangUnusedVariable', 'src/a.erl'): 6, ('ErlangUnusedVariable', 'src/b.erl'): 5,
                 ('ErlangUnboundVariable', 'src/a.erl'): 4, ('ErlangNew', 'src/a.erl'): 1}
        failures, better = smoke.compare_expectations(smoke.read_expectations(path), INPUTS, found)
        self.assertEqual(['ErlangNew: 1 problems, at most 0 expected', 'ErlangUnusedVariable: 11 problems, at most 10 '
                          'expected'], failures)
        self.assertEqual(['ErlangUnboundVariable'], better)

    def test_other_inputs_fail(self):
        path = self.path('expected.txt')
        smoke.write_expectations(path, 'cowboy', smoke.LIMIT, 'cowboy 2.18.0', {})
        failures, _ = smoke.compare_expectations(smoke.read_expectations(path), INPUTS, {})
        self.assertEqual(1, len(failures))
        self.assertIn('refresh them with ' + smoke.REFRESH_EXPECTED, failures[0])

    def test_malformed_and_missing_files_are_errors(self):
        path = self.path('expected.txt')
        smoke.write_expectations(path, 'cowboy', smoke.LIMIT, INPUTS, {})
        with open(path, 'a') as f:
            f.write('3 ErlangX src/a.erl\n')
        with self.assertRaises(smoke.SmokeError):
            smoke.read_expectations(path)
        with self.assertRaises(smoke.SmokeError):
            smoke.read_expectations(self.path('none.txt'))

    def write_run(self, name, project, by_file, failures=()):
        os.makedirs(self.path('work', 'results'), exist_ok=True)
        with open(self.path('work', 'results', 'inspect-%s.json' % name), 'w') as f:
            json.dump({'check': 'inspect', 'name': name, 'project': project, 'byFile': by_file,
                       'runFailures': [f for f in failures if ' problems' not in f], 'failures': list(failures)}, f)

    def test_refresh_takes_the_most_per_inspection_in_limit_mode(self):
        self.write_run('2026.2-cowboy', 'cowboy', {'ErlangA src/a.erl': 3, 'ErlangB src/b.erl': 1},
                       ['ErlangA: 3 problems, at most 2 expected'])
        self.write_run('2026.3-cowboy', 'cowboy', {'ErlangA src/a.erl': 2, 'ErlangA src/c.erl': 2})
        code, _ = quiet(smoke.main, ['expectations', self.path('work'), '--project', 'cowboy', '--file',
                                     self.path('e.txt'), '--inputs-id', INPUTS])
        self.assertEqual(0, code)
        self.assertEqual({'ErlangA': 4, 'ErlangB': 1}, smoke.read_expectations(self.path('e.txt'))['counts'])

    def test_refresh_refuses_ides_that_disagree_in_exact_mode_and_failed_runs(self):
        self.write_run('2026.2-smoke_app', 'smoke_app', {'ErlangA src/a.erl': 1})
        self.write_run('2026.3-smoke_app', 'smoke_app', {'ErlangA src/a.erl': 2})
        code, _ = quiet(smoke.main, ['expectations', self.path('work'), '--project', 'smoke_app', '--file',
                                     self.path('e.txt'), '--inputs-id', INPUTS])
        self.assertEqual(2, code)
        self.assertFalse(os.path.exists(self.path('e.txt')))
        self.write_run('2026.3-smoke_app', 'smoke_app', {'ErlangA src/a.erl': 1}, ['the IDE did not load the plugin'])
        code, _ = quiet(smoke.main, ['expectations', self.path('work'), '--project', 'smoke_app', '--file',
                                     self.path('e.txt'), '--inputs-id', INPUTS])
        self.assertEqual(2, code)


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

    def test_mcp_server_is_off(self):
        self.assertIn('<option name="enableMcpServer" value="false" />', smoke.MCP_SERVER_SETTINGS)


class ParserTest(TempTest):
    def setUp(self):
        super().setUp()
        self.baseline = self.path('baseline.txt')
        smoke.write_baseline(self.baseline, 'cowboy 1, Erlang/OTP 27', 10, {'otp/a.erl': 2, 'cowboy/b.erl': 1})

    def write_report(self, errors, files=10, exceptions=()):
        path = self.path('report.json')
        with open(path, 'w') as f:
            json.dump({'files': files, 'errors': [{'file': e, 'line': 3, 'description': 'x expected'} for e in errors],
                       'exceptions': list(exceptions)}, f)
        return path

    def report(self, errors, files=10, exceptions=()):
        return smoke.read_corpus_report(self.write_report(errors, files, exceptions))

    def run_parser(self, *extra, report=None):
        args = ['parser', report or self.path('report.json'), self.baseline, '--corpus-id', 'cowboy 1, Erlang/OTP 27',
                '--result', self.path('parser.json')] + list(extra)
        code, out = quiet(smoke.main, args)
        return code, self.result('parser.json'), out

    def test_baseline_round_trip(self):
        baseline = smoke.read_baseline(self.baseline)
        self.assertEqual({'corpus': 'cowboy 1, Erlang/OTP 27', 'files': 10,
                          'counts': {'otp/a.erl': 2, 'cowboy/b.erl': 1}}, baseline)
        with open(self.baseline) as f:
            self.assertIn('#   scripts/smoke/run.sh inputs parser-baseline', f.read())

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
                          '"cowboy 2, Erlang/OTP 27"; refresh the baseline with scripts/smoke/run.sh inputs parser-baseline',
                          'the test parsed 9 files, the baseline has 10; when the corpus changed on purpose, refresh the '
                          'baseline with scripts/smoke/run.sh inputs parser-baseline',
                          'the parser threw on x.erl: E'], failures)

    def test_malformed_baseline_is_an_error(self):
        with open(self.baseline, 'a') as f:
            f.write('two otp/x.erl\n')
        with self.assertRaises(smoke.SmokeError):
            smoke.read_baseline(self.baseline)

    def test_command_passes_on_the_baseline(self):
        self.write_report(['otp/a.erl', 'otp/a.erl', 'cowboy/b.erl'])
        code, result, _ = self.run_parser()
        self.assertEqual((0, 'passed', []), (code, result['status'], result['failures']))

    def test_command_fails_when_gradle_failed(self):
        self.write_report([])
        code, result, _ = self.run_parser('--gradle-exit', '1')
        self.assertEqual((1, 'failed'), (code, result['status']))
        self.assertEqual('ErlangCorpusParsingTest failed with exit code 1; see the Gradle log', result['failures'][0])

    def test_command_fails_without_a_report(self):
        code, result, _ = self.run_parser(report=self.path('none.json'))
        self.assertEqual((1, 'failed'), (code, result['status']))
        self.assertIn('wrote no report', result['failures'][0])

    def test_command_fails_on_more_errors(self):
        self.write_report(['otp/a.erl'] * 3)
        code, result, _ = self.run_parser()
        self.assertEqual(1, code)
        self.assertEqual(['otp/a.erl'], [w['file'] for w in result['worse']])

    def test_update_writes_the_baseline_and_refuses_exceptions(self):
        self.write_report(['otp/z.erl'], files=12)
        code, result, _ = self.run_parser('--update')
        self.assertEqual(0, code)
        self.assertEqual({'corpus': 'cowboy 1, Erlang/OTP 27', 'files': 12, 'counts': {'otp/z.erl': 1}},
                         smoke.read_baseline(self.baseline))
        self.write_report([], files=12, exceptions=[{'file': 'x.erl', 'exception': 'E'}])
        code, result, _ = self.run_parser('--update')
        self.assertEqual(1, code)
        self.assertEqual({'otp/z.erl': 1}, smoke.read_baseline(self.baseline)['counts'])


class ReportTest(TempTest):
    def write(self, name, result):
        os.makedirs(self.path('results'), exist_ok=True)
        with open(self.path('results', name + '.json'), 'w') as f:
            json.dump(result, f)

    def test_no_results_fail(self):
        text, failed = smoke.report(self.dir)
        self.assertTrue(failed)
        self.assertIn('No results', text)

    def test_passed_and_failed_results(self):
        self.write('archive', {'check': 'archive', 'name': 'archive', 'status': 'passed', 'failures': [],
                               'archive': 'a.zip', 'version': '1', 'sinceBuild': '262', 'untilBuild': '263.*'})
        self.write('inspect-x', {'check': 'inspect', 'name': 'smoke_app 2026.3', 'project': 'smoke_app', 'ide': '2026.3',
                                 'build': '263.1', 'status': 'passed', 'failures': [], 'erlangProblems': 2,
                                 'pluginExceptions': 0, 'dumbTasks': 1, 'indexingFinished': 1, 'unfinishedDumbTasks': [],
                                 'seconds': 40})
        text, failed = smoke.report(self.dir)
        self.assertFalse(failed)
        self.assertIn('| smoke_app | 2026.3 (263.1) | 2 | 0 | finished | 40 s | passed |', text)
        self.write('parser', {'check': 'parser', 'name': 'parser', 'status': 'failed', 'failures': ['x: more']})
        text, failed = smoke.report(self.dir)
        self.assertTrue(failed)
        self.assertIn('- parser: x: more', text)


if __name__ == '__main__':
    unittest.main()
