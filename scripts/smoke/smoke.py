#!/usr/bin/env python3
"""The checks of the release smoke (scripts/smoke/run.sh, job release-smoke of .github/workflows/gradle.yml).

Subcommands:
  archive ZIP --repo DIR --build-number N --result FILE
      check the plugin archive: the version, since-build and until-build, the jars, the searchable options, java-deps.xml
      and the beams of the debugger and of EUnit, which must be the committed ones byte for byte
  inspect --ide IDE_JSON --zip ZIP --sdk OTP_ROOT --project NAME --source DIR --out DIR --result FILE [--timeout S]
      install the plugin archive in a fresh sandbox of the IDE in IDE_JSON (written by the Gradle task compatIdeInfo of
      scripts/compat), copy the project there with OTP_ROOT as its Erlang SDK, run the headless `inspect` command with
      every Erlang inspection on, and check the IDE log
  corpus --out DIR --smoke-app DIR --cowboy DIR --otp OTP_ROOT
      copy the .erl and .hrl files of the parser check into one directory
  parser REPORT BASELINE --corpus-id TEXT --result FILE [--gradle-exit N] [--update]
      compare the report of ErlangCorpusParsingTest with the committed baseline, or with --update write the baseline
  fail CHECK NAME MESSAGE --result FILE
      write a failed result, for a step of run.sh that failed before its check could
  report WORK
      print the Markdown summary of WORK/results/*.json; exit with 1 if a result failed

Each check writes one JSON result with `status` passed or failed and the list `failures`, and exits with 1 when it
failed. Python 3 standard library only.
"""

import argparse
import collections
import glob
import io
import json
import os
import platform
import re
import shutil
import signal
import subprocess
import sys
import time
import xml.etree.ElementTree as ElementTree
import zipfile

PASSED = 'passed'
FAILED = 'failed'
TEXT_LIMIT = 3000

# The compatibility range of the archive: the IDE it is built against and supportedUntilBuild of build.gradle.
SINCE_BUILD = '262'
UNTIL_BUILD = '263.*'
PLUGIN_ID = 'org.jetbrains.erlang'
PLUGIN_JAR = 'lib/intellij-erlang.jar'
# Committed beams that the plugin jar must hold unchanged, at the same path under resources/.
BEAM_DIRS = ['debugger/beams', 'eunit/beams']

# What marks an entry of the IDE log as an exception of the plugin: a frame or a class of its packages, or the plugin id
# that the IDE writes into a PluginException.
PLUGIN_MARKERS = ('org.intellij.erlang.', PLUGIN_ID)
# Platform inspections that are not needed and that are slow or noisy on big projects; one once hung the inspect run.
DISABLED_INSPECTIONS = ['VulnerableLibrariesGlobalInspection', 'SpellCheckingInspection', 'DuplicatedCode', 'TodoComment',
                        'NonAsciiCharacters', 'LossyEncoding', 'Annotator']

# The sample projects: source roots of the module (test: marks a test root) and the problems each run must report, as
# (inspection, file): they show that the plugin ran its inspections. Every project must report one Erlang problem.
PROJECTS = {
    'smoke_app': {'roots': ['src'], 'expect': [('ErlangUnusedFunction', 'src/smoke_canary.erl')]},
    'cowboy': {'roots': ['src', 'test:test'], 'expect': []},
}

# The applications of Erlang/OTP whose src and include directories the parser check parses.
OTP_APPS = ['stdlib', 'kernel']

LOG_ENTRY_START = re.compile(r'\n(?=\d{4}-\d\d-\d\d \d\d:\d\d:\d\d,\d+ )')
LOG_ERROR = re.compile(r'^\S+ \S+ \[\s*\d+\]\s+(ERROR|SEVERE) ')
# 2026.2 writes "Erlang (0.11.1303)", 2026.3 "Erlang (org.jetbrains.erlang, 0.11.1303)".
LOADED_PLUGIN = re.compile(r'Loaded custom plugins: (?:.*, )?Erlang \((?:' + re.escape(PLUGIN_ID) + r', )?([^\s(),]+)\)')
DUMB_TASK_STARTED = re.compile(r'MergingQueueGuiExecutor - Running task: \(dumb mode task\) (.*)')
DUMB_TASK_FINISHED = re.compile(r'MergingQueueGuiExecutor - Task finished: \(dumb mode task\) (.*)')
INDEXING_FINISHED = re.compile(r'UnindexedFilesIndexer - Finished for ')
CONSOLE_PLUGIN_FRAME = re.compile(r'^\s*at org\.intellij\.erlang\.', re.MULTILINE)


class SmokeError(Exception):
    """The check could not be set up."""


def read_text(path):
    try:
        with open(path, encoding='utf-8', errors='replace') as f:
            return f.read()
    except FileNotFoundError:
        return None


def read_bytes(path):
    with open(path, 'rb') as f:
        return f.read()


def write_json(path, value):
    os.makedirs(os.path.dirname(os.path.abspath(path)), exist_ok=True)
    with open(path, 'w', encoding='utf-8') as f:
        json.dump(value, f, indent=1, sort_keys=True)
        f.write('\n')


def finish(result, path):
    result['status'] = FAILED if result['failures'] else PASSED
    write_json(path, result)
    for failure in result['failures']:
        print('FAIL ' + failure)
    return 1 if result['failures'] else 0


def move_aside(path):
    """Moves an existing `path` to <parent>/old/<name>-<time>, so that a check starts from nothing and deletes nothing."""
    if os.path.lexists(path):
        old = os.path.join(os.path.dirname(os.path.abspath(path)), 'old')
        os.makedirs(old, exist_ok=True)
        shutil.move(path, os.path.join(old, '%s-%d' % (os.path.basename(path), time.time_ns())))


# archive

def gradle_version(gradle_properties):
    match = re.search(r'^version\s*=\s*(\S+)\s*$', read_text(gradle_properties) or '', re.MULTILINE)
    if not match:
        raise SmokeError('%s has no version' % gradle_properties)
    return match.group(1)


def check_archive(zip_path, repo, expected_version):
    """The failures of the archive and what it holds."""
    failures = []
    info = {'archive': os.path.basename(zip_path), 'size': os.path.getsize(zip_path)}
    with zipfile.ZipFile(zip_path) as archive:
        names = archive.namelist()
        roots = {n.split('/', 1)[0] for n in names}
        if len(roots) != 1:
            return ['the archive has %d top directories, expected 1: %s' % (len(roots), sorted(roots))], info
        root = roots.pop() + '/'
        lib = sorted(n[len(root):] for n in names if n.startswith(root + 'lib/') and n.endswith('.jar'))
        info['jars'] = lib
        for jar in [PLUGIN_JAR, 'lib/intellij-erlang.jps-plugin.jar', 'lib/OtpErlang.jar']:
            if jar not in lib:
                failures.append('no %s in the archive' % jar)
        if not any(re.fullmatch(r'lib/intellij-erlang-.*-searchableOptions\.jar', j) for j in lib):
            failures.append('no searchable options jar in the archive')
        if PLUGIN_JAR not in lib:
            return failures, info
        with zipfile.ZipFile(io.BytesIO(archive.read(root + PLUGIN_JAR))) as jar:
            entries = set(jar.namelist())
            plugin_xml = jar.read('META-INF/plugin.xml').decode('utf-8') if 'META-INF/plugin.xml' in entries else ''
            if not plugin_xml:
                failures.append('no META-INF/plugin.xml in %s' % PLUGIN_JAR)
            version = re.search(r'<version>\s*([^<\s]*)\s*</version>', plugin_xml)
            idea_version = re.search(r'<idea-version\b[^>]*>', plugin_xml)
            since = re.search(r'since-build="([^"]*)"', idea_version.group(0)) if idea_version else None
            until = re.search(r'until-build="([^"]*)"', idea_version.group(0)) if idea_version else None
            info['version'] = version and version.group(1)
            info['sinceBuild'] = since and since.group(1)
            info['untilBuild'] = until and until.group(1)
            if info['version'] != expected_version:
                failures.append('version %s, expected %s' % (info['version'], expected_version))
            if info['sinceBuild'] != SINCE_BUILD:
                failures.append('since-build %s, expected %s' % (info['sinceBuild'], SINCE_BUILD))
            if info['untilBuild'] != UNTIL_BUILD:
                failures.append('until-build %s, expected %s' % (info['untilBuild'], UNTIL_BUILD))
            if 'META-INF/java-deps.xml' not in entries:
                failures.append('no META-INF/java-deps.xml in %s' % PLUGIN_JAR)
            beams = []
            for beam_dir in BEAM_DIRS:
                committed = sorted(glob.glob(os.path.join(repo, 'resources', beam_dir, '**', '*.beam'), recursive=True))
                if not committed:
                    failures.append('no committed beams under resources/%s' % beam_dir)
                for path in committed:
                    name = os.path.relpath(path, os.path.join(repo, 'resources')).replace(os.sep, '/')
                    beams.append(name)
                    if name not in entries:
                        failures.append('no %s in %s' % (name, PLUGIN_JAR))
                    elif jar.read(name) != read_bytes(path):
                        failures.append('%s in %s differs from resources/%s' % (name, PLUGIN_JAR, name))
            info['beams'] = beams
    return failures, info


def cmd_archive(args):
    version = '%s.%s' % (gradle_version(os.path.join(args.repo, 'gradle.properties')), args.build_number)
    failures, info = check_archive(args.zip, args.repo, version)
    result = dict(info, check='archive', name='archive', failures=failures)
    print('Archive %s: version %s, since-build %s, until-build %s, %d jars, %d beams, %d bytes' % (
        info['archive'], info.get('version'), info.get('sinceBuild'), info.get('untilBuild'), len(info.get('jars', [])),
        len(info.get('beams', [])), info['size']))
    return finish(result, args.result)


# inspect

def erlang_inspections(zip_path):
    """(shortName, level) of every inspection that the plugin in the archive registers."""
    with zipfile.ZipFile(zip_path) as archive:
        jar_name = [n for n in archive.namelist() if n.endswith('/' + PLUGIN_JAR)]
        if len(jar_name) != 1:
            raise SmokeError('%s has no %s' % (zip_path, PLUGIN_JAR))
        with zipfile.ZipFile(io.BytesIO(archive.read(jar_name[0]))) as jar:
            descriptor = jar.read('META-INF/ErlangPlugin.xml')
    tools = []
    for element in ElementTree.fromstring(descriptor).iter():
        if element.tag in ('localInspection', 'globalInspection') and element.get('shortName'):
            tools.append((element.get('shortName'), element.get('level', 'WARNING')))
    if not tools:
        raise SmokeError('META-INF/ErlangPlugin.xml of %s registers no inspection' % zip_path)
    return tools


def inspection_profile(tools):
    """The inspection profile with every inspection in `tools` on and DISABLED_INSPECTIONS off."""
    lines = ['<profile version="1.0">', '  <option name="myName" value="ErlangOnly" />']
    for name, level in tools:
        lines.append('  <inspection_tool class="%s" enabled="true" level="%s" enabled_by_default="true" />' % (name, level))
    for name in DISABLED_INSPECTIONS:
        lines.append('  <inspection_tool class="%s" enabled="false" level="WARNING" enabled_by_default="false" />' % name)
    lines.append('</profile>')
    return '\n'.join(lines) + '\n'


def sdk_release(otp_root):
    """The version string the plugin reads from an Erlang/OTP root: releases/*/OTP_VERSION and the erts-* directory."""
    versions = sorted(glob.glob(os.path.join(otp_root, 'releases', '*', 'OTP_VERSION')))
    erts = sorted(glob.glob(os.path.join(otp_root, 'erts-*')) or glob.glob(os.path.join(otp_root, 'lib', 'erts-*')))
    if not versions or not erts:
        raise SmokeError('%s is not an Erlang/OTP root: no releases/*/OTP_VERSION or erts-*' % otp_root)
    otp = read_text(versions[0]).strip()
    return otp, 'OTP %s, erts-%s' % (otp, os.path.basename(erts[0])[len('erts-'):])


def jdk_table(otp_root):
    otp, version_string = sdk_release(otp_root)
    lib = 'file://' + os.path.join(os.path.abspath(otp_root), 'lib')
    return '''<application>
  <component name="ProjectJdkTable">
    <jdk version="2">
      <name value="Erlang %s" />
      <type value="Erlang SDK" />
      <version value="%s" />
      <homePath value="%s" />
      <roots>
        <annotationsPath><root type="composite" /></annotationsPath>
        <classPath><root type="composite"><root url="%s" type="simple" /></root></classPath>
        <javadocPath><root type="composite" /></javadocPath>
        <sourcePath><root type="composite"><root url="%s" type="simple" /></root></sourcePath>
      </roots>
      <additional />
    </jdk>
  </component>
</application>
''' % (otp, version_string, os.path.abspath(otp_root), lib, lib), 'Erlang %s' % otp


def write_project(directory, name, roots, sdk_name):
    """The .idea directory and the module file of an Erlang project with `roots` as its source roots."""
    os.makedirs(os.path.join(directory, '.idea'), exist_ok=True)
    with open(os.path.join(directory, '.idea', 'misc.xml'), 'w') as f:
        f.write('<?xml version="1.0" encoding="UTF-8"?>\n<project version="4">\n'
                '  <component name="ProjectRootManager" version="2" project-jdk-name="%s" project-jdk-type="Erlang SDK" />\n'
                '</project>\n' % sdk_name)
    with open(os.path.join(directory, '.idea', 'modules.xml'), 'w') as f:
        f.write('<?xml version="1.0" encoding="UTF-8"?>\n<project version="4">\n  <component name="ProjectModuleManager">\n'
                '    <modules>\n      <module fileurl="file://$PROJECT_DIR$/%s.iml" filepath="$PROJECT_DIR$/%s.iml" />\n'
                '    </modules>\n  </component>\n</project>\n' % (name, name))
    folders = []
    for root in roots:
        test = root.startswith('test:')
        folders.append('      <sourceFolder url="file://$MODULE_DIR$/%s" isTestSource="%s" />'
                       % (root[len('test:'):] if test else root, 'true' if test else 'false'))
    with open(os.path.join(directory, name + '.iml'), 'w') as f:
        f.write('<?xml version="1.0" encoding="UTF-8"?>\n<module type="ERLANG_MODULE" version="4">\n'
                '  <component name="NewModuleRootManager" inherit-compiler-output="true">\n    <exclude-output />\n'
                '    <content url="file://$MODULE_DIR$">\n%s\n      <excludeFolder url="file://$MODULE_DIR$/.idea" />\n'
                '    </content>\n    <orderEntry type="inheritedJdk" />\n    <orderEntry type="sourceFolder" forTests="false" />\n'
                '  </component>\n</module>\n' % '\n'.join(folders))


def product_info(ide_home):
    """The path and the content of product-info.json: at the root of a Linux or Windows IDE, in Resources on macOS."""
    for relative in ('product-info.json', os.path.join('Resources', 'product-info.json')):
        path = os.path.join(ide_home, relative)
        if os.path.isfile(path):
            with open(path, encoding='utf-8') as f:
                return path, json.load(f)
    raise SmokeError('no product-info.json in %s' % ide_home)


def host_os_arch():
    os_name = {'linux': 'Linux', 'darwin': 'macOS', 'win32': 'Windows'}.get(sys.platform, sys.platform)
    machine = platform.machine().lower()
    return os_name, {'x86_64': 'amd64', 'amd64': 'amd64', 'arm64': 'aarch64', 'aarch64': 'aarch64'}.get(machine, machine)


def launch_record(info, os_name, arch):
    records = [r for r in info.get('launch', []) if r.get('os') == os_name and r.get('arch', arch) == arch]
    if not records:
        raise SmokeError('product-info.json has no launch record for %s %s' % (os_name, arch))
    return records[0]


def launch_command(ide_home, info_path, launch, vm_options, properties_file, args):
    """The java command that starts the IDE as its launcher does: the bundled JBR, the boot class path, the JVM options
    of `vm_options` and of the launch record of product-info.json, and the main class."""
    base = os.path.dirname(info_path)

    def substitute(value):
        return value.replace('$APP_PACKAGE/Contents', ide_home).replace('$IDE_HOME', ide_home)

    java = os.path.normpath(os.path.join(base, launch['javaExecutablePath']))
    class_path = os.pathsep.join(os.path.join(ide_home, 'lib', jar) for jar in launch['bootClassPathJarNames'])
    options = list(vm_options)
    options += [substitute(a) for a in launch.get('additionalJvmArguments', [])]
    options += ['-Didea.home.path=' + ide_home, '-Didea.properties.file=' + properties_file,
                '-Djava.class.path=' + class_path]
    return [java] + options + [launch['mainClass']] + list(args)


def problems(results_dir, erlang_names):
    """Problems per Erlang inspection and per severity, and the (inspection, file) pairs, from the XML of `inspect`."""
    by_inspection, by_severity, found = {}, collections.Counter(), set()
    for path in sorted(glob.glob(os.path.join(results_dir, '*.xml'))):
        name = os.path.basename(path)[:-len('.xml')]
        if name.startswith('.') or name not in erlang_names:
            continue
        root = ElementTree.parse(path).getroot()
        entries = root.findall('problem')
        by_inspection[name] = len(entries)
        for problem in entries:
            problem_class = problem.find('problem_class')
            by_severity[problem_class.get('severity', '?') if problem_class is not None else '?'] += 1
            file_element = problem.find('file')
            if file_element is not None and file_element.text:
                found.add((name, re.sub(r'^.*\$PROJECT_DIR\$/', '', file_element.text)))
    return by_inspection, dict(by_severity), found


def analyze_log(log, console):
    """What the IDE log and the console say: the loaded plugin version, the errors, the exceptions of the plugin, and the
    dumb mode tasks that started and did not finish."""
    errors, plugin_exceptions = [], []
    for entry in LOG_ENTRY_START.split(log):
        if LOG_ERROR.match(entry):
            errors.append(entry.split('\n', 1)[0][:300])
            if any(marker in entry for marker in PLUGIN_MARKERS):
                plugin_exceptions.append(entry[:TEXT_LIMIT])
    loaded = LOADED_PLUGIN.search(log)
    started = collections.Counter(m.group(1).strip() for m in DUMB_TASK_STARTED.finditer(log))
    finished = collections.Counter(m.group(1).strip() for m in DUMB_TASK_FINISHED.finditer(log))
    return {
        'pluginVersion': loaded.group(1) if loaded else None,
        'logErrors': len(errors),
        'logErrorHeads': errors[:20],
        'pluginExceptions': len(plugin_exceptions),
        'pluginExceptionTexts': plugin_exceptions[:5],
        'consolePluginFrames': len(CONSOLE_PLUGIN_FRAME.findall(console)),
        'dumbTasks': sum(started.values()),
        'unfinishedDumbTasks': sorted((started - finished).elements()),
        'indexingFinished': len(INDEXING_FINISHED.findall(log)),
    }


def inspect_failures(project, rc, timed_out, expected_version, log_info, erlang_problems, found):
    failures = []
    if timed_out:
        failures.append('the inspect run did not end in time; thread dump in log/threaddump-timeout.txt')
    elif rc != 0:
        failures.append('the inspect run exited with %s' % rc)
    if log_info['pluginVersion'] is None:
        failures.append('the IDE did not load the plugin')
    elif log_info['pluginVersion'] != expected_version:
        failures.append('the IDE loaded plugin version %s, expected %s' % (log_info['pluginVersion'], expected_version))
    if log_info['pluginExceptions']:
        failures.append('%d exceptions of the plugin in the IDE log' % log_info['pluginExceptions'])
    if log_info['consolePluginFrames']:
        failures.append('%d stack frames of the plugin on the console' % log_info['consolePluginFrames'])
    if log_info['unfinishedDumbTasks']:
        failures.append('indexing did not finish: %s' % ', '.join(log_info['unfinishedDumbTasks']))
    elif not log_info['indexingFinished']:
        failures.append('the IDE log shows no finished indexing')
    if erlang_problems == 0:
        failures.append('no Erlang problem found: the Erlang inspections did not run')
    for inspection, path in PROJECTS.get(project, {}).get('expect', []):
        if (inspection, path) not in found:
            failures.append('no %s problem in %s' % (inspection, path))
    return failures


def archive_version(zip_path):
    with zipfile.ZipFile(zip_path) as archive:
        jar_name = [n for n in archive.namelist() if n.endswith('/' + PLUGIN_JAR)][0]
        with zipfile.ZipFile(io.BytesIO(archive.read(jar_name))) as jar:
            match = re.search(r'<version>\s*([^<\s]*)\s*</version>', jar.read('META-INF/plugin.xml').decode('utf-8'))
    return match.group(1) if match else None


def run_with_watchdog(command, stdout_path, timeout, thread_dump_path, jcmd):
    """Runs `command`; after `timeout` seconds writes a thread dump and stops it. Returns (exit code, timed out)."""
    with open(stdout_path, 'w') as out:
        out.write('CMD: %s <%d JVM options> %s\n' % (command[0], len(command) - 1, ' '.join(command[-6:])))
        out.flush()
        process = subprocess.Popen(command, stdout=out, stderr=subprocess.STDOUT, start_new_session=True)
        try:
            return process.wait(timeout=timeout), False
        except subprocess.TimeoutExpired:
            with open(thread_dump_path, 'w') as dump:
                try:
                    subprocess.run([jcmd, str(process.pid), 'Thread.print'], stdout=dump, stderr=subprocess.STDOUT,
                                   timeout=60, check=False)
                except (OSError, subprocess.TimeoutExpired) as e:
                    dump.write('no thread dump: %s\n' % e)
            os.killpg(process.pid, signal.SIGTERM)
            try:
                process.wait(timeout=30)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait()
            return process.returncode, True


def cmd_inspect(args):
    with open(args.ide, encoding='utf-8') as f:
        ide = json.load(f)
    ide_home = ide['platformPath']
    expected_branch = re.sub(r'^20(\d\d)\.(\d+).*$', r'\1\2', args.ide_version or ide.get('version', ''))
    result = {'check': 'inspect', 'name': '%s %s' % (args.project, ide.get('version')), 'project': args.project,
              'ide': ide.get('version'), 'build': ide.get('buildNumber'), 'sandbox': args.out, 'failures': []}
    if not str(ide.get('buildNumber', '')).startswith(expected_branch + '.'):
        result['failures'].append('the IDE is build %s, expected %s.*' % (ide.get('buildNumber'), expected_branch))
        return finish(result, args.result)

    out = os.path.abspath(args.out)
    move_aside(out)
    for d in ('config/options', 'system', 'plugins', 'log', 'res'):
        os.makedirs(os.path.join(out, d))
    table, sdk_name = jdk_table(args.sdk)
    with open(os.path.join(out, 'config', 'options', 'jdk.table.xml'), 'w') as f:
        f.write(table)
    with zipfile.ZipFile(args.zip) as archive:
        archive.extractall(os.path.join(out, 'plugins'))
    project = os.path.join(out, 'proj')
    shutil.copytree(args.source, project, ignore=shutil.ignore_patterns('.git', '.idea', '*.iml'))
    write_project(project, args.project, PROJECTS[args.project]['roots'], sdk_name)
    tools = erlang_inspections(args.zip)
    profile = os.path.join(out, 'ErlangOnly.xml')
    with open(profile, 'w') as f:
        f.write(inspection_profile(tools))

    properties = os.path.join(out, 'idea.properties')
    paths = {'config': os.path.join(out, 'config'), 'system': os.path.join(out, 'system'),
             'plugins': os.path.join(out, 'plugins'), 'log': os.path.join(out, 'log')}
    with open(properties, 'w') as f:
        f.write(''.join('idea.%s.path=%s\n' % kv for kv in paths.items()))
        f.write('idea.trust.all.projects=true\nidea.initially.ask.config=never\njb.consents.confirmation.enabled=false\n'
                'jb.privacy.policy.text=<!--999.999-->\nide.show.tips.on.startup.default.value=false\n')
    info_path, info = product_info(ide_home)
    launch = launch_record(info, *host_os_arch())
    ide_vm_options = os.path.normpath(os.path.join(os.path.dirname(info_path), launch['vmOptionsFilePath']))
    ide_vm_text = read_text(ide_vm_options)
    if ide_vm_text is None:
        raise SmokeError('the IDE has no %s' % ide_vm_options)
    vm_options = [l.strip() for l in ide_vm_text.split('\n') if l.strip() and not l.strip().startswith('#')]
    vm_options += ['-Xmx%s' % args.heap] + ['-Didea.%s.path=%s' % kv for kv in paths.items()]
    vm_options += ['-Dide.no.platform.update=true', '-Didea.suppress.statistics.report=true',
                   '-XX:ErrorFile=' + os.path.join(out, 'java_error_%p.log')]
    vm_options_file = os.path.join(out, 'idea.vmoptions')
    with open(vm_options_file, 'w') as f:
        f.write('\n'.join(vm_options) + '\n')
    vm_options.append('-Djb.vmOptionsFile=' + vm_options_file)
    command = launch_command(ide_home, info_path, launch, vm_options, properties,
                             ['inspect', project, profile, os.path.join(out, 'res'), '-v2', '-format', 'xml'])
    java = command[0]

    start = time.time()
    rc, timed_out = run_with_watchdog(command, os.path.join(out, 'stdout.txt'), args.timeout,
                                      os.path.join(out, 'log', 'threaddump-timeout.txt'),
                                      os.path.join(os.path.dirname(java), 'jcmd'))
    result['seconds'] = int(time.time() - start)
    result['rc'] = rc
    result['timedOut'] = timed_out

    log = ''.join(read_text(p) or '' for p in sorted(glob.glob(os.path.join(out, 'log', 'idea*.log'))))
    log_info = analyze_log(log, read_text(os.path.join(out, 'stdout.txt')) or '')
    by_inspection, by_severity, found = problems(os.path.join(out, 'res'), {n for n, _ in tools})
    result.update(log_info)
    result.update({'erlangInspections': len(tools), 'erlangProblems': sum(by_inspection.values()),
                   'bySeverity': by_severity, 'byInspection': by_inspection})
    result['failures'] = inspect_failures(args.project, rc, timed_out, archive_version(args.zip), log_info,
                                          result['erlangProblems'], found)
    print('%s on %s (%s): rc=%s %ss, %d Erlang problems %s, %d plugin exceptions, %d other log errors' % (
        args.project, ide.get('version'), ide.get('buildNumber'), rc, result['seconds'], result['erlangProblems'],
        json.dumps(by_severity, sort_keys=True), log_info['pluginExceptions'],
        log_info['logErrors'] - log_info['pluginExceptions']))
    for text in log_info['pluginExceptionTexts']:
        print('PLUGIN EXCEPTION:\n' + '\n'.join(text.split('\n')[:25]))
    return finish(result, args.result)


# parser

def copy_sources(source, target):
    """Copies the .erl and .hrl files under `source` to the same relative paths under `target`; returns their number."""
    count = 0
    for directory, dirs, files in os.walk(source):
        dirs[:] = sorted(d for d in dirs if not d.startswith('.'))
        for name in sorted(files):
            if name.endswith('.erl') or name.endswith('.hrl'):
                relative = os.path.relpath(os.path.join(directory, name), source)
                os.makedirs(os.path.dirname(os.path.join(target, relative)), exist_ok=True)
                shutil.copyfile(os.path.join(directory, name), os.path.join(target, relative))
                count += 1
    return count


def otp_app_dir(otp_root, app):
    found = sorted(glob.glob(os.path.join(otp_root, 'lib', app + '-*')))
    if len(found) != 1:
        raise SmokeError('expected one %s/lib/%s-*, found %d' % (otp_root, app, len(found)))
    return found[0]


def cmd_corpus(args):
    out = os.path.abspath(args.out)
    move_aside(out)
    parts = [('smoke_app/src', os.path.join(args.smoke_app, 'src')),
             ('cowboy/src', os.path.join(args.cowboy, 'src')),
             ('cowboy/test', os.path.join(args.cowboy, 'test'))]
    for app in OTP_APPS:
        for sub in ('src', 'include'):
            parts.append(('otp/%s/%s' % (app, sub), os.path.join(otp_app_dir(args.otp, app), sub)))
    for target, source in parts:
        if not os.path.isdir(source):
            raise SmokeError('%s is not a directory' % source)
        print('%5d files from %s' % (copy_sources(source, os.path.join(out, target)), target))
    return 0


def read_corpus_report(path):
    """(parsed files, error elements per file, the first error per file, exceptions) of ErlangCorpusParsingTest."""
    with open(path, encoding='utf-8') as f:
        report = json.load(f)
    counts, first = collections.Counter(), {}
    for error in report.get('errors', []):
        counts[error['file']] += 1
        first.setdefault(error['file'], 'line %s: %s' % (error.get('line'), (error.get('description') or '')[:120]))
    return report['files'], counts, first, report.get('exceptions', [])


def read_baseline(path):
    """{'corpus': id, 'files': N, 'counts': {file: error elements}} of the baseline file."""
    baseline = {'corpus': None, 'files': None, 'counts': {}}
    for number, line in enumerate((read_text(path) or '').split('\n'), 1):
        line = line.strip()
        if not line or line.startswith('#'):
            continue
        key, _, value = line.partition(' ')
        if key == 'corpus':
            baseline['corpus'] = value.strip()
        elif key == 'files':
            baseline['files'] = int(value)
        elif key.isdigit() and value.strip():
            baseline['counts'][value.strip()] = int(key)
        else:
            raise SmokeError('%s:%d is not "corpus ID", "files N" or "ERRORS FILE": %s' % (path, number, line))
    if baseline['corpus'] is None or baseline['files'] is None:
        raise SmokeError('%s has no corpus or files line' % path)
    return baseline


BASELINE_HEADER = '''# The parser baseline of the release smoke (scripts/smoke/run.sh, job release-smoke of .github/workflows/gradle.yml).
# The corpus is the .erl and .hrl files of the inputs that scripts/smoke/inputs.sh pins (cowboy src and test, Erlang/OTP
# stdlib and kernel src and include) and of testData/smoke/smoke_app/src. ErlangCorpusParsingTest parses them, and the
# check fails when a file has more error elements than this file lists (a file not listed must have none), when the
# parser throws, or when the corpus is another one. Every file compiles with erlc, so each error element is a parser bug.
# After a parser fix, or after a change of the pins, refresh the baseline from the root of the checkout:
#   scripts/smoke/run.sh inputs parser-baseline
# and commit it with the change; the pull request says why the counts changed.
'''


def write_baseline(path, corpus_id, files, counts):
    lines = [BASELINE_HEADER.rstrip('\n'), 'corpus ' + corpus_id, 'files %d' % files]
    lines += ['%d %s' % (counts[name], name) for name in sorted(counts)]
    with open(path, 'w', encoding='utf-8') as f:
        f.write('\n'.join(lines) + '\n')


def compare_with_baseline(baseline, corpus_id, files, counts, first, exceptions):
    """(failures, worse files, better files) of a parse of the corpus against the baseline."""
    failures = []
    if baseline['corpus'] != corpus_id:
        failures.append('the baseline is for the corpus "%s", this corpus is "%s"; refresh the baseline'
                        % (baseline['corpus'], corpus_id))
    if baseline['files'] != files:
        failures.append('the test parsed %d files, the baseline has %d' % (files, baseline['files']))
    for exception in exceptions:
        failures.append('the parser threw on %s: %s' % (exception.get('file'), exception.get('exception')))
    old = baseline['counts']
    worse = [{'file': f, 'baseline': old.get(f, 0), 'now': n, 'firstError': first.get(f)}
             for f, n in sorted(counts.items()) if n > old.get(f, 0)]
    better = [{'file': f, 'baseline': n, 'now': counts.get(f, 0)} for f, n in sorted(old.items()) if counts.get(f, 0) < n]
    for w in worse:
        failures.append('%s: %d error elements, the baseline has %d (%s)' % (w['file'], w['now'], w['baseline'],
                                                                             w['firstError']))
    return failures, worse, better


def cmd_parser(args):
    result = {'check': 'parser', 'name': 'parser', 'failures': []}
    if args.gradle_exit:
        result['failures'].append('ErlangCorpusParsingTest failed with exit code %d; see the Gradle log' % args.gradle_exit)
    if not os.path.isfile(args.report):
        result['failures'].append('ErlangCorpusParsingTest wrote no report %s' % args.report)
        return finish(result, args.result)
    files, counts, first, exceptions = read_corpus_report(args.report)
    result.update({'files': files, 'errorElements': sum(counts.values()), 'filesWithErrors': len(counts),
                   'exceptions': exceptions[:20]})
    print('Parsed %d files: %d error elements in %d files, %d exceptions' % (files, sum(counts.values()), len(counts),
                                                                            len(exceptions)))
    if args.update:
        if result['failures'] or exceptions:
            result['failures'].extend('the parser threw on %s' % e.get('file') for e in exceptions)
            return finish(result, args.result)
        write_baseline(args.baseline, args.corpus_id, files, counts)
        print('Wrote %s' % args.baseline)
        return finish(result, args.result)
    baseline = read_baseline(args.baseline)
    failures, worse, better = compare_with_baseline(baseline, args.corpus_id, files, counts, first, exceptions)
    result['failures'] += failures
    result.update({'baselineErrorElements': sum(baseline['counts'].values()), 'worse': worse, 'better': better})
    print('Baseline: %d error elements in %d files' % (sum(baseline['counts'].values()), len(baseline['counts'])))
    for b in better:
        print('fewer: %s %d -> %d' % (b['file'], b['baseline'], b['now']))
    if better and not failures:
        print('%d files have fewer error elements than the baseline; refresh it with '
              'scripts/smoke/run.sh inputs parser-baseline to keep the gain' % len(better))
    return finish(result, args.result)


# report

def report(work):
    results = []
    for path in sorted(glob.glob(os.path.join(work, 'results', '*.json'))):
        with open(path, encoding='utf-8') as f:
            results.append(json.load(f))
    lines = ['## Release smoke', '']
    inspections = [r for r in results if r['check'] == 'inspect']
    for r in results:
        if r['check'] == 'archive':
            lines.append('- Archive %s, version %s, since-build %s, until-build %s: **%s**' % (
                r.get('archive'), r.get('version'), r.get('sinceBuild'), r.get('untilBuild'), r['status']))
        elif r['check'] == 'parser':
            if 'files' in r:
                lines.append('- Parser: %d files, %d error elements in %d files (baseline %s), %d files worse, %d better: '
                             '**%s**' % (r['files'], r['errorElements'], r['filesWithErrors'],
                                         r.get('baselineErrorElements', '-'), len(r.get('worse', [])),
                                         len(r.get('better', [])), r['status']))
            else:
                lines.append('- Parser: **%s**' % r['status'])
    if inspections:
        lines += ['', '| Project | IDE | Erlang problems | Plugin exceptions | Indexing | Time | Result |',
                  '|---|---|---|---|---|---|---|']
        for r in inspections:
            indexing = 'finished' if r.get('indexingFinished') and not r.get('unfinishedDumbTasks') else 'not finished'
            lines.append('| %s | %s (%s) | %s | %s | %s | %s s | %s |' % (
                r.get('project', r['name']), r.get('ide'), r.get('build'), r.get('erlangProblems', '-'),
                r.get('pluginExceptions', '-'), indexing if 'dumbTasks' in r else '-', r.get('seconds', '-'),
                r['status']))
    failed = [r for r in results if r['status'] != PASSED]
    if failed:
        lines += ['', '### Failures', '']
        for r in failed:
            lines += ['- %s: %s' % (r['name'], f) for f in r['failures']]
    if not results:
        lines.append('No results in %s.' % os.path.join(work, 'results'))
    return '\n'.join(lines) + '\n', bool(failed) or not results


def cmd_fail(args):
    return finish({'check': args.check, 'name': args.name, 'failures': [args.message]}, args.result)


def cmd_report(args):
    text, failed = report(args.work)
    sys.stdout.write(text)
    return 1 if failed else 0


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    sub = parser.add_subparsers(dest='command', required=True)
    p = sub.add_parser('archive')
    p.add_argument('zip')
    p.add_argument('--repo', required=True)
    p.add_argument('--build-number', required=True)
    p.add_argument('--result', required=True)
    p.set_defaults(func=cmd_archive)
    p = sub.add_parser('inspect')
    p.add_argument('--ide', required=True, help='the ide.json of compatIdeInfo')
    p.add_argument('--ide-version', help='the IDE version that was asked for, for example 2026.3')
    p.add_argument('--zip', required=True)
    p.add_argument('--sdk', required=True)
    p.add_argument('--project', required=True, choices=sorted(PROJECTS))
    p.add_argument('--source', required=True)
    p.add_argument('--out', required=True)
    p.add_argument('--result', required=True)
    p.add_argument('--timeout', type=int, default=600)
    p.add_argument('--heap', default='3g')
    p.set_defaults(func=cmd_inspect)
    p = sub.add_parser('corpus')
    p.add_argument('--out', required=True)
    p.add_argument('--smoke-app', required=True)
    p.add_argument('--cowboy', required=True)
    p.add_argument('--otp', required=True)
    p.set_defaults(func=cmd_corpus)
    p = sub.add_parser('parser')
    p.add_argument('report')
    p.add_argument('baseline')
    p.add_argument('--corpus-id', required=True)
    p.add_argument('--result', required=True)
    p.add_argument('--gradle-exit', type=int, default=0)
    p.add_argument('--update', action='store_true')
    p.set_defaults(func=cmd_parser)
    p = sub.add_parser('fail')
    p.add_argument('check')
    p.add_argument('name')
    p.add_argument('message')
    p.add_argument('--result', required=True)
    p.set_defaults(func=cmd_fail)
    p = sub.add_parser('report')
    p.add_argument('work')
    p.set_defaults(func=cmd_report)
    args = parser.parse_args(argv)
    try:
        return args.func(args)
    except SmokeError as e:
        print('::error::%s' % e)
        if getattr(args, 'result', None):
            name = '%s %s' % (args.project, args.ide_version) if args.command == 'inspect' else args.command
            finish({'check': args.command, 'name': name, 'failures': [str(e)]}, args.result)
        return 2


if __name__ == '__main__':
    sys.exit(main())
