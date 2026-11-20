#!/usr/bin/env python3
"""The checks of the release smoke (scripts/smoke/run.sh, job release-smoke of .github/workflows/gradle.yml).

Subcommands:
  archive ZIP --repo DIR --build-number N --result FILE
      check the plugin archive: the version, since-build and until-build, the jars, the searchable options, the files
      that plugin.xml names (config-file, xi:include), and the beams of the debugger and of EUnit, which must be the
      committed ones byte for byte
  extensions --ide IDE_JSON --zip ZIP --result FILE
      check that the IDE declares every extension point that the descriptors of the plugin use; the IDE drops an
      extension of an unknown extension point without a word in its log
  inspect --ide IDE_JSON --zip ZIP --sdk OTP_ROOT --project NAME --source DIR --expected FILE --inputs-id TEXT
          --out DIR --result FILE [--timeout S]
      install the plugin archive in a fresh sandbox of the IDE in IDE_JSON (written by the Gradle task compatIdeInfo of
      scripts/compat), copy the project there with OTP_ROOT as its Erlang SDK, run the headless `inspect` command with
      every Erlang inspection and the annotators on, check the IDE log, and compare the problems with the expected file
  expectations WORK --project NAME --file FILE --inputs-id TEXT
      write the expected file of a project from the inspect results in WORK/results (run.sh phase inspect-baseline)
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

# What marks an ERROR or SEVERE entry of the IDE log as an exception of the plugin: a frame or a class of its packages,
# the plugin id that the IDE writes into a PluginException, or the plugin that the IDE blames. A WARN entry counts when
# it has a stack frame of the plugin.
PLUGIN_MARKERS = ('org.intellij.erlang.', PLUGIN_ID, 'Plugin to blame: Erlang ')
# Platform inspections that are not needed and that are slow or noisy on big projects; one once hung the inspect run.
DISABLED_INSPECTIONS = ['VulnerableLibrariesGlobalInspection', 'SpellCheckingInspection', 'DuplicatedCode', 'TodoComment',
                        'NonAsciiCharacters', 'LossyEncoding']
# The platform inspection that runs the annotators, ErlangAnnotator among them. Its problems count with the Erlang ones.
ANNOTATOR = 'Annotator'

# The sample projects: the source roots of the module (test: marks a test root) and how the problems are compared with
# scripts/smoke/expected-<project>.txt. exact: the same problems per (inspection, file), no more and no fewer. limit: per
# inspection at most the number in the file. Every project must also report one Erlang problem.
PROJECTS = {
    'smoke_app': {'roots': ['src'], 'mode': 'exact'},
    'cowboy': {'roots': ['src', 'test:test'], 'mode': 'limit'},
}
EXACT = 'exact'
LIMIT = 'limit'
REFRESH_PARSER = 'scripts/smoke/run.sh inputs parser-baseline'
REFRESH_EXPECTED = 'scripts/smoke/run.sh inputs build inspect-baseline'

# The applications of Erlang/OTP whose src and include directories the parser check parses.
OTP_APPS = ['stdlib', 'kernel']

LOG_ENTRY_START = re.compile(r'\n(?=\d{4}-\d\d-\d\d \d\d:\d\d:\d\d,\d+ )')
LOG_ERROR = re.compile(r'^\S+ \S+ \[\s*\d+\]\s+(ERROR|SEVERE) ')
LOG_WARN = re.compile(r'^\S+ \S+ \[\s*\d+\]\s+WARN ')
PLUGIN_FRAME = re.compile(r'\n\s*at org\.intellij\.erlang\.')
MISSING_DESCRIPTIONS = re.compile(r'Descriptions are missed for tools: ([^\n]*)')
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
            info['descriptors'] = named_descriptors(plugin_xml)
            for name in info['descriptors']:
                if name not in entries:
                    failures.append('plugin.xml names %s, which is not in %s' % (name, PLUGIN_JAR))
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


def named_descriptors(plugin_xml):
    """The jar entries that plugin.xml names: the config-file of each <depends> (relative to META-INF) and the href of
    each xi:include (relative to META-INF, or to the jar root when it starts with a slash)."""
    names = re.findall(r'\bconfig-file="([^"]+)"', plugin_xml)
    names += re.findall(r'<xi:include\b[^>]*\bhref="([^"]+)"', plugin_xml)
    names = [n[1:] if n.startswith('/') else 'META-INF/' + n for n in names]
    return sorted(set(names))


def plugin_descriptors(zip_path):
    """{entry: root element} of the META-INF/*.xml descriptors (root idea-plugin) of the plugin jar in the archive."""
    descriptors = {}
    with zipfile.ZipFile(zip_path) as archive:
        jar_name = [n for n in archive.namelist() if n.endswith('/' + PLUGIN_JAR)]
        if len(jar_name) != 1:
            raise SmokeError('%s has no %s' % (zip_path, PLUGIN_JAR))
        with zipfile.ZipFile(io.BytesIO(archive.read(jar_name[0]))) as jar:
            for name in jar.namelist():
                if re.fullmatch(r'META-INF/[^/]+\.xml', name):
                    root = ElementTree.fromstring(jar.read(name))
                    if root.tag == 'idea-plugin':
                        descriptors[name] = root
    return descriptors


def used_extensions(descriptors):
    """(the (namespace, name) of each extension point that the descriptors use, the qualified names of the extension
    points that they declare)."""
    used, declared = set(), set()
    for root in descriptors.values():
        for extensions in root.iter('extensions'):
            namespace = extensions.get('defaultExtensionNs')
            for child in extensions:
                if not isinstance(child.tag, str):
                    continue
                match = re.fullmatch(r'\{([^}]*)\}(.*)', child.tag)
                if match:
                    used.add((match.group(1), match.group(2)))
                elif namespace:
                    used.add((namespace, child.tag))
                else:
                    used.add(tuple(child.tag.rsplit('.', 1)) if '.' in child.tag else ('', child.tag))
        for point in root.iter('extensionPoint'):
            declared.add(point.get('qualifiedName') or '%s.%s' % (PLUGIN_ID, point.get('name')))
    return used, declared


EXTENSION_POINT_TAG = re.compile(rb'<extensionPoint\b[^>]*>')


def ide_extension_points(ide_home):
    """(qualified names, short names) of the extension points that the descriptors in the jars of the IDE declare; a
    short name is declared with name= and gets the id of its plugin as the namespace, which the scan does not know."""
    qualified, short = set(), set()
    for directory, _, files in os.walk(ide_home):
        for file_name in files:
            if not file_name.endswith('.jar'):
                continue
            try:
                jar = zipfile.ZipFile(os.path.join(directory, file_name))
            except (zipfile.BadZipFile, OSError):
                continue
            with jar:
                for name in jar.namelist():
                    if not name.endswith('.xml') or ('/' in name and not name.startswith('META-INF/')):
                        continue
                    data = jar.read(name)
                    if b'<extensionPoint' not in data:
                        continue
                    for tag in EXTENSION_POINT_TAG.finditer(data):
                        attributes = dict(re.findall(rb'(\w+)="([^"]*)"', tag.group(0)))
                        if b'qualifiedName' in attributes:
                            qualified.add(attributes[b'qualifiedName'].decode())
                        elif b'name' in attributes:
                            short.add(attributes[b'name'].decode())
    return qualified, short


def unknown_extensions(used, declared, qualified, short):
    """The extension points in `used` that neither the plugin nor the IDE declares, as qualified names."""
    unknown = []
    for namespace, name in sorted(used):
        full = '%s.%s' % (namespace, name) if namespace else name
        if full not in declared and full not in qualified and name not in short:
            unknown.append(full)
    return unknown


def cmd_extensions(args):
    with open(args.ide, encoding='utf-8') as f:
        ide = json.load(f)
    used, declared = used_extensions(plugin_descriptors(args.zip))
    qualified, short = ide_extension_points(ide['platformPath'])
    unknown = unknown_extensions(used, declared, qualified, short)
    result = {'check': 'extensions', 'name': 'extension points %s' % ide.get('version'), 'ide': ide.get('version'),
              'build': ide.get('buildNumber'), 'used': len(used), 'unknown': unknown,
              'failures': ['IntelliJ IDEA %s declares no extension point %s' % (ide.get('version'), u) for u in unknown]}
    if len(qualified) + len(short) < 500:
        result['failures'].append('the IDE in %s declares only %d extension points; the scan does not work'
                                  % (ide['platformPath'], len(qualified) + len(short)))
    print('Extension points on %s (%s): the plugin uses %d, the IDE declares %d, %d unknown' % (
        ide.get('version'), ide.get('buildNumber'), len(used), len(qualified) + len(short), len(unknown)))
    return finish(result, args.result)


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
    """The inspection profile with every inspection in `tools` and the annotators on, and DISABLED_INSPECTIONS off."""
    lines = ['<profile version="1.0">', '  <option name="myName" value="ErlangOnly" />']
    for name, level in tools + [(ANNOTATOR, 'ERROR')]:
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


def problems(results_dir, names):
    """Problems per inspection, per severity and per (inspection, file) of the inspections in `names`, from the XML that
    `inspect` writes, one file per inspection, and the number of duplicates left out. The headless inspection sometimes
    reports every problem of a file twice (seen with 2026.2 on cowboy), so a problem that is the same in every element
    as one before it counts once."""
    by_inspection, by_severity, by_file = {}, collections.Counter(), collections.Counter()
    duplicates = 0
    for path in sorted(glob.glob(os.path.join(results_dir, '*.xml'))):
        name = os.path.basename(path)[:-len('.xml')]
        if name.startswith('.') or name not in names:
            continue
        root = ElementTree.parse(path).getroot()
        entries, seen = [], set()
        for problem in root.findall('problem'):
            key = ElementTree.tostring(problem)
            if key in seen:
                duplicates += 1
            else:
                seen.add(key)
                entries.append(problem)
        by_inspection[name] = len(entries)
        for problem in entries:
            problem_class = problem.find('problem_class')
            by_severity[problem_class.get('severity', '?') if problem_class is not None else '?'] += 1
            file_element = problem.find('file')
            path_text = file_element.text if file_element is not None and file_element.text else '?'
            by_file[(name, re.sub(r'^.*\$PROJECT_DIR\$/', '', path_text))] += 1
    return by_inspection, dict(by_severity), by_file, duplicates


def analyze_log(log, console, inspection_names=()):
    """What the IDE log and the console say: the loaded plugin version, the errors, the exceptions of the plugin, the
    inspections in `inspection_names` that the IDE found no description for, and the dumb mode tasks that started and
    did not finish. Each entry of the log is looked at on its own, so an entry of the platform cannot hide one of the
    plugin."""
    errors, plugin_exceptions, missing_descriptions = [], [], []
    for entry in LOG_ENTRY_START.split(log):
        if LOG_ERROR.match(entry):
            errors.append(entry.split('\n', 1)[0][:300])
            if any(marker in entry for marker in PLUGIN_MARKERS):
                plugin_exceptions.append(entry[:TEXT_LIMIT])
            missing = MISSING_DESCRIPTIONS.search(entry)
            if missing:
                tools = {t.strip() for t in missing.group(1).split(',')}
                missing_descriptions += sorted(tools & set(inspection_names))
        elif LOG_WARN.match(entry) and PLUGIN_FRAME.search(entry):
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
        'missingDescriptions': sorted(set(missing_descriptions)),
        'consolePluginFrames': len(CONSOLE_PLUGIN_FRAME.findall(console)),
        'dumbTasks': sum(started.values()),
        'unfinishedDumbTasks': sorted((started - finished).elements()),
        'indexingFinished': len(INDEXING_FINISHED.findall(log)),
    }


def inspect_failures(rc, timed_out, expected_version, log_info, erlang_problems):
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
    if log_info['missingDescriptions']:
        failures.append('the IDE found no description of %s' % ', '.join(log_info['missingDescriptions']))
    if log_info['unfinishedDumbTasks']:
        failures.append('indexing did not finish: %s' % ', '.join(log_info['unfinishedDumbTasks']))
    elif not log_info['indexingFinished']:
        failures.append('the IDE log shows no finished indexing')
    if erlang_problems == 0:
        failures.append('no Erlang problem found: the Erlang inspections did not run')
    return failures


# The expected problems of a project: scripts/smoke/expected-<project>.txt.

def expectation_key(mode, inspection, path):
    return inspection if mode == LIMIT else '%s %s' % (inspection, path)


def expectation_counts(mode, by_file):
    """The counts of `by_file` ((inspection, file) -> problems) in the form of an expected file of `mode`."""
    counts = collections.Counter()
    for (inspection, path), n in by_file.items():
        counts[expectation_key(mode, inspection, path)] += n
    return dict(counts)


def read_expectations(path):
    """{'inputs': id, 'mode': exact or limit, 'counts': {key: problems}}; a key is "INSPECTION FILE" in exact mode and
    "INSPECTION" in limit mode."""
    expected = {'inputs': None, 'mode': None, 'counts': {}}
    text = read_text(path)
    if text is None:
        raise SmokeError('%s is missing; write it with %s' % (path, REFRESH_EXPECTED))
    for number, line in enumerate(text.split('\n'), 1):
        line = line.strip()
        if not line or line.startswith('#'):
            continue
        key, _, value = line.partition(' ')
        value = value.strip()
        if key == 'inputs':
            expected['inputs'] = value
        elif key == 'mode' and value in (EXACT, LIMIT):
            expected['mode'] = value
        elif key.isdigit() and value and expected['mode'] and (len(value.split()) == 1) == (expected['mode'] == LIMIT):
            expected['counts'][value] = int(key)
        else:
            raise SmokeError('%s:%d is not "inputs ID", "mode exact|limit" or "COUNT INSPECTION%s": %s'
                             % (path, number, ' FILE' if expected['mode'] == EXACT else '', line))
    if expected['inputs'] is None or expected['mode'] is None:
        raise SmokeError('%s has no inputs or mode line' % path)
    return expected


EXPECTED_HEADER = '''# The expected problems of the project %s in the release smoke (scripts/smoke/run.sh, job release-smoke of
# .github/workflows/gradle.yml), with every Erlang inspection and the annotators on, the same in IntelliJ IDEA 2026.2
# and 2026.3.
# %s
# A change of the plugin that changes these problems on purpose refreshes this file from the root of the checkout:
#   %s
# and the pull request says why the problems changed.
'''
MODE_TEXT = {
    EXACT: 'mode exact: the run must report these problems per inspection and file, no more and no fewer.',
    LIMIT: 'mode limit: no inspection may report more problems than here; one not listed must report none.',
}


def write_expectations(path, project, mode, inputs_id, counts):
    lines = [(EXPECTED_HEADER % (project, MODE_TEXT[mode], REFRESH_EXPECTED)).rstrip('\n'), 'inputs ' + inputs_id,
             'mode ' + mode]
    lines += ['%d %s' % (counts[key], key) for key in sorted(counts)]
    with open(path, 'w', encoding='utf-8') as f:
        f.write('\n'.join(lines) + '\n')


def compare_expectations(expected, inputs_id, by_file):
    """(failures, keys with fewer problems than expected in limit mode) of the problems of a run."""
    failures = []
    if expected['inputs'] != inputs_id:
        failures.append('the expected problems are for the inputs "%s", these are "%s"; refresh them with %s'
                        % (expected['inputs'], inputs_id, REFRESH_EXPECTED))
    mode, want = expected['mode'], expected['counts']
    got = expectation_counts(mode, by_file)
    better = []
    for key in sorted(set(want) | set(got)):
        w, g = want.get(key, 0), got.get(key, 0)
        if mode == EXACT and w != g:
            failures.append('%s: %d problems, expected %d' % (key, g, w))
        elif mode == LIMIT and g > w:
            failures.append('%s: %d problems, at most %d expected' % (key, g, w))
        elif mode == LIMIT and g < w:
            better.append(key)
    return failures, better


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


# The MCP server of the IDE listens on a fixed port, so the second of two IDEs that run at the same time logged a
# SEVERE BindException. The smoke does not need it.
MCP_SERVER_SETTINGS = '''<application>
  <component name="McpServerSettings">
    <option name="enableMcpServer" value="false" />
  </component>
</application>
'''


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
    try:
        expected = read_expectations(args.expected)
        if expected['mode'] != PROJECTS[args.project]['mode']:
            raise SmokeError('%s has mode %s, the project %s needs %s' % (
                args.expected, expected['mode'], args.project, PROJECTS[args.project]['mode']))
    except SmokeError as e:
        # The run still goes on, so that the inspect-baseline phase can write the file.
        expected = {'error': str(e)}

    out = os.path.abspath(args.out)
    move_aside(out)
    for d in ('config/options', 'system', 'plugins', 'log', 'res'):
        os.makedirs(os.path.join(out, d))
    table, sdk_name = jdk_table(args.sdk)
    with open(os.path.join(out, 'config', 'options', 'jdk.table.xml'), 'w') as f:
        f.write(table)
    with open(os.path.join(out, 'config', 'options', 'mcpServer.xml'), 'w') as f:
        f.write(MCP_SERVER_SETTINGS)
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

    names = {n for n, _ in tools}
    log = ''.join(read_text(p) or '' for p in sorted(glob.glob(os.path.join(out, 'log', 'idea*.log'))))
    log_info = analyze_log(log, read_text(os.path.join(out, 'stdout.txt')) or '', names)
    by_inspection, by_severity, by_file, duplicates = problems(os.path.join(out, 'res'), names | {ANNOTATOR})
    result.update(log_info)
    result.update({'erlangInspections': len(tools), 'erlangProblems': sum(by_inspection.values()),
                   'bySeverity': by_severity, 'byInspection': by_inspection, 'duplicateProblems': duplicates,
                   'byFile': {'%s %s' % key: n for key, n in sorted(by_file.items())}})
    failures = inspect_failures(rc, timed_out, archive_version(args.zip), log_info, result['erlangProblems'])
    if 'error' in expected:
        expectation_failures, better = [expected['error']], []
    else:
        expectation_failures, better = compare_expectations(expected, args.inputs_id, by_file)
    result['runFailures'] = failures
    result['failures'] = failures + expectation_failures
    result['fewerThanExpected'] = better
    print('%s on %s (%s): rc=%s %ss, %d Erlang problems %s (%d duplicates left out), %d plugin exceptions, '
          '%d other log errors' % (args.project, ide.get('version'), ide.get('buildNumber'), rc, result['seconds'],
                                   result['erlangProblems'], json.dumps(by_severity, sort_keys=True), duplicates,
                                   log_info['pluginExceptions'], log_info['logErrors'] - log_info['pluginExceptions']))
    for text in log_info['pluginExceptionTexts']:
        print('PLUGIN EXCEPTION:\n' + '\n'.join(text.split('\n')[:25]))
    if better:
        print('fewer problems than %s allows: %s; refresh it with %s to keep the gain'
              % (os.path.basename(args.expected), ', '.join(better), REFRESH_EXPECTED))
    return finish(result, args.result)


def cmd_expectations(args):
    """Writes the expected file of a project from its inspect results of every IDE. In exact mode the IDEs must agree;
    in limit mode the limit of an inspection is the most that one IDE reported."""
    mode = PROJECTS[args.project]['mode']
    runs = []
    for path in sorted(glob.glob(os.path.join(args.work, 'results', 'inspect-*.json'))):
        with open(path, encoding='utf-8') as f:
            r = json.load(f)
        if r.get('project') == args.project and 'byFile' in r:
            runs.append(r)
    if not runs:
        raise SmokeError('no inspect result of %s in %s' % (args.project, os.path.join(args.work, 'results')))
    blocking = [(r['name'], f) for r in runs for f in r.get('runFailures', r['failures'])]
    if blocking:
        raise SmokeError('the runs failed for other reasons than the problems: %s' % '; '.join('%s: %s' % b for b in blocking))
    per_run = [expectation_counts(mode, {tuple(k.split(' ', 1)): n for k, n in r['byFile'].items()}) for r in runs]
    if mode == EXACT and any(c != per_run[0] for c in per_run):
        raise SmokeError('the IDEs report other problems on %s: %s' % (args.project, json.dumps(
            {r['name']: c for r, c in zip(runs, per_run)}, sort_keys=True)))
    counts = {}
    for c in per_run:
        for key, n in c.items():
            counts[key] = max(n, counts.get(key, 0))
    write_expectations(args.file, args.project, mode, args.inputs_id, counts)
    print('Wrote %s: %d problems from %s' % (args.file, sum(counts.values()), ', '.join(r['name'] for r in runs)))
    return 0


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
# parser throws, or when the corpus is another one (other pins or another number of files). Every file compiles with
# erlc, so each error element is a parser bug.
# After a parser fix, after a change of the pins, or after a change of testData/smoke/smoke_app, refresh the baseline
# from the root of the checkout:
#   %s
# and commit it with the change; the pull request says why the counts changed.
'''


def write_baseline(path, corpus_id, files, counts):
    lines = [(BASELINE_HEADER % REFRESH_PARSER).rstrip('\n'), 'corpus ' + corpus_id, 'files %d' % files]
    lines += ['%d %s' % (counts[name], name) for name in sorted(counts)]
    with open(path, 'w', encoding='utf-8') as f:
        f.write('\n'.join(lines) + '\n')


def compare_with_baseline(baseline, corpus_id, files, counts, first, exceptions):
    """(failures, worse files, better files) of a parse of the corpus against the baseline."""
    failures = []
    if baseline['corpus'] != corpus_id:
        failures.append('the baseline is for the corpus "%s", this corpus is "%s"; refresh the baseline with %s'
                        % (baseline['corpus'], corpus_id, REFRESH_PARSER))
    if baseline['files'] != files:
        failures.append('the test parsed %d files, the baseline has %d; when the corpus changed on purpose, refresh the '
                        'baseline with %s' % (files, baseline['files'], REFRESH_PARSER))
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
        print('%d files have fewer error elements than the baseline; refresh it with %s to keep the gain'
              % (len(better), REFRESH_PARSER))
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
        elif r['check'] == 'extensions':
            lines.append('- Extension points on %s (%s): the plugin uses %s, %d unknown: **%s**' % (
                r.get('ide'), r.get('build'), r.get('used', '-'), len(r.get('unknown', [])), r['status']))
    if inspections:
        lines += ['', 'Erlang problems count the Erlang inspections and the annotators.', '',
                  '| Project | IDE | Erlang problems | Plugin exceptions | Indexing | Time | Result |',
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
    p = sub.add_parser('extensions')
    p.add_argument('--ide', required=True, help='the ide.json of compatIdeInfo')
    p.add_argument('--zip', required=True)
    p.add_argument('--result', required=True)
    p.set_defaults(func=cmd_extensions)
    p = sub.add_parser('inspect')
    p.add_argument('--ide', required=True, help='the ide.json of compatIdeInfo')
    p.add_argument('--ide-version', help='the IDE version that was asked for, for example 2026.3')
    p.add_argument('--zip', required=True)
    p.add_argument('--sdk', required=True)
    p.add_argument('--project', required=True, choices=sorted(PROJECTS))
    p.add_argument('--source', required=True)
    p.add_argument('--expected', required=True, help='scripts/smoke/expected-<project>.txt')
    p.add_argument('--inputs-id', required=True, help='the content of corpus-id of inputs.sh')
    p.add_argument('--out', required=True)
    p.add_argument('--result', required=True)
    p.add_argument('--timeout', type=int, default=600)
    p.add_argument('--heap', default='3g')
    p.set_defaults(func=cmd_inspect)
    p = sub.add_parser('expectations')
    p.add_argument('work')
    p.add_argument('--project', required=True, choices=sorted(PROJECTS))
    p.add_argument('--file', required=True)
    p.add_argument('--inputs-id', required=True)
    p.set_defaults(func=cmd_expectations)
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
