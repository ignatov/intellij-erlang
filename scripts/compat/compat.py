#!/usr/bin/env python3
"""Builds the results of the compatibility check (.github/workflows/compat.yml).

Each job of the workflow runs its checks with run-check.sh, which leaves NAME.log and NAME.exit in the logs directory,
and then calls this script, which reads those files, the JUnit XML results, the Plugin Verifier reports and the corpus
report, and writes one JSON result and a Markdown summary. The JSON is read by programs, so its shape is stable; see
SCHEMA_VERSION and the PR that added this script for the schema.

Every check has a status: passed, failed or not-run. A check that ran and found problems is failed and lists its
findings. When the check itself could not be run because of the infrastructure (an IDE that cannot be resolved, a
download that fails), no JSON is written and the script exits with 2, so that a run without the result artifact means
an infrastructure failure.

Subcommands:
  pins GRADLE_YML         print OTP_VERSION, REBAR3_VERSION and REBAR3_SHA256 pinned in gradle.yml as NAME=value lines
  ide-result ...          the result of the ide job
  otp-result ...          the result of the otp job
  conclusion RESULT_JSON  exit with 1 if the result's status is failed

Python 3 standard library only.
"""

import argparse
import glob
import json
import os
import re
import sys
import xml.etree.ElementTree as ElementTree

SCHEMA_VERSION = 1
LIST_LIMIT = 100
TEXT_LIMIT = 2000
LOG_TAIL_LINES = 40

PASSED = 'passed'
FAILED = 'failed'
NOT_RUN = 'not-run'

# The failure levels of verifyPlugin in build.gradle, and the phrases of verification-verdict.txt that the IntelliJ
# Platform Gradle Plugin 2.20.1 (VerifyPluginTask.metadataOf) maps to them.
VERIFIER_FAILURE_LEVELS = ['COMPATIBILITY_PROBLEMS', 'MISSING_DEPENDENCIES', 'INVALID_PLUGIN', 'OVERRIDE_ONLY_API_USAGES']
VERIFIER_VERDICT_MARKERS = {
    'COMPATIBILITY_WARNINGS': 'compatibility warning',
    'COMPATIBILITY_PROBLEMS': 'compatibility problem',
    'DEPRECATED_API_USAGES': 'of deprecated API',
    'SCHEDULED_FOR_REMOVAL_API_USAGES': 'of scheduled for removal API',
    'EXPERIMENTAL_API_USAGES': 'of experimental API',
    'INTERNAL_API_USAGES': 'of internal API',
    'OVERRIDE_ONLY_API_USAGES': 'override-only API usage',
    'NON_EXTENDABLE_API_USAGES': 'non-extendable API usage',
    'PLUGIN_STRUCTURE_WARNINGS': 'plugin configuration defect',
    'MISSING_DEPENDENCIES': 'missing mandatory',
    'INVALID_PLUGIN': 'Plugin is invalid:',
}
# Files next to verification-verdict.txt that are not lists of problems.
VERIFIER_NON_PROBLEM_FILES = {'verification-verdict.txt', 'dependencies.txt', 'telemetry.txt'}

# Gradle output that means a download failed, not that the plugin has a problem.
INFRASTRUCTURE_PATTERNS = [
    re.compile(p) for p in (
        r'Could not resolve all (files|dependencies|artifacts) for configuration',
        r'Could not (GET|HEAD|download) ',
        r'Could not resolve [\w.\-]+:[\w.\-]+:',
        r"Couldn't resolve \w+ download URL",
        r'Connection (reset|refused|timed out)',
        r'Read timed out',
        r'No space left on device',
    )
]

JAVAC_ERROR = re.compile(r'^(?P<file>\S+?\.java):(?P<line>\d+): error: (?P<message>.*)$')
JAVAC_GLOBAL_ERROR = re.compile(r'^error: (?P<message>.*)$')
JAVAC_DETAIL = re.compile(r'^\s+(symbol|location|required|found|reason)\s*:')


class InfrastructureError(Exception):
    """The check could not be run; the job writes no result."""


def trimmed(items, limit=LIST_LIMIT):
    """A list in the result: at most `limit` items, with the total and whether it was trimmed."""
    items = list(items)
    return {'total': len(items), 'trimmed': len(items) > limit, 'items': items[:limit]}


def text(value, limit=TEXT_LIMIT):
    if value is None:
        return None
    value = str(value)
    return value if len(value) <= limit else value[:limit] + '... [trimmed]'


def read_file(path):
    try:
        with open(path, encoding='utf-8', errors='replace') as f:
            return f.read()
    except FileNotFoundError:
        return None


def read_exit(logs, name):
    """The exit code that run-check.sh wrote for check `name`, or None if the check did not run."""
    content = read_file(os.path.join(logs, name + '.exit'))
    return int(content.strip()) if content and content.strip() else None


def log_tail(log, lines=LOG_TAIL_LINES):
    if not log:
        return None
    return text('\n'.join(log.rstrip('\n').split('\n')[-lines:]), TEXT_LIMIT * 2)


def check_infrastructure(name, log):
    """Raises InfrastructureError if the log of a failed Gradle run shows a download or environment failure."""
    for pattern in INFRASTRUCTURE_PATTERNS:
        match = pattern.search(log or '')
        if match:
            line = (log[:match.start()].rsplit('\n', 1)[-1] + log[match.start():].split('\n', 1)[0]).strip()
            raise InfrastructureError('%s: %s' % (name, line))


def relative(path, root):
    if root and path.startswith(root.rstrip('/') + '/'):
        return path[len(root.rstrip('/')) + 1:]
    return path


# Parsers


def parse_javac_errors(log, root=None):
    """The javac errors in a Gradle log: file (relative to root), line and message with symbol/location details."""
    errors = []
    current = None
    for line in (log or '').split('\n'):
        match = JAVAC_ERROR.match(line)
        if match:
            current = {'file': relative(match.group('file'), root), 'line': int(match.group('line')),
                       'message': match.group('message').strip()}
            errors.append(current)
            continue
        match = JAVAC_GLOBAL_ERROR.match(line)
        if match:
            current = {'file': None, 'line': None, 'message': match.group('message').strip()}
            errors.append(current)
            continue
        if current is not None and JAVAC_DETAIL.match(line):
            current['message'] += '; ' + line.strip()
        elif current is not None and not line.startswith(' '):
            current = None
    for error in errors:
        error['message'] = text(error['message'])
    return errors


def parse_junit_results(directories):
    """Totals and the failed and skipped test cases of the JUnit XML files in `directories`."""
    totals = {'files': 0, 'tests': 0, 'failures': 0, 'errors': 0, 'skipped': 0}
    failed, skipped = [], []
    for directory in directories:
        for path in sorted(glob.glob(os.path.join(directory, '*.xml'))):
            try:
                suite = ElementTree.parse(path).getroot()
            except ElementTree.ParseError as e:
                failed.append({'class': os.path.basename(path), 'name': None, 'kind': 'unreadable-result',
                               'message': text(str(e))})
                continue
            totals['files'] += 1
            for case in suite.iter('testcase'):
                totals['tests'] += 1
                name = {'class': case.get('classname'), 'name': case.get('name')}
                problem = case.find('failure')
                kind = 'failure'
                if problem is None:
                    problem = case.find('error')
                    kind = 'error'
                if problem is not None:
                    totals['failures' if kind == 'failure' else 'errors'] += 1
                    message = problem.get('message') or (problem.text or '').strip().split('\n', 1)[0]
                    failed.append(dict(name, kind=kind, message=text(message)))
                elif case.find('skipped') is not None:
                    totals['skipped'] += 1
                    skipped.append(name)
    return totals, failed, skipped


def verdict_levels(verdict):
    return [level for level, marker in VERIFIER_VERDICT_MARKERS.items() if marker in verdict]


def parse_verifier_reports(directory):
    """One entry per (IDE, plugin) verdict that the Plugin Verifier wrote under `directory`, with its problems."""
    reports = []
    pattern = os.path.join(directory, '*', 'plugins', '*', '*', 'verification-verdict.txt')
    for verdict_path in sorted(glob.glob(pattern)):
        plugin_dir = os.path.dirname(verdict_path)
        version_dir, plugin_version = os.path.split(plugin_dir)
        plugin_id = os.path.basename(version_dir)
        ide = os.path.basename(os.path.dirname(os.path.dirname(version_dir)))
        verdict = read_file(verdict_path).strip()
        levels = verdict_levels(verdict)
        problems = {}
        for problem_path in sorted(glob.glob(os.path.join(plugin_dir, '*.txt'))):
            name = os.path.basename(problem_path)
            if name in VERIFIER_NON_PROBLEM_FILES:
                continue
            lines = [text(line.strip()) for line in read_file(problem_path).split('\n') if line.strip()]
            problems[name[:-len('.txt')]] = trimmed(lines)
        reports.append({
            'ide': ide,
            'pluginId': plugin_id,
            'pluginVersion': plugin_version,
            'verdict': text(verdict),
            'levels': levels,
            'failingLevels': [level for level in levels if level in VERIFIER_FAILURE_LEVELS],
            'problems': problems,
        })
    return reports


# Checks. Each returns a dict with `status` first.


def gradle_failure(check, name, log):
    """Fills a failed check whose Gradle run failed without a recognized finding."""
    check_infrastructure(name, log)
    check['status'] = FAILED
    check['reason'] = 'Gradle failed without a recognized finding; see logTail'
    check['logTail'] = log_tail(log)
    return check


def compile_check(logs, root):
    exit_code = read_exit(logs, 'compile')
    if exit_code is None:
        return {'status': NOT_RUN, 'reason': 'not run'}
    log = read_file(os.path.join(logs, 'compile.log')) or ''
    errors = parse_javac_errors(log, root)
    check = {'status': PASSED if exit_code == 0 else FAILED, 'exitCode': exit_code, 'errors': trimmed(errors)}
    if exit_code != 0 and not errors:
        gradle_failure(check, 'compile', log)
    return check


def tests_check(logs, name, result_dirs, fail_on_skipped=False):
    exit_code = read_exit(logs, name)
    if exit_code is None:
        return {'status': NOT_RUN, 'reason': 'not run'}
    log = read_file(os.path.join(logs, name + '.log')) or ''
    totals, failed, skipped = parse_junit_results(result_dirs)
    check = {'status': PASSED, 'exitCode': exit_code}
    check.update(totals)
    check['failedTests'] = trimmed(failed)
    check['skippedTests'] = trimmed(skipped)
    if failed:
        check['status'] = FAILED
    if fail_on_skipped and skipped:
        check['status'] = FAILED
        check['reason'] = 'tests were skipped'
    if fail_on_skipped and totals['tests'] == 0 and exit_code == 0:
        check['status'] = FAILED
        check['reason'] = 'no test ran'
    if exit_code != 0 and not failed:
        gradle_failure(check, name, log)
    return check


def verifier_check(logs, name, reports_dir, extra=None):
    exit_code = read_exit(logs, name)
    check = {'status': NOT_RUN}
    check.update(extra or {})
    if exit_code is None:
        check.setdefault('reason', 'not run')
        return check
    log = read_file(os.path.join(logs, name + '.log')) or ''
    reports = parse_verifier_reports(reports_dir)
    check['exitCode'] = exit_code
    check['failureLevels'] = VERIFIER_FAILURE_LEVELS
    check['reports'] = reports
    if exit_code == 0:
        check['status'] = PASSED
    elif any(report['failingLevels'] for report in reports):
        check['status'] = FAILED
    else:
        gradle_failure(check, name, log)
    return check


def corpus_check(logs, report_path):
    exit_code = read_exit(logs, 'corpus')
    if exit_code is None:
        return {'status': NOT_RUN, 'reason': 'not run'}
    log = read_file(os.path.join(logs, 'corpus.log')) or ''
    content = read_file(report_path)
    if content is None:
        check = {'status': FAILED, 'exitCode': exit_code, 'reason': 'ErlangCorpusParsingTest wrote no report'}
        if exit_code != 0:
            gradle_failure(check, 'corpus', log)
            check['reason'] = 'ErlangCorpusParsingTest wrote no report; see logTail'
        return check
    report = json.loads(content)
    errors = [dict(e, description=text(e.get('description'))) for e in report.get('errors', [])]
    exceptions = [dict(e, exception=text(e.get('exception'))) for e in report.get('exceptions', [])]
    slow_files = report.get('slowFiles', [])
    check = {
        'status': PASSED,
        'exitCode': exit_code,
        'corpusDir': report.get('corpusDir'),
        'sdkRelease': report.get('sdkRelease'),
        'files': report.get('files'),
        'filesWithErrors': report.get('filesWithErrors'),
        'errorElements': report.get('errorElements'),
        'slowFileMillis': report.get('slowFileMillis'),
        'errors': trimmed(errors),
        'exceptions': trimmed(exceptions),
        'slowFiles': trimmed(slow_files),
    }
    if errors or exceptions or slow_files:
        check['status'] = FAILED
    if exit_code != 0:
        check['status'] = FAILED
        check['reason'] = 'the test task failed after the report was written; see logTail'
        check['logTail'] = log_tail(log)
    return check


SDK_RELEASE_CONSTANT = re.compile(r'public\s+static\s+final\s+ErlangSdkRelease\s+(\w+)\s*=\s*new\s+ErlangSdkRelease\("([^"]+)"')


def known_release_check(root, otp_release):
    """Whether the plugin knows the major release: an ErlangSdkRelease constant and a testData/mockSdk-R<NN>."""
    source = read_file(os.path.join(root, 'src/org/intellij/erlang/sdk/ErlangSdkRelease.java')) or ''
    constants = {release: name for name, release in SDK_RELEASE_CONSTANT.findall(source)}
    mock_sdk = 'testData/mockSdk-R%s' % otp_release
    has_mock_sdk = os.path.isdir(os.path.join(root, mock_sdk))
    known = sorted(constants, key=lambda r: (1, int(r), '') if r.isdigit() else (0, 0, r))
    return {
        'status': PASSED if otp_release in constants and has_mock_sdk else FAILED,
        'release': otp_release,
        'sdkReleaseConstant': constants.get(otp_release),
        'mockSdk': mock_sdk if has_mock_sdk else None,
        'knownReleases': known,
    }


# Inputs


def ide_matches(requested, info):
    """Whether the IDE the build resolved is the one requested (None if nothing was requested)."""
    if not requested:
        return None
    build = re.sub(r'^[A-Z]+-', '', info.get('buildNumber') or '')
    version = info.get('version') or ''
    wanted = re.sub(r'^[A-Z]+-(?=\d)', '', requested)
    snapshot = re.match(r'^(.*?)(-EAP)?(-CANDIDATE)?-SNAPSHOT$', wanted)
    if snapshot:
        prefix = snapshot.group(1)
        return prefix == 'LATEST' or build == prefix or build.startswith(prefix + '.')
    if wanted == 'latest':
        return True
    if re.match(r'^\d{3}(\.|$)', wanted):
        return build == wanted
    return version == wanted


def resolve_ide(requested, info_path):
    content = read_file(info_path)
    if content is None:
        raise InfrastructureError('the IDE was not resolved: %s is missing' % info_path)
    info = json.loads(content)
    matches = ide_matches(requested, info)
    if matches is False:
        raise InfrastructureError('the build resolved %s %s (%s), not the requested %s; does the ref read -PplatformVersion?'
                                  % (info.get('name'), info.get('version'), info.get('buildNumber'), requested))
    return {
        'name': info.get('name'),
        'productCode': info.get('productCode'),
        'version': info.get('version'),
        'versionSuffix': info.get('versionSuffix'),
        'buildNumber': info.get('buildNumber'),
        'build': '%s-%s' % (info.get('productCode'), info.get('buildNumber')),
        'matchesRequest': matches,
    }


def resolve_otp(otp_root):
    """The release (major) and full version of the Erlang/OTP installed in `otp_root`."""
    versions = sorted(glob.glob(os.path.join(otp_root, 'releases', '*', 'OTP_VERSION')))
    if not versions:
        raise InfrastructureError('no releases/*/OTP_VERSION in %s' % otp_root)
    path = versions[-1]
    return {
        'release': os.path.basename(os.path.dirname(path)),
        'version': read_file(path).strip(),
        'root': otp_root,
    }


def read_pins(gradle_yml):
    content = read_file(gradle_yml)
    if content is None:
        raise InfrastructureError('%s is missing' % gradle_yml)
    pins = {}
    for name in ('OTP_VERSION', 'REBAR3_VERSION', 'REBAR3_SHA256'):
        match = re.search(r'^\s*%s:\s*[\'"]?([^\'"\s]+)[\'"]?\s*$' % name, content, re.MULTILINE)
        if not match:
            raise InfrastructureError('%s pins no %s' % (gradle_yml, name))
        pins[name] = match.group(1)
    return pins


def run_info():
    server = os.environ.get('GITHUB_SERVER_URL', 'https://github.com')
    repository = os.environ.get('GITHUB_REPOSITORY')
    run_id = os.environ.get('GITHUB_RUN_ID')
    return {
        'repository': repository,
        'event': os.environ.get('GITHUB_EVENT_NAME'),
        'runId': int(run_id) if run_id else None,
        'runAttempt': int(os.environ['GITHUB_RUN_ATTEMPT']) if os.environ.get('GITHUB_RUN_ATTEMPT') else None,
        'url': '%s/%s/actions/runs/%s' % (server, repository, run_id) if repository and run_id else None,
        'workflowSha': os.environ.get('GITHUB_SHA'),
    }


def overall(checks):
    return FAILED if any(check['status'] == FAILED for check in checks.values()) else PASSED


# Results


def ide_result(args):
    released_meta = json.loads(read_file(os.path.join(args.logs, 'released.json')) or '{}')
    released_extra = {'release': released_meta.get('tag'), 'asset': released_meta.get('asset')}
    if released_meta.get('error'):
        released_extra['reason'] = released_meta['error']
    checks = {
        'compile': compile_check(args.logs, args.root),
        'test': tests_check(args.logs, 'test', [os.path.join(args.root, 'build/test-results/test'),
                                                os.path.join(args.root, 'jps-plugin/build/test-results/test')]),
        'verifyPlugin': verifier_check(args.logs, 'verify', os.path.join(args.root, 'build/reports/pluginVerifier')),
        'releasedPlugin': verifier_check(args.logs, 'released', os.path.join(args.root, 'build/compat/released-verifier'),
                                         released_extra),
    }
    for name in ('test', 'verifyPlugin'):
        if checks[name]['status'] == NOT_RUN and checks['compile']['status'] == FAILED:
            checks[name]['reason'] = 'the plugin does not compile against this IDE'
    return {
        'schemaVersion': SCHEMA_VERSION,
        'job': 'ide',
        'status': overall(checks),
        'limits': {'listItems': LIST_LIMIT, 'textChars': TEXT_LIMIT},
        'run': run_info(),
        'inputs': {'ide': args.ide or '', 'ref': args.ref or ''},
        'resolved': {'commit': args.commit, 'ide': resolve_ide(args.ide, args.ide_info)},
        'checks': checks,
    }


def otp_result(args):
    otp = resolve_otp(args.otp_root)
    otp['requested'] = args.otp_version
    otp['matchesRequest'] = otp['version'] == args.otp_version
    if otp['release'] != re.split(r'[.-]', args.otp_version)[0]:
        raise InfrastructureError('installed Erlang/OTP %s, not the requested %s' % (otp['version'], args.otp_version))
    checks = {
        'compilationTests': tests_check(args.logs, 'compilation', [
            os.path.join(args.root, 'build/test-results/testCompilation'),
            os.path.join(args.root, 'jps-plugin/build/test-results/testCompilation')], fail_on_skipped=True),
        'corpus': corpus_check(args.logs, args.corpus_report),
        'knownRelease': known_release_check(args.root, otp['release']),
    }
    return {
        'schemaVersion': SCHEMA_VERSION,
        'job': 'otp',
        'status': overall(checks),
        'limits': {'listItems': LIST_LIMIT, 'textChars': TEXT_LIMIT},
        'run': run_info(),
        'inputs': {'otp': args.otp or '', 'ref': args.ref or ''},
        'resolved': {'commit': args.commit, 'otp': otp, 'rebar3': args.rebar3_version},
        'checks': checks,
    }


# Summary


def check_facts(name, check):
    if check['status'] == NOT_RUN:
        return check.get('reason', '')
    if 'errors' in check and name == 'compile':
        return '%d compiler errors' % check['errors']['total']
    if 'tests' in check:
        facts = '%d tests, %d failed, %d skipped' % (check['tests'], check['failures'] + check['errors'], check['skipped'])
    elif 'reports' in check:
        verdicts = ['%s: %s' % (r['ide'], r['verdict']) for r in check['reports']]
        facts = '; '.join(verdicts)
        if check.get('release'):
            facts = '%s (%s): %s' % (check['release'], check.get('asset'), facts)
    elif 'files' in check:
        facts = '%s files, %s with errors, %s error elements, %d exceptions, %d slow files' % (
            check['files'], check['filesWithErrors'], check['errorElements'], check['exceptions']['total'],
            check['slowFiles']['total'])
    elif 'release' in check:
        facts = 'OTP %s: ErlangSdkRelease %s, %s' % (check['release'], check['sdkReleaseConstant'] or 'missing',
                                                    check['mockSdk'] or 'no mock SDK')
    else:
        facts = ''
    if check.get('reason'):
        facts = (facts + '; ' if facts else '') + check['reason']
    return facts


def markdown_cell(value):
    return str(value).replace('|', '\\|').replace('\n', ' ')


def summary(result):
    lines = ['## Compatibility check: %s, %s' % (result['job'], result['status']), '']
    resolved = result['resolved']
    if 'ide' in resolved:
        ide = resolved['ide']
        lines.append('IDE: %s %s%s (%s), requested `%s`' % (
            ide['name'], ide['version'], ' ' + ide['versionSuffix'] if ide.get('versionSuffix') else '', ide['build'],
            result['inputs']['ide'] or 'default'))
    if 'otp' in resolved:
        otp = resolved['otp']
        lines.append('Erlang/OTP %s (%s), requested `%s`' % (otp['release'], otp['version'], result['inputs']['otp'] or 'default'))
    lines += ['', 'Commit: `%s`' % resolved['commit'], '', '| Check | Status | Facts |', '|---|---|---|']
    for name, check in result['checks'].items():
        lines.append('| %s | %s | %s |' % (name, check['status'], markdown_cell(check_facts(name, check))))
    details = []
    for name, check in result['checks'].items():
        if check['status'] != FAILED:
            continue
        items = []
        if name == 'compile':
            items = ['%s:%s: %s' % (e['file'], e['line'], e['message']) for e in check['errors']['items']]
        elif 'failedTests' in check:
            items = ['%s.%s: %s' % (t['class'], t['name'], t['message']) for t in check['failedTests']['items']]
            items += ['skipped: %s.%s' % (t['class'], t['name']) for t in check['skippedTests']['items']]
        elif 'reports' in check:
            for report in check['reports']:
                for category, problems in report['problems'].items():
                    items += ['%s: %s' % (category, p) for p in problems['items']]
        elif name == 'corpus':
            items = ['%s:%s:%s: %s' % (e['file'], e['line'], e['column'], e['description']) for e in check['errors']['items']]
            items += ['%s: %s' % (e['file'], e['exception']) for e in check['exceptions']['items']]
            items += ['%s: %d ms' % (e['file'], e['millis']) for e in check['slowFiles']['items']]
        if check.get('logTail'):
            items.append(check['logTail'])
        if items:
            details += ['', '<details><summary>%s</summary>' % name, '', '```']
            details += [text(item, 500) for item in items[:30]]
            details += ['```', '</details>']
    return '\n'.join(lines + details) + '\n'


def write_result(result, out, summary_path):
    with open(out, 'w', encoding='utf-8') as f:
        json.dump(result, f, indent=2)
        f.write('\n')
    markdown = summary(result)
    if summary_path:
        with open(summary_path, 'a', encoding='utf-8') as f:
            f.write(markdown)
    print(markdown)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split('\n', 1)[0])
    commands = parser.add_subparsers(dest='command', required=True)

    pins = commands.add_parser('pins')
    pins.add_argument('gradle_yml')

    for name in ('ide-result', 'otp-result'):
        command = commands.add_parser(name)
        command.add_argument('--root', required=True, help='the checked-out repository')
        command.add_argument('--logs', required=True, help='the directory of run-check.sh')
        command.add_argument('--commit', required=True)
        command.add_argument('--ref', default='')
        command.add_argument('--out', required=True)
        command.add_argument('--summary', default=os.environ.get('GITHUB_STEP_SUMMARY'))
        if name == 'ide-result':
            command.add_argument('--ide', default='', help='the ide input')
            command.add_argument('--ide-info', required=True, help='build/compat/ide.json of compatIdeInfo')
        else:
            command.add_argument('--otp', default='', help='the otp input')
            command.add_argument('--otp-version', required=True, help='the version that was installed')
            command.add_argument('--otp-root', required=True)
            command.add_argument('--rebar3-version', default=None)
            command.add_argument('--corpus-report', required=True)

    conclusion = commands.add_parser('conclusion')
    conclusion.add_argument('result')

    args = parser.parse_args(argv)
    try:
        if args.command == 'pins':
            for name, value in read_pins(args.gradle_yml).items():
                print('%s=%s' % (name, value))
        elif args.command in ('ide-result', 'otp-result'):
            result = ide_result(args) if args.command == 'ide-result' else otp_result(args)
            write_result(result, args.out, args.summary)
        else:
            result = json.loads(read_file(args.result))
            failed = [name for name, check in result['checks'].items() if check['status'] == FAILED]
            if result['status'] == FAILED:
                print('::error::The %s compatibility check found problems in: %s' % (result['job'], ', '.join(failed)))
                return 1
            print('The %s compatibility check passed' % result['job'])
    except InfrastructureError as e:
        print('::error::Infrastructure failure, no result: %s' % e)
        summary_path = getattr(args, 'summary', None)
        if summary_path:
            with open(summary_path, 'a', encoding='utf-8') as f:
                f.write('## Compatibility check: infrastructure failure\n\n%s\n' % e)
        return 2
    return 0


if __name__ == '__main__':
    sys.exit(main())
