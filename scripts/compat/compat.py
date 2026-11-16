#!/usr/bin/env python3
"""Builds the results of the compatibility check (.github/workflows/compat.yml).

Each job of the workflow runs its checks with run-check.sh, which leaves NAME.log and NAME.exit in the logs directory,
and then calls this script, which reads those files, the JUnit XML results, the Plugin Verifier reports and the corpus
report, and writes one JSON result and a Markdown summary. The JSON is read by programs, so its shape is stable; see
SCHEMA_VERSION and the PR that added this script for the schema.

Every check has a status: passed, failed or not-run. A check that ran and found problems is failed and lists its
findings. A check that did not work is not-run with a reason, also when its step ran: no test ran, the Gradle task or
option does not exist, the check wrote no report or an unreadable one, its result is missing, its step started and left
no exit code (it timed out or was cancelled), or the checked commit predates the compatibility check (PREDATES). A
check that was not run because a check it needs found problems has blockedBy, the name of that check. The result's
status is incomplete if a check is not-run without a failed blockedBy, otherwise failed if a check failed, and passed
otherwise. When the job could not get as far as its checks (an IDE that cannot be resolved, an Erlang/OTP that cannot
be installed), no JSON is written and the script exits with 2, so that a run without the result artifact means an
infrastructure failure. Any other Gradle failure once the IDE is resolved is a finding of the check that failed.

Subcommands:
  inputs --job JOB --event EVENT
                          validate the workflow inputs (and the pins of gradle.yml) before anything uses them, resolve
                          `ref` to a commit of this repository and write them to $GITHUB_OUTPUT
  tree --job JOB --root DIR [--ide IDE]
                          before the checks run, write current=true to $GITHUB_OUTPUT if the checked-out commit has
                          what the checks of JOB need, current=false if it predates the compatibility check
  pins GRADLE_YML         print OTP_VERSION, REBAR3_VERSION and REBAR3_SHA256 pinned in gradle.yml as NAME=value lines
  release-tag             print the tag of the published plugin, read from `gh release list --json tagName` on stdin
  ide-result ...          the result of the ide job
  otp-result ...          the result of the otp job
  conclusion RESULT_JSON --event EVENT
                          exit with 1 if the result is incomplete, or if it failed and the event is not pull_request

Python 3 standard library only.
"""

import argparse
import glob
import json
import os
import re
import sys
import urllib.error
import urllib.parse
import urllib.request
import xml.etree.ElementTree as ElementTree

SCHEMA_VERSION = 1
LIST_LIMIT = 100
TEXT_LIMIT = 2000
LOG_TAIL_LINES = 40

EVENTS = ['pull_request', 'workflow_dispatch']
PASSED = 'passed'
FAILED = 'failed'
NOT_RUN = 'not-run'
INCOMPLETE = 'incomplete'
TRIMMED = '... [trimmed]'
TIMED_OUT = 'timed out or was cancelled'
PREDATES = 'the checked commit predates the compatibility check'

# The documented forms of the inputs; nothing else reaches a command or an output file. ASCII digits only: \d would
# also match other Unicode digits.
INPUT_PATTERNS = {
    'ide': re.compile(r'(20[0-9][0-9]\.[0-9]+(\.[0-9]+)?|[0-9]{3}\.[0-9]+(\.[0-9]+)?|latest'
                      r'|([0-9]{3}|[0-9]{3}\.[0-9]+(\.[0-9]+)?|LATEST)-EAP-SNAPSHOT)'),
    'otp': re.compile(r'[0-9]+(\.[0-9]+)*(-rc[0-9]+)?'),
    'ref': re.compile(r'v[0-9]+(\.[0-9]+)*|[0-9a-f]{40}'),
}
INPUT_FORMS = {
    'ide': 'a release version (2026.3, 2026.3.1), a build number (263.7307.44), latest, 263-EAP-SNAPSHOT, '
           '<build>-EAP-SNAPSHOT or LATEST-EAP-SNAPSHOT',
    'otp': 'an Erlang/OTP version like 27.3.4.9, 29.1 or 30.0-rc1',
    'ref': 'a published release tag v<digits and dots> of this repository or the full SHA of a commit of its default '
           'branch',
}
PIN_PATTERNS = {
    'OTP_VERSION': INPUT_PATTERNS['otp'],
    'REBAR3_VERSION': re.compile(r'[0-9]+\.[0-9]+(\.[0-9]+)*'),
    'REBAR3_SHA256': re.compile(r'[0-9a-f]{64}'),
}
SHA = re.compile(r'[0-9a-f]{40}')

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

# Gradle output that suggests a download or environment failure. Once the IDE is resolved, such a failure is still a
# finding of the check that failed; the matching line is only named in its reason.
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

# Gradle output of a check that could not run at all in the checked-out build: the check is not-run, not a finding.
BROKEN_PATTERNS = [
    re.compile(p) for p in (
        r"Task '[^']+' not found in (root )?project",
        r'Cannot locate tasks? that match',
        r"Unknown command-line option '[^']+'",
        r'No tests found for given includes',
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
    """`value` as a string of at most `limit` characters; a longer one keeps its start and ends with TRIMMED."""
    if value is None:
        return None
    value = str(value)
    return value if len(value) <= limit else value[:max(limit - len(TRIMMED), 0)] + TRIMMED


def read_file(path):
    try:
        with open(path, encoding='utf-8', errors='replace') as f:
            return f.read()
    except FileNotFoundError:
        return None


def read_exit(logs, name):
    """The exit code that run-check.sh wrote for check `name`, or None if the check did not run or did not finish."""
    content = read_file(os.path.join(logs, name + '.exit'))
    return int(content.strip()) if content and content.strip() else None


def unfinished(logs, name):
    """A check that did not work if run-check.sh started check `name` and wrote no exit code, else None."""
    if read_exit(logs, name) is not None or not os.path.exists(os.path.join(logs, name + '.started')):
        return None
    return {'status': NOT_RUN, 'exitCode': None, 'reason': TIMED_OUT,
            'logTail': log_tail(read_file(os.path.join(logs, name + '.log')))}


def not_run(logs, name, extra=None):
    """The check `name` that has no exit code: not-run, with the reason TIMED_OUT if it started."""
    check = unfinished(logs, name)
    if check is None:
        check = {'status': NOT_RUN}
        check.update(extra or {})
        check.setdefault('reason', 'not run')
        return check
    check.update({k: v for k, v in (extra or {}).items() if k not in check})
    return check


def log_tail(log, lines=LOG_TAIL_LINES):
    """The last `lines` lines of `log`, at most TEXT_LIMIT characters; a longer tail keeps its end."""
    if not log:
        return None
    tail = '\n'.join(log.rstrip('\n').split('\n')[-lines:])
    return tail if len(tail) <= TEXT_LIMIT else TRIMMED + tail[len(tail) - TEXT_LIMIT + len(TRIMMED):]


def matching_line(patterns, log):
    """The first line of `log` that matches one of `patterns`, or None."""
    for pattern in patterns:
        match = pattern.search(log or '')
        if match:
            return (log[:match.start()].rsplit('\n', 1)[-1] + log[match.start():].split('\n', 1)[0]).strip()
    return None


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


def broken(check, reason, log):
    """Makes `check` not-run: it did not work, so it gives no answer, even though its step ran."""
    check['status'] = NOT_RUN
    check['reason'] = text(reason)
    check['logTail'] = log_tail(log)
    return check


def gradle_failure(check, log):
    """Fills a check whose Gradle run failed without a recognized finding: not-run if the check could not run in this
    build (BROKEN_PATTERNS), otherwise failed, a finding of this check only."""
    cause = matching_line(BROKEN_PATTERNS, log)
    if cause:
        return broken(check, 'the check could not run: ' + cause, log)
    check['status'] = FAILED
    check['reason'] = 'Gradle failed without a recognized finding; see logTail'
    hint = matching_line(INFRASTRUCTURE_PATTERNS, log)
    if hint:
        check['reason'] += '; the log shows a possible download or environment failure: ' + hint
    check['reason'] = text(check['reason'])
    check['logTail'] = log_tail(log)
    return check


def compile_check(logs, root):
    exit_code = read_exit(logs, 'compile')
    if exit_code is None:
        return not_run(logs, 'compile')
    log = read_file(os.path.join(logs, 'compile.log')) or ''
    errors = parse_javac_errors(log, root)
    check = {'status': PASSED if exit_code == 0 else FAILED, 'exitCode': exit_code, 'errors': trimmed(errors)}
    if exit_code != 0 and not errors:
        gradle_failure(check, log)
    return check


def tests_check(logs, name, result_dirs, fail_on_skipped=False):
    exit_code = read_exit(logs, name)
    if exit_code is None:
        return not_run(logs, name)
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
    if exit_code != 0 and not failed:
        gradle_failure(check, log)
    elif totals['tests'] == 0:
        broken(check, 'no test ran', log)
    return check


def verifier_check(logs, name, reports_dir, extra=None):
    exit_code = read_exit(logs, name)
    if exit_code is None:
        return not_run(logs, name, extra)
    check = {'status': NOT_RUN}
    check.update(extra or {})
    log = read_file(os.path.join(logs, name + '.log')) or ''
    reports = parse_verifier_reports(reports_dir)
    check['exitCode'] = exit_code
    check['failureLevels'] = VERIFIER_FAILURE_LEVELS
    check['reports'] = reports
    if exit_code == 0 and not reports:
        broken(check, 'the Plugin Verifier wrote no report', log)
    elif exit_code == 0:
        check['status'] = PASSED
    elif any(report['failingLevels'] for report in reports):
        check['status'] = FAILED
    else:
        gradle_failure(check, log)
    return check


def corpus_check(logs, report_path):
    exit_code = read_exit(logs, 'corpus')
    if exit_code is None:
        return not_run(logs, 'corpus')
    log = read_file(os.path.join(logs, 'corpus.log')) or ''
    content = read_file(report_path)
    if content is None:
        cause = matching_line(BROKEN_PATTERNS, log)
        return broken({'status': NOT_RUN, 'exitCode': exit_code}, 'ErlangCorpusParsingTest wrote no report'
                      + ('; the check could not run: ' + cause if cause else '; see logTail'), log)
    try:
        report = json.loads(content)
    except ValueError as e:
        return broken({'status': NOT_RUN, 'exitCode': exit_code}, 'the corpus report is not valid JSON: %s' % e, log)
    if not isinstance(report, dict):
        return broken({'status': NOT_RUN, 'exitCode': exit_code}, 'the corpus report is not a JSON object', log)
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
    if not report.get('files'):
        broken(check, 'the corpus has no .erl or .hrl file, so nothing was parsed', log)
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


IDE_CHECKS = ['compile', 'test', 'verifyPlugin', 'releasedPlugin']
CORPUS_TEST = 'tests/org/intellij/erlang/parser/ErlangCorpusParsingTest.java'


def predating_checks(root, job, ide=''):
    """The checks of `job` that the commit checked out in `root` cannot run because it predates the compatibility
    check: for ide, all of them when an IDE is requested and build.gradle does not read the property platformVersion;
    for otp, corpus when there is no ErlangCorpusParsingTest."""
    if job == 'ide':
        build = read_file(os.path.join(root, 'build.gradle')) or ''
        return IDE_CHECKS if ide and not re.search(r'\bplatformVersion\b', build) else []
    return [] if os.path.isfile(os.path.join(root, CORPUS_TEST)) else ['corpus']


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


def resolve_ide(requested, info_path, predates=False):
    """The IDE that the build resolved. Another IDE than the requested one is an infrastructure failure, unless the
    checked commit predates the compatibility check and so ignores -PplatformVersion (`predates`)."""
    content = read_file(info_path)
    if content is None:
        raise InfrastructureError('the IDE was not resolved: %s is missing' % info_path)
    info = json.loads(content)
    matches = ide_matches(requested, info)
    if matches is False and not predates:
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


class InputError(Exception):
    """An input of the workflow is not one of its documented forms; the job fails at once."""


def validate_input(name, value):
    """`value` if it is empty or a documented form of input `name`; raises InputError otherwise."""
    value = value or ''
    if value and not INPUT_PATTERNS[name].fullmatch(value):
        raise InputError('the input %s=%r is not %s' % (name, value, INPUT_FORMS[name]))
    return value


def validate_pins(pins):
    for name, value in pins.items():
        if not PIN_PATTERNS[name].fullmatch(value):
            raise InputError('gradle.yml pins %s=%r, which is not a valid value' % (name, value))
    return pins


def github_api(path):
    """GET `path` of the GitHub REST API, with $GH_TOKEN if set: the JSON, or None for 404 and 422 (no such ref)."""
    url = '%s/%s' % (os.environ.get('GITHUB_API_URL', 'https://api.github.com'), path)
    headers = {'Accept': 'application/vnd.github+json'}
    if os.environ.get('GH_TOKEN'):
        headers['Authorization'] = 'Bearer ' + os.environ['GH_TOKEN']
    request = urllib.request.Request(url, headers=headers)
    try:
        with urllib.request.urlopen(request, timeout=60) as response:
            return json.load(response)
    except urllib.error.HTTPError as e:
        if e.code in (404, 422):
            return None
        raise InfrastructureError('GitHub API %s: HTTP %d' % (path, e.code))
    except (urllib.error.URLError, OSError) as e:
        raise InfrastructureError('GitHub API %s: %s' % (path, e))


def tag_commit(repository, tag, api):
    """The commit of the tag `tag`, looked up as exactly refs/tags/<tag> (git/ref does not fall back to other ref
    namespaces, as commits/<ref> does) and dereferenced through annotated tags; None if there is no such tag."""
    ref = api('repos/%s/git/ref/tags/%s' % (repository, urllib.parse.quote(tag, safe='')))
    if not isinstance(ref, dict) or ref.get('ref') != 'refs/tags/' + tag:
        return None
    target = ref.get('object') or {}
    for _ in range(5):
        if target.get('type') == 'commit':
            return target.get('sha')
        if target.get('type') != 'tag' or not SHA.fullmatch(target.get('sha') or ''):
            return None
        target = (api('repos/%s/git/tags/%s' % (repository, target['sha'])) or {}).get('object') or {}
    return None


def resolve_ref(ref, repository, default_branch, api=github_api):
    """The commit SHA of `ref`: empty for the head of the default branch, the tag v<digits and dots> of a published
    release of `repository`, or the full SHA of the head of the default branch or of an ancestor of it.

    Anything else (a branch, a pull request, a fork, a tag without a release) is refused: the job runs the Gradle build
    of that commit."""
    if not default_branch:
        raise InputError('the default branch of %s is not known' % repository)
    quote = lambda value: urllib.parse.quote(value, safe='/')
    commit = None
    if not ref:
        commit = api('repos/%s/commits/%s' % (repository, quote('heads/' + default_branch)))
    elif SHA.fullmatch(ref):
        comparison = api('repos/%s/compare/%s...%s' % (repository, quote(default_branch), ref))
        if comparison and comparison.get('status') in ('behind', 'identical'):
            return ref
    else:
        release = api('repos/%s/releases/tags/%s' % (repository, urllib.parse.quote(ref, safe='')))
        if isinstance(release, dict) and release.get('tag_name') == ref and not release.get('draft'):
            commit = {'sha': tag_commit(repository, ref, api)}
    if isinstance(commit, dict) and SHA.fullmatch(commit.get('sha') or ''):
        return commit['sha']
    raise InputError('the input ref=%r is not the tag of a published release of %s, nor a commit of its default '
                     'branch %s' % (ref, repository, default_branch))


def check_inputs(job, event, raw, workflow_sha=None, repository=None, default_branch=None, gradle_yml=None,
                 api=github_api):
    """The validated inputs of `job` as step outputs: ide, otp, ref, the commit to check and, for otp, the pins.

    Every value is checked against its documented form before it is used, so no input can add a line to an output.
    On pull_request the commit is `workflow_sha`, the merge commit of the event that the scripts come from, so that
    the scripts and the plugin come from one tree."""
    values = {name: validate_input(name, raw.get(name)) for name in ('ide', 'otp', 'ref')}
    if event == 'pull_request':
        if not SHA.fullmatch(workflow_sha or ''):
            raise InputError('the commit of the workflow %r is not a commit SHA' % workflow_sha)
        values['commit'] = workflow_sha
    elif event == 'workflow_dispatch':
        values['commit'] = resolve_ref(values['ref'], repository, default_branch, api)
    else:
        raise InputError('the event %r is neither pull_request nor workflow_dispatch' % event)
    if job == 'otp':
        pins = validate_pins(read_pins(gradle_yml))
        values.update(pins)
        values['otp'] = values['otp'] or pins['OTP_VERSION']
    return values


def write_outputs(values, path):
    with open(path, 'a', encoding='utf-8') as f:
        for name, value in values.items():
            if '\n' in value or '\r' in value:
                raise InputError('the output %s has a line break' % name)
            f.write('%s=%s\n' % (name, value))


def release_tag(releases, version):
    """The tag of the published plugin: the release v<version>.<N> with the largest N, not GitHub's latest release."""
    pattern = re.compile(r'v%s\.([0-9]+)' % re.escape(version))
    builds = [(int(match.group(1)), release['tagName']) for release in releases
              if not release.get('isDraft') and (match := pattern.fullmatch(release.get('tagName') or ''))]
    if not builds:
        raise InputError('no release of version %s (a tag v%s.<N>)' % (version, version))
    return max(builds)[1]


def gradle_property(path, name):
    match = re.search(r'^\s*%s\s*=\s*(\S+)\s*$' % re.escape(name), read_file(path) or '', re.MULTILINE)
    if not match:
        raise InputError('%s has no %s' % (path, name))
    return match.group(1)


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


def incomplete_checks(checks):
    """The checks that gave no answer: not-run, unless the check named by their blockedBy failed with findings."""
    return [name for name, check in checks.items() if check['status'] == NOT_RUN
            and (checks.get(check.get('blockedBy')) or {}).get('status') != FAILED]


def overall(checks):
    """incomplete if a check gave no answer, otherwise failed if a check failed, and passed otherwise."""
    if incomplete_checks(checks):
        return INCOMPLETE
    return FAILED if any(check['status'] == FAILED for check in checks.values()) else PASSED


# Results


def ide_result(args):
    released_meta = json.loads(read_file(os.path.join(args.logs, 'released.json')) or '{}')
    released_extra = {'release': text(released_meta.get('tag')), 'asset': text(released_meta.get('asset'))}
    if released_meta.get('error'):
        released_extra['reason'] = text(released_meta['error'])
    checks = {
        'compile': compile_check(args.logs, args.root),
        'test': tests_check(args.logs, 'test', [os.path.join(args.root, 'build/test-results/test'),
                                                os.path.join(args.root, 'jps-plugin/build/test-results/test')]),
        'verifyPlugin': verifier_check(args.logs, 'verify', os.path.join(args.root, 'build/reports/pluginVerifier')),
        'releasedPlugin': verifier_check(args.logs, 'released', os.path.join(args.root, 'build/compat/released-verifier'),
                                         released_extra),
    }
    for name in ('test', 'verifyPlugin'):
        if checks[name]['status'] != NOT_RUN or 'exitCode' in checks[name]:
            continue
        if checks['compile']['status'] == FAILED:
            checks[name]['reason'] = 'the plugin does not compile against this IDE'
            checks[name]['blockedBy'] = 'compile'
        elif checks['compile'].get('reason') == TIMED_OUT:
            checks[name]['reason'] = 'the compile check did not finish'
            checks[name]['blockedBy'] = 'compile'
    predates = predating_checks(args.root, 'ide', args.ide)
    checks.update({name: {'status': NOT_RUN, 'reason': PREDATES} for name in predates})
    return {
        'schemaVersion': SCHEMA_VERSION,
        'job': 'ide',
        'status': overall(checks),
        'limits': {'listItems': LIST_LIMIT, 'textChars': TEXT_LIMIT},
        'run': run_info(),
        'inputs': {'ide': args.ide or '', 'ref': args.ref or ''},
        'resolved': {'commit': args.commit, 'ide': resolve_ide(args.ide, args.ide_info, bool(predates))},
        'checks': checks,
    }


def otp_result(args):
    otp = resolve_otp(args.otp_root)
    otp['requested'] = args.otp_version
    otp['matchesRequest'] = otp['version'] == args.otp_version
    if otp['release'] != re.split(r'[.-]', args.otp_version)[0]:
        raise InfrastructureError('installed Erlang/OTP %s, not the requested %s' % (otp['version'], args.otp_version))
    compilation = tests_check(args.logs, 'compilation', [
        os.path.join(args.root, 'build/test-results/testCompilation'),
        os.path.join(args.root, 'jps-plugin/build/test-results/testCompilation')], fail_on_skipped=True)
    rebar3_exit = read_exit(args.logs, 'rebar3')
    if rebar3_exit:
        # The pinned rebar3 does not run on this Erlang/OTP: the rebar tests cannot pass, and that is a finding. A
        # check that did not work stays not-run, with the rebar3 failure next to its reason.
        first_line = (read_file(os.path.join(args.logs, 'rebar3.log')) or '').strip().split('\n')[0]
        rebar3_failure = ('rebar3 %s does not run on Erlang/OTP %s: `escript rebar3 version` exited with %d: %s'
                          % (args.rebar3_version, otp['version'], rebar3_exit, first_line))
        if compilation['status'] == NOT_RUN:
            compilation['reason'] = text('%s; also, %s' % (compilation.get('reason'), rebar3_failure))
        else:
            compilation['status'] = FAILED
            compilation['reason'] = text(rebar3_failure)
    checks = {
        'compilationTests': compilation,
        'corpus': corpus_check(args.logs, args.corpus_report),
        'knownRelease': known_release_check(args.root, otp['release']),
    }
    checks.update({name: {'status': NOT_RUN, 'reason': PREDATES} for name in predating_checks(args.root, 'otp')})
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


def items(check, key):
    """The items of the list `key` of `check`; none if the check has no such list (it did not run or finish)."""
    return (check.get(key) or {}).get('items', [])


def check_facts(name, check):
    """One line about `check`. Works for every check a result can have: any key but status may be missing."""
    if check['status'] == NOT_RUN:
        return check.get('reason', '')
    facts = []
    if name == 'compile' and 'errors' in check:
        facts.append('%d compiler errors' % check['errors']['total'])
    if 'tests' in check:
        facts.append('%d tests, %d failed, %d skipped' % (check['tests'], check['failures'] + check['errors'],
                                                          check['skipped']))
    if name == 'releasedPlugin' and check.get('release'):
        facts.append('%s (%s)' % (check['release'], check.get('asset')))
    if 'reports' in check:
        facts.append('; '.join('%s: %s' % (r['ide'], r['verdict']) for r in check['reports']))
    if name == 'corpus' and 'files' in check:
        facts.append('%s files, %s with errors, %s error elements, %d exceptions, %d slow files' % (
            check['files'], check.get('filesWithErrors'), check.get('errorElements'),
            (check.get('exceptions') or {}).get('total', 0), (check.get('slowFiles') or {}).get('total', 0)))
    if name == 'knownRelease':
        facts.append('OTP %s: ErlangSdkRelease %s, %s' % (check['release'], check['sdkReleaseConstant'] or 'missing',
                                                         check['mockSdk'] or 'no mock SDK'))
    if check.get('reason'):
        facts.append(check['reason'])
    return '; '.join(fact for fact in facts if fact)


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
        if check['status'] == PASSED:
            continue
        lines_of_check = []
        if name == 'compile':
            lines_of_check = ['%s:%s: %s' % (e['file'], e['line'], e['message']) for e in items(check, 'errors')]
        elif name == 'corpus':
            lines_of_check = ['%s:%s:%s: %s' % (e['file'], e['line'], e['column'], e['description'])
                              for e in items(check, 'errors')]
            lines_of_check += ['%s: %s' % (e['file'], e['exception']) for e in items(check, 'exceptions')]
            lines_of_check += ['%s: %s ms' % (e['file'], e['millis']) for e in items(check, 'slowFiles')]
        else:
            lines_of_check = ['%s.%s: %s' % (t['class'], t['name'], t['message']) for t in items(check, 'failedTests')]
            lines_of_check += ['skipped: %s.%s' % (t['class'], t['name']) for t in items(check, 'skippedTests')]
            for report in check.get('reports', []):
                for category, problems in report['problems'].items():
                    lines_of_check += ['%s: %s' % (category, p) for p in problems['items']]
        if check.get('logTail'):
            lines_of_check.append(check['logTail'])
        if lines_of_check:
            details += ['', '<details><summary>%s</summary>' % name, '', '```']
            details += [text(line, 500) for line in lines_of_check[:30]]
            details += ['```', '</details>']
    return '\n'.join(lines + details) + '\n'


def conclusion(path, event):
    """The exit code of the job. An incomplete result (a check that did not run or did not work), or no result, is red
    on every event. Findings are red on workflow_dispatch only: on pull_request the job tests the workflow, which works
    when every check ran and gave an answer."""
    content = read_file(path)
    if content is None:
        print('::error::There is no result %s' % path)
        return 1
    result = json.loads(content)
    checks = result['checks']
    incomplete = incomplete_checks(checks)
    failed = [name for name, check in checks.items() if check['status'] == FAILED]
    if incomplete:
        print('::error::The %s compatibility check is incomplete; these checks did not run or did not work: %s' % (
            result['job'], ', '.join('%s (%s)' % (name, checks[name].get('reason')) for name in incomplete)))
        return 1
    if failed and event == 'pull_request':
        print('::notice::The %s compatibility check found problems in: %s. Every check ran and gave an answer, so the '
              'job passes on pull_request.' % (result['job'], ', '.join(failed)))
        return 0
    if failed:
        print('::error::The %s compatibility check found problems in: %s' % (result['job'], ', '.join(failed)))
        return 1
    print('The %s compatibility check passed' % result['job'])
    return 0


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

    inputs = commands.add_parser('inputs', help='reads INPUT_IDE, INPUT_OTP, INPUT_REF, WORKFLOW_SHA, GITHUB_REPOSITORY '
                                                'and DEFAULT_BRANCH from the environment')
    inputs.add_argument('--job', required=True, choices=['ide', 'otp'])
    inputs.add_argument('--event', required=True, choices=EVENTS)
    inputs.add_argument('--gradle-yml', help='the gradle.yml whose pins the otp job uses')
    inputs.add_argument('--output', default=os.environ.get('GITHUB_OUTPUT'))

    tree = commands.add_parser('tree')
    tree.add_argument('--job', required=True, choices=['ide', 'otp'])
    tree.add_argument('--root', required=True, help='the checked-out repository')
    tree.add_argument('--ide', default='', help='the checked ide input')
    tree.add_argument('--output', default=os.environ.get('GITHUB_OUTPUT'))

    pins = commands.add_parser('pins')
    pins.add_argument('gradle_yml')

    release = commands.add_parser('release-tag')
    release.add_argument('--gradle-properties', required=True, help='the gradle.properties whose version is released')

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

    conclude = commands.add_parser('conclusion')
    conclude.add_argument('result')
    conclude.add_argument('--event', required=True, choices=EVENTS)

    args = parser.parse_args(argv)
    try:
        if args.command == 'inputs':
            raw = {name: os.environ.get('INPUT_' + name.upper(), '') for name in ('ide', 'otp', 'ref')}
            values = check_inputs(args.job, args.event, raw, os.environ.get('WORKFLOW_SHA'),
                                  os.environ.get('GITHUB_REPOSITORY'), os.environ.get('DEFAULT_BRANCH'), args.gradle_yml)
            write_outputs(values, args.output)
            print('\n'.join('%s=%s' % item for item in values.items()))
        elif args.command == 'tree':
            predates = predating_checks(args.root, args.job, args.ide)
            if predates:
                print('::warning::The checked commit predates the compatibility check; these checks do not run: %s'
                      % ', '.join(predates))
            write_outputs({'current': 'false' if predates else 'true'}, args.output)
        elif args.command == 'pins':
            for name, value in read_pins(args.gradle_yml).items():
                print('%s=%s' % (name, value))
        elif args.command == 'release-tag':
            print(release_tag(json.load(sys.stdin), gradle_property(args.gradle_properties, 'version')))
        elif args.command in ('ide-result', 'otp-result'):
            result = ide_result(args) if args.command == 'ide-result' else otp_result(args)
            write_result(result, args.out, args.summary)
        else:
            return conclusion(args.result, args.event)
    except InputError as e:
        print('::error::%s' % e)
        return 1
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
