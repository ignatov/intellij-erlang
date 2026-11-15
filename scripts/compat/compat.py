#!/usr/bin/env python3
"""Builds the results of the compatibility check (.github/workflows/compat.yml).

Each job of the workflow runs its checks with run-check.sh, which leaves NAME.log and NAME.exit in the logs directory,
and then calls this script, which reads those files, the JUnit XML results, the Plugin Verifier reports and the corpus
report, and writes one JSON result and a Markdown summary. The JSON is read by programs, so its shape is stable; see
SCHEMA_VERSION and the PR that added this script for the schema.

Every check has a status: passed, failed or not-run. A check that ran and found problems is failed and lists its
findings; a check that started and did not finish (its step timed out) is failed with a reason. The result's status is
failed if a check failed, incomplete if none failed but one did not run, and passed otherwise. When the job could not
get as far as its checks (an IDE that cannot be resolved, an Erlang/OTP that cannot be installed), no JSON is written
and the script exits with 2, so that a run without the result artifact means an infrastructure failure. A Gradle
failure once the IDE is resolved is a finding of the check that failed.

Subcommands:
  inputs --job JOB        validate the workflow inputs (and the pins of gradle.yml) before anything uses them, resolve
                          `ref` to a commit of this repository and write them to $GITHUB_OUTPUT
  pins GRADLE_YML         print OTP_VERSION, REBAR3_VERSION and REBAR3_SHA256 pinned in gradle.yml as NAME=value lines
  release-tag             print the tag of the published plugin, read from `gh release list --json tagName` on stdin
  ide-result ...          the result of the ide job
  otp-result ...          the result of the otp job
  conclusion RESULT_JSON  exit with 1 if a check did not run, or if one failed and the event is not pull_request

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

PASSED = 'passed'
FAILED = 'failed'
NOT_RUN = 'not-run'
INCOMPLETE = 'incomplete'
TRIMMED = '... [trimmed]'

# The documented forms of the inputs; nothing else reaches a command or an output file.
INPUT_PATTERNS = {
    'ide': re.compile(r'(20\d\d\.\d+(\.\d+)?|\d{3}\.\d+(\.\d+)?|latest|(\d{3}|\d{3}\.\d+(\.\d+)?|LATEST)-EAP-SNAPSHOT)'),
    'otp': re.compile(r'\d+(\.\d+)*(-rc\d+)?'),
    'ref': re.compile(r'(?!-)(?!refs/(?!heads/|tags/))(?!.*\.\.)(?!.*//)(?!.*/$)(?!.*\.lock$)[A-Za-z0-9._/-]{1,200}'),
}
INPUT_FORMS = {
    'ide': 'a release version (2026.3, 2026.3.1), a build number (263.7307.44), latest, 263-EAP-SNAPSHOT, '
           '<build>-EAP-SNAPSHOT or LATEST-EAP-SNAPSHOT',
    'otp': 'an Erlang/OTP version like 27.3.4.9, 29.1 or 30.0-rc1',
    'ref': 'a branch, a tag or a commit SHA of this repository',
}
PIN_PATTERNS = {
    'OTP_VERSION': INPUT_PATTERNS['otp'],
    'REBAR3_VERSION': re.compile(r'\d+\.\d+(\.\d+)*'),
    'REBAR3_SHA256': re.compile(r'[0-9a-f]{64}'),
}
SHA = re.compile(r'[0-9a-f]{7,40}')

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
    """A failed check if run-check.sh started check `name` and wrote no exit code (its step timed out), else None."""
    if read_exit(logs, name) is not None or not os.path.exists(os.path.join(logs, name + '.started')):
        return None
    return {'status': FAILED, 'exitCode': None,
            'reason': 'the check started but did not finish: its step timed out or was killed; see logTail',
            'logTail': log_tail(read_file(os.path.join(logs, name + '.log')))}


def not_run(logs, name, extra=None):
    """The check `name` that has no exit code: failed if it did not finish, otherwise not-run."""
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


def infrastructure_hint(log):
    """The first line of a failed Gradle run that suggests a download or environment failure, or None."""
    for pattern in INFRASTRUCTURE_PATTERNS:
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


def gradle_failure(check, log):
    """Fills a failed check whose Gradle run failed without a recognized finding. It is a finding of this check only."""
    check['status'] = FAILED
    check['reason'] = 'Gradle failed without a recognized finding; see logTail'
    hint = infrastructure_hint(log)
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
    if fail_on_skipped and totals['tests'] == 0 and exit_code == 0:
        check['status'] = FAILED
        check['reason'] = 'no test ran'
    if exit_code != 0 and not failed:
        gradle_failure(check, log)
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
    if exit_code == 0:
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
        check = {'status': FAILED, 'exitCode': exit_code, 'reason': 'ErlangCorpusParsingTest wrote no report'}
        if exit_code != 0:
            gradle_failure(check, log)
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
    if not report.get('files'):
        check['status'] = FAILED
        check['reason'] = 'the corpus has no .erl or .hrl file, so nothing was parsed'
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


def resolve_ref(ref, repository, default_branch, api=github_api):
    """The commit SHA of `ref`, which must be a branch or a tag of `repository` or a commit of its default branch.

    Code from anywhere else (a pull request, a fork) is refused: the job runs the Gradle build of that commit."""
    ref = ref or default_branch
    quote = lambda value: urllib.parse.quote(value, safe='/')
    if SHA.fullmatch(ref):
        comparison = api('repos/%s/compare/%s...%s' % (repository, quote(default_branch), ref))
        if comparison and comparison.get('status') in ('behind', 'identical'):
            return comparison['merge_base_commit']['sha']
    kinds = ['heads', 'tags']
    for prefix in ('refs/heads/', 'refs/tags/'):
        if ref.startswith(prefix):
            kinds, ref = [prefix.split('/')[1]], ref[len(prefix):]
    for kind in kinds:
        commit = api('repos/%s/commits/%s' % (repository, quote('%s/%s' % (kind, ref))))
        if isinstance(commit, dict) and commit.get('sha'):
            return commit['sha']
    raise InputError('the input ref=%r is not a branch or a tag of %s, nor a commit of its default branch %s'
                     % (ref, repository, default_branch))


def check_inputs(job, event, raw, pr_head_sha=None, repository=None, default_branch=None, gradle_yml=None,
                 api=github_api):
    """The validated inputs of `job` as step outputs: ide, otp, ref, the commit to check and, for otp, the pins.

    Every value is checked against its documented form before it is used, so no input can add a line to an output."""
    values = {name: validate_input(name, raw.get(name)) for name in ('ide', 'otp', 'ref')}
    if event == 'pull_request':
        if not SHA.fullmatch(pr_head_sha or ''):
            raise InputError('the pull request head %r is not a commit SHA' % pr_head_sha)
        values['commit'] = pr_head_sha
    else:
        values['commit'] = resolve_ref(values['ref'], repository, default_branch, api)
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
    pattern = re.compile(r'v%s\.(\d+)' % re.escape(version))
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


def overall(checks):
    """failed if a check failed, incomplete if none failed but one did not run, passed otherwise."""
    statuses = [check['status'] for check in checks.values()]
    return FAILED if FAILED in statuses else INCOMPLETE if NOT_RUN in statuses else PASSED


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
            checks[name]['reason'] = ('the compile check did not finish' if checks['compile']['exitCode'] is None
                                      else 'the plugin does not compile against this IDE')
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
    compilation = tests_check(args.logs, 'compilation', [
        os.path.join(args.root, 'build/test-results/testCompilation'),
        os.path.join(args.root, 'jps-plugin/build/test-results/testCompilation')], fail_on_skipped=True)
    rebar3_exit = read_exit(args.logs, 'rebar3')
    if rebar3_exit:
        # The pinned rebar3 does not run on this Erlang/OTP: the rebar tests cannot pass, and that is a finding.
        first_line = (read_file(os.path.join(args.logs, 'rebar3.log')) or '').strip().split('\n')[0]
        compilation['status'] = FAILED
        compilation['reason'] = text('rebar3 %s does not run on Erlang/OTP %s: `escript rebar3 version` exited with %d: %s'
                                     % (args.rebar3_version, otp['version'], rebar3_exit, first_line))
    checks = {
        'compilationTests': compilation,
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


def conclusion(path, event):
    """The exit code of the job. A check that did not run, or no result, is red on every event. Findings are red on
    workflow_dispatch only: on pull_request the job tests the workflow, which works when every check gave a result."""
    content = read_file(path)
    if content is None:
        print('::error::There is no result %s' % path)
        return 1
    result = json.loads(content)
    not_run = [name for name, check in result['checks'].items() if check['status'] == NOT_RUN]
    failed = [name for name, check in result['checks'].items() if check['status'] == FAILED]
    if not_run:
        print('::error::The %s compatibility check did not run: %s' % (result['job'], ', '.join(
            '%s (%s)' % (name, result['checks'][name].get('reason')) for name in not_run)))
        return 1
    if failed and event == 'pull_request':
        print('::notice::The %s compatibility check found problems in: %s. Every check ran and gave a result, so the '
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

    inputs = commands.add_parser('inputs', help='reads INPUT_IDE, INPUT_OTP and INPUT_REF from the environment')
    inputs.add_argument('--job', required=True, choices=['ide', 'otp'])
    inputs.add_argument('--event', default=os.environ.get('GITHUB_EVENT_NAME'))
    inputs.add_argument('--gradle-yml', help='the gradle.yml whose pins the otp job uses')
    inputs.add_argument('--output', default=os.environ.get('GITHUB_OUTPUT'))

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
    conclude.add_argument('--event', default=os.environ.get('GITHUB_EVENT_NAME'))

    args = parser.parse_args(argv)
    try:
        if args.command == 'inputs':
            raw = {name: os.environ.get('INPUT_' + name.upper(), '') for name in ('ide', 'otp', 'ref')}
            values = check_inputs(args.job, args.event, raw, os.environ.get('PR_HEAD_SHA'),
                                  os.environ.get('GITHUB_REPOSITORY'), os.environ.get('DEFAULT_BRANCH'), args.gradle_yml)
            write_outputs(values, args.output)
            print('\n'.join('%s=%s' % item for item in values.items()))
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
