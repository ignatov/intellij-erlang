#!/usr/bin/env python3
"""Files one GitHub issue per test that fails on CI without a reason in the code.

The workflow .github/workflows/flaky-tests.yml runs this script twice a day. It reads the runs of Java CI
(gradle.yml) of the last --days days and the logs of their failed jobs. Gradle prints one line
"Class > testMethod FAILED" per failed test, followed by the indented exception.

A failed test counts in two cases:
  rerun   the job failed in one attempt of a run and the job of the same name passed in another attempt of that run.
          All attempts of a run test the same commit, so the code did not change between them.
  master  the job failed in the last attempt of a run for a push to master and passed in none. Pull requests are
          tested before the merge, so either the test is not stable or two merges broke master together; both need
          a person or the factory to look.
A failure of a pull request run that was never rerun does not count: the pull request may have broken the test.

Rules:
  - One issue per test, with the labels flaky-test and Bug. The body starts with two hidden markers: the test
    (Class.method) and the state, which lists the failures counted so far. The body shows them newest first and the
    exception of the newest one.
  - A new failure of a test with an open issue: the body is written first, then one comment. A run that stops
    between the two loses that comment, but the body has the failure and no later run posts it twice.
  - A closed issue is reopened only when it was closed as completed and the job with the new failure started after
    the close: a pull request run tests the merge with master, so that job had the fix. An issue closed as not
    planned or as a duplicate is never reopened.
  - Skipped and counted: a job whose log cannot be read, a job with no failed test in its log (a build or network
    failure), a job with more than MAX_FAILED_PER_JOB failed tests (a broken build, not single tests), and a test
    class that is not in tests/ or jps-plugin/tests/ of this checkout (a test that only a pull request has).
  - At most --max-new new issues and --max-updates other changes per run; the rest waits for the next run. A second
    run right after the first changes nothing.
  - Text from logs is text from outside: a pull request can print anything. Class, method and job names must match
    strict patterns. The exception is shown in a code block, with at most EXCERPT_LINES lines of at most
    EXCERPT_WIDTH characters, without backticks and control characters.

Modes:
  --dry-run  print what a run would do and the body of the first new issue; write nothing.
  --run      write the issues.

GitHub: the REST API with GITHUB_TOKEN (actions: read, issues: write). The link to a job log is a redirect to a
storage link; it is followed without the token. Python 3 standard library only.
"""

import argparse
import datetime
import json
import os
import re
import sys
import urllib.error
import urllib.parse
import urllib.request

GITHUB = 'https://api.github.com'
REPOSITORY = 'ignatov/intellij-erlang'
WORKFLOW = 'gradle.yml'
USER_AGENT = 'intellij-erlang-flaky-tests'
TIMEOUT = 30
REDIRECTS = (301, 302, 303, 307, 308)

LABEL = 'flaky-test'
LABEL_COLOR = 'fbca04'
LABEL_DESCRIPTION = 'A test that fails on CI without a reason in the code, filed by flaky-tests.yml'
BUG_LABEL = 'Bug'
TITLE_PREFIX = 'Flaky test: '

DEFAULT_DAYS = 3
MAX_DAYS = 14
DEFAULT_MAX_NEW = 5
DEFAULT_MAX_UPDATES = 10
MAX_ATTEMPTS = 10
MAX_FAILED_PER_JOB = 10
MAX_RUN_PAGES = 10
STATE_LIMIT = 50
SHOWN_LIMIT = 20
EXCERPT_LINES = 15
EXCERPT_WIDTH = 200
LOG_LIMIT = 50 * 1024 * 1024

RERUN = 'rerun'
MASTER = 'master'
KIND_TEXT = {
    RERUN: 'the same job passed in another attempt of this run, on the same commit',
    MASTER: 'on a push to master',
}

NAME = r'[A-Za-z_][A-Za-z0-9_]{0,150}'
FAILED_LINE = re.compile(r'(%s) > (%s)(?:\(\))? FAILED' % (NAME, NAME))
JOB_NAME = re.compile(r'[A-Za-z0-9._-]{1,60}')
TIME = re.compile(r'[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z')
LOG_TIMESTAMP = re.compile(r'^\ufeff?[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:.]+Z ?')
KEY_MARKER = re.compile(r'<!-- flaky-test: (%s\.%s) -->' % (NAME, NAME))
STATE_MARKER = re.compile(r'<!-- flaky-test-state: (\{[^<>]*\}) -->')
TEST_ROOTS = ('tests', os.path.join('jps-plugin', 'tests'))


class Failure(Exception):
    """An error that ends the run with a message."""


# ---------------------------------------------------------------- GitHub


class _NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


class Http:
    """Sends one request; redirects are not followed and error codes are returned."""

    def __init__(self):
        self.opener = urllib.request.build_opener(_NoRedirect)

    def request(self, method, url, headers, data=None):
        request = urllib.request.Request(url, data=data, method=method, headers=headers)
        try:
            with self.opener.open(request, timeout=TIMEOUT) as response:
                return response.status, dict(response.headers.items()), response.read(LOG_LIMIT)
        except urllib.error.HTTPError as error:
            return error.code, dict(error.headers.items()) if error.headers else {}, error.read() or b''
        except (urllib.error.URLError, OSError) as error:
            reason = getattr(error, 'reason', error)
            raise Failure('%s %s failed: %s' % (method, urllib.parse.urlsplit(url).netloc, type(reason).__name__))


class GitHub:
    def __init__(self, token, http=None, repository=REPOSITORY):
        if not token:
            raise Failure('GITHUB_TOKEN is not set')
        self.http = http or Http()
        self.base = '%s/repos/%s' % (GITHUB, repository)
        self.headers = {'User-Agent': USER_AGENT, 'Authorization': 'Bearer ' + token,
                        'Accept': 'application/vnd.github+json', 'X-GitHub-Api-Version': '2022-11-28'}

    def _call(self, method, path, payload=None, accepted=(200,)):
        headers = dict(self.headers)
        data = None
        if payload is not None:
            data = json.dumps(payload).encode('utf-8')
            headers['Content-Type'] = 'application/json'
        status, _, body = self.http.request(method, self.base + path, headers, data)
        if status not in accepted:
            raise Failure('%s %s: HTTP %d' % (method, path.split('?')[0], status))
        try:
            return json.loads(body.decode('utf-8')) if body else None
        except (UnicodeDecodeError, ValueError):
            raise Failure('%s %s: the answer is not JSON' % (method, path.split('?')[0]))

    def runs(self, since):
        """The runs of Java CI created at or after since, and whether GitHub cut the list (it shows 1000 at most)."""
        found = []
        query = urllib.parse.quote('>=' + since)
        for page in range(1, MAX_RUN_PAGES + 1):
            data = self._call('GET', '/actions/workflows/%s/runs?created=%s&per_page=100&page=%d'
                              % (WORKFLOW, query, page))
            runs = data.get('workflow_runs') or []
            found.extend(runs)
            if len(runs) < 100:
                return found, False
        return found, True

    def jobs(self, run, attempt):
        return self._call('GET', '/actions/runs/%d/attempts/%d/jobs?per_page=100' % (run, attempt)).get('jobs') or []

    def log(self, job):
        """The log of a job, or None when it cannot be read (expired, a failed redirect)."""
        try:
            status, headers, body = self.http.request('GET', '%s/actions/jobs/%d/logs' % (self.base, job), self.headers)
            hops = 0
            while status in REDIRECTS and hops < 3:
                hops += 1
                location = {name.lower(): value for name, value in headers.items()}.get('location')
                if not location or urllib.parse.urlsplit(location).scheme != 'https':
                    return None
                status, headers, body = self.http.request('GET', location, {'User-Agent': USER_AGENT})
        except Failure:
            return None
        return body.decode('utf-8', errors='replace') if status == 200 else None

    def issues(self):
        found = []
        for page in range(1, 11):
            data = self._call('GET', '/issues?labels=%s&state=all&per_page=100&page=%d' % (LABEL, page))
            found.extend(item for item in data if 'pull_request' not in item)
            if len(data) < 100:
                break
        return found

    def ensure_label(self):
        self._call('POST', '/labels', {'name': LABEL, 'color': LABEL_COLOR, 'description': LABEL_DESCRIPTION},
                   accepted=(201, 422))

    def create(self, title, body):
        return self._call('POST', '/issues', {'title': title, 'body': body, 'labels': [LABEL, BUG_LABEL]},
                          accepted=(201,))['number']

    def update(self, number, fields):
        self._call('PATCH', '/issues/%d' % number, fields)

    def comment(self, number, body):
        self._call('POST', '/issues/%d/comments' % number, {'body': body}, accepted=(201,))


# ---------------------------------------------------------------- reading the runs


class Observation:
    """One failure of one test in one job of one attempt of a run."""

    def __init__(self, test, run, attempt, job, job_name, kind, at, excerpt=()):
        self.test = test
        self.run = run
        self.attempt = attempt
        self.job = job
        self.job_name = job_name
        self.kind = kind
        self.at = at
        self.excerpt = list(excerpt)

    def stored(self):
        return [self.run, self.attempt, self.job, self.job_name, self.kind, self.at]

    @staticmethod
    def of(test, item):
        """An observation from the state of an issue, or None when the entry is not one."""
        if not isinstance(item, list) or len(item) != 6:
            return None
        run, attempt, job, job_name, kind, at = item
        if not all(isinstance(value, int) and not isinstance(value, bool) and value > 0 for value in (run, attempt, job)):
            return None
        if not isinstance(job_name, str) or not JOB_NAME.fullmatch(job_name) or kind not in KIND_TEXT:
            return None
        if not isinstance(at, str) or not TIME.fullmatch(at):
            return None
        return Observation(test, run, attempt, job, job_name, kind, at)


def clean_line(line):
    line = ''.join(char if char.isprintable() else ' ' for char in line.replace('\t', '    '))
    return line.replace('`', "'").rstrip()[:EXCERPT_WIDTH]


def parse_failures(log):
    """The failed tests of a job log, in the order of the log: [(Class.method, the lines of the exception)]."""
    lines = [LOG_TIMESTAMP.sub('', line) for line in log.splitlines()]
    found = {}
    for index, line in enumerate(lines):
        match = FAILED_LINE.fullmatch(line.rstrip())
        if not match:
            continue
        test = '%s.%s' % match.groups()
        if test in found:
            continue
        excerpt = []
        for following in lines[index + 1:]:
            if not following.startswith('    ') or len(excerpt) == EXCERPT_LINES:
                break
            excerpt.append(clean_line(following[4:]))
        found[test] = excerpt
    return list(found.items())


def is_master_push(run):
    return run.get('event') == 'push' and run.get('head_branch') == 'master'


def is_candidate(run):
    """Whether the jobs of the run are worth reading: it was rerun, or it is a failed push to master."""
    if run.get('status') != 'completed' or not isinstance(run.get('id'), int):
        return False
    attempts = run.get('run_attempt')
    if not isinstance(attempts, int) or not 1 <= attempts <= MAX_ATTEMPTS:
        return False
    return attempts > 1 or is_master_push(run) and run.get('conclusion') == 'failure'


def observe(github, run, counts):
    """The failures of one run that count."""
    attempts = run['run_attempt']
    jobs = {attempt: github.jobs(run['id'], attempt) for attempt in range(1, attempts + 1)}
    passed = {job.get('name') for listed in jobs.values() for job in listed if job.get('conclusion') == 'success'}
    found = []
    for attempt, listed in jobs.items():
        for job in listed:
            name, at = job.get('name'), job.get('started_at')
            if job.get('conclusion') != 'failure' or not isinstance(job.get('id'), int):
                continue
            if not isinstance(name, str) or not JOB_NAME.fullmatch(name) or not isinstance(at, str) or not TIME.fullmatch(at):
                continue
            if name in passed:
                kind = RERUN
            elif is_master_push(run) and attempt == attempts:
                kind = MASTER
            else:
                continue
            log = github.log(job['id'])
            if log is None:
                counts['logs not read'] += 1
                continue
            failures = parse_failures(log)
            if not failures:
                counts['failed jobs without a failed test'] += 1
            elif len(failures) > MAX_FAILED_PER_JOB:
                counts['failed jobs with too many failed tests'] += 1
            else:
                found.extend(Observation(test, run['id'], attempt, job['id'], name, kind, at, excerpt)
                             for test, excerpt in failures)
    return found


def test_files(root):
    """The test classes of the checkout: {simple class name: its path from the root}."""
    found = {}
    for top in TEST_ROOTS:
        for directory, _, names in sorted(os.walk(os.path.join(root, top))):
            for name in sorted(names):
                if name.endswith('.java'):
                    path = os.path.relpath(os.path.join(directory, name), root).replace(os.sep, '/')
                    found.setdefault(name[:-len('.java')], path)
    return found


# ---------------------------------------------------------------- issues


def read_state(body):
    """The failures that an issue body records, oldest first."""
    key = KEY_MARKER.search(body or '')
    state = STATE_MARKER.search(body or '')
    if not key or not state:
        return []
    try:
        data = json.loads(state.group(1))
    except ValueError:
        return []
    items = data.get('seen') if isinstance(data, dict) else None
    observations = [Observation.of(key.group(1), item) for item in items] if isinstance(items, list) else []
    return [observation for observation in observations if observation]


def listed(observation):
    url = 'https://github.com/%s/actions/runs/%d/job/%d' % (REPOSITORY, observation.run, observation.job)
    return '- %s, job [%s](%s), attempt %d: %s' % (observation.at.replace('T', ' ').replace('Z', ' UTC'),
                                                   observation.job_name, url, observation.attempt,
                                                   KIND_TEXT[observation.kind])


def render_body(test, path, observations, excerpt):
    """The body of the issue of a test. observations: oldest first; the newest STATE_LIMIT are kept."""
    kept = observations[-STATE_LIMIT:]
    state = json.dumps({'seen': [observation.stored() for observation in kept]}, separators=(',', ':'))
    newest_first = kept[::-1]
    lines = [
        '<!-- flaky-test: %s -->' % test,
        '<!-- flaky-test-state: %s -->' % state,
        '`%s` in `%s` fails on CI without a reason in the code.' % (test, path),
        '',
        'Failures counted (%d, newest first):' % len(kept),
    ]
    lines += [listed(observation) for observation in newest_first[:SHOWN_LIMIT]]
    if len(newest_first) > SHOWN_LIMIT:
        lines.append('- and %d older' % (len(newest_first) - SHOWN_LIMIT))
    if excerpt:
        lines += ['', 'The newest failure, from the job log:', '', '```text'] + excerpt + ['```']
    lines += [
        '',
        'What counts: a test that failed in a job that passed in another attempt of the same run, or that failed on '
        'a push to master. The cause can be in the test or in the plugin: look for it (a race, state left by another '
        'test, time, the order of the tests), and do not disable the test or rerun it until it passes.',
        '',
        'Filed by `.github/workflows/flaky-tests.yml` (`scripts/flaky/flaky.py`), which adds later failures here.',
    ]
    return '\n'.join(lines) + '\n'


def render_comment(new, reopened):
    lines = ['Reopened: the test failed again after this issue was closed.' if reopened
             else 'The test failed again on CI.', '']
    lines += [listed(observation) for observation in new[::-1]]
    return '\n'.join(lines) + '\n'


class Action:
    def __init__(self, kind, test, path, observations, new, number=None):
        self.kind = kind  # 'new', 'update' or 'reopen'
        self.test = test
        self.path = path
        self.observations = observations
        self.new = new
        self.number = number

    @property
    def title(self):
        return TITLE_PREFIX + self.test

    def body(self):
        return render_body(self.test, self.path, self.observations, self.new[-1].excerpt)

    def describe(self):
        what = 'new issue' if self.kind == 'new' else '%s #%d' % (self.kind, self.number)
        return '%s: %s, %d new failure(s)' % (what, self.test, len(self.new))


def plan(observations, issues, files, max_new, max_updates, counts):
    """The changes to make, the tests with the most new failures first."""
    known = {}
    for issue in issues:
        key = KEY_MARKER.search(issue.get('body') or '')
        if not key or not isinstance(issue.get('number'), int):
            continue
        other = known.get(key.group(1))
        # the open issue of a test wins over a closed one; among equals, the newest
        rank = (issue.get('state') == 'open', issue['number'])
        if not other or rank > (other.get('state') == 'open', other['number']):
            known[key.group(1)] = issue
    by_test = {}
    for observation in sorted(observations, key=lambda item: (item.at, item.job)):
        by_test.setdefault(observation.test, []).append(observation)
    actions = []
    for test, found in by_test.items():
        path = files.get(test.split('.')[0])
        if not path:
            counts['failures of tests that are not in this checkout'] += len(found)
            continue
        issue = known.get(test)
        if not issue:
            actions.append(Action('new', test, path, found, found))
            continue
        before = read_state(issue.get('body'))
        seen = {observation.job for observation in before}
        new = [observation for observation in found if observation.job not in seen]
        kind = 'update'
        if issue.get('state') != 'open':
            closed_at = issue.get('closed_at') or ''
            new = [observation for observation in new if observation.at > closed_at]
            if new and issue.get('state_reason') != 'completed':
                counts['failures of tests whose issue is closed as not planned'] += len(new)
                continue
            kind = 'reopen'
        if new:
            actions.append(Action(kind, test, path, before + new, new, issue['number']))
    actions.sort(key=lambda action: (-len(action.new), action.test))
    chosen = [action for action in actions if action.kind == 'new'][:max_new]
    chosen += [action for action in actions if action.kind != 'new'][:max_updates]
    counts['changes left for the next run'] += len(actions) - len(chosen)
    return chosen


def execute(github, actions, out):
    if any(action.kind == 'new' for action in actions):
        github.ensure_label()
    for action in actions:
        if action.kind == 'new':
            number = github.create(action.title, action.body())
        else:
            number = action.number
            fields = {'body': action.body()}
            if action.kind == 'reopen':
                fields.update(state='open', state_reason='reopened')
            github.update(number, fields)
            github.comment(number, render_comment(action.new, action.kind == 'reopen'))
        out.write('#%d: %s\n' % (number, action.describe()))


# ---------------------------------------------------------------- main

COUNTS = ('logs not read', 'failed jobs without a failed test', 'failed jobs with too many failed tests',
          'failures of tests that are not in this checkout', 'failures of tests whose issue is closed as not planned',
          'changes left for the next run')


def main(argv=None, environ=None, http=None, out=None, now=None, root=None):
    environ = os.environ if environ is None else environ
    out = out or sys.stdout
    parser = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument('--dry-run', action='store_true')
    mode.add_argument('--run', action='store_true')
    parser.add_argument('--days', type=int, default=DEFAULT_DAYS)
    parser.add_argument('--max-new', type=int, default=DEFAULT_MAX_NEW)
    parser.add_argument('--max-updates', type=int, default=DEFAULT_MAX_UPDATES)
    args = parser.parse_args(argv)
    try:
        if not 1 <= args.days <= MAX_DAYS or args.max_new < 0 or args.max_updates < 0:
            raise Failure('--days must be 1 to %d, and --max-new and --max-updates 0 or more' % MAX_DAYS)
        now = now or datetime.datetime.now(datetime.timezone.utc)
        since = (now - datetime.timedelta(days=args.days)).strftime('%Y-%m-%dT%H:%M:%SZ')
        root = root or os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..')
        github = GitHub(environ.get('GITHUB_TOKEN'), http)
        counts = dict.fromkeys(COUNTS, 0)
        runs, cut = github.runs(since)
        candidates = [run for run in runs if is_candidate(run)]
        observations = [observation for run in candidates for observation in observe(github, run, counts)]
        actions = plan(observations, github.issues(), test_files(root), args.max_new, args.max_updates, counts)
        lines = ['Java CI runs since %s: %d%s, %d rerun or failed on master, %d failure(s) that count.'
                 % (since, len(runs), ' (the list was cut at this number)' if cut else '', len(candidates),
                    len(observations))]
        lines += ['%s: %d' % (name, count) for name, count in counts.items() if count]
        lines += ['%s: %s' % ('Would do' if args.dry_run else 'To do', action.describe()) for action in actions]
        if not actions:
            lines.append('Nothing to change.')
        out.write('\n'.join(lines) + '\n')
        summary = environ.get('GITHUB_STEP_SUMMARY')
        if summary:
            with open(summary, 'a', encoding='utf-8') as file:
                file.write('\n'.join('- ' + line for line in lines) + '\n')
        if args.dry_run:
            first = next((action for action in actions if action.kind == 'new'), None)
            if first:
                out.write('\nThe first new issue would be "%s":\n\n%s' % (first.title, first.body()))
        else:
            execute(github, actions, out)
        return 0
    except Failure as failure:
        out.write('error: %s\n' % failure)
        return 1


if __name__ == '__main__':
    sys.exit(main())
