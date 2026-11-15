#!/usr/bin/env python3
"""Watches new IntelliJ IDEA builds and Erlang/OTP releases and runs the compatibility check (compat.yml) for them.

Called by .github/workflows/compat-watch.yml (see AGENTS.md). One cycle does, in this order:
  1. read the state: the JSON block between the state markers in the body of the one issue by github-actions[bot]
     that has them (the watcher creates it as "Compatibility watch" with the label compatibility-watch; neither is
     part of the lookup); a block that is missing or does not parse, or a second such issue, stops the cycle;
  2. collect the results of the runs that earlier cycles started, compare each with the baseline (the result of the
     default inputs on the same commit) and file one issue for each version with problems that neither the baseline
     has nor an earlier issue of the same major Erlang/OTP release or IDE branch reported;
  3. discover new versions in the feeds of builds.hex.pm and data.services.jetbrains.com;
  4. start runs of compat.yml for them on the head commit of the default branch, together with the baseline run;
  5. write the state.
Every step can be repeated after a crash without doing anything twice: a run is recorded as starting before it is
started, a run whose id is not known is found by the content of its result, and the issue of a version is found by its
marker before one is created.

Python 3 standard library only. With --dry-run it reads everything, prints what it would start, write and file, and
writes nothing.
"""

import argparse
import collections
import datetime
import hashlib
import io
import json
import os
import re
import secrets
import sys
import urllib.error
import urllib.parse
import urllib.request
import zipfile

import compat

# Limits.
MAX_STARTS_PER_CYCLE = 4
MAX_IN_FLIGHT = 8
MAX_ISSUES_PER_CYCLE = 3
MAX_ATTEMPTS = 3
# A run whose result cannot be read (the API fails) in this many cycles in a row is an attempt without a result.
MAX_COLLECT_ERRORS = 8
RUN_TIMEOUT = datetime.timedelta(hours=6)
# A complete baseline is run again after this time, well before its artifacts expire.
BASELINE_MAX_AGE = datetime.timedelta(days=30)
TABLE_ROWS = 60
# The characters of the key of a reported problem in the state.
KEY_CHARS = 10
VERSION_CHARS = 40
ISSUE_ITEMS = 30
ISSUE_TEXT_CHARS = 300
BODY_LIMIT = 65536
STATE_BODY_TARGET = 30000
ISSUE_BODY_TARGET = 60000
DOWNLOAD_LIMIT = 64 * 1024 * 1024
# A run of compat.yml whose id the dispatch did not return is searched among the runs created in this window.
RECOVERY_WINDOW = (datetime.timedelta(minutes=2), datetime.timedelta(minutes=30))

OTP_FEED = 'https://builds.hex.pm/builds/otp/ubuntu-24.04/builds.txt'
IDE_FEED = 'https://data.services.jetbrains.com/products/releases?code=IIU&type=release,rc,eap'
WORKFLOW = 'compat.yml'
WORKFLOW_PATH = '.github/workflows/' + WORKFLOW
API_VERSION = '2022-11-28'
BOT = 'github-actions[bot]'
STATE_TITLE = 'Compatibility watch'
STATE_LABEL = 'compatibility-watch'
ISSUE_LABEL = 'compatibility'
LABELS = {
    STATE_LABEL: ('ededed', 'The state of the compatibility watch (scripts/compat/watch.py)'),
    ISSUE_LABEL: ('d93f0b', 'A version of IntelliJ IDEA or Erlang/OTP with problems (scripts/compat/watch.py)'),
}
STATE_BEGIN = '<!-- compat-watch-state:begin -->'
STATE_END = '<!-- compat-watch-state:end -->'
STATE_FORMAT = 1

KINDS = {'otp': 'Erlang/OTP', 'ide': 'IntelliJ IDEA'}
ARTIFACTS = {'otp': 'compat-otp', 'ide': 'compat-ide'}
LOGS_ARTIFACT = 'compat-otp-logs'
CORPUS_REPORT = 'corpus-report.json'

# The states of a version, and its verdict once it is done.
QUEUED, STARTING, RUNNING, REPORT, DONE = 'queued', 'starting', 'running', 'report', 'done'
ACTIVE = (STARTING, RUNNING)
SAME, NEW_PROBLEMS, UNCHECKED, SUPERSEDED, BASELINE = 'same', 'new problems', 'unchecked', 'superseded', 'baseline'
KNOWN_PROBLEMS = 'known problems'
VERDICTS = (SAME, NEW_PROBLEMS, KNOWN_PROBLEMS, UNCHECKED, SUPERSEDED, BASELINE)
# The states of a baseline run; ACTIVE ones too.
COMPLETE, FAILED = 'complete', 'failed'

REPOSITORY = re.compile(r'[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+')
BRANCH = re.compile(r'[A-Za-z0-9._/-]+')
TIME_FORMAT = '%Y-%m-%dT%H:%M:%SZ'
APP_VERSION = re.compile(r'^([A-Za-z_][A-Za-z0-9_]*)-[0-9]+(\.[0-9]+)*$')


class WatchError(Exception):
    """Stops the cycle."""


class SourceError(Exception):
    """A feed that cannot be read or parsed: no version of that kind is discovered in this cycle."""


class ApiError(Exception):
    """A request that failed; `status` is the HTTP status, None if there was no response."""

    def __init__(self, message, status=None):
        super().__init__(message)
        self.status = status


# Versions


def valid_version(kind, value):
    """Whether `value` is a version of `kind` that the watcher uses: a form of the input of compat.py (its patterns)
    of at most VERSION_CHARS characters and numbers of at most 9 digits, for ide only its build-number form, whose
    first number is the branch."""
    if (not isinstance(value, str) or len(value) > VERSION_CHARS
            or not compat.INPUT_PATTERNS[kind].fullmatch(value)
            or any(len(number) > 9 for number in re.findall('[0-9]+', value))):
        return False
    if kind == 'otp':
        return True
    parts = value.split('.')
    return len(parts[0]) == 3 and all(part.isdigit() for part in parts)


def valid_ide_name(value):
    """Whether `value` is a release version of IntelliJ IDEA (2026.3.1), a form of the ide input of compat.py."""
    return (isinstance(value, str) and len(value) <= VERSION_CHARS
            and bool(compat.INPUT_PATTERNS['ide'].fullmatch(value)) and value.startswith('20')
            and all(part.isdigit() for part in value.split('.')))


def group(version):
    """The major release of an Erlang/OTP version, the branch of an IDE build."""
    return int(re.split(r'[.-]', version)[0])


def order(version):
    """A sort key: numbers first, a release candidate before the release."""
    base, _, rc = version.partition('-rc')
    numbers = tuple(int(part) for part in base.split('.'))
    return (numbers, 0, int(rc)) if rc else (numbers, 1, 0)


def build_number(value):
    """The build number of resolved.ide.buildNumber without a product code (IU-263.7307.200)."""
    return re.sub(r'^[A-Z]+-', '', value) if isinstance(value, str) else None


def iso(time):
    return time.strftime(TIME_FORMAT)


def parse_time(value):
    return datetime.datetime.strptime(value, TIME_FORMAT).replace(tzinfo=datetime.timezone.utc)


# Text that reaches an issue or the log


def safe(value, limit=ISSUE_TEXT_CHARS):
    """`value` for a code block or code span of an issue: one line, no backtick, no @ that could mention anyone, no
    HTML comment that could be read as a marker, at most `limit` characters."""
    value = ''.join(ch if ch.isprintable() else ' ' for ch in str(value))
    value = value.replace('`', "'").replace('@', '\uff20').replace('<!--', '<! --').replace('-->', '-- >')
    return value if len(value) <= limit else value[:limit - 1] + '\u2026'


def print_untrusted(text, out=None):
    """Prints `text` with workflow commands disabled, so that no line of it is read as one."""
    out = out or sys.stdout
    token = secrets.token_hex(16)
    print('::stop-commands::%s' % token, file=out)
    print(text, file=out)
    print('::%s::' % token, file=out)


# HTTP


class Response:
    def __init__(self, status, data=b''):
        self.status = status
        self.data = data

    def json(self):
        return json.loads(self.data.decode('utf-8'))


class _HttpsOnlyRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        if urllib.parse.urlsplit(newurl).scheme != 'https':
            raise urllib.error.HTTPError(req.full_url, code, 'redirect to a URL that is not https', headers, fp)
        return super().redirect_request(req, fp, code, msg, headers, newurl)


class Http:
    """HTTP with urllib. The token is sent only where `auth` is set, and never to the target of a redirect."""

    def __init__(self, token=None):
        self.token = token
        self.opener = urllib.request.build_opener(_HttpsOnlyRedirect)

    def request(self, method, url, body=None, headers=None, auth=False):
        data = json.dumps(body).encode('utf-8') if body is not None else None
        request = urllib.request.Request(url, data=data, method=method, headers=dict(headers or {}))
        if data is not None:
            request.add_header('Content-Type', 'application/json')
        if auth and self.token:
            request.add_unredirected_header('Authorization', 'Bearer ' + self.token)
        try:
            with self.opener.open(request, timeout=120) as response:
                content = response.read(DOWNLOAD_LIMIT + 1)
                if len(content) > DOWNLOAD_LIMIT:
                    raise ApiError('%s %s: the response is larger than %d bytes' % (method, url, DOWNLOAD_LIMIT))
                return Response(response.status, content)
        except urllib.error.HTTPError as e:
            return Response(e.code)
        except (urllib.error.URLError, OSError, ValueError) as e:
            raise ApiError('%s %s: %s' % (method, url, e))


class GitHub:
    """The calls of the GitHub REST API that the watcher makes. The methods that write are never called in a dry run."""

    def __init__(self, http, repository, api_url='https://api.github.com'):
        self.http = http
        self.repository = repository
        self.api_url = api_url.rstrip('/')

    def call(self, method, path, query=None, body=None, ok=(200,), missing=(404,)):
        url = '%s/repos/%s%s' % (self.api_url, self.repository, path)
        if query:
            url += '?' + urllib.parse.urlencode(query)
        headers = {'Accept': 'application/vnd.github+json', 'X-GitHub-Api-Version': API_VERSION}
        response = self.http.request(method, url, body=body, headers=headers, auth=True)
        if response.status in missing:
            return None
        if response.status not in ok:
            raise ApiError('GitHub API %s %s: HTTP %d' % (method, path, response.status), response.status)
        return response

    def get_json(self, path, query=None, missing=(404,)):
        response = self.call('GET', path, query, missing=missing)
        if response is None:
            return None
        try:
            return response.json()
        except (ValueError, RecursionError):
            raise ApiError('GitHub API GET %s: the response is not JSON' % path)

    def pages(self, path, query, key=None, limit=10, missing=(404,)):
        """The items of all pages; none if the resource is `missing`."""
        items = []
        for page in range(1, limit + 1):
            data = self.get_json(path, dict(query, per_page=100, page=page), missing)
            if data is None:
                return items
            batch = data.get(key) if key and isinstance(data, dict) else data
            if not isinstance(batch, list):
                raise ApiError('GitHub API GET %s: not a list' % path)
            items += batch
            if len(batch) < 100:
                return items
        raise ApiError('GitHub API GET %s: more than %d pages' % (path, limit))

    def default_branch(self):
        branch = (self.get_json('') or {}).get('default_branch')
        if not isinstance(branch, str) or not BRANCH.fullmatch(branch):
            raise ApiError('the repository has no valid default branch')
        return branch

    def head_commit(self, branch):
        sha = (self.get_json('/commits/' + urllib.parse.quote('heads/' + branch, safe='/')) or {}).get('sha')
        if not isinstance(sha, str) or not compat.SHA.fullmatch(sha):
            raise ApiError('the head of the branch %s is not known' % branch)
        return sha

    def run(self, run_id):
        return self.get_json('/actions/runs/%d' % run_id)

    def runs_created(self, start, end):
        query = {'event': 'workflow_dispatch', 'created': '%s..%s' % (iso(start), iso(end))}
        return self.pages('/actions/workflows/%s/runs' % WORKFLOW, query, key='workflow_runs')

    def artifact_id(self, run_id, name):
        """The id of the newest artifact `name` of run `run_id` that has not expired; None if there is none."""
        artifacts = self.pages('/actions/runs/%d/artifacts' % run_id, {'name': name}, key='artifacts')
        ids = [a['id'] for a in artifacts if isinstance(a, dict) and a.get('name') == name and not a.get('expired')
               and isinstance(a.get('id'), int) and not isinstance(a['id'], bool) and a['id'] > 0]
        return max(ids) if ids else None

    def artifact_file(self, artifact_id, member):
        """The file `member` of the artifact `artifact_id`; None if there is no such artifact or file."""
        response = self.call('GET', '/actions/artifacts/%d/zip' % artifact_id, missing=(404, 410))
        if response is None:
            return None
        try:
            with zipfile.ZipFile(io.BytesIO(response.data)) as archive:
                info = next((i for i in archive.infolist() if i.filename == member), None)
                if info is None or info.file_size > DOWNLOAD_LIMIT:
                    return None
                return archive.read(info)
        except (zipfile.BadZipFile, OSError, RuntimeError, ValueError):
            return None

    def issues(self, label):
        return self.pages('/issues', {'labels': label, 'state': 'all'}, missing=())

    def bot_issues(self):
        """The issues that BOT created, open and closed. A 404 is an error: it must never look like "no state issue"."""
        return self.pages('/issues', {'creator': BOT, 'state': 'all'}, missing=())

    def ensure_label(self, name):
        if self.get_json('/labels/' + urllib.parse.quote(name, safe='')) is None:
            color, description = LABELS[name]
            # 422: it was created in the meantime.
            self.call('POST', '/labels', body={'name': name, 'color': color, 'description': description},
                      ok=(201, 422))

    def create_issue(self, title, body, label):
        self.ensure_label(label)
        number = self.call('POST', '/issues', body={'title': title, 'body': body, 'labels': [label]},
                           ok=(201,)).json().get('number')
        if not isinstance(number, int):
            raise ApiError('the created issue has no number')
        return number

    def update_issue(self, number, body):
        # A 404 is an error: a state that was not written must never look written.
        self.call('PATCH', '/issues/%d' % number, body={'body': body}, missing=())

    def dispatch(self, branch, inputs):
        """Starts compat.yml; the id of its run, or None if the response does not name it."""
        response = self.call('POST', '/actions/workflows/%s/dispatches' % WORKFLOW, missing=(),
                             body={'ref': branch, 'inputs': inputs, 'return_run_details': True}, ok=(200, 204))
        if response.status == 204:
            return None
        try:
            run_id = response.json().get('workflow_run_id')
        except (ValueError, AttributeError, RecursionError):
            return None
        return run_id if isinstance(run_id, int) and run_id > 0 else None


def is_ours(issue):
    return (isinstance(issue, dict) and (issue.get('user') or {}).get('login') == BOT
            and 'pull_request' not in issue and isinstance(issue.get('number'), int))


def label_names(issue):
    labels = issue.get('labels')
    return [label.get('name') for label in labels if isinstance(label, dict)] if isinstance(labels, list) else []


# Feeds


def read_otp_feed(http):
    """The Erlang/OTP versions of builds.txt and the number of rejected names; lines that are not versions (maint,
    master) are ignored."""
    try:
        response = http.request('GET', OTP_FEED)
    except ApiError as e:
        raise SourceError(str(e))
    if response.status != 200:
        raise SourceError('%s: HTTP %d' % (OTP_FEED, response.status))
    versions, rejected = {}, 0
    for line in response.data.decode('utf-8', errors='replace').splitlines():
        fields = line.split()
        if not fields or not fields[0].startswith('OTP-'):
            continue
        if valid_version('otp', fields[0][len('OTP-'):]):
            versions[fields[0][len('OTP-'):]] = None
        else:
            rejected += 1
    if not versions:
        raise SourceError('%s lists no Erlang/OTP version' % OTP_FEED)
    return versions, rejected


def read_ide_feed(http):
    """The builds of IntelliJ IDEA in the releases feed, with their release version where it is valid, and the number
    of rejected builds."""
    try:
        response = http.request('GET', IDE_FEED)
    except ApiError as e:
        raise SourceError(str(e))
    if response.status != 200:
        raise SourceError('%s: HTTP %d' % (IDE_FEED, response.status))
    try:
        releases = response.json().get('IIU')
    except (ValueError, AttributeError, RecursionError):
        raise SourceError('%s is not a JSON object' % IDE_FEED)
    if not isinstance(releases, list):
        raise SourceError('%s has no list IIU' % IDE_FEED)
    builds, rejected = {}, 0
    for release in releases:
        build = release.get('build') if isinstance(release, dict) else None
        if not valid_version('ide', build):
            rejected += 1
            continue
        name = release.get('version')
        builds[build] = name if valid_ide_name(name) else builds.get(build)
    if not builds:
        raise SourceError('%s lists no build' % IDE_FEED)
    return builds, rejected


# The state


ENTRY_DEFAULTS = {'name': None, 'verdict': None, 'sha': None, 'at': None, 'run': None, 'runs': [], 'tries': 0,
                  'issue': None, 'errors': 0}
# `artifacts` of a complete baseline: the ids of the artifacts that were accepted, {'ide': id, 'otp': id, 'logs': id
# or None}; a comparison reads exactly those.
BASELINE_DEFAULTS = {'run': None, 'ide': None, 'otp': None, 'tries': 1, 'errors': 0, 'artifacts': None}
KEY = re.compile(r'[0-9a-f]{%d}' % KEY_CHARS)


def new_state(now):
    """`reported` maps each kind to {major release or branch: {'issue': the latest issue of that line, 'keys': the
    keys of the problems that the issues of that line reported}}."""
    return {'format': STATE_FORMAT, 'created': iso(now), 'pending': sorted(KINDS), 'floor': {'ide': None, 'otp': None},
            'seen': {'ide': [], 'otp': []}, 'reported': {'ide': {}, 'otp': {}}, 'baselines': [], 'entries': []}


def _check(condition, what):
    if not condition:
        raise WatchError('the state is not valid: %s' % what)


def _time_or_none(value):
    if value is None:
        return True
    try:
        parse_time(value)
        return True
    except (TypeError, ValueError):
        return False


def _int_or_none(value):
    return value is None or (isinstance(value, int) and not isinstance(value, bool) and value > 0)


def _count(value, limit):
    return isinstance(value, int) and not isinstance(value, bool) and 0 <= value <= limit


def validate_state(state):
    """`state` with the defaults of its entries filled in; WatchError if any value is not one the watcher writes."""
    _check(isinstance(state, dict) and state.get('format') == STATE_FORMAT, 'format')
    _check(isinstance(state.get('pending'), list) and set(state['pending']) <= set(KINDS), 'pending')
    _check(isinstance(state.get('floor'), dict) and set(state['floor']) == set(KINDS)
           and all(_int_or_none(v) for v in state['floor'].values()), 'floor')
    _check(isinstance(state.get('seen'), dict) and set(state['seen']) == set(KINDS), 'seen')
    for kind, versions in state['seen'].items():
        _check(isinstance(versions, list) and all(valid_version(kind, v) for v in versions), 'seen ' + kind)
    _check(isinstance(state.get('reported'), dict) and set(state['reported']) == set(KINDS), 'reported')
    for lines in state['reported'].values():
        _check(isinstance(lines, dict), 'reported lines')
        for line, reported in lines.items():
            _check(re.fullmatch(r'[0-9]{1,9}', line) and isinstance(reported, dict)
                   and set(reported) == {'issue', 'keys'} and _int_or_none(reported['issue']) and reported['issue']
                   and isinstance(reported['keys'], list)
                   and all(isinstance(key, str) and KEY.fullmatch(key) for key in reported['keys']), 'reported line')
    _check(isinstance(state.get('baselines'), list), 'baselines')
    baselines = []
    for baseline in state['baselines']:
        _check(isinstance(baseline, dict), 'baseline')
        baseline = dict(BASELINE_DEFAULTS, **baseline)
        baselines.append(baseline)
        _check(set(baseline) == set(BASELINE_DEFAULTS) | {'sha', 'at', 'state'}, 'baseline keys')
        artifacts = baseline['artifacts']
        _check(_count(baseline['tries'], MAX_ATTEMPTS) and _count(baseline['errors'], MAX_COLLECT_ERRORS)
               and (artifacts is None or (isinstance(artifacts, dict) and set(artifacts) == {'ide', 'otp', 'logs'}
                                          and all(_int_or_none(a) for a in artifacts.values())
                                          and artifacts['ide'] and artifacts['otp'])), 'baseline attempts')
        _check((baseline.get('state') == COMPLETE) == (artifacts is not None), 'baseline artifacts')
        _check(compat.SHA.fullmatch(baseline.get('sha') or '')
               and baseline.get('state') in ACTIVE + (COMPLETE, FAILED) and _int_or_none(baseline.get('run'))
               and _time_or_none(baseline.get('at')) and baseline.get('at') is not None
               and (baseline.get('ide') is None or valid_version('ide', baseline['ide']))
               and (baseline.get('otp') is None or valid_version('otp', baseline['otp'])), 'baseline')
        _check(baseline['state'] != COMPLETE or (baseline.get('ide') and baseline.get('otp')), 'complete baseline')
    state['baselines'] = baselines
    _check(isinstance(state.get('entries'), list), 'entries')
    entries = []
    for entry in state['entries']:
        _check(isinstance(entry, dict) and entry.get('kind') in KINDS, 'entry')
        entry = dict(ENTRY_DEFAULTS, **entry)
        kind = entry['kind']
        _check(valid_version(kind, entry.get('version')), 'entry version')
        _check(entry['name'] is None or (kind == 'ide' and valid_ide_name(entry['name'])), 'entry name')
        _check(entry.get('state') in (QUEUED, STARTING, RUNNING, REPORT, DONE), 'entry state')
        _check((entry['state'] == DONE) == (entry['verdict'] in VERDICTS) and
               (entry['verdict'] is None or entry['verdict'] in VERDICTS), 'entry verdict')
        _check(isinstance(entry.get('seen'), str) and re.fullmatch(r'[0-9]{4}-[0-9]{2}-[0-9]{2}', entry['seen']),
               'entry date')
        _check(entry['sha'] is None or compat.SHA.fullmatch(entry['sha']), 'entry commit')
        _check(_time_or_none(entry['at']) and _int_or_none(entry['run']) and _int_or_none(entry['issue']), 'entry run')
        _check(isinstance(entry['runs'], list) and all(_int_or_none(r) and r for r in entry['runs']), 'entry runs')
        _check(_count(entry['tries'], MAX_ATTEMPTS) and _count(entry['errors'], MAX_COLLECT_ERRORS), 'entry tries')
        _check(entry['state'] not in ACTIVE + (REPORT,) or (entry['sha'] and entry['at']), 'active entry')
        _check(entry['state'] not in (RUNNING, REPORT) or entry['run'], 'running entry')
        entry['runs'] = list(entry['runs'])  # never the list of ENTRY_DEFAULTS, which every entry would share
        entries.append(entry)
    state['entries'] = entries
    return state


def parse_state(body):
    """The state in the body of the state issue; WatchError if its block is missing or does not parse."""
    body = (body or '').replace('\r\n', '\n')
    start = body.find(STATE_BEGIN)
    end = body.find(STATE_END, start + 1) if start >= 0 else -1
    if start < 0 or end < 0:
        raise WatchError('the state issue has no state block between %s and %s' % (STATE_BEGIN, STATE_END))
    match = re.fullmatch(r'```json\n(.*)\n```', body[start + len(STATE_BEGIN):end].strip('\n'), re.DOTALL)
    if not match:
        raise WatchError('the state block of the state issue is not one fenced JSON block')
    try:
        state = json.loads(match.group(1))
    except (ValueError, RecursionError) as e:
        raise WatchError('the state block of the state issue does not parse: %s' % e)
    try:
        return validate_state(state)
    except (TypeError, AttributeError, KeyError, ValueError, RecursionError) as e:
        raise WatchError('the state is not valid: %s' % type(e).__name__)


def compact(entry):
    return {key: value for key, value in entry.items() if key not in ENTRY_DEFAULTS or value != ENTRY_DEFAULTS[key]}


def display_version(entry):
    return '%s (%s)' % (entry['name'], entry['version']) if entry.get('name') else entry['version']


def render_state(state, server_url, repository):
    run_url = lambda run: '%s/%s/actions/runs/%d' % (server_url, repository, run)
    lines = [
        'The state of the compatibility watch: `.github/workflows/compat-watch.yml` runs `scripts/compat/watch.py`, '
        'which starts `.github/workflows/compat.yml` for new IntelliJ IDEA builds and Erlang/OTP versions and files '
        'one issue (label `%s`) for each version with problems that the default configuration does not have. '
        'The watcher rewrites this issue on every run; the JSON block at the end is its state.' % ISSUE_LABEL,
        '',
        '| Kind | Version | Seen | Verdict | Run | Issue |',
        '|---|---|---|---|---|---|',
    ]
    for entry in state['entries']:
        verdict = entry['verdict'] or {REPORT: 'new problems, issue to be filed'}.get(entry['state'], entry['state'])
        runs = entry['runs'] if entry['verdict'] == UNCHECKED else entry['runs'][-1:]
        lines.append('| %s | %s | %s | %s | %s | %s |' % (
            KINDS[entry['kind']], display_version(entry), entry['seen'], verdict,
            ' '.join('[run](%s)' % run_url(run) for run in runs), '#%d' % entry['issue'] if entry['issue'] else ''))
    stored = dict(state, entries=[compact(entry) for entry in state['entries']])
    lines += ['', STATE_BEGIN, '```json', json.dumps(stored, separators=(',', ':')), '```', STATE_END, '']
    return '\n'.join(lines)


# Comparison


def without_app_version(path):
    """`path` with the version of each application removed: lib/stdlib-6.2.2.3/src/x.erl is lib/stdlib/src/x.erl."""
    return '/'.join(APP_VERSION.sub(r'\1', part) for part in str(path).split('/'))


def trimmed_list(value):
    """The items, the total and whether the list was trimmed, of a list of a result (compat.trimmed)."""
    if not isinstance(value, dict) or not isinstance(value.get('items'), list):
        return [], 0, False
    total = value.get('total') if isinstance(value.get('total'), int) else len(value['items'])
    return value['items'], total, bool(value.get('trimmed'))


# The lists of the Plugin Verifier that count: the ones of the levels that failureLevel of verifyPlugin in build.gradle
# gates (compat.VERIFIER_FAILURE_LEVELS). MISSING_DEPENDENCIES has no list of its own in the result. Deprecated,
# experimental, internal and non-extendable usages are never compared.
VERIFIER_CATEGORIES = {'COMPATIBILITY_PROBLEMS': 'compatibility-problems',
                       'OVERRIDE_ONLY_API_USAGES': 'override-only-usages',
                       'INVALID_PLUGIN': 'invalid-plugin'}


def verifier_problems(check):
    """The problems of the gated levels of all reports of `check`; a failing level without a listed problem is one
    item of the category verdict."""
    items, total, trimmed = [], 0, False
    for report in check.get('reports') if isinstance(check.get('reports'), list) else []:
        if not isinstance(report, dict):
            continue
        problems = report.get('problems') if isinstance(report.get('problems'), dict) else {}
        levels = report.get('failingLevels') if isinstance(report.get('failingLevels'), list) else []
        for level in compat.VERIFIER_FAILURE_LEVELS:
            lines, count, cut = trimmed_list(problems.get(VERIFIER_CATEGORIES.get(level)))
            if not count and level in levels:
                items.append({'category': 'verdict', 'text': level})
                count = 1
            else:
                items += [{'category': VERIFIER_CATEGORIES[level], 'text': line} for line in lines]
            total += count
            trimmed = trimmed or cut
    return items, total, trimmed


def test_name(test):
    return '%s.%s' % (test.get('class'), test.get('name'))


def corpus_key(error):
    return without_app_version(error.get('file')), str(error.get('description'))


# The lists that are compared: (check, title, the list of the check, the key of an item, the line of an item).
LISTS = {
    'ide': [
        ('compile', 'compiler errors', lambda c: trimmed_list(c.get('errors')),
         lambda e: (str(e.get('file')), str(e.get('message'))), lambda e: '%s: %s' % (e.get('file'), e.get('message'))),
        ('test', 'failed tests', lambda c: trimmed_list(c.get('failedTests')),
         test_name, lambda t: '%s: %s' % (test_name(t), t.get('message'))),
        ('verifyPlugin', 'Plugin Verifier problems of the built plugin', verifier_problems,
         lambda p: str(p['text']), lambda p: '%s: %s' % (p['category'], p['text'])),
        ('releasedPlugin', 'Plugin Verifier problems of the released plugin', verifier_problems,
         lambda p: str(p['text']), lambda p: '%s: %s' % (p['category'], p['text'])),
    ],
    'otp': [
        ('compilationTests', 'failed compilation tests', lambda c: trimmed_list(c.get('failedTests')),
         test_name, lambda t: '%s: %s' % (test_name(t), t.get('message'))),
        ('compilationTests', 'skipped compilation tests', lambda c: trimmed_list(c.get('skippedTests')),
         test_name, test_name),
        ('corpus', 'parser errors in the Erlang/OTP sources', lambda c: trimmed_list(c.get('errors')),
         corpus_key, lambda e: '%s: %s' % (e.get('file'), e.get('description'))),
        ('corpus', 'parser exceptions in the Erlang/OTP sources', lambda c: trimmed_list(c.get('exceptions')),
         lambda e: (without_app_version(e.get('file')), str(e.get('exception'))),
         lambda e: '%s: %s' % (e.get('file'), e.get('exception'))),
    ],
}
CHECKS = {'ide': ['compile', 'test', 'verifyPlugin', 'releasedPlugin'],
          'otp': ['compilationTests', 'corpus', 'knownRelease']}

# A problem that the baseline does not have: `key` identifies it in the state (reported), `line` goes into the issue,
# `count` is how many more times it occurs than in the baseline.
Problem = collections.namedtuple('Problem', 'key line count')


def problem(check, title, key, line, count=1):
    digest = hashlib.sha256(json.dumps([check, title, key]).encode('utf-8')).hexdigest()
    return Problem(digest[:KEY_CHARS], line, count)


def mapping(value):
    """`value` if it is a JSON object, an empty one otherwise: a result is read without trusting its shape."""
    return value if isinstance(value, dict) else {}


def checks_of(result):
    return {name: check for name, check in mapping(mapping(result).get('checks')).items() if isinstance(check, dict)}


def new_items(items, baseline_items, key):
    """(key, the first item with it, the number of times more) of the keys that are more often in `items` than in
    `baseline_items`."""
    items = [item for item in items if isinstance(item, dict)]
    counts = collections.Counter(key(item) for item in items)
    counts.subtract(key(item) for item in baseline_items if isinstance(item, dict))
    first = {}
    for item in items:
        first.setdefault(key(item), item)
    return [(k, first[k], n) for k, n in counts.items() if n > 0]


def compare(kind, result, baseline, full_corpus=None):
    """The problems of `result` that `baseline` does not have: {'sections': [(check, title, [Problem])], 'notes':
    [...]}. With notes the comparison is not complete, because a list was trimmed in a result and its full form could
    not be read: the sections then say neither that there are new problems nor that there are none.

    `full_corpus` maps 'result' and 'baseline' to the full list of parser errors, read from corpus-report.json when the
    list of the result was trimmed, or to the reason why it could not be read."""
    checks, base = checks_of(result), checks_of(baseline)
    sections, notes, reported = [], [], set()
    for name, title, of, key, line in LISTS[kind]:
        mine, theirs = of(checks.get(name, {})), of(base.get(name, {}))
        (items, total, trimmed), (base_items, base_total, base_trimmed) = mine, theirs
        if name == 'corpus' and title.startswith('parser errors') and (trimmed or base_trimmed):
            full = full_corpus or {}
            for side, cut in (('result', trimmed), ('baseline', base_trimmed)):
                if not cut:
                    continue
                if isinstance(full.get(side), list):
                    if side == 'result':
                        items, trimmed = full[side], False
                    else:
                        base_items, base_trimmed = full[side], False
                else:
                    notes.append('the full list of parser errors of the %s could not be read: %s' % (
                        'version' if side == 'result' else 'baseline', full.get(side) or 'it was not read'))
        elif trimmed or base_trimmed:
            notes.append('%s: the version has %d, the baseline %d, and the result lists only the first %d'
                         % (title, total, base_total, compat.LIST_LIMIT))
        problems = [problem(name, title, k, line(item), n) for k, item, n in new_items(items, base_items, key)]
        if problems:
            sections.append((name, title, problems))
            reported.add(name)
    known = checks.get('knownRelease')
    if kind == 'otp' and known and known.get('status') == compat.FAILED:
        sections.append(('knownRelease', 'the release is unknown to the plugin', [
            problem('knownRelease', 'release', str(known.get('release')),
                    'Erlang/OTP %s: ErlangSdkRelease constant %s, %s' % (
                        known.get('release'), known.get('sdkReleaseConstant') or 'missing',
                        known.get('mockSdk') or 'no testData/mockSdk-R%s' % known.get('release')))]))
        reported.add('knownRelease')
    for name in CHECKS[kind]:
        status = checks.get(name, {}).get('status')
        if (base.get(name, {}).get('status') == compat.PASSED and status in (compat.FAILED, compat.NOT_RUN)
                and name not in reported):
            reason = checks[name].get('reason')
            text = '%s: passed in the baseline, %s here%s' % (name, status, ': %s' % reason if reason else '')
            sections.append((name, 'status', [problem(name, 'status', status, text)]))
    return {'sections': sections, 'notes': notes}


def lines_of(sections):
    """(check, title, the lines of the problems) of each section, as they are listed in an issue."""
    return [(check, title, [p.line + (' (%d times)' % p.count if p.count > 1 else '') for p in problems])
            for check, title, problems in sections]


# Issues of versions


def marker(kind, version):
    return '<!-- compat-watch:%s:%s -->' % (kind, version)


def issue_title(entry):
    if entry['kind'] == 'otp':
        return 'Compatibility: Erlang/OTP %s' % entry['version']
    return 'Compatibility: IntelliJ IDEA %s' % display_version(entry)


def line_name(kind, version):
    """The major Erlang/OTP release or the IDE branch of `version`, as it is named in an issue."""
    return '%s %d' % (KINDS[kind], group(version))


def issue_body(entry, sections, run_url, baseline_url, items=ISSUE_ITEMS, earlier=None):
    """The body of the issue of a version. `sections` are the problems that no earlier issue of its major release or
    branch reported; `earlier` is the number of the latest such issue."""
    kind, version, sha = entry['kind'], entry['version'], entry['sha']
    lines = [
        marker(kind, version),
        'The compatibility check found problems with %s `%s` that the default configuration of the plugin does not '
        'have on the same commit.' % (KINDS[kind], version),
        '',
        '- %s: `%s`%s' % (KINDS[kind], version, ' (`%s`)' % entry['name'] if entry.get('name') else ''),
        '- Commit: `%s`' % sha,
        '- Run: %s' % run_url,
        '- Baseline run (the default inputs on the same commit): %s' % baseline_url,
    ]
    if earlier:
        lines.append('- Earlier issue of %s: #%d. The problems reported there are not repeated here.'
                     % (line_name(kind, version), earlier))
    lines += ['', '### New problems']
    for (check, title, problems), (_, _, texts) in zip(sections, lines_of(sections)):
        shown = texts[:items]
        cut = ', the first %d listed' % len(shown) if len(shown) < len(texts) else ''
        lines += ['', '**%s, %s**: %d distinct, %d occurrences%s' % (check, title, len(problems),
                                                                    sum(p.count for p in problems), cut), '', '```']
        lines += [safe(text) for text in shown]
        lines.append('```')
    lines += ['', 'To repeat the check: `gh workflow run %s -f %s=%s -f ref=%s`' % (WORKFLOW, kind, version, sha), '']
    return '\n'.join(lines)


# The cycle


class Watcher:
    def __init__(self, github, http, pins, now, server_url='https://github.com', dry_run=False, out=None):
        self.github = github
        self.http = http
        self.pins = pins
        self.now = now
        self.server_url = server_url.rstrip('/')
        self.dry_run = dry_run
        self.out = out or sys.stdout
        self.failed = False
        self.issue = None
        self.state = None
        self.results = {}
        self.artifact_ids = {}
        self.version_issues = None
        self.filed = 0
        self.started = []

    def log(self, message):
        print(message, file=self.out)

    def error(self, message):
        print('::error::%s' % message, file=self.out)
        self.failed = True

    def run_url(self, run_id):
        return '%s/%s/actions/runs/%d' % (self.server_url, self.github.repository, run_id)

    def cycle(self):
        self.read_state()
        self.collect()
        self.discover()
        self.start_runs()
        self.write_state('at the end of the cycle')
        return 1 if self.failed else 0

    # 1. The state

    def read_state(self):
        """The state issue is the issue by BOT whose body has a state marker, open or closed; its title and labels
        are not part of the lookup, so renaming it or removing its label does not make the watcher seed again.
        The issues with the state label are looked through as well: a first cycle needs both listings to have none."""
        found = self.github.bot_issues() + self.github.issues(STATE_LABEL)
        issues = sorted({i['number']: i for i in found if is_ours(i)}.values(), key=lambda i: i['number'])
        states = [i for i in issues if STATE_BEGIN in (i.get('body') or '') or STATE_END in (i.get('body') or '')]
        if len(states) > 1:
            raise WatchError('%d issues by %s have the state markers (%s); there must be one, so the watcher stops'
                             % (len(states), BOT, ', '.join('#%d' % i['number'] for i in states)))
        if not states:
            # The issue that the watcher created, with its block removed: not a first cycle.
            emptied = [i for i in issues if i.get('title') == STATE_TITLE or STATE_LABEL in label_names(i)]
            if emptied:
                raise WatchError('#%d: the state issue has no state block between %s and %s; the watcher stops and '
                                 'does not seed again over an existing state issue'
                                 % (emptied[0]['number'], STATE_BEGIN, STATE_END))
            self.log('No issue by %s with the state markers: this is the first cycle, it seeds.' % BOT)
            self.state = new_state(self.now)
            return
        self.issue = states[0]['number']
        try:
            self.state = parse_state(states[0].get('body'))
        except WatchError as e:
            raise WatchError('#%d: %s; the watcher stops and does not seed again over an existing state issue'
                             % (self.issue, e))
        self.log('State: issue #%d' % self.issue)

    def write_state(self, when):
        self.prune()
        body = render_state(self.state, self.server_url, self.github.repository)
        if len(body) > BODY_LIMIT:
            raise WatchError('the state has %d characters, more than an issue body can have' % len(body))
        if self.dry_run:
            self.log('[dry run] would %s the state issue %s (%d characters)'
                     % ('create' if self.issue is None else 'update #%d of' % self.issue, when, len(body)))
            if when == 'at the end of the cycle':
                print_untrusted(body, self.out)
            return
        if self.issue is None:
            self.issue = self.github.create_issue(STATE_TITLE, body, STATE_LABEL)
            self.log('Created the state issue #%d %s' % (self.issue, when))
        else:
            self.github.update_issue(self.issue, body)
            self.log('Wrote the state %s' % when)

    def prune(self):
        """Keeps the state small. Dropped, in this order: the table rows beyond the newest TABLE_ROWS and the
        baselines that nothing needs; the reported keys of the lines that are not watched any more; then, while the
        body is above STATE_BODY_TARGET (room for about 2000 keys), the oldest table row and, when no row is left, the
        oldest reported keys of the line that has the most. The versions seen are never dropped. A problem whose
        reported key was dropped is reported again by the next version of its line that has it."""
        state = self.state
        entries = state['entries']
        room = max(TABLE_ROWS - sum(1 for e in entries if e['state'] != DONE), 0)
        kept = []
        for entry in entries:
            if entry['state'] != DONE:
                kept.append(entry)
            elif room > 0:
                kept.append(entry)
                room -= 1
        state['entries'] = kept
        needed = {e['sha'] for e in kept if e['state'] in ACTIVE + (REPORT,)}
        latest = self.latest_baseline()
        # The latest failed one is kept for its attempts: no baseline is started again for a commit that used them up.
        failed = [b for b in state['baselines'] if b['state'] == FAILED]
        last_failed = max(failed, key=lambda b: b['at']) if failed else None
        state['baselines'] = [b for b in state['baselines']
                              if b['state'] in ACTIVE or b['sha'] in needed or b is latest or b is last_failed]
        for kind, lines in state['reported'].items():
            floor = state['floor'][kind]
            for line in list(lines):
                if floor is not None and int(line) < floor:
                    del lines[line]
        while len(render_state(state, self.server_url, self.github.repository)) > STATE_BODY_TARGET:
            done = [i for i, e in enumerate(state['entries']) if e['state'] == DONE]
            if done:
                del state['entries'][done[-1]]
                continue
            longest = max((reported for lines in state['reported'].values() for reported in lines.values()),
                          key=lambda reported: len(reported['keys']), default=None)
            if longest is None or not longest['keys']:
                break
            del longest['keys'][:50]

    def latest_baseline(self):
        complete = [b for b in self.state['baselines'] if b['state'] == COMPLETE]
        return max(complete, key=lambda b: b['at']) if complete else None

    def baseline_of(self, sha):
        return next((b for b in self.state['baselines'] if b['sha'] == sha), None)

    def known_runs(self):
        runs = {b['run'] for b in self.state['baselines'] if b['run']}
        for entry in self.state['entries']:
            runs.update(entry['runs'])
        return runs

    # 2. Collect

    def artifact(self, run_id, name):
        """The id of the artifact `name` of run `run_id`, or None."""
        if (run_id, name) not in self.artifact_ids:
            self.artifact_ids[(run_id, name)] = self.github.artifact_id(run_id, name)
        return self.artifact_ids[(run_id, name)]

    def read_result(self, artifact_id, kind):
        """The JSON result of kind `kind` in the artifact `artifact_id`, or None if there is none or it does not
        parse."""
        if artifact_id is None:
            return None
        if artifact_id not in self.results:
            data = self.github.artifact_file(artifact_id, ARTIFACTS[kind] + '.json')
            try:
                result = json.loads(data.decode('utf-8')) if data is not None else None
            except (ValueError, RecursionError):
                result = None
            self.results[artifact_id] = result if isinstance(result, dict) else None
        return self.results[artifact_id]

    def result(self, run_id, kind):
        """The JSON result of kind `kind` of run `run_id`, or None if there is none or it does not parse."""
        return self.read_result(self.artifact(run_id, ARTIFACTS[kind]), kind)

    def corpus_errors(self, artifact_id):
        """The full list of parser errors in corpus-report.json of the compat-otp-logs artifact `artifact_id`, or the
        reason why there is none."""
        if artifact_id is None:
            return 'the run has no artifact %s' % LOGS_ARTIFACT
        data = self.github.artifact_file(artifact_id, CORPUS_REPORT)
        if data is None:
            return 'the artifact %s (%d) has no %s' % (LOGS_ARTIFACT, artifact_id, CORPUS_REPORT)
        try:
            errors = json.loads(data.decode('utf-8')).get('errors')
        except (ValueError, AttributeError, RecursionError):
            return '%s of the artifact %d is not a JSON object' % (CORPUS_REPORT, artifact_id)
        if not isinstance(errors, list):
            return '%s of the artifact %d has no list of errors' % (CORPUS_REPORT, artifact_id)
        # The same description as in the result, which compat.py cuts to compat.TEXT_LIMIT characters.
        return [dict(e, description=compat.text(e.get('description'))) for e in errors if isinstance(e, dict)]

    @staticmethod
    def matches(result, kind, sha, version, run_id):
        """Whether `result` is the result of `version` (the empty string for the baseline) on commit `sha` by run
        `run_id`."""
        resolved, inputs = mapping(result.get('resolved')), mapping(result.get('inputs'))
        if result.get('job') != kind or resolved.get('commit') != sha or inputs.get('ref') != sha:
            return False
        if mapping(result.get('run')).get('runId') not in (None, run_id) or inputs.get(kind) != version:
            return False
        if not version:
            return True
        if kind == 'otp':
            return mapping(resolved.get('otp')).get('version') == version
        return build_number(mapping(resolved.get('ide')).get('buildNumber')) == version

    def timed_out(self, record):
        return self.now - parse_time(record['at']) > RUN_TIMEOUT

    def finished_run(self, run_id):
        """'running', 'done', or why the run cannot give a result."""
        run = self.github.run(run_id)
        if run is None:
            return 'the run %d does not exist' % run_id
        if str(run.get('path', '')).split('@')[0] != WORKFLOW_PATH or run.get('event') != 'workflow_dispatch':
            return 'the run %d is not a workflow_dispatch run of %s' % (run_id, WORKFLOW_PATH)
        return 'done' if run.get('status') == 'completed' else 'running'

    def recover(self, record, kind=None):
        """The id of the run that was started for `record` (a version of `kind`, or a baseline) and whose id is not
        known, found by the content of its result; None if no finished run has it."""
        at = parse_time(record['at'])
        known = self.known_runs()
        for run in sorted(self.github.runs_created(at - RECOVERY_WINDOW[0], at + RECOVERY_WINDOW[1]),
                          key=lambda r: r.get('id', 0) if isinstance(r, dict) else 0):
            run_id = run.get('id') if isinstance(run, dict) else None
            if (not isinstance(run_id, int) or run_id in known or run.get('status') != 'completed'
                    or str(run.get('path', '')).split('@')[0] != WORKFLOW_PATH):
                continue
            for result_kind in ([kind] if kind else sorted(KINDS)):
                result = self.result(run_id, result_kind)
                if result and self.matches(result, result_kind, record['sha'],
                                           record['version'] if kind else '', run_id):
                    self.log('Found run %d of %s by its result' % (run_id, record.get('version', 'the baseline')))
                    return run_id
        return None

    def collect(self):
        for baseline in list(self.state['baselines']):
            if baseline['state'] in ACTIVE:
                self.reading(baseline, self.collect_baseline, self.baseline_failed, 'baseline of %s' % baseline['sha'])
        self.apply_baseline()
        for entry in reversed(list(self.state['entries'])):
            if entry['state'] in ACTIVE + (REPORT,):
                self.reading(entry, self.collect_entry, self.attempt_failed,
                             '%s %s' % (KINDS[entry['kind']], entry['version']))

    def reading(self, record, collect, give_up, what):
        """Collects `record`. A request that fails is an error of this cycle and is tried again in the next one;
        after MAX_COLLECT_ERRORS cycles in a row the run counts as an attempt without a result. So does a result
        that the script cannot handle: one record never stops the cycle of the others."""
        try:
            collect(record)
            record['errors'] = 0
        except Exception as e:
            # Only the type of an unexpected exception: its text may come from a result.
            self.error('%s: %s' % (what, e if isinstance(e, ApiError) else 'unexpected %s' % type(e).__name__))
            record['errors'] += 1
            if record['errors'] >= MAX_COLLECT_ERRORS:
                give_up(record, 'its result could not be read in %d cycles in a row' % MAX_COLLECT_ERRORS)

    def unusable(self, result, kind, sha, version, run_id):
        """Why `result` is not the complete result of `version` (the empty string for the baseline) on commit `sha`
        by run `run_id`; None if it is."""
        if result is None:
            return 'run %d has no result artifact %s' % (run_id, ARTIFACTS[kind])
        if result.get('schemaVersion') != compat.SCHEMA_VERSION:
            self.error('the result %s of run %d has a schemaVersion that %s does not know (it knows %d)'
                       % (ARTIFACTS[kind], run_id, os.path.basename(__file__), compat.SCHEMA_VERSION))
            return 'the result of run %d has an unknown schemaVersion' % run_id
        if not self.matches(result, kind, sha, version, run_id):
            return 'the result of run %d is not the one of %s on %s' % (run_id, version or 'the default inputs', sha)
        statuses = (compat.PASSED, compat.FAILED, compat.NOT_RUN)
        missing = [name for name in CHECKS[kind] if checks_of(result).get(name, {}).get('status') not in statuses]
        if missing:
            # Without them the comparison would find nothing and call it the same.
            return 'the result of run %d has no check %s' % (run_id, ', '.join(missing))
        if result.get('status') == compat.INCOMPLETE:
            return 'the result of run %d is incomplete' % run_id
        return None

    def baseline_failed(self, baseline, reason):
        """An attempt of the baseline without a complete result. After MAX_ATTEMPTS no baseline is started for its
        commit any more, and the cycle is red."""
        baseline.update(state=FAILED, artifacts=None, errors=0)
        message = 'Baseline of %s: attempt %d without a complete result: %s' % (baseline['sha'], baseline['tries'],
                                                                               reason)
        if baseline['tries'] >= MAX_ATTEMPTS:
            self.error('%s; no baseline is started for this commit any more' % message)
        else:
            self.log(message)

    def collect_baseline(self, baseline):
        if baseline['run'] is None:
            baseline['run'] = self.recover(baseline)
            if baseline['run'] is None:
                if self.timed_out(baseline):
                    self.baseline_failed(baseline, 'no run with its result was found')
                return
            baseline['state'] = RUNNING
        status = self.finished_run(baseline['run'])
        if status == 'running':
            if self.timed_out(baseline):
                self.baseline_failed(baseline, 'run %d did not finish within %s' % (baseline['run'], RUN_TIMEOUT))
            return
        if status != 'done':
            self.baseline_failed(baseline, status)
            return
        artifacts = {kind: self.artifact(baseline['run'], ARTIFACTS[kind]) for kind in KINDS}
        results = {kind: self.read_result(artifacts[kind], kind) for kind in KINDS}
        problems = [self.unusable(results[kind], kind, baseline['sha'], '', baseline['run']) for kind in sorted(KINDS)]
        problems = [problem for problem in problems if problem]
        ide = build_number(mapping(mapping(mapping(results['ide']).get('resolved')).get('ide')).get('buildNumber'))
        otp = mapping(mapping(mapping(results['otp']).get('resolved')).get('otp')).get('version')
        if not problems and not (valid_version('ide', ide) and valid_version('otp', otp)):
            problems = ['the results of run %d name no valid version' % baseline['run']]
        if problems:
            self.baseline_failed(baseline, '; '.join(problems))
            return
        # A comparison reads exactly these artifacts, whatever a later attempt of the run uploads.
        artifacts['logs'] = self.artifact(baseline['run'], LOGS_ARTIFACT)
        baseline.update(state=COMPLETE, ide=ide, otp=otp, artifacts=artifacts)
        self.log('Baseline of %s (run %d): complete, IntelliJ IDEA %s, Erlang/OTP %s'
                 % (baseline['sha'], baseline['run'], ide, otp))

    def apply_baseline(self):
        """The branch of the IDE that the plugin is built against, from the latest complete baseline: older branches
        are not watched, and its very build is the baseline."""
        latest = self.latest_baseline()
        if latest is None:
            return
        floor = max(self.state['floor']['ide'] or 0, group(latest['ide']))
        self.state['floor']['ide'] = floor
        for entry in list(self.state['entries']):
            if entry['kind'] == 'ide' and entry['state'] == QUEUED:
                if group(entry['version']) < floor:
                    self.state['entries'].remove(entry)
                elif entry['version'] == latest['ide']:
                    entry.update(state=DONE, verdict=BASELINE)
        self.state['seen']['ide'] = [v for v in self.state['seen']['ide'] if group(v) >= floor]

    def attempt_failed(self, entry, reason):
        self.log('%s %s: attempt %d without a result: %s' % (KINDS[entry['kind']], entry['version'], entry['tries'],
                                                              reason))
        entry.update(sha=None, at=None, run=None, errors=0)
        if entry['tries'] >= MAX_ATTEMPTS:
            entry.update(state=DONE, verdict=UNCHECKED)
            self.error('%s %s: unchecked, %d attempts gave no result' % (KINDS[entry['kind']], entry['version'],
                                                                         MAX_ATTEMPTS))
        else:
            entry['state'] = QUEUED

    def check_again(self, entry, reason):
        """The version is checked again without losing an attempt: its own run gave a result."""
        self.log('%s %s: %s; checked again' % (KINDS[entry['kind']], entry['version'], reason))
        entry.update(state=QUEUED, sha=None, at=None, run=None, errors=0, tries=max(entry['tries'] - 1, 0))

    def collect_entry(self, entry):
        kind, version = entry['kind'], entry['version']
        if entry['state'] == STARTING:
            run_id = self.recover(entry, kind)
            if run_id is None:
                if self.timed_out(entry):
                    self.attempt_failed(entry, 'no run of %s with its result was found' % WORKFLOW)
                return
            entry.update(state=RUNNING, run=run_id)
            entry['runs'].append(run_id)
        if entry['state'] == RUNNING:
            status = self.finished_run(entry['run'])
            if status == 'running':
                if self.timed_out(entry):
                    self.attempt_failed(entry, 'run %d did not finish within %s' % (entry['run'], RUN_TIMEOUT))
                return
            if status != 'done':
                self.attempt_failed(entry, status)
                return
        result = self.result(entry['run'], kind)
        reason = self.unusable(result, kind, entry['sha'], version, entry['run'])
        if reason:
            self.attempt_failed(entry, reason)
            return
        baseline = self.baseline_of(entry['sha'])
        if baseline is not None and baseline['state'] in ACTIVE:
            self.log('%s %s: run %d finished; waiting for the baseline' % (KINDS[kind], version, entry['run']))
            return
        if baseline is None or baseline['state'] != COMPLETE:
            self.check_again(entry, 'there is no complete baseline of commit %s' % entry['sha'])
            return
        base = self.read_result(baseline['artifacts'][kind], kind)
        if base is None:
            # Its artifacts expired or were deleted: a new baseline is started, with its attempts anew.
            baseline.update(state=FAILED, artifacts=None, tries=0)
            self.check_again(entry, 'the result of the baseline run %d cannot be read any more' % baseline['run'])
            return
        full = {}
        if trimmed_list(checks_of(result).get('corpus', {}).get('errors'))[2]:
            full['result'] = self.corpus_errors(self.artifact(entry['run'], LOGS_ARTIFACT))
        if trimmed_list(checks_of(base).get('corpus', {}).get('errors'))[2]:
            full['baseline'] = self.corpus_errors(baseline['artifacts']['logs'])
        comparison = compare(kind, result, base, full)
        if comparison['notes']:
            # Neither the same nor new problems: a list was trimmed and its full form could not be read.
            self.attempt_failed(entry, 'the comparison of run %d is not complete: %s' % (
                entry['run'], safe('; '.join(comparison['notes']), 1000)))
            return
        sections = comparison['sections']
        if not sections:
            entry.update(state=DONE, verdict=SAME)
            self.log('%s %s: the same problems as the baseline' % (KINDS[kind], version))
            return
        line = self.state['reported'][kind].get(str(group(version)))
        known = set(line['keys']) if line else set()
        fresh = [(check, title, [p for p in problems if p.key not in known]) for check, title, problems in sections]
        fresh = [section for section in fresh if section[2]]
        if not fresh:
            entry.update(state=DONE, verdict=KNOWN_PROBLEMS, issue=line['issue'])
            self.log('%s %s: only problems that #%d reported' % (KINDS[kind], version, line['issue']))
            return
        entry['state'] = REPORT
        self.report(entry, fresh, baseline, line['issue'] if line else None)

    def find_version_issue(self, kind, version):
        """The issue by BOT with the label whose body begins with the marker of the version."""
        if self.version_issues is None:
            self.version_issues = [i for i in self.github.issues(ISSUE_LABEL) if is_ours(i)]
        tag = marker(kind, version)
        return next((i['number'] for i in self.version_issues
                     if (i.get('body') or '').replace('\r\n', '\n').split('\n', 1)[0] == tag), None)

    def reported(self, entry, sections, number):
        """Issue `number` reported `sections`: no later version of the same major release or branch reports them
        again."""
        line = self.state['reported'][entry['kind']].setdefault(str(group(entry['version'])), {'issue': number,
                                                                                               'keys': []})
        line['issue'] = number
        line['keys'] += [p.key for _, _, problems in sections for p in problems if p.key not in line['keys']]
        entry.update(state=DONE, verdict=NEW_PROBLEMS, issue=number)

    def report(self, entry, sections, baseline, earlier):
        what = '%s %s' % (KINDS[entry['kind']], entry['version'])
        try:
            existing = self.find_version_issue(entry['kind'], entry['version'])
            if existing:
                self.reported(entry, sections, existing)
                self.log('%s: new problems, already reported in #%d' % (what, existing))
                return
            if self.filed >= MAX_ISSUES_PER_CYCLE:
                self.log('%s: new problems; the issue waits for the next cycle' % what)
                return
            title = issue_title(entry)
            for items in (ISSUE_ITEMS, 10, 3):
                body = issue_body(entry, sections, self.run_url(entry['run']), self.run_url(baseline['run']), items,
                                  earlier)
                if len(body) <= ISSUE_BODY_TARGET:
                    break
            self.filed += 1
            if self.dry_run:
                self.log('[dry run] would file the issue "%s":' % title)
                print_untrusted(body, self.out)
                return
            number = self.github.create_issue(title, body, ISSUE_LABEL)
        except ApiError as e:
            # The result was read: the issue is filed in a later cycle.
            self.error('%s: the issue could not be filed: %s' % (what, e))
            return
        self.version_issues.append({'number': number, 'body': body})
        self.reported(entry, sections, number)
        self.log('%s: new problems, filed #%d' % (what, number))

    # 3. Discover

    def discover(self):
        pinned = self.pins['OTP_VERSION']
        self.state['floor']['otp'] = max(self.state['floor']['otp'] or 0, group(pinned))
        for kind, read in (('otp', read_otp_feed), ('ide', read_ide_feed)):
            try:
                versions, rejected = read(self.http)
            except SourceError as e:
                self.error('%s: %s; no %s version is discovered in this cycle' % (KINDS[kind], e, KINDS[kind]))
                continue
            if rejected:
                # The rejected strings are not printed: nothing from a feed but a valid version reaches the log.
                print('::warning::%s: %d entries without a valid version were ignored' % (KINDS[kind], rejected),
                      file=self.out)
            self.discover_kind(kind, versions)

    def is_baseline(self, kind, version):
        if kind == 'otp':
            return version == self.pins['OTP_VERSION']
        latest = self.latest_baseline()
        return latest is not None and version == latest['ide']

    def add_entry(self, kind, version, name, verdict=None):
        if any(e['kind'] == kind and e['version'] == version for e in self.state['entries']):
            return
        entry = dict(ENTRY_DEFAULTS, kind=kind, version=version, name=name if kind == 'ide' else None,
                     seen=self.now.strftime('%Y-%m-%d'), state=DONE if verdict else QUEUED, verdict=verdict, runs=[])
        self.state['entries'].insert(0, entry)
        self.log('%s %s: %s' % (KINDS[kind], display_version(entry), verdict or 'new, queued'))

    def queue_newest(self, kind, versions, superseding, known=()):
        """Queues the newest of `versions` in each group; with `superseding`, the older ones and the queued ones of
        the group that are older are recorded as superseded, and so are all of them when the state already has a
        newer version of the group (`known`): an older version that appears later is never checked."""
        groups = collections.defaultdict(list)
        for version in versions:
            groups[group(version)].append(version)
        for number, members in sorted(groups.items()):
            members.sort(key=order)
            newest = members[-1]
            if superseding:
                top = max((order(v) for v in known if group(v) == number), default=None)
                for older in members[:-1] if top is None or top < order(newest) else members:
                    self.add_entry(kind, older, versions[older], SUPERSEDED)
                if top is not None and order(newest) < top:
                    continue
                for entry in self.state['entries']:
                    if (entry['kind'] == kind and entry['state'] == QUEUED and group(entry['version']) == group(newest)
                            and order(entry['version']) < order(newest)):
                        entry.update(state=DONE, verdict=SUPERSEDED)
            self.add_entry(kind, newest, versions[newest], BASELINE if self.is_baseline(kind, newest) else None)

    def discover_kind(self, kind, versions):
        state = self.state
        floor = state['floor'][kind]
        seen = set(state['seen'][kind])
        known = seen | {e['version'] for e in state['entries'] if e['kind'] == kind}
        if kind in state['pending']:
            if floor is None:
                # Seeded once a baseline names the branch; until then nothing is recorded.
                self.log('%s: %d versions listed; seeding waits until a baseline names the branch of the IDE'
                         % (KINDS[kind], len(versions)))
            else:
                watched = {v: n for v, n in versions.items() if group(v) >= floor}
                seen.update(watched)
                self.log('%s: seeding, %d watched versions recorded as seen' % (KINDS[kind], len(watched)))
                self.queue_newest(kind, watched, superseding=False)
                state['pending'].remove(kind)
        else:
            new = {v: n for v, n in versions.items() if v not in seen and (floor is None or group(v) >= floor)}
            seen.update(new)
            self.log('%s: %d new versions' % (KINDS[kind], len(new)))
            self.queue_newest(kind, new, superseding=True, known=known)
        if floor is not None:
            seen = {v for v in seen if group(v) >= floor}
            for entry in list(state['entries']):
                if entry['kind'] == kind and entry['state'] == QUEUED and group(entry['version']) < floor:
                    state['entries'].remove(entry)
        state['seen'][kind] = sorted(seen, key=order)

    # 4. Start

    def start_runs(self):
        state = self.state
        queued = [e for e in reversed(state['entries']) if e['state'] == QUEUED
                  and (e['kind'] != 'ide' or state['floor']['ide'] is not None)]
        active_baselines = [b for b in state['baselines'] if b['state'] in ACTIVE]
        need_branch = 'ide' in state['pending'] and state['floor']['ide'] is None and not active_baselines
        if not queued and not need_branch:
            return
        in_flight = len(active_baselines) + sum(1 for e in state['entries'] if e['state'] in ACTIVE)
        room = min(MAX_STARTS_PER_CYCLE, MAX_IN_FLIGHT - in_flight)
        try:
            branch = self.github.default_branch()
            sha = self.github.head_commit(branch)
        except ApiError as e:
            self.error('no run is started: %s' % e)
            return
        baseline = self.baseline_of(sha)
        if baseline is not None and baseline['state'] == FAILED and baseline['tries'] >= MAX_ATTEMPTS:
            self.error('no run is started: the baseline of %s gave no complete result in %d attempts; the watcher '
                       'waits for a new commit on %s' % (sha, MAX_ATTEMPTS, branch))
            return
        need_baseline = (baseline is None or baseline['state'] == FAILED
                         or (baseline['state'] == COMPLETE and self.now - parse_time(baseline['at']) > BASELINE_MAX_AGE))
        versions = queued[:max(room - (1 if need_baseline else 0), 0)]
        if room <= 0 or (queued and not versions) or (not queued and not need_baseline):
            if queued:
                self.log('%d versions wait: %d runs are in flight' % (len(queued), in_flight))
            return
        starts, previous = [], None
        if need_baseline:
            previous = baseline
            if previous is not None:
                state['baselines'].remove(previous)
            # A baseline whose result is gone or old starts its attempts anew.
            tries = previous['tries'] + 1 if previous is not None and previous['state'] == FAILED else 1
            baseline = dict(BASELINE_DEFAULTS, sha=sha, at=iso(self.now), state=STARTING, tries=tries)
            state['baselines'].insert(0, baseline)
            starts.append((baseline, {'ref': sha}))
        for entry in versions:
            entry.update(state=STARTING, sha=sha, at=iso(self.now), run=None, tries=entry['tries'] + 1)
            starts.append((entry, {entry['kind']: entry['version'], 'ref': sha}))
        if len(queued) > len(versions):
            self.log('%d versions wait for the next cycle' % (len(queued) - len(versions)))
        # Before the runs start, so that a crash does not start them twice: a version that is starting is found by
        # the content of its result in the next cycles.
        self.write_state('before starting the runs')
        for index, (record, inputs) in enumerate(starts):
            what = record.get('version') or 'the baseline'
            if self.dry_run:
                self.log('[dry run] would start %s on %s with the inputs %s' % (WORKFLOW, branch, json.dumps(inputs)))
                self.started.append(inputs)
                continue
            try:
                run_id = self.github.dispatch(branch, inputs)
            except ApiError as e:
                self.error('could not start %s for %s: %s' % (WORKFLOW, what, e))
                # A refused request started nothing; after any other failure the run is searched by its result.
                not_started = starts[index:] if e.status is not None and 400 <= e.status < 500 else starts[index + 1:]
                for other, _ in not_started:
                    if other is baseline and 'version' not in other:
                        state['baselines'].remove(other)
                        if previous is not None:
                            state['baselines'].insert(0, previous)
                    else:
                        other.update(state=QUEUED, sha=None, at=None, tries=other['tries'] - 1)
                break
            self.started.append(inputs)
            if run_id is None:
                self.log('Started %s for %s; its run is found by its result' % (WORKFLOW, what))
                continue
            record.update(state=RUNNING, run=run_id)
            if 'version' in record:
                record['runs'].append(run_id)
            self.log('Started %s for %s: %s' % (WORKFLOW, what, self.run_url(run_id)))
        self.write_state('after starting the runs')


def main(argv=None, http=None, now=None, out=None):
    parser = argparse.ArgumentParser(description=__doc__.split('\n', 1)[0])
    parser.add_argument('--dry-run', action='store_true', help='read everything, write nothing')
    parser.add_argument('--gradle-yml', default=os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..',
                                                             '.github', 'workflows', 'gradle.yml'),
                        help='the gradle.yml whose OTP_VERSION is the oldest watched major release')
    args = parser.parse_args(argv)
    out = out or sys.stdout
    repository = os.environ.get('GITHUB_REPOSITORY', '')
    try:
        if not REPOSITORY.fullmatch(repository):
            raise WatchError('GITHUB_REPOSITORY is not owner/name')
        try:
            pins = compat.validate_pins(compat.read_pins(args.gradle_yml))
        except (compat.InfrastructureError, compat.InputError) as e:
            raise WatchError(str(e))
        http = http or Http(os.environ.get('GH_TOKEN'))
        github = GitHub(http, repository, os.environ.get('GITHUB_API_URL', 'https://api.github.com'))
        watcher = Watcher(github, http, pins, now or datetime.datetime.now(datetime.timezone.utc),
                          os.environ.get('GITHUB_SERVER_URL', 'https://github.com'), args.dry_run, out)
        if args.dry_run:
            print('Dry run: nothing is started, written or filed.', file=out)
        return watcher.cycle()
    except (WatchError, ApiError) as e:
        print('::error::%s' % e, file=out)
        return 1


if __name__ == '__main__':
    sys.exit(main())
