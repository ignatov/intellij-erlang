#!/usr/bin/env python3
"""Copies the exception reports of the plugin on JetBrains Marketplace into GitHub issues.

The workflow .github/workflows/marketplace-exceptions.yml runs this script once a day. The Exceptions tab of the
plugin on Marketplace groups the reports that users send from the IDE. The script reads the groups and makes one
GitHub issue per group, so that the bug-fixing factory can fix them.

Marketplace (no documentation; the paths come from the code of the Marketplace web page):
  GET /api/plugins/{id}/exceptions           the groups, JSON. It can answer with a redirect to a temporary storage
                                             link. The script follows it without the Authorization header and never
                                             prints that link, because it is a credential while it is valid.
  GET /api/plugins/{id}/exceptions/resolved  the titles of the groups that are resolved on Marketplace, JSON.
  GET /api/plugins/{id}/exceptions/{reportId}/stacktrace
                                             the stack trace of one report, text. At most one per new or changed group.
The script never sends a POST to Marketplace. The token is read from the environment variable MARKETPLACE_TOKEN.

GitHub: the REST API with GITHUB_TOKEN. One list call (with pages) finds the issues with the label
source:marketplace-exception, open and closed. Each issue has two hidden markers at the start of its body: the key
of the group (the first 12 hex digits of the sha1 of the title) and the state (the report count and the plugin
versions that the body shows).

Rules:
  - A group is filed only when a report comes from a plugin version >= --min-version. Groups that are resolved on
    Marketplace and freeze groups (titles that start with "F:") are skipped.
  - A known group that has more reports or new plugin versions gets a new body and one short comment. A closed issue
    is reopened only when a report comes from a plugin version newer than the newest version in its body; else it is
    left closed and gets no comment.
  - At most --max-new new issues and --max-updates updates per run, the groups with the most reports first. The rest
    waits for the next run. A second run right after the first changes nothing.
  - Privacy: only the title, the report count, the plugin versions, the IDE builds, the OS names, the first and the
    last report date and one stack trace go into an issue. The exception messages are removed from the trace; home
    paths, e-mail addresses, mentions and issue references are made harmless. Other fields are never copied.

Modes:
  --probe    only check access: print the HTTP status of /exceptions/resolved and the top-level JSON type and keys.
  --dry-run  print what a run would do and the body of the first new issue; write nothing.
  --run      write the issues.
--fixture DIR reads Marketplace from files instead (exceptions.json, resolved.json, stacktrace/<reportId>.txt, and
optionally issues.json for the existing issues); it is for local checks with --dry-run only.

Python 3 standard library only.
"""

import argparse
import datetime
import hashlib
import io
import json
import os
import re
import sys
import urllib.error
import urllib.parse
import urllib.request

MARKETPLACE = 'https://plugins.jetbrains.com/api'
GITHUB = 'https://api.github.com'
PLUGIN_ID = '7083'
REPOSITORY = 'ignatov/intellij-erlang'
DASHBOARD = 'https://plugins.jetbrains.com/plugin/7083-erlang/edit/exception-analyzer'
USER_AGENT = 'intellij-erlang-marketplace-exceptions'

LABEL = 'source:marketplace-exception'
LABEL_COLOR = 'c5def5'
LABEL_DESCRIPTION = 'Copied from the exception reports of the plugin on JetBrains Marketplace'
BUG_LABEL = 'Bug'

DEFAULT_MIN_VERSION = '0.11.1300'
DEFAULT_MAX_NEW = 5
DEFAULT_MAX_UPDATES = 20

TITLE_PREFIX = 'Marketplace exception: '
TITLE_LIMIT = 120
KEY_MARKER = re.compile(r'<!-- marketplace-ea: ([0-9a-f]{12}) -->')
STATE_MARKER = re.compile(r'<!-- marketplace-ea-state: (\{[^<>]*\}) -->')
TRACE_START = '<!-- marketplace-ea-trace -->'
TRACE_END = '<!-- /marketplace-ea-trace -->'
MESSAGE_REMOVED = '<message removed>'
TRACE_LINE_LIMIT = 80
TRACE_CHAR_LIMIT = 15000
FRAME_CHAR_LIMIT = 300
LIST_LIMIT = 20
LIST_KEYS = ('issues', 'exceptions', 'groups', 'items')
REDIRECTS = (301, 302, 303, 307, 308)
TIMEOUT = 30


class Failure(Exception):
    """An error that ends the run with a message. The message never holds a token, a storage link or report data."""


# ---------------------------------------------------------------- HTTP


class Response:
    def __init__(self, status, headers, body):
        self.status = status
        self.headers = {name.lower(): value for name, value in (headers or {}).items()}
        self.body = body

    def json(self, what):
        try:
            return json.loads(self.body.decode('utf-8'))
        except (UnicodeDecodeError, ValueError):
            raise Failure('%s is not JSON' % what)


class _NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


class Http:
    """Sends one request and returns the response as it is: redirects are not followed, error codes are returned."""

    def __init__(self):
        self.opener = urllib.request.build_opener(_NoRedirect)

    def request(self, method, url, headers=None, data=None):
        request = urllib.request.Request(url, data=data, method=method, headers=dict(headers or {}))
        try:
            with self.opener.open(request, timeout=TIMEOUT) as response:
                return Response(response.status, dict(response.headers.items()), response.read())
        except urllib.error.HTTPError as error:
            return Response(error.code, dict(error.headers.items()) if error.headers else {}, error.read() or b'')
        except (urllib.error.URLError, OSError) as error:
            reason = getattr(error, 'reason', error)
            raise Failure('%s %s failed: %s' % (method, urllib.parse.urlsplit(url).netloc, type(reason).__name__))


# ---------------------------------------------------------------- Marketplace


class Marketplace:
    def __init__(self, http, token, plugin_id=PLUGIN_ID, base=MARKETPLACE):
        self.http = http
        self.token = (token or '').strip()
        self.base = '%s/plugins/%s' % (base, plugin_id)

    def _get(self, path, what):
        """GETs base + path with the token. A redirect is followed without the token; its link is never shown."""
        headers = {'User-Agent': USER_AGENT, 'Authorization': 'Bearer ' + self.token}
        url = self.base + path
        response = self.http.request('GET', url, headers)
        hops = 0
        while response.status in REDIRECTS:
            hops += 1
            location = response.headers.get('location')
            if not location or hops > 3:
                raise Failure('%s: a redirect without a usable link' % what)
            url = urllib.parse.urljoin(url, location)
            if urllib.parse.urlsplit(url).scheme != 'https':
                raise Failure('%s: a redirect to a link that is not https' % what)
            response = self.http.request('GET', url, {'User-Agent': USER_AGENT})
            if response.status != 200 and response.status not in REDIRECTS:
                raise Failure('%s: the redirect target answered HTTP %d' % (what, response.status))
        return response

    def status(self, path, what):
        return self._get(path, what)

    def groups(self):
        response = self._get('/exceptions', 'the exception list')
        if response.status != 200:
            raise Failure('the exception list: HTTP %d' % response.status)
        return parse_groups(response.json('the exception list'))

    def resolved(self):
        response = self._get('/exceptions/resolved', 'the resolved list')
        if response.status != 200:
            raise Failure('the resolved list: HTTP %d' % response.status)
        return parse_resolved(response.json('the resolved list'))

    def stacktrace(self, report_id):
        """The text of the trace, or None when Marketplace has none for this report (HTTP 404)."""
        if not re.fullmatch(r'[0-9]+', report_id):
            raise Failure('a report id is not a number')
        response = self._get('/exceptions/%s/stacktrace' % report_id, 'a stack trace')
        if response.status == 404:
            return None
        if response.status != 200:
            raise Failure('a stack trace: HTTP %d' % response.status)
        return response.body.decode('utf-8', errors='replace')


class FixtureMarketplace:
    """Marketplace read from files, for local dry runs."""

    def __init__(self, directory):
        self.directory = directory

    def _read(self, name):
        with open(os.path.join(self.directory, name), 'rb') as file:
            return file.read()

    def groups(self):
        return parse_groups(Response(200, {}, self._read('exceptions.json')).json('exceptions.json'))

    def resolved(self):
        if not os.path.exists(os.path.join(self.directory, 'resolved.json')):
            return set()
        return parse_resolved(Response(200, {}, self._read('resolved.json')).json('resolved.json'))

    def stacktrace(self, report_id):
        path = os.path.join('stacktrace', '%s.txt' % report_id)
        if not os.path.exists(os.path.join(self.directory, path)):
            return None
        return self._read(path).decode('utf-8', errors='replace')


def keys_of(value):
    if isinstance(value, dict):
        return 'an object with the keys [%s]' % ', '.join(sorted(str(key) for key in value))
    if isinstance(value, list):
        return 'a list'
    return 'a %s' % type(value).__name__


class Group:
    def __init__(self, title, report_count, reports):
        self.title = title
        self.report_count = report_count
        self.reports = reports
        self.key = group_key(title)

    def versions(self):
        return sort_versions({report.plugin_version for report in self.reports if report.plugin_version})

    def newest_version(self):
        versions = self.versions()
        return versions[0] if versions else None

    def newest_report(self):
        if not self.reports:
            return None
        return max(self.reports, key=lambda report: (version_key(report.plugin_version), report.timestamp or '',
                                                     int(report.id)))


class Report:
    def __init__(self, report_id, timestamp, app_build, os_name, plugin_version):
        self.id = report_id
        self.timestamp = timestamp
        self.app_build = app_build
        self.os_name = os_name
        self.plugin_version = plugin_version


def parse_groups(data):
    """The groups of the exception list. Unknown fields are ignored. A shape it does not know is a Failure that names
    the keys it saw, never the values."""
    if isinstance(data, dict):
        lists = [key for key in LIST_KEYS if isinstance(data.get(key), list)]
        if len(lists) != 1:
            raise Failure('the exception list has an unknown shape: %s' % keys_of(data))
        data = data[lists[0]]
    if not isinstance(data, list):
        raise Failure('the exception list has an unknown shape: %s' % keys_of(data))
    groups = []
    for index, item in enumerate(data):
        where = 'group %d of the exception list' % (index + 1)
        if not isinstance(item, dict):
            raise Failure('%s is %s, not an object' % (where, keys_of(item)))
        title = item.get('title')
        reports = item.get('reports')
        if not isinstance(title, str) or not title.strip() or not isinstance(reports, list):
            raise Failure('%s has no title string or no reports list: %s' % (where, keys_of(item)))
        count = item.get('reportCount', len(reports))
        if isinstance(count, bool) or not isinstance(count, int) or count < 0:
            raise Failure('%s has a reportCount that is not a whole number' % where)
        groups.append(Group(title, max(count, len(reports)), [parse_report(report, where) for report in reports]))
    return groups


def parse_report(item, where):
    if not isinstance(item, dict):
        raise Failure('a report of %s is %s, not an object' % (where, keys_of(item)))
    report_id = item.get('id')
    if isinstance(report_id, bool) or not isinstance(report_id, (int, str)):
        # A float here may have lost precision: report ids are 64-bit.
        raise Failure('a report of %s has an id that is not an integer: %s' % (where, keys_of(item)))
    report_id = str(report_id)
    if not re.fullmatch(r'[0-9]{1,20}', report_id):
        raise Failure('a report of %s has an id that is not an integer' % where)
    return Report(report_id, parse_date(item.get('timestamp')), safe_value(item.get('appBuild')),
                  safe_value(item.get('osName')), safe_value(item.get('pluginVersion')))


def parse_resolved(data):
    if not isinstance(data, list) or not all(isinstance(item, str) for item in data):
        raise Failure('the resolved list has an unknown shape: %s' % (
            keys_of(data) if not isinstance(data, list) else 'a list with items that are not strings'))
    return set(data)


def parse_date(value):
    """YYYY-MM-DD of a timestamp in milliseconds or seconds since the epoch, or of an ISO 8601 string; else None."""
    if isinstance(value, str) and re.fullmatch(r'[0-9]+', value):
        value = int(value)
    if isinstance(value, bool):
        return None
    if isinstance(value, (int, float)):
        seconds = value / 1000 if value > 100000000000 else value
        try:
            return datetime.datetime.fromtimestamp(seconds, datetime.timezone.utc).strftime('%Y-%m-%d')
        except (OverflowError, OSError, ValueError):
            return None
    if isinstance(value, str):
        match = re.match(r'([0-9]{4}-[0-9]{2}-[0-9]{2})', value)
        return match.group(1) if match else None
    return None


SAFE_VALUE = re.compile(r'[A-Za-z0-9][A-Za-z0-9 ._()+/-]{0,63}')


def safe_value(value):
    """A short value such as a version, a build or an OS name, or None. Anything else is never copied."""
    if isinstance(value, (int, float)) and not isinstance(value, bool):
        value = str(value)
    if not isinstance(value, str):
        return None
    value = value.strip()
    if not value:
        return None
    return value if SAFE_VALUE.fullmatch(value) else '(unrecognised value)'


def version_key(version):
    if not version:
        return ()
    return tuple(int(part) for part in re.findall(r'[0-9]+', version))


def sort_versions(versions):
    return sorted(versions, key=lambda version: (version_key(version), version), reverse=True)


def group_key(title):
    return hashlib.sha1(title.encode('utf-8')).hexdigest()[:12]


# ---------------------------------------------------------------- redaction


HOME_PATHS = [
    (re.compile(r'/Users/[^/\s:;,)\]\'"]+'), '/Users/<user>'),
    (re.compile(r'/home/[^/\s:;,)\]\'"]+'), '/home/<user>'),
    (re.compile(r'\b([A-Za-z]):\\+Users\\+[^\\/\s:;,)\]\'"]+'), r'\1:\\Users\\<user>'),
    (re.compile(r'\b([A-Za-z]):/+Users/+[^\\/\s:;,)\]\'"]+'), r'\1:/Users/<user>'),
]
EMAIL = re.compile(r'[A-Za-z0-9._%+-]+@[A-Za-z0-9-]+(?:\.[A-Za-z0-9-]+)*\.[A-Za-z]{2,}')
ZERO_WIDTH = '​'


def redact(text):
    """Removes home paths, e-mail addresses, mentions, issue references, backticks and HTML comments from text."""
    text = EMAIL.sub('<e-mail removed>', text)
    for pattern, replacement in HOME_PATHS:
        text = pattern.sub(replacement, text)
    text = text.replace('`', "'")
    text = re.sub(r'@(?=[A-Za-z0-9_-])', '@' + ZERO_WIDTH, text)
    text = re.sub(r'#(?=[0-9])', '#' + ZERO_WIDTH, text)
    text = text.replace('<!--', '<!' + ZERO_WIDTH + '--').replace('-->', '--' + ZERO_WIDTH + '>')
    return text


def clean_title(title):
    title = re.sub(r'[\x00-\x1f\x7f]+', ' ', title)
    title = redact(re.sub(r'\s+', ' ', title).strip())
    if len(title) > TITLE_LIMIT:
        title = title[:TITLE_LIMIT - 1].rstrip() + '…'
    return title


CLASS_NAME = r'[A-Za-z_$][\w$]*(?:\.[A-Za-z_$][\w$]*)+'
HEADER = re.compile(r'(?P<cls>' + CLASS_NAME + r')(?P<rest>:.*)?')
NESTED = re.compile(r'(?P<indent>\s*)(?P<kind>Caused by|Suppressed): ?(?P<what>.*)')
FRAME = re.compile(r'\s*at\s+[\w$./<>@-]+\((?:[\w$.-]+(?::[0-9]+)?|Unknown Source|Native Method)\)\s*')
MORE = re.compile(r'\s*\.\.\. [0-9]+ (?:more|common frames omitted)\s*')


def redact_header(text):
    """The class name of an exception header; its message becomes MESSAGE_REMOVED."""
    match = HEADER.fullmatch(text.strip())
    if not match:
        return MESSAGE_REMOVED
    return match.group('cls') + (': ' + MESSAGE_REMOVED if match.group('rest') is not None else '')


def redact_trace(text):
    """Keeps the class names of the first line and of the "Caused by:" and "Suppressed:" lines, the frames and the
    "... N more" lines. Messages become MESSAGE_REMOVED; other lines (the rest of a message) are dropped."""
    lines = [line.rstrip() for line in text.replace('\r\n', '\n').replace('\r', '\n').split('\n')]
    lines = [re.sub(r'[\x00-\x08\x0b-\x1f\x7f]', '', line) for line in lines]
    result = []
    header_seen = False
    for line in lines:
        if not line.strip():
            continue
        nested = NESTED.fullmatch(line)
        if not header_seen:
            result.append(redact_header(line))
            header_seen = True
        elif nested:
            result.append('%s%s: %s' % (nested.group('indent'), nested.group('kind'), redact_header(nested.group('what'))))
        elif FRAME.fullmatch(line) or MORE.fullmatch(line):
            result.append(line[:FRAME_CHAR_LIMIT])
        if len(result) >= TRACE_LINE_LIMIT:
            result.append('\t... (cut)')
            break
    trace = redact('\n'.join(result))
    if len(trace) > TRACE_CHAR_LIMIT:
        trace = trace[:TRACE_CHAR_LIMIT] + '\n\t... (cut)'
    return trace


# ---------------------------------------------------------------- issues


def issue_title(group):
    return TITLE_PREFIX + clean_title(group.title)


def read_state(body):
    """The state marker of an issue body: {'count': int, 'versions': [str], 'newest': str or None}, or None."""
    match = STATE_MARKER.search(body or '')
    if not match:
        return None
    try:
        state = json.loads(match.group(1))
    except ValueError:
        return None
    if not isinstance(state, dict):
        return None
    count = state.get('count')
    versions = state.get('versions')
    if isinstance(count, bool) or not isinstance(count, int) or not isinstance(versions, list):
        return None
    versions = [version for version in versions if isinstance(version, str)]
    newest = state.get('newest')
    return {'count': count, 'versions': versions, 'newest': newest if isinstance(newest, str) else None}


def make_state(group):
    return {'count': group.report_count, 'versions': group.versions(), 'newest': group.newest_version()}


def read_key(body):
    match = KEY_MARKER.search(body or '')
    return match.group(1) if match else None


def read_trace(body):
    """The trace section of an earlier body, to keep it when the new trace cannot be read."""
    body = body or ''
    start = body.find(TRACE_START)
    end = body.find(TRACE_END)
    if start < 0 or end < start:
        return None
    return body[start + len(TRACE_START):end].strip('\n')


def listed(values):
    values = [value for value in values if value]
    if not values:
        return 'unknown'
    shown = ', '.join(values[:LIST_LIMIT])
    if len(values) > LIST_LIMIT:
        shown += ', and %d more' % (len(values) - LIST_LIMIT)
    return shown


def render_trace(report, trace):
    if trace is None:
        return 'The stack trace could not be read from Marketplace.'
    fence = '```'
    return ('Stack trace of the newest report from the newest plugin version (%s, %s, %s). '
            'The exception messages are removed.\n\n%stext\n%s\n%s' % (
                report.plugin_version or 'unknown version', report.app_build or 'unknown build',
                report.os_name or 'unknown OS', fence, trace, fence))


def render_body(group, trace_section):
    dates = sorted(report.timestamp for report in group.reports if report.timestamp)
    builds = sort_versions({report.app_build for report in group.reports if report.app_build})
    systems = sorted({report.os_name for report in group.reports if report.os_name})
    state = json.dumps(make_state(group), sort_keys=True, separators=(',', ':'))
    return '\n'.join([
        '<!-- marketplace-ea: %s -->' % group.key,
        '<!-- marketplace-ea-state: %s -->' % state,
        'Users of the plugin sent these exception reports from the IDE to JetBrains Marketplace. '
        'The workflow `marketplace-exceptions.yml` copies them here once a day and rewrites this text '
        'when new reports come.',
        '',
        '- Group: `%s`' % clean_title(group.title),
        '- Reports: %d' % group.report_count,
        '- First report: %s' % (dates[0] if dates else 'unknown'),
        '- Last report: %s' % (dates[-1] if dates else 'unknown'),
        '- Plugin versions: %s' % listed(group.versions()),
        '- IDE builds: %s' % listed(builds),
        '- Operating systems: %s' % listed(systems),
        '',
        TRACE_START,
        trace_section,
        TRACE_END,
        '',
        'The full reports are on the [Exceptions tab](%s) of the plugin on Marketplace (for the plugin owners only).'
        % DASHBOARD,
        '',
    ])


def reports_text(count):
    return '%d %s' % (count, 'report' if count == 1 else 'reports')


def update_comment(group, state, reopened):
    new_reports = group.report_count - state['count']
    new_versions = [version for version in group.versions() if version not in state['versions']]
    parts = []
    if reopened:
        parts.append('Reopened: a report came from plugin version %s, which is newer than %s, the newest version '
                     'when this issue was closed.' % (group.newest_version(), state['newest']))
    if new_reports > 0:
        parts.append('%d new %s (%d in total).' % (new_reports, 'report' if new_reports == 1 else 'reports',
                                                   group.report_count))
    parts.append('New plugin versions: %s.' % ', '.join(new_versions) if new_versions else 'No new plugin versions.')
    return ' '.join(parts)


class GitHub:
    def __init__(self, http, token, repository=REPOSITORY, base=GITHUB):
        self.http = http
        self.token = (token or '').strip()
        self.base = '%s/repos/%s' % (base, repository)

    def _call(self, method, path, payload=None, allowed=(200, 201)):
        headers = {'User-Agent': USER_AGENT, 'Accept': 'application/vnd.github+json',
                   'X-GitHub-Api-Version': '2022-11-28'}
        if self.token:
            headers['Authorization'] = 'Bearer ' + self.token
        data = None
        if payload is not None:
            data = json.dumps(payload).encode('utf-8')
            headers['Content-Type'] = 'application/json'
        response = self.http.request(method, self.base + path, headers, data)
        if response.status not in allowed:
            raise Failure('GitHub %s %s: HTTP %d' % (method, path.split('?')[0], response.status))
        return response

    def issues(self):
        """The issues with the label, open and closed: [{'number', 'state', 'body'}]."""
        found = []
        for page in range(1, 51):
            response = self._call('GET', '/issues?state=all&labels=%s&per_page=100&page=%d' % (
                urllib.parse.quote(LABEL, safe=''), page))
            items = response.json('the GitHub issue list')
            if not isinstance(items, list):
                raise Failure('the GitHub issue list is not a list')
            for item in items:
                if 'pull_request' not in item:
                    found.append({'number': item['number'], 'state': item['state'], 'body': item.get('body') or ''})
            if len(items) < 100:
                return found
        raise Failure('more than 5000 issues have the label %s' % LABEL)

    def ensure_label(self, name, color, description):
        response = self._call('GET', '/labels/%s' % urllib.parse.quote(name, safe=''), allowed=(200, 404))
        if response.status == 404:
            self._call('POST', '/labels', {'name': name, 'color': color, 'description': description})

    def create(self, title, body, labels):
        return self._call('POST', '/issues', {'title': title, 'body': body, 'labels': labels}).json(
            'the created issue')['number']

    def edit(self, number, body, reopen):
        payload = {'body': body}
        if reopen:
            payload['state'] = 'open'
        self._call('PATCH', '/issues/%d' % number, payload)

    def comment(self, number, body):
        self._call('POST', '/issues/%d/comments' % number, {'body': body})


class FixtureGitHub:
    """Existing issues read from a file; for local dry runs, so it writes nothing."""

    def __init__(self, path):
        self.path = path

    def issues(self):
        if not os.path.exists(self.path):
            return []
        with open(self.path, encoding='utf-8') as file:
            return [{'number': item['number'], 'state': item['state'], 'body': item.get('body') or ''}
                    for item in json.load(file)]


# ---------------------------------------------------------------- planning


class Action:
    def __init__(self, kind, group, issue=None, state=None):
        self.kind = kind  # 'create', 'update' or 'reopen'
        self.group = group
        self.issue = issue
        self.state = state


def plan(groups, resolved, issues, min_version, max_new, max_updates):
    """The actions of one run and the counts for the summary. Pure: reads nothing and writes nothing."""
    counts = {name: 0 for name in ('groups', 'resolved', 'freeze', 'old', 'duplicate', 'unchanged', 'closed',
                                   'waiting_new', 'waiting_updates')}
    by_key = {}
    for issue in sorted(issues, key=lambda issue: (issue['state'] != 'open', issue['number'])):
        key = read_key(issue['body'])
        if key and key not in by_key:
            by_key[key] = issue
    minimum = version_key(min_version)
    actions = []
    new = updates = 0
    seen = set()
    for group in sorted(groups, key=lambda group: (-group.report_count, group.title)):
        counts['groups'] += 1
        if group.key in seen:
            counts['duplicate'] += 1
            continue
        seen.add(group.key)
        if group.title in resolved:
            counts['resolved'] += 1
            continue
        if group.title.startswith('F:'):
            counts['freeze'] += 1
            continue
        if not any(version_key(version) >= minimum for version in group.versions()):
            counts['old'] += 1
            continue
        issue = by_key.get(group.key)
        if issue is None:
            if new < max_new:
                actions.append(Action('create', group))
                new += 1
            else:
                counts['waiting_new'] += 1
            continue
        state = read_state(issue['body']) or {'count': 0, 'versions': [], 'newest': None}
        changed = group.report_count > state['count'] or not set(group.versions()) <= set(state['versions'])
        if not changed:
            counts['unchanged'] += 1
            continue
        if issue['state'] != 'open':
            if state['newest'] is None or version_key(group.newest_version()) <= version_key(state['newest']):
                counts['closed'] += 1
                continue
            kind = 'reopen'
        else:
            kind = 'update'
        if updates < max_updates:
            actions.append(Action(kind, group, issue, state))
            updates += 1
        else:
            counts['waiting_updates'] += 1
    return actions, counts


def trace_section(marketplace, group, old_body=None):
    report = group.newest_report()
    if report is None:
        return 'No report of this group has a stack trace.'
    text = marketplace.stacktrace(report.id)
    if text is None or not text.strip():
        old = read_trace(old_body)
        if old:
            return old
        return render_trace(report, None)
    return render_trace(report, redact_trace(text))


# ---------------------------------------------------------------- main


def execute(marketplace, github, actions, write, out):
    """Carries out the actions (write=True) or prints them. Returns the lines of the summary."""
    lines = []
    first_body = None
    labels_ready = False
    for action in actions:
        group = action.group
        old_body = action.issue['body'] if action.issue else None
        body = render_body(group, trace_section(marketplace, group, old_body))
        if action.kind == 'create':
            title = issue_title(group)
            if first_body is None:
                first_body = body
            if write:
                if not labels_ready:
                    github.ensure_label(LABEL, LABEL_COLOR, LABEL_DESCRIPTION)
                    labels_ready = True
                number = github.create(title, body, [BUG_LABEL, LABEL])
                lines.append('- created #%d: %s (%s)' % (number, title, reports_text(group.report_count)))
            else:
                lines.append('- would create: %s (%s)' % (title, reports_text(group.report_count)))
        else:
            number = action.issue['number']
            comment = update_comment(group, action.state, action.kind == 'reopen')
            verb = 'reopened' if action.kind == 'reopen' else 'updated'
            if write:
                github.edit(number, body, action.kind == 'reopen')
                github.comment(number, comment)
                lines.append('- %s #%d: %s' % (verb, number, comment))
            else:
                lines.append('- would %s #%d: %s' % ('reopen' if action.kind == 'reopen' else 'update', number, comment))
    if not write and first_body is not None:
        out.write('Body of the first new issue:\n\n%s\n' % first_body)
    return lines


def summary(mode, counts, actions, lines):
    created = sum(1 for action in actions if action.kind == 'create')
    updated = sum(1 for action in actions if action.kind == 'update')
    reopened = sum(1 for action in actions if action.kind == 'reopen')
    text = [
        '### Marketplace exceptions (%s)' % mode,
        '',
        '| | |',
        '|---|---|',
        '| Groups on Marketplace | %d |' % counts['groups'],
        '| New issues | %d |' % created,
        '| Updated issues | %d |' % updated,
        '| Reopened issues | %d |' % reopened,
        '| Unchanged | %d |' % counts['unchanged'],
        '| Closed, no newer plugin version | %d |' % counts['closed'],
        '| Skipped: resolved on Marketplace | %d |' % counts['resolved'],
        '| Skipped: only plugin versions older than the minimum | %d |' % counts['old'],
        '| Skipped: freeze groups | %d |' % counts['freeze'],
        '| Waiting for the next run (new, updates) | %d, %d |' % (counts['waiting_new'], counts['waiting_updates']),
        '',
    ]
    return '\n'.join(text + lines) + '\n'


def probe(marketplace, out):
    response = marketplace.status('/exceptions/resolved', 'the resolved list')
    out.write('GET /exceptions/resolved: HTTP %d\n' % response.status)
    if response.status == 200:
        try:
            data = json.loads(response.body.decode('utf-8'))
        except (UnicodeDecodeError, ValueError):
            out.write('The answer is not JSON.\n')
            return 1
        if isinstance(data, list):
            types = sorted({type(item).__name__ for item in data})
            out.write('Top level: a list; item types: [%s]\n' % ', '.join(types))
        else:
            out.write('Top level: %s\n' % keys_of(data))
        return 0
    return 1


def write_summary(path, text):
    if path:
        with open(path, 'a', encoding='utf-8') as file:
            file.write(text)


def main(argv=None, environ=None, http=None, out=None):
    environ = os.environ if environ is None else environ
    out = out or sys.stdout
    parser = argparse.ArgumentParser(description='Copy the Marketplace exception reports into GitHub issues.')
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument('--probe', dest='mode', action='store_const', const='probe')
    mode.add_argument('--dry-run', dest='mode', action='store_const', const='dry-run')
    mode.add_argument('--run', dest='mode', action='store_const', const='run')
    parser.add_argument('--plugin', default=PLUGIN_ID)
    parser.add_argument('--repo', default=environ.get('GITHUB_REPOSITORY') or REPOSITORY)
    parser.add_argument('--min-version', default=environ.get('MARKETPLACE_MIN_VERSION') or DEFAULT_MIN_VERSION)
    parser.add_argument('--max-new', type=int, default=DEFAULT_MAX_NEW)
    parser.add_argument('--max-updates', type=int, default=DEFAULT_MAX_UPDATES)
    parser.add_argument('--fixture', help='read Marketplace and the existing issues from this directory (dry run only)')
    parser.add_argument('--summary', default=environ.get('GITHUB_STEP_SUMMARY'))
    args = parser.parse_args(argv)
    if not re.fullmatch(r'[0-9]+', args.plugin) or not re.fullmatch(r'[\w.-]+/[\w.-]+', args.repo):
        parser.error('--plugin must be a number and --repo owner/name')
    if not version_key(args.min_version):
        parser.error('--min-version must be a version such as %s' % DEFAULT_MIN_VERSION)
    http = http or Http()
    try:
        if args.fixture:
            if args.mode != 'dry-run':
                parser.error('--fixture works only with --dry-run')
            marketplace = FixtureMarketplace(args.fixture)
            github = FixtureGitHub(os.path.join(args.fixture, 'issues.json'))
        else:
            token = environ.get('MARKETPLACE_TOKEN', '')
            if not token.strip():
                raise Failure('MARKETPLACE_TOKEN is not set')
            marketplace = Marketplace(http, token, args.plugin)
            github = GitHub(http, environ.get('GITHUB_TOKEN', ''), args.repo)
            if args.mode == 'run' and not environ.get('GITHUB_TOKEN', '').strip():
                raise Failure('GITHUB_TOKEN is not set')
        if args.mode == 'probe':
            text = io.StringIO()
            code = probe(marketplace, text)
            out.write(text.getvalue())
            write_summary(args.summary, '### Marketplace exceptions (probe)\n\n```\n%s```\n' % text.getvalue())
            return code
        groups = marketplace.groups()
        resolved = marketplace.resolved()
        actions, counts = plan(groups, resolved, github.issues(), args.min_version, args.max_new, args.max_updates)
        lines = execute(marketplace, github, actions, args.mode == 'run', out)
        text = summary(args.mode, counts, actions, lines)
        out.write(text)
        write_summary(args.summary, text)
        return 0
    except Failure as failure:
        sys.stderr.write('error: %s\n' % failure)
        return 1


if __name__ == '__main__':
    sys.exit(main())
