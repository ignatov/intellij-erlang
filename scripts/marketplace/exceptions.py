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
of the group (the first 12 hex digits of the sha1 of the title) and the state: the report count and the plugin
versions that the body shows, what the comments have announced so far, and for a closed issue when it was closed and
the newest plugin release at that time.

Rules:
  - A group is filed only when a report comes from a plugin release (0.N.N) >= --min-version. Groups that are
    resolved on Marketplace, freeze groups (titles that start with "F:") and malformed groups are skipped and counted.
  - A known open issue whose group has more reports or new plugin versions gets a new body and one short comment.
    The body is written first and says that a comment is due; the comment carries a hidden marker; then the body says
    that it was posted. So a run that stops between the steps loses no comment and posts none twice.
  - The first run that sees an issue closed (as completed) stores the newest plugin release (the newest v0.N.N
    release of the repository) in the state. The issue is reopened only when a report comes from a release newer
    than that one. An issue closed for another reason (not planned, duplicate) is never reopened. A closed issue gets
    no comment unless it is reopened.
  - At most --max-new new issues and --max-updates other changes per run, the groups with the most reports first, and
    no new change after --time-budget seconds. The rest waits for the next run. A second run right after the first
    changes nothing.
  - Privacy: only the title, the report count, the plugin versions, the IDE builds, the OS names, the first and the
    last report date and one stack trace go into an issue. Each value must match a strict pattern, or it is counted
    and not shown. The title keeps only letters, digits and a few signs. The trace keeps only exception class names
    and stack frames of a strict shape; the messages are removed. Other fields are never copied.

Modes:
  --probe    only check access: print the HTTP status of /exceptions/resolved and the top-level JSON type and keys.
  --dry-run  print what a run would do and the body of the first new issue; write nothing.
  --run      write the issues.
--fixture DIR reads Marketplace from files instead (exceptions.json, resolved.json, stacktrace/<reportId>.txt, and
optionally issues.json and releases.json for GitHub); it is for local checks with --dry-run only.

Python 3 standard library only.
"""

import argparse
import datetime
import hashlib
import io
import json
import math
import os
import re
import sys
import time
import urllib.error
import urllib.parse
import urllib.request

MARKETPLACE = 'https://plugins.jetbrains.com/api'
TOKEN_HOST = 'plugins.jetbrains.com'
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
DEFAULT_TIME_BUDGET = 300

TITLE_PREFIX = 'Marketplace exception: '
TITLE_RAW_LIMIT = 200
TITLE_LIMIT = 120
KEY_MARKER = re.compile(r'<!-- marketplace-ea: ([0-9a-f]{12}) -->')
STATE_MARKER = re.compile(r'<!-- marketplace-ea-state: (\{[^<>]*\}) -->')
NOTE_MARKER = '<!-- marketplace-ea-note: %s -->'
TRACE_START = '<!-- marketplace-ea-trace -->'
TRACE_END = '<!-- /marketplace-ea-trace -->'
MESSAGE_REMOVED = '<message removed>'
REMOVED = '<removed>'
TRACE_LINE_LIMIT = 80
TRACE_CHAR_LIMIT = 15000
TRACE_INPUT_LIMIT = 1000000
TRACE_LINE_INPUT_LIMIT = 1000
LIST_LIMIT = 20
STATE_VERSION_LIMIT = 300
LIST_KEYS = ('issues', 'exceptions', 'groups', 'items')
REDIRECTS = (301, 302, 303, 307, 308)
TIMEOUT = 20
TOKEN_PATTERN = re.compile(r'[\x21-\x7e]+')

DISPLAY_VERSION = re.compile(r'[0-9]+(?:\.[0-9]+){1,3}')
RELEASE_VERSION = re.compile(r'0\.[0-9]+\.[0-9]+')
IDE_BUILD = re.compile(r'[A-Z]{2,3}-[0-9]+(?:\.[0-9]+)+')
OS_NAME = re.compile(r'(?:Windows|macOS|Mac OS X|Linux)(?: [0-9]+(?:\.[0-9]+)*)?')
OTHER_OS = 'other'
CLOSED_AT = re.compile(r'[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z')
MAX_SECONDS = 253402300799  # 9999-12-31T23:59:59Z


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
        except (UnicodeDecodeError, ValueError, RecursionError):
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


def checked_token(name, value, required):
    """The token without surrounding blanks. It must be printable ASCII; the message of a Failure never holds it."""
    value = (value or '').strip()
    if not value:
        if required:
            raise Failure('%s is not set' % name)
        return ''
    if not TOKEN_PATTERN.fullmatch(value):
        raise Failure('%s has characters that a token cannot have' % name)
    return value


# ---------------------------------------------------------------- Marketplace


class Marketplace:
    def __init__(self, http, token, plugin_id=PLUGIN_ID, base=MARKETPLACE):
        self.http = http
        self.token = checked_token('MARKETPLACE_TOKEN', token, True)
        self.base = '%s/plugins/%s' % (base, plugin_id)

    def _get(self, path, what):
        """GETs base + path with the token. A redirect is followed without the token; its link is never shown.
        The token can be the publishing token of the plugin, so it goes only to TOKEN_HOST over https."""
        url = self.base + path
        parts = urllib.parse.urlsplit(url)
        if parts.scheme != 'https' or parts.netloc != TOKEN_HOST:
            raise Failure('the Marketplace token is sent only to https://%s' % TOKEN_HOST)
        headers = {'User-Agent': USER_AGENT, 'Authorization': 'Bearer ' + self.token}
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
        """The text of the trace, or None when it could not be read (any status but 200, a failed redirect)."""
        if not re.fullmatch(r'[0-9]+', report_id):
            return None
        try:
            response = self._get('/exceptions/%s/stacktrace' % report_id, 'a stack trace')
        except Failure:
            return None
        if response.status != 200:
            return None
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
        if not re.fullmatch(r'[0-9]+', report_id) or not os.path.exists(os.path.join(self.directory, path)):
            return None
        return self._read(path).decode('utf-8', errors='replace')


def keys_of(value):
    if isinstance(value, dict):
        return 'an object with the keys [%s]' % ', '.join(sorted(clean_key(key) for key in value))
    if isinstance(value, list):
        return 'a list'
    return 'a %s' % type(value).__name__


def clean_key(key):
    return re.sub(r'[^A-Za-z0-9_.-]', '?', str(key))[:40]


# ---------------------------------------------------------------- the groups


class Group:
    def __init__(self, title, report_count, reports, hidden):
        self.title = title
        self.report_count = report_count
        self.reports = reports
        self.hidden = hidden  # field name -> the number of values that failed their pattern
        self.key = group_key(title)

    def versions(self):
        return sort_versions({report.plugin_version for report in self.reports if report.plugin_version})

    def releases(self):
        return [version for version in self.versions() if RELEASE_VERSION.fullmatch(version)]

    def newest_release(self):
        releases = self.releases()
        return releases[0] if releases else None

    def state_versions(self):
        return self.versions()[:STATE_VERSION_LIMIT]

    def newest_report(self):
        """The most recent report among those with a valid plugin version; else the most recent one."""
        candidates = [report for report in self.reports if report.plugin_version] or self.reports
        if not candidates:
            return None
        return max(candidates, key=lambda report: (report.timestamp or '', int(report.id)))


class Report:
    def __init__(self, report_id, timestamp, app_build, os_name, plugin_version):
        self.id = report_id
        self.timestamp = timestamp
        self.app_build = app_build
        self.os_name = os_name
        self.plugin_version = plugin_version


class Groups(list):
    """The groups of the exception list, with the number of malformed groups and reports that were skipped."""

    def __init__(self, groups, malformed_groups, malformed_reports):
        super().__init__(groups)
        self.malformed_groups = malformed_groups
        self.malformed_reports = malformed_reports


def parse_groups(data):
    """The groups of the exception list. Unknown fields are ignored. A malformed group or report is skipped and
    counted. Only a top-level shape it does not know is a Failure; it names the keys it saw, never the values."""
    if isinstance(data, dict):
        lists = [key for key in LIST_KEYS if isinstance(data.get(key), list)]
        if len(lists) != 1:
            raise Failure('the exception list has an unknown shape: %s' % keys_of(data))
        data = data[lists[0]]
    if not isinstance(data, list):
        raise Failure('the exception list has an unknown shape: %s' % keys_of(data))
    groups = []
    malformed_groups = malformed_reports = 0
    for item in data:
        if not isinstance(item, dict):
            malformed_groups += 1
            continue
        title = item.get('title')
        reports = item.get('reports')
        count = item.get('reportCount', len(reports) if isinstance(reports, list) else 0)
        if (not isinstance(title, str) or not title.strip() or not isinstance(reports, list)
                or isinstance(count, bool) or not isinstance(count, int) or count < 0):
            malformed_groups += 1
            continue
        title = title.encode('utf-8', errors='replace').decode('utf-8')
        hidden = {'pluginVersion': 0, 'appBuild': 0}
        parsed = []
        for report in reports:
            value = parse_report(report, hidden)
            if value is None:
                malformed_reports += 1
            else:
                parsed.append(value)
        groups.append(Group(title, max(count, len(parsed)), parsed, hidden))
    return Groups(groups, malformed_groups, malformed_reports)


def parse_report(item, hidden):
    """A Report, or None when the item is not an object or has no integer id. Report ids are 64-bit: a float id may
    have lost precision, so it is malformed too."""
    if not isinstance(item, dict):
        return None
    report_id = item.get('id')
    if isinstance(report_id, bool) or not isinstance(report_id, (int, str)):
        return None
    report_id = str(report_id)
    if not re.fullmatch(r'[0-9]{1,20}', report_id) or int(report_id) >= 2 ** 64:
        return None
    return Report(report_id, parse_time(item.get('timestamp')),
                  checked_value(item.get('appBuild'), IDE_BUILD, hidden, 'appBuild'),
                  os_name(item.get('osName')),
                  checked_value(item.get('pluginVersion'), DISPLAY_VERSION, hidden, 'pluginVersion'))


def checked_value(value, pattern, hidden, field):
    """The value when it is a string that matches the pattern; else None, and a present value is counted."""
    if value is None:
        return None
    if isinstance(value, str) and pattern.fullmatch(value.strip()):
        return value.strip()
    hidden[field] += 1
    return None


def os_name(value):
    if value is None:
        return None
    if isinstance(value, str) and OS_NAME.fullmatch(value.strip()):
        return value.strip()
    return OTHER_OS


def parse_time(value):
    """YYYY-MM-DDTHH:MM:SS of a timestamp in milliseconds or seconds since the epoch, or of an ISO 8601 string;
    else None."""
    if isinstance(value, str) and re.fullmatch(r'[0-9]{1,20}', value):
        value = int(value)
    if isinstance(value, bool):
        return None
    if isinstance(value, (int, float)):
        if isinstance(value, float) and not math.isfinite(value):
            return None
        if value < 0 or value > MAX_SECONDS * 1000:
            return None
        seconds = value // 1000 if value > 100000000000 else value
        try:
            return datetime.datetime.fromtimestamp(int(seconds), datetime.timezone.utc).strftime('%Y-%m-%dT%H:%M:%S')
        except (OverflowError, OSError, ValueError):
            return None
    if isinstance(value, str):
        match = re.match(r'([0-9]{4}-[0-9]{2}-[0-9]{2})(?:[T ]([0-9]{2}:[0-9]{2}:[0-9]{2}))?', value)
        if match:
            return match.group(1) + 'T' + (match.group(2) or '00:00:00')
    return None


def parse_resolved(data):
    if not isinstance(data, list):
        raise Failure('the resolved list has an unknown shape: %s' % keys_of(data))
    return {item for item in data if isinstance(item, str)}


def version_key(version):
    if not version:
        return ()
    return tuple(int(part) for part in re.findall(r'[0-9]+', version))


def sort_versions(versions):
    return sorted(versions, key=lambda version: (version_key(version), version), reverse=True)


def group_key(title):
    return hashlib.sha1(title.encode('utf-8', errors='replace')).hexdigest()[:12]


# ---------------------------------------------------------------- redaction


PATHS = [
    (re.compile(r'\b[a-z][a-z0-9+.-]*://\S*', re.IGNORECASE), '<link removed>'),
    (re.compile(r'\\\\[^\\\s]+(?:\\[^\\\s]*)*'), '<path removed>'),
    (re.compile(r'/(?:root|var/folders|private/var)(?:/\S*)?', re.IGNORECASE), '<path removed>'),
    (re.compile(r'\b(Users|home|Documents and Settings)(?:%2F|%5C|[\\/])+[^\\/%]+', re.IGNORECASE), r'\1/<user>'),
    (re.compile(r'~[A-Za-z0-9_.-]+'), '~<user>'),
]
EMAIL = re.compile(r'[A-Za-z0-9._%+-]+(?:@|\s*[(\[]at[)\]]\s*)[A-Za-z0-9-]+(?:\.[A-Za-z0-9-]+)*\.[A-Za-z]{2,}',
                   re.IGNORECASE)
ZERO_WIDTH = '​'


def redact(text):
    """Removes links, home and other private paths, e-mail addresses, mentions, issue references, backticks and HTML
    comments from text."""
    text = EMAIL.sub('<e-mail removed>', text)
    for pattern, replacement in PATHS:
        text = pattern.sub(replacement, text)
    text = text.replace('`', "'")
    text = re.sub(r'@(?=[A-Za-z0-9_-])', '@' + ZERO_WIDTH, text)
    text = re.sub(r'#(?=[0-9])', '#' + ZERO_WIDTH, text)
    text = text.replace('<!--', '<!' + ZERO_WIDTH + '--').replace('-->', '--' + ZERO_WIDTH + '>')
    return text


def clean_title(title):
    """The title cut to TITLE_RAW_LIMIT characters, percent-decoded, without paths, links and e-mail addresses, with
    only letters, digits, spaces and . $ : < > _ - left, GH-N references broken, and cut to TITLE_LIMIT."""
    title = redact(urllib.parse.unquote(title[:TITLE_RAW_LIMIT], errors='replace'))
    title = re.sub(r'[^\w.$:<> -]', ' ', title, flags=re.ASCII)
    title = re.sub(r'\bGH-(?=[0-9])', 'GH- ', title, flags=re.IGNORECASE)
    title = re.sub(r' +', ' ', title).strip()
    if len(title) > TITLE_LIMIT:
        title = title[:TITLE_LIMIT - 3].rstrip() + '...'
    return title or '(no title)'


IDENTIFIER = r'[A-Za-z_$][\w$]*'
CLASS_NAME = re.compile(r'(?:[a-z_$][\w$]*\.)+[A-Z_$][\w$]*', re.ASCII)
HEADER = re.compile(r'(?P<cls>[^\s:]+)(?P<rest>:.*)?')
NESTED = re.compile(r'(?P<indent>[ \t]*)(?P<kind>Caused by|Suppressed): ?(?P<what>.*)')
FRAME = re.compile(
    r'(?P<indent>[ \t]*)at (?P<frame>(?:[\w.$@-]*/){0,2}' + IDENTIFIER + r'(?:\.' + IDENTIFIER + r'){1,}\.'
    r'(?:' + IDENTIFIER + r'|<init>|<clinit>)'
    r'\((?:[\w$-]+\.(?:java|kt|kts|groovy|scala):[0-9]+|Unknown Source|Native Method)\))'
    r'(?: ~\[[\w.:?-]*\])?[ \t]*', re.ASCII)
MORE = re.compile(r'(?P<indent>[ \t]*)\.\.\. (?P<count>[0-9]{1,6}) (?P<what>more|common frames omitted)[ \t]*')


def redact_trace(text):
    """Keeps the stack frames of a strict shape (at [module/]pkg.Class.method(File.java:N | Unknown Source | Native
    Method)) and the "... N more" lines. The first line and the "Caused by:" and "Suppressed:" lines keep only a Java
    class name, and only when the next kept line is a frame; else they become REMOVED. Messages become
    MESSAGE_REMOVED; every other line is dropped. Only the first TRACE_INPUT_LIMIT characters are read, and lines
    longer than TRACE_LINE_INPUT_LIMIT are dropped, so that the patterns stay fast."""
    items = []  # (kind, value): kind is 'header', 'frame' or 'more'
    header_seen = False
    for line in text[:TRACE_INPUT_LIMIT].replace('\r\n', '\n').replace('\r', '\n').split('\n'):
        line = line.rstrip()
        if not line.strip() or (header_seen and len(line) > TRACE_LINE_INPUT_LIMIT):
            continue
        nested = NESTED.fullmatch(line)
        frame = FRAME.fullmatch(line)
        more = MORE.fullmatch(line)
        if not header_seen:
            items.append(('header', ('', None, line.strip()[:TRACE_LINE_INPUT_LIMIT])))
            header_seen = True
        elif nested:
            items.append(('header', (nested.group('indent'), nested.group('kind'), nested.group('what'))))
        elif frame:
            items.append(('frame', '%sat %s' % (frame.group('indent'), frame.group('frame'))))
        elif more:
            items.append(('more', '%s... %s %s' % (more.group('indent'), more.group('count'), more.group('what'))))
    result = []
    for index, (kind, value) in enumerate(items):
        if kind != 'header':
            result.append(value)
        else:
            indent, prefix, what = value
            following = items[index + 1][0] if index + 1 < len(items) else None
            match = HEADER.fullmatch(what.strip())
            if following == 'frame' and match and CLASS_NAME.fullmatch(match.group('cls')):
                shown = match.group('cls') + (': ' + MESSAGE_REMOVED if match.group('rest') is not None else '')
            else:
                shown = REMOVED
            result.append('%s%s%s' % (indent, prefix + ': ' if prefix else '', shown))
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


def content_key(count, versions):
    return hashlib.sha1(json.dumps([count, list(versions)]).encode('utf-8')).hexdigest()[:12]


def read_state(body):
    """The state marker of an issue body, or None:
    {'count': int, 'versions': [str], 'noted': {'count': int, 'key': str} or None,
     'closed': {'at': str, 'release': str} or None}."""
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
    if isinstance(count, bool) or not isinstance(count, int) or count < 0 or not isinstance(versions, list):
        return None
    versions = [version for version in versions if isinstance(version, str) and DISPLAY_VERSION.fullmatch(version)]
    noted = state.get('noted')
    if not (isinstance(noted, dict) and isinstance(noted.get('count'), int) and not isinstance(noted['count'], bool)
            and isinstance(noted.get('key'), str) and re.fullmatch(r'[0-9a-f]{12}', noted['key'])):
        noted = None
    closed = state.get('closed')
    if not (isinstance(closed, dict) and isinstance(closed.get('at'), str) and CLOSED_AT.fullmatch(closed['at'])
            and isinstance(closed.get('release'), str) and RELEASE_VERSION.fullmatch(closed['release'])):
        closed = None
    return {'count': count, 'versions': versions,
            'noted': {'count': noted['count'], 'key': noted['key']} if noted else None,
            'closed': {'at': closed['at'], 'release': closed['release']} if closed else None}


def empty_state():
    return {'count': 0, 'versions': [], 'noted': None, 'closed': None}


def noted_of(state):
    """What the comments have announced: the noted field, or for an older body the state itself."""
    return state['noted'] or {'count': state['count'], 'key': content_key(state['count'], state['versions'])}


def state_marker(state):
    value = {'count': state['count'], 'versions': state['versions']}
    if state.get('noted'):
        value['noted'] = state['noted']
    if state.get('closed'):
        value['closed'] = state['closed']
    return '<!-- marketplace-ea-state: %s -->' % json.dumps(value, sort_keys=True, separators=(',', ':'))


def replace_state(body, state):
    """The body with its state marker replaced (or added after the key marker)."""
    marker = state_marker(state)
    if STATE_MARKER.search(body):
        return STATE_MARKER.sub(lambda match: marker, body, count=1)
    key = KEY_MARKER.search(body)
    if key:
        return body[:key.end()] + '\n' + marker + body[key.end():]
    return marker + '\n' + body


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


def code(value):
    return '`%s`' % value


def listed(values, hidden=0):
    values = [value for value in values if value]
    shown = ', '.join(code(value) for value in values[:LIST_LIMIT]) if values else 'unknown'
    if len(values) > LIST_LIMIT:
        shown += ', and %d more' % (len(values) - LIST_LIMIT)
    if hidden:
        shown += ' (%d %s not shown)' % (hidden, 'value' if hidden == 1 else 'values')
    return shown


def render_trace(report, trace):
    if trace is None:
        return 'The stack trace could not be read from Marketplace.'
    fence = '```'
    return ('Stack trace of the newest report (plugin version %s, %s, %s). The exception messages are removed.'
            '\n\n%stext\n%s\n%s' % (
                code(report.plugin_version or 'unknown'), code(report.app_build or 'unknown build'),
                code(report.os_name or 'unknown OS'), fence, trace, fence))


def render_body(group, trace_section, state):
    dates = sorted(report.timestamp[:10] for report in group.reports if report.timestamp)
    builds = sort_versions({report.app_build for report in group.reports if report.app_build})
    systems = sorted({report.os_name for report in group.reports if report.os_name})
    return '\n'.join([
        '<!-- marketplace-ea: %s -->' % group.key,
        state_marker(state),
        'Users of the plugin sent these exception reports from the IDE to JetBrains Marketplace. '
        'The workflow `marketplace-exceptions.yml` copies them here once a day and rewrites this text '
        'when new reports come.',
        '',
        '- Group: %s' % code(clean_title(group.title)),
        '- Reports: %d' % group.report_count,
        '- First report: %s' % (code(dates[0]) if dates else 'unknown'),
        '- Last report: %s' % (code(dates[-1]) if dates else 'unknown'),
        '- Plugin versions: %s' % listed(group.versions(), group.hidden['pluginVersion']),
        '- IDE builds: %s' % listed(builds, group.hidden['appBuild']),
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


def update_comment(group, state, reopened_after=None):
    """The comment for the change from what was noted to the group now."""
    noted = noted_of(state)
    new_reports = group.report_count - noted['count']
    new_versions = [version for version in group.state_versions() if version not in state['versions']]
    parts = []
    if reopened_after:
        parts.append('Reopened: a report came from plugin version %s, which is newer than %s, the newest release '
                     'when this issue was closed.' % (code(group.newest_release()), code(reopened_after)))
    if new_reports > 0:
        parts.append('%d new %s (%d in total).' % (new_reports, 'report' if new_reports == 1 else 'reports',
                                                   group.report_count))
    if new_versions:
        parts.append('New plugin versions: %s.' % ', '.join(code(version) for version in new_versions))
    elif not reopened_after:
        parts.append('No new plugin versions.')
    return ' '.join(parts)


def note_comment(state):
    """The comment that a run which stopped halfway did not post: from what was noted to what the body shows."""
    new_reports = state['count'] - noted_of(state)['count']
    parts = []
    if new_reports > 0:
        parts.append('%d new %s (%d in total).' % (new_reports, 'report' if new_reports == 1 else 'reports',
                                                   state['count']))
    if state['versions']:
        parts.append('Newest plugin version: %s.' % code(state['versions'][0]))
    return ' '.join(parts) or 'The reports changed.'


# ---------------------------------------------------------------- GitHub


class GitHub:
    def __init__(self, http, token, repository=REPOSITORY, base=GITHUB):
        self.http = http
        self.token = checked_token('GITHUB_TOKEN', token, False)
        self.base = '%s/repos/%s' % (base, repository)

    def _call(self, method, path, payload=None, allowed=(200, 201)):
        headers = {'User-Agent': USER_AGENT, 'Accept': 'application/vnd.github+json',
                   'X-GitHub-Api-Version': '2022-11-28'}
        if self.token:
            headers['Authorization'] = 'Bearer ' + self.token
        data = None
        if payload is not None:
            data = json.dumps(payload).encode('utf-8', errors='replace')
            headers['Content-Type'] = 'application/json'
        response = self.http.request(method, self.base + path, headers, data)
        if response.status not in allowed:
            raise Failure('GitHub %s %s: HTTP %d' % (method, path.split('?')[0], response.status))
        return response

    def issues(self):
        """The issues with the label, open and closed: [{'number', 'state', 'state_reason', 'closed_at', 'body'}]."""
        found = []
        for page in range(1, 51):
            response = self._call('GET', '/issues?state=all&labels=%s&per_page=100&page=%d' % (
                urllib.parse.quote(LABEL, safe=''), page))
            items = response.json('the GitHub issue list')
            if not isinstance(items, list):
                raise Failure('the GitHub issue list is not a list')
            for item in items:
                if isinstance(item, dict) and 'pull_request' not in item:
                    found.append(issue_of(item))
            if len(items) < 100:
                return found
        raise Failure('more than 5000 issues have the label %s' % LABEL)

    def latest_release(self):
        """The newest plugin release, the largest v0.N.N tag of a published release, without the v; or None."""
        items = self._call('GET', '/releases?per_page=100').json('the GitHub release list')
        if not isinstance(items, list):
            raise Failure('the GitHub release list is not a list')
        return newest_release_of(items)

    def has_comment(self, number, marker):
        for page in range(1, 11):
            items = self._call('GET', '/issues/%d/comments?per_page=100&page=%d' % (number, page)).json(
                'the GitHub comment list')
            if not isinstance(items, list):
                raise Failure('the GitHub comment list is not a list')
            if any(isinstance(item, dict) and marker in (item.get('body') or '') for item in items):
                return True
            if len(items) < 100:
                return False
        return False

    def ensure_label(self, name, color, description):
        response = self._call('GET', '/labels/%s' % urllib.parse.quote(name, safe=''), allowed=(200, 404))
        if response.status == 404:
            self._call('POST', '/labels', {'name': name, 'color': color, 'description': description})

    def create(self, title, body, labels):
        number = self._call('POST', '/issues', {'title': title, 'body': body, 'labels': labels}).json(
            'the created issue').get('number')
        if isinstance(number, bool) or not isinstance(number, int):
            raise Failure('GitHub did not return the number of the created issue')
        return number

    def edit(self, number, body, reopen=False):
        payload = {'body': body}
        if reopen:
            payload['state'] = 'open'
        self._call('PATCH', '/issues/%d' % number, payload)

    def comment(self, number, body):
        self._call('POST', '/issues/%d/comments' % number, {'body': body})


def issue_of(item):
    return {'number': item['number'], 'state': item.get('state'), 'state_reason': item.get('state_reason'),
            'closed_at': item.get('closed_at'), 'body': item.get('body') or ''}


def newest_release_of(items):
    versions = [item['tag_name'][1:] for item in items
                if isinstance(item, dict) and not item.get('draft') and isinstance(item.get('tag_name'), str)
                and re.fullmatch(r'v0\.[0-9]+\.[0-9]+', item['tag_name'])]
    return sort_versions(versions)[0] if versions else None


class FixtureGitHub:
    """Existing issues and releases read from files; for local dry runs, so it writes nothing."""

    def __init__(self, directory):
        self.directory = directory

    def _load(self, name):
        path = os.path.join(self.directory, name)
        if not os.path.exists(path):
            return []
        with open(path, encoding='utf-8') as file:
            return json.load(file)

    def issues(self):
        return [issue_of(item) for item in self._load('issues.json')]

    def latest_release(self):
        return newest_release_of(self._load('releases.json'))

    def has_comment(self, number, marker):
        return False


# ---------------------------------------------------------------- planning


class Action:
    def __init__(self, kind, group, issue=None, state=None, release=None):
        self.kind = kind  # 'create', 'update', 'reopen', 'note' (post a comment that is due) or 'record'
        self.group = group
        self.issue = issue
        self.state = state  # the state in the body of the issue
        self.release = release  # 'record': the newest release now; 'reopen': the one stored at the close


COUNTS = ('groups', 'malformed_groups', 'malformed_reports', 'resolved', 'freeze', 'old', 'duplicate', 'unchanged',
          'closed', 'waiting_new', 'waiting_updates')


def plan(groups, resolved, issues, min_version, max_new, max_updates, latest_release=lambda: None):
    """The actions of one run and the counts for the summary. It reads and writes nothing itself; latest_release is
    called at most once, and only when a closed issue needs it."""
    counts = {name: 0 for name in COUNTS}
    counts['malformed_groups'] = getattr(groups, 'malformed_groups', 0)
    counts['malformed_reports'] = getattr(groups, 'malformed_reports', 0)
    by_key = {}
    for issue in sorted(issues, key=lambda issue: (issue['state'] != 'open', issue['number'])):
        key = read_key(issue['body'])
        if key and key not in by_key:
            by_key[key] = issue
    minimum = version_key(min_version)
    release = []
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
        if not any(version_key(version) >= minimum for version in group.releases()):
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
        state = read_state(issue['body']) or empty_state()
        action = None
        if issue['state'] == 'open':
            grew = group.report_count > state['count'] or any(
                version not in state['versions'] for version in group.state_versions())
            if grew:
                action = Action('update', group, issue, state)
            elif noted_of(state)['key'] != content_key(state['count'], state['versions']):
                action = Action('note', group, issue, state)
        elif issue.get('state_reason') == 'completed' and CLOSED_AT.fullmatch(issue.get('closed_at') or ''):
            if state['closed'] is None or state['closed']['at'] != issue['closed_at']:
                if not release:
                    release.append(latest_release())
                if release[0]:
                    action = Action('record', group, issue, state, release[0])
            else:
                newest = group.newest_release()
                if newest and version_key(newest) > version_key(state['closed']['release']):
                    action = Action('reopen', group, issue, state, state['closed']['release'])
        if action is None:
            counts['unchanged' if issue['state'] == 'open' else 'closed'] += 1
        elif updates < max_updates:
            actions.append(action)
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


def new_state(group, noted, closed=None):
    return {'count': group.report_count, 'versions': group.state_versions(), 'noted': noted, 'closed': closed}


def execute(marketplace, github, actions, write, out, time_budget=DEFAULT_TIME_BUDGET, clock=time.monotonic):
    """Carries out the actions (write=True) or prints them. Returns the lines of the summary and the number of
    actions left for the next run because the time budget ran out."""
    lines = []
    first_body = None
    labels_ready = False
    start = clock()
    for index, action in enumerate(actions):
        if clock() - start > time_budget:
            return lines, len(actions) - index
        group = action.group
        issue = action.issue
        title = code(issue_title(group))
        if action.kind == 'create':
            key = content_key(group.report_count, group.state_versions())
            body = render_body(group, trace_section(marketplace, group),
                               new_state(group, {'count': group.report_count, 'key': key}))
            if first_body is None:
                first_body = body
            if write:
                if not labels_ready:
                    github.ensure_label(LABEL, LABEL_COLOR, LABEL_DESCRIPTION)
                    labels_ready = True
                number = github.create(issue_title(group), body, [BUG_LABEL, LABEL])
                lines.append('- created #%d: %s (%s)' % (number, title, reports_text(group.report_count)))
            else:
                lines.append('- would create: %s (%s)' % (title, reports_text(group.report_count)))
            continue
        number = issue['number']
        if action.kind == 'record':
            state = dict(action.state, closed={'at': issue['closed_at'], 'release': action.release})
            if write:
                github.edit(number, replace_state(issue['body'], state))
            lines.append('- %s #%d: %s, closed; newest release %s' % (
                'recorded' if write else 'would record', number, title, code(action.release)))
            continue
        if action.kind == 'note':
            state = action.state
            key = content_key(state['count'], state['versions'])
            comment = note_comment(state)
            if write:
                if not github.has_comment(number, NOTE_MARKER % key):
                    github.comment(number, comment + '\n\n' + NOTE_MARKER % key)
                github.edit(number, replace_state(issue['body'], dict(state, noted={'count': state['count'],
                                                                                     'key': key})))
            lines.append('- %s #%d: %s: %s' % ('commented on' if write else 'would comment on', number, title,
                                               comment))
            continue
        # 'update' or 'reopen': the body first, with the comment still due; then the comment; then the body says so.
        reopen = action.kind == 'reopen'
        comment = update_comment(group, action.state, action.release if reopen else None)
        due = new_state(group, noted_of(action.state))
        body = render_body(group, trace_section(marketplace, group, issue['body']), due)
        key = content_key(due['count'], due['versions'])
        verb = 'reopened' if reopen else 'updated'
        if write:
            github.edit(number, body, reopen)
            github.comment(number, comment + '\n\n' + NOTE_MARKER % key)
            github.edit(number, replace_state(body, dict(due, noted={'count': due['count'], 'key': key})))
            lines.append('- %s #%d: %s: %s' % (verb, number, title, comment))
        else:
            lines.append('- would %s #%d: %s: %s' % ('reopen' if reopen else 'update', number, title, comment))
    if not write and first_body is not None:
        out.write('Body of the first new issue:\n\n%s\n' % first_body)
    return lines, 0


def summary(mode, counts, actions, lines):
    def number(kind):
        return sum(1 for action in actions if action.kind == kind)
    text = [
        '### Marketplace exceptions (%s)' % mode,
        '',
        '| | |',
        '|---|---|',
        '| Groups on Marketplace | %d |' % counts['groups'],
        '| New issues | %d |' % number('create'),
        '| Updated issues | %d |' % number('update'),
        '| Reopened issues | %d |' % number('reopen'),
        '| Comments that a stopped run left due | %d |' % number('note'),
        '| Closed issues: release recorded | %d |' % number('record'),
        '| Unchanged | %d |' % counts['unchanged'],
        '| Closed, left closed | %d |' % counts['closed'],
        '| Skipped: resolved on Marketplace | %d |' % counts['resolved'],
        '| Skipped: no release at or above the minimum version | %d |' % counts['old'],
        '| Skipped: freeze groups | %d |' % counts['freeze'],
        '| Skipped: malformed groups, malformed reports | %d, %d |' % (counts['malformed_groups'],
                                                                       counts['malformed_reports']),
        '| Waiting for the next run (new, other) | %d, %d |' % (counts['waiting_new'], counts['waiting_updates']),
        '',
    ]
    return '\n'.join(text + lines) + '\n'


def probe(marketplace, out):
    response = marketplace.status('/exceptions/resolved', 'the resolved list')
    out.write('GET /exceptions/resolved: HTTP %d\n' % response.status)
    if response.status == 200:
        try:
            data = json.loads(response.body.decode('utf-8'))
        except (UnicodeDecodeError, ValueError, RecursionError):
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
        with open(path, 'a', encoding='utf-8', errors='replace') as file:
            file.write(text)


def main(argv=None, environ=None, http=None, out=None, clock=time.monotonic):
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
    parser.add_argument('--time-budget', type=int, default=DEFAULT_TIME_BUDGET,
                        help='seconds after which no new change starts')
    parser.add_argument('--fixture', help='read Marketplace and GitHub from this directory (dry run only)')
    parser.add_argument('--summary', default=environ.get('GITHUB_STEP_SUMMARY'))
    args = parser.parse_args(argv)
    if not re.fullmatch(r'[0-9]+', args.plugin) or not re.fullmatch(r'[\w.-]+/[\w.-]+', args.repo):
        parser.error('--plugin must be a number and --repo owner/name')
    if not RELEASE_VERSION.fullmatch(args.min_version):
        parser.error('--min-version must be a release version such as %s' % DEFAULT_MIN_VERSION)
    if args.fixture and args.mode != 'dry-run':
        parser.error('--fixture works only with --dry-run')
    try:
        return run(args, environ, http or Http(), out, clock)
    except Failure as failure:
        sys.stderr.write('error: %s\n' % failure)
        return 1
    except Exception as error:  # noqa: the message of an unknown error could hold a token or report data
        sys.stderr.write('error: unexpected %s\n' % type(error).__name__)
        return 1


def run(args, environ, http, out, clock):
    if args.fixture:
        marketplace = FixtureMarketplace(args.fixture)
        github = FixtureGitHub(args.fixture)
    else:
        marketplace = Marketplace(http, environ.get('MARKETPLACE_TOKEN', ''), args.plugin)
        github = GitHub(http, environ.get('GITHUB_TOKEN', ''), args.repo)
        if args.mode == 'run':
            checked_token('GITHUB_TOKEN', environ.get('GITHUB_TOKEN', ''), True)
    if args.mode == 'probe':
        text = io.StringIO()
        code_ = probe(marketplace, text)
        out.write(text.getvalue())
        write_summary(args.summary, '### Marketplace exceptions (probe)\n\n```\n%s```\n' % text.getvalue())
        return code_
    groups = marketplace.groups()
    resolved = marketplace.resolved()
    actions, counts = plan(groups, resolved, github.issues(), args.min_version, args.max_new, args.max_updates,
                           github.latest_release)
    lines, left = execute(marketplace, github, actions, args.mode == 'run', out, args.time_budget, clock)
    if left:
        counts['waiting_updates'] += left
        actions = actions[:len(actions) - left]
    text = summary(args.mode, counts, actions, lines)
    out.write(text)
    write_summary(args.summary, text)
    return 0


if __name__ == '__main__':
    sys.exit(main())
