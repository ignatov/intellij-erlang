"""Unit tests of exceptions.py: python3 -m unittest discover -s scripts/marketplace -p 'test_*.py' -v"""

import contextlib
import http.server
import io
import json
import os
import re
import tempfile
import threading
import unittest
import urllib.parse

import exceptions as ea

TESTDATA = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'testdata')
API = 'https://plugins.jetbrains.com/api/plugins/7083'
STORAGE = 'https://storage.example.com/7083/reports.json?X-Signature=secret-signature'
TOKEN = 'perm:test-token'


def report(report_id, version, timestamp=1763251200000, build='IU-263.7307.44', os_name='Linux', **extra):
    item = {'id': report_id, 'timestamp': timestamp, 'appBuild': build, 'osName': os_name, 'pluginVersion': version}
    item.update(extra)
    return item


def group(title, reports, count=None):
    return {'title': title, 'reportCount': len(reports) if count is None else count, 'reports': reports}


class FakeMarketplace:
    """Answers the Marketplace URLs; the exception list comes through a redirect, as the API is said to do."""

    def __init__(self, groups, resolved=(), traces=None, redirect=True):
        self.exceptions = json.dumps(groups).encode('utf-8')
        self.resolved = json.dumps(list(resolved)).encode('utf-8')
        self.traces = traces or {}
        self.redirect = redirect

    def handle(self, method, url, headers, data):
        if method != 'GET':
            raise AssertionError('Marketplace got a %s' % method)
        if url == STORAGE:
            if 'Authorization' in headers:
                raise AssertionError('the token was sent to the storage link')
            return ea.Response(200, {}, self.exceptions)
        if headers.get('Authorization') != 'Bearer ' + TOKEN:
            return ea.Response(403, {}, b'{}')
        if url == API + '/exceptions':
            if self.redirect:
                return ea.Response(301, {'Location': STORAGE}, b'')
            return ea.Response(200, {}, self.exceptions)
        if url == API + '/exceptions/resolved':
            return ea.Response(200, {}, self.resolved)
        match = re.fullmatch(re.escape(API) + r'/exceptions/([0-9]+)/stacktrace', url)
        if match:
            trace = self.traces.get(match.group(1))
            return ea.Response(200, {}, trace.encode('utf-8')) if trace is not None else ea.Response(404, {}, b'')
        raise AssertionError('unexpected URL')


class FakeGitHub:
    """The part of the GitHub REST API that the script uses, in memory."""

    BASE = 'https://api.github.com/repos/ignatov/intellij-erlang'

    def __init__(self, labels=('Bug',)):
        self.issues = {}
        self.comments = {}
        self.labels = set(labels)
        self.writes = []
        self.list_calls = 0

    def add(self, number, state, body, labels=('Bug', ea.LABEL)):
        self.issues[number] = {'number': number, 'state': state, 'body': body, 'title': 't', 'labels': list(labels)}

    def handle(self, method, url, headers, data):
        if not url.startswith(self.BASE):
            raise AssertionError('unexpected URL')
        path = url[len(self.BASE):]
        payload = json.loads(data.decode('utf-8')) if data else None
        if method != 'GET':
            self.writes.append((method, path, payload))
        parts = urllib.parse.urlsplit(path)
        query = urllib.parse.parse_qs(parts.query)
        if method == 'GET' and parts.path == '/issues':
            self.list_calls += 1
            label = query['labels'][0]
            page = int(query['page'][0])
            per_page = int(query['per_page'][0])
            items = [dict(issue) for _, issue in sorted(self.issues.items()) if label in issue['labels']]
            return self.json(200, items[(page - 1) * per_page:page * per_page])
        if method == 'GET' and parts.path.startswith('/labels/'):
            name = urllib.parse.unquote(parts.path[len('/labels/'):])
            return self.json(200 if name in self.labels else 404, {})
        if method == 'POST' and parts.path == '/labels':
            self.labels.add(payload['name'])
            return self.json(201, {})
        if method == 'POST' and parts.path == '/issues':
            for label in payload['labels']:
                if label not in self.labels:
                    raise AssertionError('label %s does not exist' % label)
            number = max(self.issues, default=0) + 1
            self.issues[number] = {'number': number, 'state': 'open', 'body': payload['body'],
                                   'title': payload['title'], 'labels': payload['labels']}
            return self.json(201, {'number': number})
        match = re.fullmatch(r'/issues/([0-9]+)', parts.path)
        if method == 'PATCH' and match:
            self.issues[int(match.group(1))].update(payload)
            return self.json(200, {})
        match = re.fullmatch(r'/issues/([0-9]+)/comments', parts.path)
        if method == 'POST' and match:
            self.comments.setdefault(int(match.group(1)), []).append(payload['body'])
            return self.json(201, {})
        raise AssertionError('unexpected request %s %s' % (method, path))

    @staticmethod
    def json(status, value):
        return ea.Response(status, {}, json.dumps(value).encode('utf-8'))


class FakeHttp:
    def __init__(self, marketplace, github=None):
        self.marketplace = marketplace
        self.github = github or FakeGitHub()
        self.requests = []

    def request(self, method, url, headers=None, data=None):
        headers = dict(headers or {})
        self.requests.append((method, url, headers))
        if url.startswith('https://api.github.com/'):
            return self.github.handle(method, url, headers, data)
        return self.marketplace.handle(method, url, headers, data)


def run(http, mode='--run', *args):
    out = io.StringIO()
    err = io.StringIO()
    environ = {'MARKETPLACE_TOKEN': TOKEN, 'GITHUB_TOKEN': 'gh-token'}
    with contextlib.redirect_stderr(err):
        code = ea.main([mode] + list(args), environ=environ, http=http, out=out)
    return code, out.getvalue(), err.getvalue()


TRACE = '\n'.join([
    'com.intellij.openapi.util.InvalidDataException: cannot read /Users/jdoe/work/x.erl',
    '-module(secret_project).',
    '\tat org.intellij.erlang.psi.impl.ErlangPsiImplUtil.resolve(ErlangPsiImplUtil.java:812)',
    'Caused by: java.io.IOException: denied',
    '\tat java.base/java.io.FileInputStream.open0(Native Method)',
    '\t... 3 more',
])


class ParseTest(unittest.TestCase):
    def test_documented_fields(self):
        groups = ea.parse_groups([group('A', [report(5, '0.11.1300')], count=9)])
        self.assertEqual(1, len(groups))
        self.assertEqual('A', groups[0].title)
        self.assertEqual(9, groups[0].report_count)
        only = groups[0].reports[0]
        self.assertEqual(('5', '2025-11-16', 'IU-263.7307.44', 'Linux', '0.11.1300'),
                         (only.id, only.timestamp, only.app_build, only.os_name, only.plugin_version))

    def test_64_bit_ids_stay_exact(self):
        data = json.loads('[{"title": "A", "reports": [{"id": 9007199254740993, "pluginVersion": "0.11.1300"}]}]')
        self.assertEqual('9007199254740993', ea.parse_groups(data)[0].reports[0].id)
        self.assertEqual('18446744073709551615', ea.parse_groups(
            [group('A', [{'id': '18446744073709551615'}])])[0].reports[0].id)

    def test_float_id_fails(self):
        data = json.loads('[{"title": "A", "reports": [{"id": 9.007199254740993e15}]}]')
        with self.assertRaisesRegex(ea.Failure, 'not an integer'):
            ea.parse_groups(data)

    def test_unknown_fields_are_ignored(self):
        groups = ea.parse_groups([dict(group('A', [report(1, '0.11.1300', comment='secret')]), extra={'x': 1})])
        self.assertFalse(hasattr(groups[0].reports[0], 'comment'))

    def test_unknown_shape_names_keys_only(self):
        with self.assertRaises(ea.Failure) as raised:
            ea.parse_groups({'data': 'private value', 'count': 3})
        self.assertIn('[count, data]', str(raised.exception))
        self.assertNotIn('private value', str(raised.exception))

    def test_group_without_title_names_keys_only(self):
        with self.assertRaises(ea.Failure) as raised:
            ea.parse_groups([{'name': 'private value', 'reports': []}])
        self.assertIn('[name, reports]', str(raised.exception))
        self.assertNotIn('private value', str(raised.exception))

    def test_wrapped_list(self):
        self.assertEqual(['A'], [g.title for g in ea.parse_groups({'issues': [group('A', [])], 'total': 1})])

    def test_resolved(self):
        self.assertEqual({'A', 'B'}, ea.parse_resolved(['A', 'B']))
        with self.assertRaisesRegex(ea.Failure, r'\[titles\]'):
            ea.parse_resolved({'titles': ['A']})

    def test_dates(self):
        self.assertEqual('2025-11-16', ea.parse_date(1763251200000))
        self.assertEqual('2025-11-16', ea.parse_date(1763251200))
        self.assertEqual('2026-11-19', ea.parse_date('2026-11-19T10:00:00Z'))
        self.assertIsNone(ea.parse_date('yesterday'))

    def test_unsafe_values_are_not_copied(self):
        self.assertEqual('(unrecognised value)', ea.safe_value('Linux <script>'))
        self.assertEqual('(unrecognised value)', ea.safe_value('@someone'))
        self.assertIsNone(ea.safe_value({'name': 'x'}))
        self.assertEqual('Windows 11', ea.safe_value('Windows 11'))

    def test_versions(self):
        self.assertTrue(ea.version_key('0.11.1300') > ea.version_key('0.11.999'))
        self.assertEqual(['0.11.1301', '0.11.1300', '0.11.999'], ea.sort_versions({'0.11.999', '0.11.1301', '0.11.1300'}))

    def test_fixture_file(self):
        groups = ea.FixtureMarketplace(TESTDATA).groups()
        self.assertEqual('9007199254740993', groups[0].reports[0].id)


class MarketplaceTest(unittest.TestCase):
    def test_redirect_is_followed_without_the_token(self):
        http = FakeHttp(FakeMarketplace([group('A', [report(1, '0.11.1300')])]))
        groups = ea.Marketplace(http, TOKEN).groups()
        self.assertEqual(['A'], [g.title for g in groups])
        self.assertEqual([STORAGE], [url for _, url, headers in http.requests if 'Authorization' not in headers])

    def test_direct_answer(self):
        http = FakeHttp(FakeMarketplace([group('A', [])], redirect=False))
        self.assertEqual(['A'], [g.title for g in ea.Marketplace(http, TOKEN).groups()])

    def test_redirect_to_http_fails(self):
        class Insecure(FakeMarketplace):
            def handle(self, method, url, headers, data):
                return ea.Response(302, {'Location': 'http://storage.example.com/x?sig=secret'}, b'')
        with self.assertRaises(ea.Failure) as raised:
            ea.Marketplace(FakeHttp(Insecure([])), TOKEN).groups()
        self.assertNotIn('secret', str(raised.exception))

    def test_failed_storage_link_is_not_shown(self):
        class Expired(FakeMarketplace):
            def handle(self, method, url, headers, data):
                if url == STORAGE:
                    return ea.Response(403, {}, b'expired')
                return super().handle(method, url, headers, data)
        code, out, err = run(FakeHttp(Expired([])), '--dry-run')
        self.assertEqual(1, code)
        self.assertIn('HTTP 403', err)
        self.assertNotIn('storage.example.com', out + err)
        self.assertNotIn('secret-signature', out + err)

    def test_no_post_to_marketplace(self):
        http = FakeHttp(FakeMarketplace([group('A', [report(1, '0.11.1300')])], traces={'1': TRACE}))
        run(http)
        self.assertEqual(set(), {method for method, url, _ in http.requests if 'jetbrains.com' in url} - {'GET'})
        self.assertFalse(any(url.endswith('/resolve') for _, url, _ in http.requests))

    def test_stack_trace_errors(self):
        class Denied(FakeMarketplace):
            def handle(self, method, url, headers, data):
                return ea.Response(403, {}, b'') if url.endswith('/stacktrace') else ea.Response(200, {}, b'[]')
        http = FakeHttp(FakeMarketplace([], traces={'1': 'x.Y'}))
        self.assertEqual('x.Y', ea.Marketplace(http, TOKEN).stacktrace('1'))
        self.assertIsNone(ea.Marketplace(http, TOKEN).stacktrace('2'))
        with self.assertRaisesRegex(ea.Failure, 'HTTP 403'):
            ea.Marketplace(FakeHttp(Denied([])), TOKEN).stacktrace('1')

    def test_probe_prints_status_and_type_only(self):
        http = FakeHttp(FakeMarketplace([], resolved=['private title']))
        code, out, _ = run(http, '--probe')
        self.assertEqual(0, code)
        self.assertEqual('GET /exceptions/resolved: HTTP 200\nTop level: a list; item types: [str]\n', out)
        self.assertEqual([API + '/exceptions/resolved'], [url for _, url, _ in http.requests])

    def test_probe_with_a_bad_token(self):
        class Denied(FakeMarketplace):
            def handle(self, method, url, headers, data):
                return ea.Response(403, {}, b'{"message": "no"}')
        code, out, _ = run(FakeHttp(Denied([])), '--probe')
        self.assertEqual(1, code)
        self.assertEqual('GET /exceptions/resolved: HTTP 403\n', out)

    def test_missing_token(self):
        err = io.StringIO()
        with contextlib.redirect_stderr(err):
            code = ea.main(['--dry-run'], environ={}, http=FakeHttp(FakeMarketplace([])), out=io.StringIO())
        self.assertEqual(1, code)
        self.assertIn('MARKETPLACE_TOKEN is not set', err.getvalue())


class HttpTest(unittest.TestCase):
    """The real transport against a local server: it returns redirects and errors instead of following them."""

    def setUp(self):
        class Handler(http.server.BaseHTTPRequestHandler):
            seen = []

            def do_GET(self):
                Handler.seen.append((self.path, self.headers.get('Authorization')))
                if self.path == '/moved':
                    self.send_response(301)
                    self.send_header('Location', '/target')
                    self.end_headers()
                elif self.path == '/target':
                    self.send_response(200)
                    self.end_headers()
                    self.wfile.write(b'[]')
                else:
                    self.send_response(404)
                    self.end_headers()
                    self.wfile.write(b'missing')

            def log_message(self, *args):
                pass

        self.handler = Handler
        self.server = http.server.HTTPServer(('127.0.0.1', 0), Handler)
        threading.Thread(target=self.server.serve_forever, daemon=True).start()
        self.base = 'http://127.0.0.1:%d' % self.server.server_port

    def tearDown(self):
        self.server.shutdown()
        self.server.server_close()

    def test_redirect_is_not_followed(self):
        response = ea.Http().request('GET', self.base + '/moved', {'Authorization': 'Bearer x'})
        self.assertEqual((301, '/target'), (response.status, response.headers['location']))
        self.assertEqual([('/moved', 'Bearer x')], self.handler.seen)

    def test_error_is_returned(self):
        response = ea.Http().request('GET', self.base + '/nothing')
        self.assertEqual((404, b'missing'), (response.status, response.body))


class RedactionTest(unittest.TestCase):
    def test_messages_are_removed(self):
        trace = ea.redact_trace(TRACE)
        self.assertEqual('\n'.join([
            'com.intellij.openapi.util.InvalidDataException: <message removed>',
            '\tat org.intellij.erlang.psi.impl.ErlangPsiImplUtil.resolve(ErlangPsiImplUtil.java:812)',
            'Caused by: java.io.IOException: <message removed>',
            '\tat java.base/java.io.FileInputStream.open0(Native Method)',
            '\t... 3 more',
        ]), trace)

    def test_header_without_message(self):
        self.assertEqual('java.lang.NullPointerException\n\tat a.B.c(B.java:1)',
                         ea.redact_trace('java.lang.NullPointerException\n\tat a.B.c(B.java:1)'))

    def test_first_line_that_is_not_a_class(self):
        self.assertEqual('<message removed>\n\tat a.B.c(B.java:1)',
                         ea.redact_trace('Cannot open /Users/jdoe/x\n\tat a.B.c(B.java:1)'))

    def test_suppressed_and_message_lines_that_look_like_frames(self):
        trace = ea.redact_trace('a.E: m\n  at offset 12 in /home/jdoe/x\n\tat a.B.c(B.kt:1)\n'
                                '\tSuppressed: a.F: secret\n\t\tat a.B.d(Unknown Source)')
        self.assertEqual('a.E: <message removed>\n\tat a.B.c(B.kt:1)\n\tSuppressed: a.F: <message removed>\n'
                         '\t\tat a.B.d(Unknown Source)', trace)

    def test_paths(self):
        self.assertEqual('/Users/<user>/x /home/<user>/y C:\\Users\\<user>\\z c:/Users/<user>/w',
                         ea.redact('/Users/jdoe/x /home/j.doe/y C:\\Users\\JDoe\\z c:/Users/jdoe/w'))

    def test_emails(self):
        self.assertEqual('mail <e-mail removed> now', ea.redact('mail j.doe+x@mail.example.com now'))

    def test_mentions_references_fences_and_comments(self):
        text = ea.redact('@someone #123 ``` <!-- marketplace-ea: 0123456789ab -->')
        self.assertNotRegex(text, r'@[A-Za-z]')
        self.assertNotRegex(text, r'#[0-9]')
        self.assertNotIn('`', text)
        self.assertNotIn('<!--', text)
        self.assertIsNone(ea.read_key(text))

    def test_frames_are_redacted(self):
        trace = ea.redact_trace('a.E\n\tat a.B.c@someone(B.java:1)\n\tat a.B.d(B.java:2)')
        self.assertNotRegex(trace, r'@[A-Za-z]')

    def test_long_traces_are_cut(self):
        trace = ea.redact_trace('a.E\n' + '\tat a.B.c(B.java:1)\n' * 500)
        self.assertLessEqual(len(trace.split('\n')), ea.TRACE_LINE_LIMIT + 1)

    def test_title(self):
        title = ea.clean_title('REWA: `x` @someone #12 jdoe@example.com /Users/jdoe/p\n' + 'y' * 300)
        self.assertLessEqual(len(title), ea.TITLE_LIMIT)
        self.assertNotIn('`', title)
        self.assertNotIn('jdoe', title)
        self.assertNotRegex(title, r'@[A-Za-z]|#[0-9]')

    def test_issue_holds_no_private_data(self):
        groups = [group('A', [report(1, '0.11.1300', comment='my secret comment', email='jdoe@example.com',
                                     hostId='device-1234', attachments=[{'content': 'private file'}],
                                     storageUrl=STORAGE)])]
        http = FakeHttp(FakeMarketplace(groups, traces={'1': TRACE}))
        run(http)
        body = http.github.issues[1]['body']
        for private in ('secret comment', 'jdoe', 'device-1234', 'private file', 'storage.example.com',
                        'secret_project', 'cannot read', 'denied', 'perm:'):
            self.assertNotIn(private, body)
        self.assertIn('ErlangPsiImplUtil.resolve(ErlangPsiImplUtil.java:812)', body)
        self.assertEqual(1, body.count('```text'))
        self.assertEqual(2, body.count('```'))

    def test_fixture_body(self):
        out = io.StringIO()
        code = ea.main(['--dry-run', '--fixture', TESTDATA], environ={}, out=out)
        self.assertEqual(0, code)
        text = out.getvalue()
        for private in ('jdoe', 'secret', 'example.com', 'device-1234', 'unknown fields', 'report.txt'):
            self.assertNotIn(private, text)
        self.assertIn('java.io.FileNotFoundException: <message removed>', text)


class PlanTest(unittest.TestCase):
    def test_minimum_version_resolved_and_freeze(self):
        groups = ea.parse_groups([group('old', [report(1, '0.11.1200')]), group('new', [report(2, '0.11.1300')]),
                                  group('done', [report(3, '0.11.1301')]), group('F: freeze', [report(4, '0.11.1301')]),
                                  group('none', [{'id': 5}])])
        actions, counts = ea.plan(groups, {'done'}, [], '0.11.1300', 5, 20)
        self.assertEqual(['new'], [action.group.title for action in actions])
        self.assertEqual((2, 1, 1), (counts['old'], counts['resolved'], counts['freeze']))

    def test_dedupe_by_marker_in_open_and_closed_issues(self):
        groups = ea.parse_groups([group('A', [report(1, '0.11.1300')]), group('B', [report(2, '0.11.1300')])])
        issues = [{'number': 7, 'state': 'closed', 'body': ea.render_body(groups[0], '')},
                  {'number': 8, 'state': 'open', 'body': ea.render_body(groups[1], '')}]
        actions, counts = ea.plan(groups, set(), issues, '0.11.1300', 5, 20)
        self.assertEqual([], actions)
        self.assertEqual(2, counts['unchanged'])

    def test_one_list_call_per_run(self):
        http = FakeHttp(FakeMarketplace([group('G%d' % i, [report(i, '0.11.1300')]) for i in range(1, 9)]))
        for i in range(1, 4):
            http.github.add(i, 'open', '')
        run(http)
        self.assertEqual(1, http.github.list_calls)

    def test_listing_reads_all_pages(self):
        github = FakeGitHub()
        for i in range(1, 151):
            github.add(i, 'open', '<!-- marketplace-ea: %012x -->' % i)
        github.add(151, 'open', 'not ours', labels=('Bug',))
        issues = ea.GitHub(FakeHttp(None, github), 't').issues()
        self.assertEqual(150, len(issues))
        self.assertEqual(2, github.list_calls)

    def test_caps_most_reported_first(self):
        groups = ea.parse_groups([group('G%02d' % i, [report(i, '0.11.1300')], count=i) for i in range(1, 9)])
        actions, counts = ea.plan(groups, set(), [], '0.11.1300', 5, 20)
        self.assertEqual(['G08', 'G07', 'G06', 'G05', 'G04'], [action.group.title for action in actions])
        self.assertEqual(3, counts['waiting_new'])

    def test_update_cap(self):
        old = ea.parse_groups([group('G%02d' % i, [report(i, '0.11.1300')]) for i in range(25)])
        issues = [{'number': i + 1, 'state': 'open', 'body': ea.render_body(g, '')} for i, g in enumerate(old)]
        new = ea.parse_groups([group('G%02d' % i, [report(i, '0.11.1300')], count=2) for i in range(25)])
        actions, counts = ea.plan(new, set(), issues, '0.11.1300', 5, 20)
        self.assertEqual(20, len(actions))
        self.assertEqual(5, counts['waiting_updates'])


class RunTest(unittest.TestCase):
    def setUp(self):
        self.marketplace = FakeMarketplace([group('A', [report(1, '0.11.1300')]),
                                            group('B', [report(2, '0.11.1301'), report(3, '0.11.1300')])],
                                           traces={'1': TRACE, '2': TRACE})
        self.http = FakeHttp(self.marketplace)
        self.github = self.http.github

    def set_groups(self, groups):
        self.marketplace.exceptions = json.dumps(groups).encode('utf-8')

    def test_create_labels_and_idempotence(self):
        code, _, err = run(self.http)
        self.assertEqual(0, code, err)
        self.assertEqual({1, 2}, set(self.github.issues))
        self.assertIn(ea.LABEL, self.github.labels)
        self.assertEqual('Marketplace exception: B', self.github.issues[1]['title'])
        self.assertEqual(['Bug', ea.LABEL], self.github.issues[1]['labels'])
        writes = len(self.github.writes)
        code, out, _ = run(self.http)
        self.assertEqual(0, code)
        self.assertEqual(writes, len(self.github.writes))
        self.assertIn('| Unchanged | 2 |', out)

    def test_dry_run_writes_nothing(self):
        code, out, _ = run(self.http, '--dry-run')
        self.assertEqual(0, code)
        self.assertEqual([], self.github.writes)
        self.assertIn('would create: Marketplace exception: B (2 reports)', out)
        self.assertIn('Body of the first new issue:', out)
        self.assertIn('<!-- marketplace-ea: %s -->' % ea.group_key('B'), out)

    def test_update_of_an_open_issue(self):
        run(self.http)
        self.set_groups([group('A', [report(1, '0.11.1300'), report(4, '0.11.1302')], count=5),
                         group('B', [report(2, '0.11.1301'), report(3, '0.11.1300')])])
        self.marketplace.traces['4'] = TRACE
        run(self.http)
        number = [n for n, issue in self.github.issues.items() if ea.read_key(issue['body']) == ea.group_key('A')][0]
        self.assertEqual(['4 new reports (5 in total). New plugin versions: 0.11.1302.'], self.github.comments[number])
        self.assertEqual({'count': 5, 'versions': ['0.11.1302', '0.11.1300'], 'newest': '0.11.1302'},
                         ea.read_state(self.github.issues[number]['body']))
        self.assertIn('- Reports: 5', self.github.issues[number]['body'])
        writes = len(self.github.writes)
        run(self.http)
        self.assertEqual(writes, len(self.github.writes))

    def test_update_keeps_the_old_trace_when_the_new_one_is_missing(self):
        run(self.http)
        self.set_groups([group('A', [report(1, '0.11.1300'), report(9, '0.11.1302')])])
        run(self.http)
        body = [issue['body'] for issue in self.github.issues.values() if 'Group: `A`' in issue['body']][0]
        self.assertIn('ErlangPsiImplUtil.resolve', body)

    def test_closed_issue_without_a_newer_version_stays_closed(self):
        run(self.http)
        number = [n for n, issue in self.github.issues.items() if ea.read_key(issue['body']) == ea.group_key('B')][0]
        self.github.issues[number]['state'] = 'closed'
        self.set_groups([group('A', [report(1, '0.11.1300')]),
                         group('B', [report(2, '0.11.1301'), report(3, '0.11.1300'), report(5, '0.11.1301')], 9)])
        writes = len(self.github.writes)
        code, out, _ = run(self.http)
        self.assertEqual(0, code)
        self.assertEqual(writes, len(self.github.writes))
        self.assertEqual('closed', self.github.issues[number]['state'])
        self.assertIn('| Closed, no newer plugin version | 1 |', out)

    def test_closed_issue_with_a_newer_version_is_reopened(self):
        run(self.http)
        number = [n for n, issue in self.github.issues.items() if ea.read_key(issue['body']) == ea.group_key('B')][0]
        self.github.issues[number]['state'] = 'closed'
        self.set_groups([group('A', [report(1, '0.11.1300')]),
                         group('B', [report(2, '0.11.1301'), report(3, '0.11.1300'), report(6, '0.11.1310')])])
        self.marketplace.traces['6'] = TRACE
        run(self.http)
        self.assertEqual('open', self.github.issues[number]['state'])
        self.assertEqual(1, len(self.github.comments[number]))
        self.assertTrue(self.github.comments[number][0].startswith(
            'Reopened: a report came from plugin version 0.11.1310, which is newer than 0.11.1301'))
        writes = len(self.github.writes)
        run(self.http)
        self.assertEqual(writes, len(self.github.writes))

    def test_one_stack_trace_per_new_or_changed_group(self):
        run(self.http)
        traces = [url for _, url, _ in self.http.requests if url.endswith('/stacktrace')]
        self.assertEqual(sorted([API + '/exceptions/1/stacktrace', API + '/exceptions/2/stacktrace']), sorted(traces))
        self.http.requests.clear()
        run(self.http)
        self.assertEqual([], [url for _, url, _ in self.http.requests if url.endswith('/stacktrace')])

    def test_summary_file(self):
        with tempfile.TemporaryDirectory() as directory:
            path = os.path.join(directory, 'summary.md')
            run(self.http, '--dry-run', '--summary', path)
            with open(path, encoding='utf-8') as file:
                self.assertIn('### Marketplace exceptions (dry-run)', file.read())

    def test_run_needs_a_github_token(self):
        err = io.StringIO()
        with contextlib.redirect_stderr(err):
            code = ea.main(['--run'], environ={'MARKETPLACE_TOKEN': TOKEN}, http=self.http, out=io.StringIO())
        self.assertEqual(1, code)
        self.assertIn('GITHUB_TOKEN is not set', err.getvalue())


if __name__ == '__main__':
    unittest.main()
