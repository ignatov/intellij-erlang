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
GITHUB_TOKEN = 'ghs_testtoken'
CLOSED_AT = '2026-11-01T10:00:00Z'


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
    """The part of the GitHub REST API that the script uses, in memory. fail is a set of (method, path regex) that
    answer 502 once each."""

    BASE = 'https://api.github.com/repos/ignatov/intellij-erlang'

    def __init__(self, labels=('Bug',), releases=('v0.11.1310', 'v0.11.1305')):
        self.issues = {}
        self.comments = {}
        self.labels = set(labels)
        self.releases = [{'tag_name': tag, 'draft': False} for tag in releases]
        self.writes = []
        self.list_calls = 0
        self.release_calls = 0
        self.fail = []

    def add(self, number, state, body, labels=('Bug', ea.LABEL), state_reason=None, closed_at=None):
        self.issues[number] = {'number': number, 'state': state, 'body': body, 'title': 't', 'labels': list(labels),
                               'state_reason': state_reason, 'closed_at': closed_at}

    def close(self, number, reason='completed', at=CLOSED_AT):
        self.issues[number].update(state='closed', state_reason=reason, closed_at=at)

    def handle(self, method, url, headers, data):
        if not url.startswith(self.BASE):
            raise AssertionError('unexpected URL')
        path = url[len(self.BASE):]
        payload = json.loads(data.decode('utf-8')) if data else None
        parts = urllib.parse.urlsplit(path)
        for index, (fail_method, fail_path) in enumerate(self.fail):
            if fail_method == method and re.fullmatch(fail_path, parts.path):
                del self.fail[index]
                return ea.Response(502, {}, b'')
        if method != 'GET':
            self.writes.append((method, path, payload))
        query = urllib.parse.parse_qs(parts.query)
        if method == 'GET' and parts.path == '/issues':
            self.list_calls += 1
            label = query['labels'][0]
            state = query.get('state', ['open'])[0]
            page = int(query['page'][0])
            per_page = int(query['per_page'][0])
            items = [dict(issue) for _, issue in sorted(self.issues.items())
                     if label in issue['labels'] and state in ('all', issue['state'])]
            return self.json(200, items[(page - 1) * per_page:page * per_page])
        if method == 'GET' and parts.path == '/releases':
            self.release_calls += 1
            return self.json(200, self.releases)
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
            self.add(number, 'open', payload['body'], payload['labels'])
            self.issues[number]['title'] = payload['title']
            return self.json(201, {'number': number})
        match = re.fullmatch(r'/issues/([0-9]+)', parts.path)
        if method == 'PATCH' and match:
            issue = self.issues[int(match.group(1))]
            issue.update(payload)
            if payload.get('state') == 'open':
                issue.update(state_reason='reopened', closed_at=None)
            return self.json(200, {})
        match = re.fullmatch(r'/issues/([0-9]+)/comments', parts.path)
        if method == 'POST' and match:
            self.comments.setdefault(int(match.group(1)), []).append(payload['body'])
            return self.json(201, {})
        if method == 'GET' and match:
            page = int(query['page'][0])
            items = [{'body': body} for body in self.comments.get(int(match.group(1)), [])]
            return self.json(200, items[(page - 1) * 100:page * 100])
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


def run(http, mode='--run', *args, environ=None):
    out = io.StringIO()
    err = io.StringIO()
    if environ is None:
        environ = {'MARKETPLACE_TOKEN': TOKEN, 'GITHUB_TOKEN': GITHUB_TOKEN}
    with contextlib.redirect_stderr(err):
        code = ea.main([mode] + list(args), environ=environ, http=http, out=out)
    return code, out.getvalue(), err.getvalue()


def parsed(*groups):
    return ea.parse_groups(list(groups))


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
        groups = parsed(group('A', [report(5, '0.11.1300')], count=9))
        self.assertEqual(1, len(groups))
        self.assertEqual('A', groups[0].title)
        self.assertEqual(9, groups[0].report_count)
        only = groups[0].reports[0]
        self.assertEqual(('5', '2025-11-16T00:00:00', 'IU-263.7307.44', 'Linux', '0.11.1300'),
                         (only.id, only.timestamp, only.app_build, only.os_name, only.plugin_version))

    def test_64_bit_ids_stay_exact(self):
        data = json.loads('[{"title": "A", "reports": [{"id": 9007199254740993, "pluginVersion": "0.11.1300"}]}]')
        self.assertEqual('9007199254740993', ea.parse_groups(data)[0].reports[0].id)
        self.assertEqual('18446744073709551615', parsed(group('A', [{'id': '18446744073709551615'}]))[0].reports[0].id)

    def test_bad_ids_make_the_report_malformed(self):
        data = json.loads('[{"title": "A", "reports": [{"id": 9.007199254740993e15}, {"id": -1}, {"id": true},'
                          ' {"id": "18446744073709551616"}, null, {"id": 7}]}]')
        groups = ea.parse_groups(data)
        self.assertEqual(['7'], [only.id for only in groups[0].reports])
        self.assertEqual(5, groups.malformed_reports)

    def test_malformed_groups_are_skipped_and_counted(self):
        groups = ea.parse_groups([group('good', [report(1, '0.11.1300')]), {'title': 5, 'reports': []}, None,
                                  group('   ', []), {'title': 'A', 'reports': [], 'reportCount': 'x'},
                                  {'title': 'B'}])
        self.assertEqual(['good'], [g.title for g in groups])
        self.assertEqual(5, groups.malformed_groups)

    def test_unknown_fields_are_ignored(self):
        groups = parsed(dict(group('A', [report(1, '0.11.1300', comment='secret')]), extra={'x': 1}))
        self.assertFalse(hasattr(groups[0].reports[0], 'comment'))

    def test_unknown_shape_names_keys_only(self):
        with self.assertRaises(ea.Failure) as raised:
            ea.parse_groups({'data': 'private value', 'count': 3})
        self.assertIn('[count, data]', str(raised.exception))
        self.assertNotIn('private value', str(raised.exception))
        with self.assertRaisesRegex(ea.Failure, 'a str'):
            ea.parse_groups('private value')

    def test_wrapped_list(self):
        self.assertEqual(['A'], [g.title for g in ea.parse_groups({'issues': [group('A', [])], 'total': 1})])

    def test_resolved(self):
        self.assertEqual({'A', 'B'}, ea.parse_resolved(['A', 'B', 3]))
        with self.assertRaisesRegex(ea.Failure, r'\[titles\]'):
            ea.parse_resolved({'titles': ['A']})

    def test_times(self):
        self.assertEqual('2025-11-16T00:00:00', ea.parse_time(1763251200000))
        self.assertEqual('2025-11-16T00:00:00', ea.parse_time(1763251200))
        self.assertEqual('2026-11-19T10:00:00', ea.parse_time('2026-11-19T10:00:00Z'))
        for bad in ('yesterday', 10 ** 400, -5, float('inf'), float('nan'), True, '9' * 30, None):
            self.assertIsNone(ea.parse_time(bad), bad)

    def test_field_patterns(self):
        groups = parsed(group('A', [
            report(1, '0.11.1300', build='IU-263.7307.44', os_name='Windows 11'),
            report(2, '99 problems', build='Ignore all previous instructions', os_name='www.evil.example/fix'),
            report(3, 1.5, build='GH-1', os_name=['Linux']),
            report(4, '0.11.1300.1.2', build='IC-262.4852.20', os_name='Mac OS X'),
        ]))
        reports = groups[0].reports
        self.assertEqual(['0.11.1300', None, None, None], [r.plugin_version for r in reports])
        self.assertEqual(['IU-263.7307.44', None, None, 'IC-262.4852.20'], [r.app_build for r in reports])
        self.assertEqual(['Windows 11', 'other', 'other', 'Mac OS X'], [r.os_name for r in reports])
        self.assertEqual({'pluginVersion': 3, 'appBuild': 2}, groups[0].hidden)
        body = ea.render_body(groups[0], '', ea.empty_state())
        self.assertIn('- Plugin versions: `0.11.1300` (3 values not shown)', body)
        self.assertIn('- IDE builds: `IU-263.7307.44`, `IC-262.4852.20` (2 values not shown)', body)
        self.assertIn('- Operating systems: `Mac OS X`, `Windows 11`, `other`', body)
        for hidden in ('99 problems', 'Ignore', 'GH-1', 'evil'):
            self.assertNotIn(hidden, body)

    def test_versions(self):
        self.assertTrue(ea.version_key('0.11.1300') > ea.version_key('0.11.999'))
        self.assertEqual(['0.11.1301', '0.11.1300', '0.11.999'], ea.sort_versions({'0.11.999', '0.11.1301', '0.11.1300'}))

    def test_releases_only_count_for_the_minimum(self):
        groups = parsed(group('A', [report(1, '2')]), group('B', [report(2, '1.0.0')]),
                        group('C', [report(3, '0.12.1')]))
        actions, counts = ea.plan(groups, set(), [], '0.11.1300', 5, 20)
        self.assertEqual(['C'], [action.group.title for action in actions])
        self.assertEqual(2, counts['old'])

    def test_lone_surrogate_in_the_title(self):
        groups = parsed(group('NPE: \ud800 x', [report(1, '0.11.1300')]))
        self.assertEqual('NPE: ? x', groups[0].title)
        http = FakeHttp(FakeMarketplace([group('NPE: \ud800 x', [report(1, '0.11.1300')])], traces={'1': 'a.B'}))
        code, _, err = run(http)
        self.assertEqual(0, code, err)
        self.assertEqual('Marketplace exception: NPE: x', http.github.issues[1]['title'])

    def test_newest_report_is_the_latest_with_a_valid_version(self):
        groups = parsed(group('A', [report(1, '0.11.1310', timestamp=1700000000000),
                                    report(2, '0.11.1300', timestamp=1763251200000),
                                    report(3, 'junk', timestamp=1763300000000)]))
        self.assertEqual('2', groups[0].newest_report().id)

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
                if url == API + '/exceptions':
                    return ea.Response(302, {'Location': 'http://storage.example.com/x?sig=secret'}, b'')
                return ea.Response(200, {}, b'[]')
        with self.assertRaises(ea.Failure) as raised:
            ea.Marketplace(FakeHttp(Insecure([])), TOKEN).groups()
        self.assertIn('not https', str(raised.exception))
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

    def test_stack_trace_errors_mean_could_not_be_read(self):
        class Flaky(FakeMarketplace):
            def handle(self, method, url, headers, data):
                if url.endswith('/exceptions/1/stacktrace'):
                    return ea.Response(500, {}, b'oops')
                if url.endswith('/exceptions/2/stacktrace'):
                    return ea.Response(302, {'Location': 'http://x/'}, b'')
                return super().handle(method, url, headers, data)
        http = FakeHttp(Flaky([group('top', [report(1, '0.11.1300')], count=99), group('second', [report(2, '0.11.1300')]),
                               group('third', [report(3, '0.11.1300')])], traces={'3': 'a.B'}))
        self.assertIsNone(ea.Marketplace(http, TOKEN).stacktrace('1'))
        self.assertIsNone(ea.Marketplace(http, TOKEN).stacktrace('2'))
        self.assertIsNone(ea.Marketplace(http, TOKEN).stacktrace('9'))
        code, _, err = run(http)
        self.assertEqual(0, code, err)
        self.assertEqual(3, len(http.github.issues))
        self.assertIn('could not be read', http.github.issues[1]['body'])

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
        code, _, err = run(FakeHttp(FakeMarketplace([])), '--dry-run', environ={})
        self.assertEqual(1, code)
        self.assertIn('MARKETPLACE_TOKEN is not set', err)


class SecretTest(unittest.TestCase):
    """No error path prints the token: not in stdout, stderr or the job summary."""

    SECRET = 'SECRETPART'

    def check(self, http, environ, mode='--run'):
        with tempfile.TemporaryDirectory() as directory:
            path = os.path.join(directory, 'summary.md')
            code, out, err = run(http, mode, '--summary', path, environ=environ)
            text = out + err
            if os.path.exists(path):
                with open(path, encoding='utf-8') as file:
                    text += file.read()
        self.assertNotIn(self.SECRET, text)
        self.assertNotIn('perm:', text)
        return code, err

    def test_tokens_with_characters_a_token_cannot_have(self):
        for token in ('perm:SECRETPART1\nSECRETPART2', 'perm:SECRETPART\x00X', 'perm:SECRETPART\u2019X',
                      'perm:SECRETPART X'):
            for environ in ({'MARKETPLACE_TOKEN': token, 'GITHUB_TOKEN': GITHUB_TOKEN},
                            {'MARKETPLACE_TOKEN': TOKEN, 'GITHUB_TOKEN': token}):
                code, err = self.check(FakeHttp(FakeMarketplace([])), environ)
                self.assertEqual(1, code)
                self.assertIn('has characters that a token cannot have', err)

    def test_every_error_path(self):
        token = 'perm:SECRETPART-ok'

        class Marketplace(FakeMarketplace):
            def __init__(self, answer):
                super().__init__([group('A', [report(1, '0.11.1300')])], traces={'1': TRACE})
                self.answer = answer

            def handle(self, method, url, headers, data):
                headers = dict(headers)
                if headers.get('Authorization') == 'Bearer ' + token:
                    headers['Authorization'] = 'Bearer ' + TOKEN
                return self.answer(url, headers) or super().handle(method, url, headers, data)

        def raising(url, headers):
            raise RuntimeError('boom %s' % headers.get('Authorization'))

        answers = [
            lambda url, headers: ea.Response(403, {}, b'') if url.endswith('/exceptions') else None,
            lambda url, headers: ea.Response(200, {}, b'{"a": 1}') if url == STORAGE else None,
            lambda url, headers: ea.Response(200, {}, b'not json') if url.endswith('/resolved') else None,
            lambda url, headers: ea.Response(302, {}, b'') if url.endswith('/exceptions') else None,
            raising,
        ]
        environ = {'MARKETPLACE_TOKEN': token, 'GITHUB_TOKEN': GITHUB_TOKEN}
        for answer in answers:
            code, err = self.check(FakeHttp(Marketplace(answer)), environ)
            self.assertEqual(1, code)
        self.assertIn('unexpected RuntimeError', err)
        github = FakeGitHub()
        github.fail.append(('POST', '/issues'))
        code, err = self.check(FakeHttp(Marketplace(lambda url, headers: None), github), environ)
        self.assertEqual(1, code)
        self.assertIn('HTTP 502', err)
        self.assertNotIn(GITHUB_TOKEN, err)
        code, _ = self.check(FakeHttp(Marketplace(lambda url, headers: None)), environ, '--probe')
        self.assertEqual(0, code)


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
        for first in ('Cannot open /Users/jdoe/x', 'internal.acme.secret_project', 'jdoe.example.com: password',
                      'Secret: x'):
            self.assertEqual('<removed>\n\tat a.B.c(B.java:1)', ea.redact_trace(first + '\n\tat a.B.c(B.java:1)'))

    def test_header_needs_a_frame_after_it(self):
        trace = ea.redact_trace('a.E: m\nCaused by: com.acme.CustomerPaymentsDb\nCaused by: a.F: x\n\t... 2 more\n'
                                'Caused by: a.G\n\tat a.B.c(B.java:1)')
        self.assertEqual('<removed>\nCaused by: <removed>\nCaused by: <removed>\n\t... 2 more\nCaused by: a.G\n'
                         '\tat a.B.c(B.java:1)', trace)

    def test_only_strict_frames_are_kept(self):
        lines = [
            'java.lang.IllegalStateException: Assertion failed',
            '  at /Users/jdoe/acme-secret-project/src(billing.erl:12)',
            '  at customer_acme_corp.handle_payment(Secret)',
            '  at line(42)',
            '  at offset 12 in /home/jdoe/x',
            '    at jdoe@example.com(x)',
            '\tat x.Y.z(https)',
            '\tat a.B.d(B.java:3) secret-trailing',
            '\tat a/b/c/d.E.f(E.java:1)',
            '\tat a.B.c(B.java:1)',
            '\tat app//com.foo.Bar.<init>(Bar.kt:7)',
            '\tat java.base@21.0.1/java.lang.Thread.run(Thread.java:1583) ~[?:?]',
            '\tat com.foo.Bar$Inner.lambda$run$0(Unknown Source) ~[idea.jar:263.1]',
            '\tat a.B.groovy(B.groovy:2)',
            '\t... 12 common frames omitted',
        ]
        self.assertEqual('\n'.join([
            'java.lang.IllegalStateException: <message removed>',
            '\tat a.B.c(B.java:1)',
            '\tat app//com.foo.Bar.<init>(Bar.kt:7)',
            '\tat java.base@\u200b21.0.1/java.lang.Thread.run(Thread.java:1583)',
            '\tat com.foo.Bar$Inner.lambda$run$0(Unknown Source)',
            '\tat a.B.groovy(B.groovy:2)',
            '\t... 12 common frames omitted',
        ]), ea.redact_trace('\n'.join(lines)))

    def test_suppressed(self):
        trace = ea.redact_trace('a.E: m\n\tat a.B.c(B.kt:1)\n\tSuppressed: x.Y: secret at a.B.c(B.java:1)\n'
                                '\t\tat a.B.d(Unknown Source)')
        self.assertEqual('a.E: <message removed>\n\tat a.B.c(B.kt:1)\n\tSuppressed: x.Y: <message removed>\n'
                         '\t\tat a.B.d(Unknown Source)', trace)

    def test_hostile_trace(self):
        with open(os.path.join(TESTDATA, 'hostile', 'stacktrace', '12.txt'), encoding='utf-8') as file:
            trace = ea.redact_trace(file.read())
        for secret in ('secretmsg', 'secretproj', 'secretcustomer', 'secret-trailing', 'jdoe', '```', '~~~', '<!--',
                       '-->'):
            self.assertNotIn(secret, trace)
        # A class-shaped name in a "Caused by:" line that a frame follows is kept: it is the class of the cause.
        self.assertIn('Caused by: secretcls.inmsg.Thing\n\tat org.intellij.erlang.Foo.bar(Foo.java:1)', trace)

    def test_paths(self):
        self.assertEqual('/Users/<user>/x /home/<user>/y C:\\Users/<user>\\z c:/Users/<user>/w',
                         ea.redact('/Users/jdoe/x /home/j.doe/y C:\\Users\\JDoe\\z c:/Users/jdoe/w'))

    def test_emails(self):
        self.assertEqual('mail <e-mail removed> now', ea.redact('mail j.doe+x@mail.example.com now'))
        self.assertEqual('mail <e-mail removed> now', ea.redact('mail jdoe(at)example.com now'))

    def test_mentions_references_fences_and_comments(self):
        text = ea.redact('@someone #123 ``` <!-- marketplace-ea: 0123456789ab -->')
        self.assertNotRegex(text, r'@[A-Za-z]')
        self.assertNotRegex(text, r'#[0-9]')
        self.assertNotIn('`', text)
        self.assertNotIn('<!--', text)
        self.assertIsNone(ea.read_key(text))

    def test_long_traces_are_cut(self):
        trace = ea.redact_trace('a.E\n' + '\tat a.B.c(B.java:1)\n' * 500)
        self.assertLessEqual(len(trace.split('\n')), ea.TRACE_LINE_LIMIT + 1)

    def test_titles(self):
        cases = {
            'NPE: \\\\corp-fs01\\home$\\jdoe\\proj': 'jdoe',
            'NPE: c:\\users\\jdoe\\proj': 'jdoe',
            'NPE: C:\\Documents and Settings\\jdoe\\x': 'jdoe',
            'NPE: /root/secret': 'secret',
            'NPE: /var/folders/xy/T/jdoe': 'jdoe',
            'NPE: \\\\wsl$\\Ubuntu\\home\\jdoe\\p': 'jdoe',
            'NPE: https://ci.acme.com/job?token=ghp_abcdef123456': 'ghp_',
            'NPE: file:///Users/jdoe/p': 'jdoe',
            'NPE: /Users/John Doe/p': 'Doe',
            'NPE: GH-12 www.evil.example/pay': '/',
            'NPE: <img src=x onerror=1> **bold** [x](https://evil)': '*',
            'NPE: %2FUsers%2Fjdoe': 'jdoe',
            'NPE: ~jdoe/secret': 'jdoe',
            'NPE: @someone #12 x@example.com `code`': '@',
        }
        for title, secret in cases.items():
            cleaned = ea.clean_title(title)
            self.assertNotIn(secret, cleaned, title)
            self.assertRegex(cleaned, r'^[\w.$:<> -]+$', title)
        self.assertEqual('REWA: PsiManagerImpl.findFile', ea.clean_title('REWA: PsiManagerImpl.findFile'))
        self.assertEqual('<init> Foo$Bar', ea.clean_title('<init> Foo$Bar'))
        self.assertEqual(ea.TITLE_LIMIT, len(ea.clean_title('y' * 10000)))

    def test_title_is_cut_before_the_patterns(self):
        calls = []
        original = ea.redact
        try:
            ea.redact = lambda text: calls.append(len(text)) or original(text)
            ea.clean_title('a' * 100000)
        finally:
            ea.redact = original
        self.assertEqual([ea.TITLE_RAW_LIMIT], calls)

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

    def test_hostile_fixture(self):
        out = io.StringIO()
        code = ea.main(['--dry-run', '--fixture', os.path.join(TESTDATA, 'hostile')], environ={}, out=out)
        self.assertEqual(0, code)
        text = out.getvalue()
        for private in ('secretmsg', 'secretproj', 'secretcustomer', 'secretdeep', 'secretcomment', 'secrethost',
                        'secretatt', 'SECRET', 'jdoe', 'ghp_', 'Ignore all', 'evil', '@ignatov', '#1 ', 'GH-1',
                        '<!-- marketplace-ea: 000000000000'):
            self.assertNotIn(private, text)
        self.assertEqual(2, text.count('```'))


class PlanTest(unittest.TestCase):
    def test_minimum_version_resolved_and_freeze(self):
        groups = parsed(group('old', [report(1, '0.11.1200')]), group('new', [report(2, '0.11.1300')]),
                        group('done', [report(3, '0.11.1301')]), group('F: freeze', [report(4, '0.11.1301')]),
                        group('none', [{'id': 5}]))
        actions, counts = ea.plan(groups, {'done'}, [], '0.11.1300', 5, 20)
        self.assertEqual(['new'], [action.group.title for action in actions])
        self.assertEqual((2, 1, 1), (counts['old'], counts['resolved'], counts['freeze']))

    def test_dedupe_by_marker_in_open_and_closed_issues(self):
        groups = parsed(group('A', [report(1, '0.11.1300')]), group('B', [report(2, '0.11.1300')]))
        issues = [{'number': 7, 'state': 'closed', 'state_reason': 'not_planned', 'closed_at': CLOSED_AT,
                   'body': ea.render_body(groups[0], '', ea.new_state(groups[0], None))},
                  {'number': 8, 'state': 'open', 'body': ea.render_body(groups[1], '', ea.new_state(groups[1], None))}]
        actions, counts = ea.plan(groups, set(), issues, '0.11.1300', 5, 20)
        self.assertEqual([], actions)
        self.assertEqual((1, 1), (counts['unchanged'], counts['closed']))

    def test_one_list_call_per_run_and_closed_issues_are_listed(self):
        github = FakeGitHub()
        http = FakeHttp(FakeMarketplace([group('A', [report(1, '0.11.1300')])], traces={'1': TRACE}), github)
        run(http)
        github.close(1, reason='not_planned')
        writes = len(github.writes)
        run(http)
        self.assertEqual(2, github.list_calls)
        self.assertEqual(writes, len(github.writes))
        self.assertEqual(1, len(github.issues))

    def test_listing_reads_all_pages(self):
        github = FakeGitHub()
        for i in range(1, 151):
            github.add(i, 'open' if i % 2 else 'closed', '<!-- marketplace-ea: %012x -->' % i)
        github.add(151, 'open', 'not ours', labels=('Bug',))
        issues = ea.GitHub(FakeHttp(None, github), 't').issues()
        self.assertEqual(150, len(issues))
        self.assertEqual(2, github.list_calls)

    def test_caps_most_reported_first(self):
        groups = parsed(*[group('G%02d' % i, [report(i, '0.11.1300')], count=i) for i in range(1, 9)])
        actions, counts = ea.plan(groups, set(), [], '0.11.1300', 5, 20)
        self.assertEqual(['G08', 'G07', 'G06', 'G05', 'G04'], [action.group.title for action in actions])
        self.assertEqual(3, counts['waiting_new'])

    def test_update_cap(self):
        old = parsed(*[group('G%02d' % i, [report(i, '0.11.1300')]) for i in range(25)])
        issues = [{'number': i + 1, 'state': 'open', 'body': ea.render_body(g, '', ea.new_state(g, None))}
                  for i, g in enumerate(old)]
        new = parsed(*[group('G%02d' % i, [report(i, '0.11.1300')], count=2) for i in range(25)])
        actions, counts = ea.plan(new, set(), issues, '0.11.1300', 5, 20)
        self.assertEqual(20, len(actions))
        self.assertEqual(5, counts['waiting_updates'])

    def test_time_budget(self):
        ticks = iter(range(0, 10000, 100))
        http = FakeHttp(FakeMarketplace([group('G%d' % i, [report(i, '0.11.1300')]) for i in range(1, 5)]))
        out = io.StringIO()
        code = ea.main(['--run', '--time-budget', '250'], environ={'MARKETPLACE_TOKEN': TOKEN,
                                                                  'GITHUB_TOKEN': GITHUB_TOKEN},
                       http=http, out=out, clock=lambda: next(ticks))
        self.assertEqual(0, code)
        self.assertEqual(2, len(http.github.issues))
        self.assertIn('| Waiting for the next run (new, other) | 0, 2 |', out.getvalue())


class RunTest(unittest.TestCase):
    def setUp(self):
        self.marketplace = FakeMarketplace([group('A', [report(1, '0.11.1300')]),
                                            group('B', [report(2, '0.11.1301'), report(3, '0.11.1300')])],
                                           traces={'1': TRACE, '2': TRACE, '3': TRACE})
        self.http = FakeHttp(self.marketplace)
        self.github = self.http.github

    def set_groups(self, *groups):
        self.marketplace.exceptions = json.dumps(list(groups)).encode('utf-8')

    def number(self, title):
        return [n for n, issue in self.github.issues.items() if ea.read_key(issue['body']) == ea.group_key(title)][0]

    def unchanged_by_another_run(self):
        writes = len(self.github.writes)
        code, _, err = run(self.http)
        self.assertEqual(0, code, err)
        self.assertEqual(writes, len(self.github.writes))

    def test_create_labels_and_idempotence(self):
        code, _, err = run(self.http)
        self.assertEqual(0, code, err)
        self.assertEqual({1, 2}, set(self.github.issues))
        self.assertIn(ea.LABEL, self.github.labels)
        self.assertEqual('Marketplace exception: B', self.github.issues[1]['title'])
        self.assertEqual(['Bug', ea.LABEL], self.github.issues[1]['labels'])
        self.unchanged_by_another_run()

    def test_dry_run_writes_nothing(self):
        code, out, _ = run(self.http, '--dry-run')
        self.assertEqual(0, code)
        self.assertEqual([], self.github.writes)
        self.assertIn('would create: `Marketplace exception: B` (2 reports)', out)
        self.assertIn('Body of the first new issue:', out)
        self.assertIn('<!-- marketplace-ea: %s -->' % ea.group_key('B'), out)

    def test_update_of_an_open_issue(self):
        run(self.http)
        self.set_groups(group('A', [report(1, '0.11.1300'), report(4, '0.11.1302')], count=5),
                        group('B', [report(2, '0.11.1301'), report(3, '0.11.1300')]))
        self.marketplace.traces['4'] = TRACE
        run(self.http)
        number = self.number('A')
        self.assertEqual(1, len(self.github.comments[number]))
        self.assertTrue(self.github.comments[number][0].startswith(
            '4 new reports (5 in total). New plugin versions: `0.11.1302`.'))
        state = ea.read_state(self.github.issues[number]['body'])
        self.assertEqual((5, ['0.11.1302', '0.11.1300']), (state['count'], state['versions']))
        self.assertEqual(ea.content_key(5, ['0.11.1302', '0.11.1300']), state['noted']['key'])
        self.assertIn('- Reports: 5', self.github.issues[number]['body'])
        self.unchanged_by_another_run()

    def test_update_keeps_the_old_trace_when_the_new_one_is_missing(self):
        run(self.http)
        self.set_groups(group('A', [report(1, '0.11.1300'), report(9, '0.11.1302')]))
        run(self.http)
        self.assertIn('ErlangPsiImplUtil.resolve', self.github.issues[self.number('A')]['body'])

    def test_failed_comment_is_posted_by_the_next_run(self):
        run(self.http)
        number = self.number('A')
        self.set_groups(group('A', [report(1, '0.11.1300'), report(4, '0.11.1302')], count=5))
        self.github.fail.append(('POST', '/issues/%d/comments' % number))
        code, _, _ = run(self.http)
        self.assertEqual(1, code)
        self.assertNotIn(number, self.github.comments)
        self.assertIn('- Reports: 5', self.github.issues[number]['body'])
        code, _, err = run(self.http)
        self.assertEqual(0, code, err)
        self.assertEqual(1, len(self.github.comments[number]))
        self.assertTrue(self.github.comments[number][0].startswith('4 new reports (5 in total).'))
        self.unchanged_by_another_run()
        self.assertEqual(1, len(self.github.comments[number]))

    def test_failed_last_edit_does_not_post_the_comment_twice(self):
        run(self.http)
        number = self.number('A')
        self.set_groups(group('A', [report(1, '0.11.1300'), report(4, '0.11.1302')], count=5))
        original = self.github.handle
        edits = []

        def handle(method, url, headers, data):
            if method == 'PATCH':
                edits.append(url)
                if len(edits) == 2:
                    return ea.Response(502, {}, b'')
            return original(method, url, headers, data)
        self.github.handle = handle
        self.assertEqual(1, run(self.http)[0])
        self.github.handle = original
        self.assertEqual(1, len(self.github.comments[number]))
        code, _, err = run(self.http)
        self.assertEqual(0, code, err)
        self.assertEqual(1, len(self.github.comments[number]))
        self.unchanged_by_another_run()

    def close_b(self, reason='completed', at=CLOSED_AT):
        run(self.http)
        number = self.number('B')
        self.github.close(number, reason, at)
        return number

    def test_closing_records_the_newest_release(self):
        number = self.close_b()
        self.unchanged_by_another_run_except_record(number)
        state = ea.read_state(self.github.issues[number]['body'])
        self.assertEqual({'at': CLOSED_AT, 'release': '0.11.1310'}, state['closed'])
        self.assertEqual('closed', self.github.issues[number]['state'])
        self.assertNotIn(number, self.github.comments)
        self.unchanged_by_another_run()
        self.assertEqual(1, self.github.release_calls)

    def unchanged_by_another_run_except_record(self, number):
        writes = len(self.github.writes)
        run(self.http)
        self.assertEqual([('PATCH', '/issues/%d' % number)], [(m, p) for m, p, _ in self.github.writes[writes:]])

    def test_closed_issue_without_a_newer_release_stays_closed(self):
        number = self.close_b()
        run(self.http)
        self.set_groups(group('A', [report(1, '0.11.1300')]),
                        group('B', [report(2, '0.11.1301'), report(3, '0.11.1305'), report(5, '0.11.1310')], 9))
        writes = len(self.github.writes)
        code, out, _ = run(self.http)
        self.assertEqual(0, code)
        self.assertEqual(writes, len(self.github.writes))
        self.assertEqual('closed', self.github.issues[number]['state'])
        self.assertIn('| Closed, left closed | 1 |', out)

    def test_closed_issue_with_a_newer_release_is_reopened(self):
        number = self.close_b()
        run(self.http)
        self.set_groups(group('A', [report(1, '0.11.1300')]),
                        group('B', [report(2, '0.11.1301'), report(3, '0.11.1300'), report(6, '0.11.1311')]))
        self.marketplace.traces['6'] = TRACE
        run(self.http)
        self.assertEqual('open', self.github.issues[number]['state'])
        self.assertEqual(1, len(self.github.comments[number]))
        self.assertTrue(self.github.comments[number][0].startswith(
            'Reopened: a report came from plugin version `0.11.1311`, which is newer than `0.11.1310`'))
        self.assertIsNone(ea.read_state(self.github.issues[number]['body'])['closed'])
        self.unchanged_by_another_run()

    def test_odd_versions_do_not_reopen(self):
        number = self.close_b()
        run(self.http)
        self.set_groups(group('B', [report(2, '0.11.1301'), report(7, '99'), report(8, '1.0.0'),
                                    report(9, 'IU-263.1')]))
        run(self.http)
        self.assertEqual('closed', self.github.issues[number]['state'])

    def test_issue_closed_as_not_planned_is_never_reopened(self):
        for reason in ('not_planned', 'duplicate', None):
            self.setUp()
            number = self.close_b(reason)
            self.set_groups(group('B', [report(2, '0.11.1301'), report(6, '0.11.1400')]))
            writes = len(self.github.writes)
            run(self.http)
            run(self.http)
            self.assertEqual(writes, len(self.github.writes), reason)
            self.assertEqual('closed', self.github.issues[number]['state'])
            self.assertEqual(0, self.github.release_calls)

    def test_closed_again_records_again(self):
        number = self.close_b()
        run(self.http)
        self.set_groups(group('A', [report(1, '0.11.1300')]), group('B', [report(2, '0.11.1301'),
                                                                          report(6, '0.11.1311')]))
        self.marketplace.traces['6'] = TRACE
        run(self.http)
        self.assertEqual('open', self.github.issues[number]['state'])
        self.github.releases.insert(0, {'tag_name': 'v0.11.1320', 'draft': False})
        self.github.close(number, at='2026-11-15T10:00:00Z')
        run(self.http)
        self.assertEqual('0.11.1320', ea.read_state(self.github.issues[number]['body'])['closed']['release'])
        self.set_groups(group('A', [report(1, '0.11.1300')]), group('B', [report(2, '0.11.1301'),
                                                                          report(6, '0.11.1311'),
                                                                          report(7, '0.11.1315')]))
        self.unchanged_by_another_run()

    def test_newest_release_ignores_drafts_and_other_tags(self):
        self.assertEqual('0.11.1310', ea.newest_release_of([
            {'tag_name': 'v0.11.1400', 'draft': True}, {'tag_name': 'v1.0.0'}, {'tag_name': 'v0.11.1310'},
            {'tag_name': 'v0.11.999'}, {'tag_name': 'nightly'}, 'junk']))
        self.assertIsNone(ea.newest_release_of([]))

    def test_one_stack_trace_per_new_or_changed_group(self):
        run(self.http)
        traces = [url for _, url, _ in self.http.requests if url.endswith('/stacktrace')]
        self.assertEqual(sorted([API + '/exceptions/1/stacktrace', API + '/exceptions/3/stacktrace']), sorted(traces))
        self.http.requests.clear()
        run(self.http)
        self.assertEqual([], [url for _, url, _ in self.http.requests if url.endswith('/stacktrace')])

    def test_summary_file(self):
        with tempfile.TemporaryDirectory() as directory:
            path = os.path.join(directory, 'summary.md')
            run(self.http, '--dry-run', '--summary', path)
            with open(path, encoding='utf-8') as file:
                text = file.read()
        self.assertIn('### Marketplace exceptions (dry-run)', text)
        self.assertIn('`Marketplace exception: A`', text)

    def test_run_needs_a_github_token(self):
        code, _, err = run(self.http, environ={'MARKETPLACE_TOKEN': TOKEN})
        self.assertEqual(1, code)
        self.assertIn('GITHUB_TOKEN is not set', err)


if __name__ == '__main__':
    unittest.main()
