"""Unit tests of watch.py: python3 -m unittest discover -s scripts/compat -p 'test_*.py' -v

The feeds and the GitHub REST API are faked (World); nothing goes to the network."""

import copy
import datetime
import io
import json
import os
import re
import unittest
import unittest.mock
import urllib.parse
import zipfile

import compat
import watch

try:
    import yaml
except ImportError:  # PyYAML is on the GitHub runners; without it only the workflow tests are skipped.
    yaml = None

REPOSITORY = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..')
WATCH_YML = os.path.join(REPOSITORY, '.github', 'workflows', 'compat-watch.yml')
GRADLE_YML = os.path.join(REPOSITORY, '.github', 'workflows', 'gradle.yml')
PINNED = compat.read_pins(GRADLE_YML)['OTP_VERSION']
MAJOR = int(PINNED.split('.')[0])

SHA = 'a' * 40
SHA2 = 'b' * 40
BASE_IDE = '263.7307.200'
T0 = datetime.datetime(2026, 11, 15, 0, 17, tzinfo=datetime.timezone.utc)
HOUR = datetime.timedelta(hours=1)
BOT = {'login': 'github-actions[bot]'}


def zipped(members):
    data = io.BytesIO()
    with zipfile.ZipFile(data, 'w') as archive:
        for name, content in members.items():
            archive.writestr(name, content if isinstance(content, (bytes, str)) else json.dumps(content))
    return data.getvalue()


def corpus_error(file, description, line=1, column=1):
    return {'file': file, 'line': line, 'column': column, 'description': description}


def listed(items, total=None, limit=compat.LIST_LIMIT):
    total = len(items) if total is None else total
    return {'total': total, 'trimmed': total > limit, 'items': items[:limit]}


def otp_result(sha, version, run_id, requested='', errors=(), status='failed', known=True, total=None,
               failed_tests=()):
    release = re.split(r'[.-]', version)[0]
    errors = list(errors)
    return {
        'schemaVersion': 1, 'job': 'otp', 'status': status,
        'run': {'runId': run_id, 'event': 'workflow_dispatch'},
        'inputs': {'otp': requested, 'ref': sha},
        'resolved': {'commit': sha, 'otp': {'release': release, 'version': version}},
        'checks': {
            'compilationTests': {'status': 'failed' if failed_tests else 'passed', 'failedTests': listed(list(failed_tests)),
                                 'skippedTests': listed([])},
            'corpus': {'status': 'failed' if errors else 'passed', 'errors': listed(errors, total),
                       'exceptions': listed([])},
            'knownRelease': {'status': 'passed' if known else 'failed', 'release': release,
                             'sdkReleaseConstant': 'V_%s_0' % release if known else None,
                             'mockSdk': 'testData/mockSdk-R%s' % release if known else None},
        },
    }


def ide_result(sha, build, run_id, requested='', status='passed', failed_tests=(), verifier=()):
    report = {'ide': 'IU-' + build, 'problems': {'compatibility-problems': listed(list(verifier))}}
    return {
        'schemaVersion': 1, 'job': 'ide', 'status': status,
        'run': {'runId': run_id, 'event': 'workflow_dispatch'},
        'inputs': {'ide': requested, 'ref': sha},
        'resolved': {'commit': sha, 'ide': {'productCode': 'IU', 'buildNumber': build, 'build': 'IU-' + build}},
        'checks': {
            'compile': {'status': 'passed', 'errors': listed([])},
            'test': {'status': 'failed' if failed_tests else 'passed', 'failedTests': listed(list(failed_tests))},
            'verifyPlugin': {'status': 'failed' if verifier else 'passed', 'reports': [report]},
            'releasedPlugin': {'status': 'passed', 'reports': []},
        },
    }


BASE_ERRORS = [corpus_error('lib/stdlib-6.2.2.3/src/a.erl', "'maybe' unexpected", 10),
               corpus_error('lib/compiler-8.6/src/b.erl', '<expression> expected', 5)]


class World:
    """The feeds and the GitHub REST API, in memory."""

    API = 'https://api.github.com/repos/owner/repo'

    def __init__(self, otp=(), ide=()):
        self.otp_feed = 'maint\nmaint-%d\nmaster\n' % MAJOR + ''.join(
            'OTP-%s 0123456789abcdef 2026-11-01T00:00:00Z sha256\n' % v for v in otp)
        self.ide_feed = {'IIU': [{'build': b, 'version': n, 'type': 'release'} for b, n in ide]}
        self.feed_status = {}
        self.head = SHA
        self.issues = []
        self.labels = set()
        self.runs = {}
        self.artifacts = {}
        self.next_run = 1000
        self.next_artifact = 1
        self.dispatched = []
        self.dispatch_status = 200
        self.crash_on_dispatch = None
        self.writes = []
        self.now = T0

    # Setting up

    def add_issue(self, title, body, label, user=BOT, **extra):
        issue = dict({'number': len(self.issues) + 1, 'title': title, 'body': body, 'labels': [{'name': label}],
                      'user': user, 'state': 'open'}, **extra)
        self.issues.append(issue)
        return issue

    def finish(self, run_id, results=None, logs=None):
        """Run `run_id` completes with the result files `results` ({kind: result}) and the corpus report `logs`."""
        self.runs[run_id]['status'] = 'completed'
        for kind, result in (results or {}).items():
            self.add_artifact(run_id, 'compat-' + kind, {'compat-%s.json' % kind: result})
        if logs is not None:
            self.add_artifact(run_id, 'compat-otp-logs', {'corpus-report.json': logs})

    def add_artifact(self, run_id, name, members):
        self.artifacts.setdefault(run_id, []).append((self.next_artifact, name, zipped(members)))
        self.next_artifact += 1

    def run_of(self, inputs):
        return next(r for r, i in self.dispatched if i == inputs)

    # The state issue

    def state_issue(self):
        found = [i for i in self.issues if i['title'] == 'Compatibility watch' and i['user'] == BOT
                 and i['labels'] == [{'name': 'compatibility-watch'}]]
        return found[0] if found else None

    def state(self):
        return watch.parse_state(self.state_issue()['body'])

    def version_issues(self):
        return [i for i in self.issues if i['title'].startswith('Compatibility: ')]

    # HTTP

    def request(self, method, url, body=None, headers=None, auth=False):
        if url == watch.OTP_FEED:
            return watch.Response(self.feed_status.get('otp', 200), self.otp_feed.encode())
        if url == watch.IDE_FEED:
            return watch.Response(self.feed_status.get('ide', 200), json.dumps(self.ide_feed).encode())
        assert auth, url
        assert url.startswith(self.API), url
        parts = urllib.parse.urlsplit(url[len(self.API):])
        path, query = parts.path, dict(urllib.parse.parse_qsl(parts.query))
        if method != 'GET':
            self.writes.append((method, path))
        return self.handle(method, path, query, body)

    def json(self, data, status=200):
        return watch.Response(status, json.dumps(data).encode())

    def handle(self, method, path, query, body):
        if method == 'GET' and path == '':
            return self.json({'default_branch': 'master'})
        if method == 'GET' and path == '/commits/heads/master':
            return self.json({'sha': self.head})
        match = re.fullmatch(r'/actions/runs/([0-9]+)', path)
        if method == 'GET' and match:
            run = self.runs.get(int(match.group(1)))
            return self.json(run) if run else watch.Response(404)
        if method == 'GET' and path == '/actions/workflows/compat.yml/runs':
            start, end = [watch.parse_time(t) for t in query['created'].split('..')]
            runs = [r for r in self.runs.values() if start <= watch.parse_time(r['created_at']) <= end]
            return self.json({'workflow_runs': runs if query.get('page') == '1' else []})
        match = re.fullmatch(r'/actions/runs/([0-9]+)/artifacts', path)
        if method == 'GET' and match:
            found = [{'id': i, 'name': n} for i, n, _ in self.artifacts.get(int(match.group(1)), [])
                     if n == query.get('name')]
            return self.json({'artifacts': found if query.get('page') == '1' else []})
        match = re.fullmatch(r'/actions/artifacts/([0-9]+)/zip', path)
        if method == 'GET' and match:
            for artifacts in self.artifacts.values():
                for i, _, data in artifacts:
                    if i == int(match.group(1)):
                        return watch.Response(200, data)
            return watch.Response(404)
        if method == 'GET' and path == '/issues':
            found = [i for i in self.issues if query['labels'] in [label['name'] for label in i['labels']]]
            return self.json(copy.deepcopy(found) if query.get('page') == '1' else [])
        match = re.fullmatch(r'/labels/(.+)', path)
        if method == 'GET' and match:
            return self.json({'name': match.group(1)}) if match.group(1) in self.labels else watch.Response(404)
        if method == 'POST' and path == '/labels':
            self.labels.add(body['name'])
            return self.json(body, 201)
        if method == 'POST' and path == '/issues':
            assert set(body['labels']) <= self.labels
            issue = self.add_issue(body['title'], body['body'], body['labels'][0])
            return self.json({'number': issue['number']}, 201)
        match = re.fullmatch(r'/issues/([0-9]+)', path)
        if method == 'PATCH' and match:
            self.issues[int(match.group(1)) - 1]['body'] = body['body']
            return self.json({})
        if method == 'POST' and path == '/actions/workflows/compat.yml/dispatches':
            assert body['ref'] == 'master' and body['return_run_details'] is True
            if self.dispatch_status not in (200, 204):
                return watch.Response(self.dispatch_status)
            run_id = self.next_run
            self.next_run += 1
            self.runs[run_id] = {'id': run_id, 'path': '.github/workflows/compat.yml', 'event': 'workflow_dispatch',
                                 'status': 'in_progress', 'created_at': watch.iso(self.now)}
            self.dispatched.append((run_id, body['inputs']))
            if self.crash_on_dispatch is not None and len(self.dispatched) == self.crash_on_dispatch:
                raise KeyboardInterrupt('crash')
            if self.dispatch_status == 204:
                return watch.Response(204)
            return self.json({'workflow_run_id': run_id, 'run_url': '', 'html_url': ''})
        raise AssertionError('unexpected call %s %s' % (method, path))


def cycle(world, now=None, dry_run=False):
    """One cycle of the watcher at `now`; (exit code, output)."""
    world.now = now or world.now
    out = io.StringIO()
    argv = ['--gradle-yml', GRADLE_YML] + (['--dry-run'] if dry_run else [])
    with unittest.mock.patch.dict(os.environ, {'GITHUB_REPOSITORY': 'owner/repo'}):
        code = watch.main(argv, http=world, now=world.now, out=out)
    return code, out.getvalue()


def entry(state, kind, version):
    return next(e for e in state['entries'] if e['kind'] == kind and e['version'] == version)


class Seeded(unittest.TestCase):
    """A world in which the first two cycles ran: the baseline of SHA is complete and both kinds are seeded."""

    OTP = [PINNED, '%d.0' % (MAJOR + 1), '%d.0.1' % (MAJOR + 1), '%d.3' % (MAJOR - 1)]
    IDE = [(BASE_IDE, '2026.3'), ('263.7307.112', None), ('253.1.1', '2025.3')]

    def setUp(self):
        self.world = World(self.OTP, self.IDE)
        code, self.first = cycle(self.world)
        self.assertEqual(code, 0, self.first)
        world = self.world
        self.baseline_run = world.run_of({'ref': SHA})
        world.finish(self.baseline_run, {'otp': otp_result(SHA, PINNED, self.baseline_run, errors=BASE_ERRORS),
                                         'ide': ide_result(SHA, BASE_IDE, self.baseline_run)})

    def finish_otp(self, version, errors=BASE_ERRORS, sha=SHA, **kwargs):
        run_id = self.world.run_of({'otp': version, 'ref': sha})
        self.world.finish(run_id, {'otp': otp_result(sha, version, run_id, version, errors=errors, **kwargs)})
        return run_id

    def finish_ide(self, build, sha=SHA, **kwargs):
        run_id = self.world.run_of({'ide': build, 'ref': sha})
        self.world.finish(run_id, {'ide': ide_result(sha, build, run_id, build, **kwargs)})
        return run_id


class SeedingTest(Seeded):
    def test_first_cycle_records_everything_and_queues_the_newest(self):
        state = self.world.state()
        self.assertEqual(state['pending'], ['ide'])
        self.assertEqual(state['floor']['otp'], MAJOR)
        self.assertEqual(state['seen']['otp'], [PINNED, '%d.0' % (MAJOR + 1), '%d.0.1' % (MAJOR + 1)])
        # The newest of each watched major release; the pinned one is the baseline, the older major is not watched.
        self.assertEqual(entry(state, 'otp', PINNED)['verdict'], 'baseline')
        self.assertEqual(entry(state, 'otp', '%d.0.1' % (MAJOR + 1))['state'], 'running')
        self.assertNotIn('%d.0' % (MAJOR + 1), [e['version'] for e in state['entries']])
        # The IDE waits for the branch, which the baseline names.
        self.assertFalse([e for e in state['entries'] if e['kind'] == 'ide'])
        self.assertEqual(state['seen']['ide'], [])
        self.assertEqual([i for _, i in self.world.dispatched], [{'ref': SHA}, {'otp': '%d.0.1' % (MAJOR + 1), 'ref': SHA}])
        self.assertEqual(self.world.labels, {'compatibility-watch'})
        self.assertEqual(self.world.version_issues(), [])

    def test_second_cycle_seeds_the_ide_from_the_baseline(self):
        code, out = cycle(self.world, T0 + HOUR)
        self.assertEqual(code, 0, out)
        state = self.world.state()
        self.assertEqual(state['pending'], [])
        self.assertEqual(state['floor']['ide'], 263)
        self.assertEqual(state['baselines'][0]['state'], 'complete')
        self.assertEqual(state['seen']['ide'], ['263.7307.112', BASE_IDE])
        # The newest of the branch is the very build of the baseline: recorded, not checked.
        self.assertEqual(entry(state, 'ide', BASE_IDE)['verdict'], 'baseline')
        self.assertEqual(entry(state, 'ide', BASE_IDE)['name'], '2026.3')
        self.assertEqual(len(self.world.dispatched), 2)

    def test_a_dry_run_writes_nothing(self):
        world = World(self.OTP, self.IDE)
        code, out = cycle(world, dry_run=True)
        self.assertEqual(code, 0, out)
        self.assertEqual(world.writes, [])
        self.assertIn('[dry run] would start compat.yml on master with the inputs {"ref": "%s"}' % SHA, out)
        self.assertIn('[dry run] would create the state issue at the end of the cycle', out)

    def test_seeding_never_files_more_than_the_limit(self):
        world = World(['%d.0' % m for m in range(MAJOR, MAJOR + 9)], self.IDE)
        filed = []
        for hour in range(6):
            for run_id, inputs in world.dispatched:
                if world.runs[run_id]['status'] == 'completed':
                    continue
                if 'otp' in inputs:
                    world.finish(run_id, {'otp': otp_result(SHA, inputs['otp'], run_id, inputs['otp'], known=False)})
                else:
                    world.finish(run_id, {'otp': otp_result(SHA, PINNED, run_id),
                                          'ide': ide_result(SHA, BASE_IDE, run_id)})
            before = len(world.version_issues())
            code, out = cycle(world, T0 + hour * HOUR)
            self.assertEqual(code, 0, out)
            filed.append(len(world.version_issues()) - before)
            self.assertLessEqual(len(world.dispatched) - sum(1 for r in world.runs.values()
                                                             if r['status'] == 'completed'), watch.MAX_IN_FLIGHT)
        self.assertEqual(max(filed), watch.MAX_ISSUES_PER_CYCLE)
        self.assertEqual(sum(filed), 9)


class LaterCycleTest(Seeded):
    def setUp(self):
        super().setUp()
        self.next = '%d.0.1' % (MAJOR + 1)
        cycle(self.world, T0 + HOUR)

    def test_a_new_patch_version_is_checked_against_the_baseline(self):
        world = self.world
        self.finish_otp(self.next)
        cycle(world, T0 + 2 * HOUR)
        self.assertEqual(entry(world.state(), 'otp', self.next)['verdict'], 'same')
        patch = '%d.0.2' % (MAJOR + 1)
        world.otp_feed += 'OTP-%s x y\n' % patch
        cycle(world, T0 + 3 * HOUR)
        self.assertEqual(world.dispatched[-1][1], {'otp': patch, 'ref': SHA})
        self.assertEqual(entry(world.state(), 'otp', patch)['state'], 'running')
        run_id = self.finish_otp(patch, BASE_ERRORS + [corpus_error('lib/ssl-12/src/new.erl', "'x' unexpected")])
        code, out = cycle(world, T0 + 4 * HOUR)
        self.assertEqual(code, 0, out)
        state = world.state()
        issue = world.version_issues()[0]
        self.assertEqual(issue['title'], 'Compatibility: Erlang/OTP %s' % patch)
        self.assertEqual(issue['labels'], [{'name': 'compatibility'}])
        self.assertIn('<!-- compat-watch:otp:%s -->' % patch, issue['body'])
        self.assertIn("lib/ssl-12/src/new.erl: 'x' unexpected", issue['body'])
        self.assertNotIn('a.erl', issue['body'])
        self.assertIn('/owner/repo/actions/runs/%d' % run_id, issue['body'])
        self.assertIn('/owner/repo/actions/runs/%d' % self.baseline_run, issue['body'])
        self.assertIn('`gh workflow run compat.yml -f otp=%s -f ref=%s`' % (patch, SHA), issue['body'])
        self.assertEqual(entry(state, 'otp', patch)['verdict'], 'new problems')
        self.assertEqual(entry(state, 'otp', patch)['issue'], issue['number'])
        # Later cycles neither file again nor start anything.
        writes = len(world.dispatched)
        cycle(world, T0 + 5 * HOUR)
        self.assertEqual(len(world.version_issues()), 1)
        self.assertEqual(len(world.dispatched), writes)

    def test_newer_versions_supersede_older_ones(self):
        world = self.world
        world.otp_feed += ''.join('OTP-%s x y\n' % v for v in ('%d.1' % (MAJOR + 1), '%d.1.1' % (MAJOR + 1),
                                                                 '%d.1.2' % (MAJOR + 1)))
        world.dispatch_status = 403
        cycle(world, T0 + 2 * HOUR)
        state = world.state()
        self.assertEqual(entry(state, 'otp', '%d.1' % (MAJOR + 1))['verdict'], 'superseded')
        self.assertEqual(entry(state, 'otp', '%d.1.1' % (MAJOR + 1))['verdict'], 'superseded')
        self.assertEqual(entry(state, 'otp', '%d.1.2' % (MAJOR + 1))['state'], 'queued')
        world.otp_feed += 'OTP-%d.1.3 x y\n' % (MAJOR + 1)
        world.dispatch_status = 200
        cycle(world, T0 + 3 * HOUR)
        state = world.state()
        self.assertEqual(entry(state, 'otp', '%d.1.2' % (MAJOR + 1))['verdict'], 'superseded')
        self.assertEqual(world.dispatched[-1][1], {'otp': '%d.1.3' % (MAJOR + 1), 'ref': SHA})

    def test_the_limits_of_starts_and_runs_in_flight(self):
        world = self.world
        world.otp_feed += ''.join('OTP-%d.0 x y\n' % m for m in range(MAJOR + 2, MAJOR + 12))
        before = len(world.dispatched)
        cycle(world, T0 + 2 * HOUR)
        self.assertEqual(len(world.dispatched) - before, watch.MAX_STARTS_PER_CYCLE)
        cycle(world, T0 + 3 * HOUR)
        state = world.state()
        running = [e for e in state['entries'] if e['state'] in ('starting', 'running')]
        self.assertEqual(len(running), watch.MAX_IN_FLIGHT)
        self.assertEqual(len([e for e in state['entries'] if e['state'] == 'queued']), 3)
        cycle(world, T0 + 4 * HOUR)
        self.assertEqual(len(world.dispatched) - before, watch.MAX_IN_FLIGHT - 1)

    def test_a_crash_between_starting_runs_and_writing_the_state(self):
        world = self.world
        world.otp_feed += 'OTP-%d.0 x y\nOTP-%d.0 x y\n' % (MAJOR + 2, MAJOR + 3)
        world.crash_on_dispatch = len(world.dispatched) + 1
        with self.assertRaises(KeyboardInterrupt):
            cycle(world, T0 + 2 * HOUR)
        # The state was written before the runs were started.
        state = world.state()
        self.assertEqual(entry(state, 'otp', '%d.0' % (MAJOR + 2))['state'], 'starting')
        self.assertEqual(entry(state, 'otp', '%d.0' % (MAJOR + 3))['state'], 'starting')
        world.crash_on_dispatch = None
        started = len(world.dispatched)
        cycle(world, T0 + 2 * HOUR + datetime.timedelta(minutes=10))
        self.assertEqual(len(world.dispatched), started)
        # The run that did start is found by its result.
        run_id = self.finish_otp('%d.0' % (MAJOR + 2))
        code, out = cycle(world, T0 + 3 * HOUR)
        self.assertEqual(code, 0, out)
        state = world.state()
        self.assertEqual(entry(state, 'otp', '%d.0' % (MAJOR + 2))['verdict'], 'same')
        self.assertEqual(entry(state, 'otp', '%d.0' % (MAJOR + 2))['runs'], [run_id])
        self.assertEqual(len(world.dispatched), started)
        # The one that never started is an attempt without a result after the timeout, and starts again.
        code, out = cycle(world, T0 + 9 * HOUR)
        self.assertEqual(entry(world.state(), 'otp', '%d.0' % (MAJOR + 3))['tries'], 2)
        self.assertEqual(world.dispatched[-1][1], {'otp': '%d.0' % (MAJOR + 3), 'ref': SHA})

    def test_runs_whose_id_is_not_returned_are_found_by_their_result(self):
        world = self.world
        world.dispatch_status = 204
        version = '%d.0' % (MAJOR + 2)
        world.otp_feed += 'OTP-%s x y\n' % version
        cycle(world, T0 + 2 * HOUR)
        self.assertEqual(entry(world.state(), 'otp', version)['state'], 'starting')
        # A finished run in the same window with the result of another version is not taken for it.
        world.runs[5000] = dict(world.runs[world.run_of({'otp': version, 'ref': SHA})], id=5000)
        world.finish(5000, {'otp': otp_result(SHA, '%d.1' % (MAJOR + 2), 5000, '%d.1' % (MAJOR + 2))})
        cycle(world, T0 + 3 * HOUR)
        self.assertEqual(entry(world.state(), 'otp', version)['state'], 'starting')
        run_id = self.finish_otp(version)
        cycle(world, T0 + 4 * HOUR)
        e = entry(world.state(), 'otp', version)
        self.assertEqual((e['verdict'], e['runs']), ('same', [run_id]))

    def finish_without_result(self, version, how):
        run_id = self.world.run_of({'otp': version, 'ref': SHA})
        self.world.dispatched = [(r, i) for r, i in self.world.dispatched if r != run_id]
        if how == 'no artifact':
            self.world.finish(run_id)
        else:
            self.world.finish(run_id, {'otp': otp_result(SHA, version, run_id, version, status='incomplete')})

    def test_three_attempts_without_a_result_leave_the_version_unchecked(self):
        world = self.world
        for attempt, how in enumerate(['no artifact', 'incomplete', 'no artifact']):
            self.finish_without_result(self.next, how)
            code, out = cycle(world, T0 + (2 + attempt) * HOUR)
            self.assertEqual(code, 0, out)
            e = entry(world.state(), 'otp', self.next)
            if attempt < 2:
                self.assertEqual((e['state'], e['tries']), ('running', attempt + 2), out)
        e = entry(world.state(), 'otp', self.next)
        self.assertEqual((e['state'], e['verdict'], len(e['runs'])), ('done', 'unchecked', 3))
        self.assertEqual(world.version_issues(), [])
        body = world.state_issue()['body']
        for run_id in e['runs']:
            self.assertIn('/actions/runs/%d' % run_id, body)

    def test_a_run_that_never_finishes_is_an_attempt(self):
        world = self.world
        cycle(world, T0 + 5 * HOUR)
        self.assertEqual(entry(world.state(), 'otp', self.next)['tries'], 1)
        cycle(world, T0 + 7 * HOUR)
        self.assertEqual(entry(world.state(), 'otp', self.next)['tries'], 2)

    def test_a_result_of_another_version_or_commit_is_not_used(self):
        world = self.world
        run_id = world.run_of({'otp': self.next, 'ref': SHA})
        world.finish(run_id, {'otp': otp_result(SHA, '%d.0' % (MAJOR + 1), run_id, self.next,
                                                errors=[corpus_error('x/y.erl', 'boom')])})
        cycle(world, T0 + 2 * HOUR)
        e = entry(world.state(), 'otp', self.next)
        self.assertEqual((e['state'], e['tries']), ('running', 2))
        run_id = e['run']
        world.finish(run_id, {'otp': otp_result(SHA2, self.next, run_id, self.next,
                                                errors=[corpus_error('x/y.erl', 'boom')])})
        cycle(world, T0 + 3 * HOUR)
        self.assertEqual(entry(world.state(), 'otp', self.next)['tries'], 3)
        self.assertEqual(world.version_issues(), [])

    def test_an_ide_build_is_checked_with_its_build_number(self):
        world = self.world
        world.ide_feed['IIU'].insert(0, {'build': '263.8000.12', 'version': '2026.3.1', 'type': 'release'})
        world.ide_feed['IIU'].insert(0, {'build': '263.7999.1', 'version': None, 'type': 'eap'})
        cycle(world, T0 + 2 * HOUR)
        self.assertEqual(world.dispatched[-1][1], {'ide': '263.8000.12', 'ref': SHA})
        self.assertEqual(entry(world.state(), 'ide', '263.7999.1')['verdict'], 'superseded')
        self.finish_ide('263.8000.12', failed_tests=[{'class': 'org.A', 'name': 'testB', 'message': 'boom'}],
                        status='failed')
        cycle(world, T0 + 3 * HOUR)
        issue = world.version_issues()[0]
        self.assertEqual(issue['title'], 'Compatibility: IntelliJ IDEA 2026.3.1 (263.8000.12)')
        self.assertIn('<!-- compat-watch:ide:263.8000.12 -->', issue['body'])
        self.assertIn('org.A.testB: boom', issue['body'])

    def test_a_baseline_whose_result_is_gone_is_run_again(self):
        world = self.world
        world.artifacts.pop(self.baseline_run)
        self.finish_otp(self.next)
        cycle(world, T0 + 2 * HOUR)
        # The same cycle starts a new baseline and the version again; the lost baseline cost no attempt.
        e = entry(world.state(), 'otp', self.next)
        self.assertEqual((e['state'], e['tries']), ('running', 1))
        self.assertEqual([b['state'] for b in world.state()['baselines']], ['running'])
        self.assertEqual([i for _, i in world.dispatched[-2:]], [{'ref': SHA}, {'otp': self.next, 'ref': SHA}])
        base = world.dispatched[-2][0]
        world.finish(base, {'otp': otp_result(SHA, PINNED, base, errors=BASE_ERRORS),
                            'ide': ide_result(SHA, BASE_IDE, base)})
        run_id = world.dispatched[-1][0]
        world.finish(run_id, {'otp': otp_result(SHA, self.next, run_id, self.next, errors=BASE_ERRORS)})
        cycle(world, T0 + 4 * HOUR)
        e = entry(world.state(), 'otp', self.next)
        self.assertEqual((e['verdict'], e['tries']), ('same', 1))

    def test_an_old_baseline_is_run_again(self):
        world = self.world
        self.finish_otp(self.next)
        cycle(world, T0 + 2 * HOUR)
        world.otp_feed += 'OTP-%d.0.2 x y\n' % (MAJOR + 1)
        cycle(world, T0 + watch.BASELINE_MAX_AGE + 3 * HOUR)
        self.assertEqual([i for _, i in world.dispatched[-2:]], [{'ref': SHA}, {'otp': '%d.0.2' % (MAJOR + 1),
                                                                              'ref': SHA}])

    def test_a_new_head_commit_gets_its_own_baseline(self):
        world = self.world
        self.finish_otp(self.next)
        world.head = SHA2
        world.otp_feed += 'OTP-%d.0.2 x y\n' % (MAJOR + 1)
        cycle(world, T0 + 2 * HOUR)
        self.assertEqual([i for _, i in world.dispatched[-2:]], [{'ref': SHA2}, {'otp': '%d.0.2' % (MAJOR + 1),
                                                                               'ref': SHA2}])
        world.otp_feed += 'OTP-%d.0.3 x y\n' % (MAJOR + 1)
        cycle(world, T0 + 3 * HOUR)
        # The baseline of SHA2 is in flight: no second one.
        self.assertEqual(world.dispatched[-1][1], {'otp': '%d.0.3' % (MAJOR + 1), 'ref': SHA2})


class StateTest(unittest.TestCase):
    def test_an_unparsable_state_body_stops_the_cycle(self):
        for body in ['no block', '<!-- compat-watch-state:begin -->\n```json\n{oops\n```\n<!-- compat-watch-state:end -->',
                     '<!-- compat-watch-state:begin -->\n```json\n{"format": 1}\n```\n'
                     '<!-- compat-watch-state:end -->']:
            world = World([PINNED], [(BASE_IDE, '2026.3')])
            world.add_issue('Compatibility watch', body, 'compatibility-watch')
            code, out = cycle(world)
            self.assertEqual(code, 1)
            self.assertIn('does not seed again', out)
            self.assertEqual(world.writes, [])
            self.assertEqual(world.dispatched, [])

    def test_a_foreign_issue_is_neither_the_state_nor_the_issue_of_a_version(self):
        world = World([PINNED, '%d.0' % (MAJOR + 1)], [(BASE_IDE, '2026.3')])
        state = watch.render_state(watch.new_state(T0), 'https://github.com', 'owner/repo')
        world.add_issue('Compatibility watch', state, 'compatibility-watch', user={'login': 'mallory'})
        world.add_issue('Compatibility watch', state, 'other', user=BOT)
        world.add_issue('Compatibility: Erlang/OTP %d.0' % (MAJOR + 1),
                        '<!-- compat-watch:otp:%d.0 -->' % (MAJOR + 1), 'compatibility', user={'login': 'mallory'})
        code, out = cycle(world)
        self.assertEqual(code, 0, out)
        # Neither the issue of another author with the label nor the bot's issue without it is the state.
        self.assertEqual(world.state_issue()['number'], 4)
        self.assertEqual([i['body'] for i in world.issues[:2]], [state, state])
        self.assertEqual(world.state()['pending'], ['ide'])
        base = world.run_of({'ref': SHA})
        world.finish(base, {'otp': otp_result(SHA, PINNED, base), 'ide': ide_result(SHA, BASE_IDE, base)})
        run_id = world.run_of({'otp': '%d.0' % (MAJOR + 1), 'ref': SHA})
        world.finish(run_id, {'otp': otp_result(SHA, '%d.0' % (MAJOR + 1), run_id, '%d.0' % (MAJOR + 1),
                                                known=False)})
        cycle(world, T0 + HOUR)
        mine = [i for i in world.version_issues() if i['user'] == BOT]
        self.assertEqual(len(mine), 1)
        self.assertEqual(world.issues[2]['body'], '<!-- compat-watch:otp:%d.0 -->' % (MAJOR + 1))

    def test_the_issue_of_a_version_is_found_after_a_crash(self):
        world = World([PINNED, '%d.0' % (MAJOR + 1)], [(BASE_IDE, '2026.3')])
        cycle(world)
        world.add_issue('Compatibility: Erlang/OTP %d.0' % (MAJOR + 1), 'filed before the crash\n'
                        '<!-- compat-watch:otp:%d.0 -->' % (MAJOR + 1), 'compatibility')
        base = world.run_of({'ref': SHA})
        world.finish(base, {'otp': otp_result(SHA, PINNED, base), 'ide': ide_result(SHA, BASE_IDE, base)})
        run_id = world.run_of({'otp': '%d.0' % (MAJOR + 1), 'ref': SHA})
        world.finish(run_id, {'otp': otp_result(SHA, '%d.0' % (MAJOR + 1), run_id, '%d.0' % (MAJOR + 1),
                                                known=False)})
        cycle(world, T0 + HOUR)
        self.assertEqual(len(world.version_issues()), 1)
        self.assertEqual(entry(world.state(), 'otp', '%d.0' % (MAJOR + 1))['issue'], 2)

    def test_the_state_stays_small(self):
        state = watch.new_state(T0)
        state['seen']['otp'] = ['%d.%d.%d' % (MAJOR, i, j) for i in range(30) for j in range(20)]
        state['entries'] = [dict(watch.ENTRY_DEFAULTS, kind='otp', version='%d.%d' % (MAJOR, i), seen='2026-11-15',
                                 state='done', verdict='same', run=10 ** 11 + i, runs=[10 ** 11 + i])
                            for i in range(200)]
        watcher = watch.Watcher(watch.GitHub(None, 'owner/repo'), None, {'OTP_VERSION': PINNED}, T0)
        watcher.state = state
        watcher.prune()
        body = watch.render_state(state, 'https://github.com', 'owner/repo')
        self.assertEqual(len(state['entries']), watch.TABLE_ROWS)
        self.assertLess(len(body), watch.STATE_BODY_TARGET)
        self.assertEqual(watch.parse_state(body)['seen'], state['seen'])

    def test_the_state_rejects_values_the_watcher_never_writes(self):
        state = watch.new_state(T0)
        state['seen']['otp'] = ['27.0`@x']
        body = watch.render_state(watch.new_state(T0), 'https://github.com', 'owner/repo')
        self.assertEqual(watch.parse_state(body)['pending'], ['ide', 'otp'])
        with self.assertRaises(watch.WatchError):
            watch.parse_state(body.replace('"otp":[]', '"otp":["27.0`@x"]'))


class MatchTest(unittest.TestCase):
    def test_a_result_must_be_the_one_that_was_asked_for(self):
        self.assertTrue(watch.Watcher.matches(otp_result(SHA, '29.1', 7, '29.1'), 'otp', SHA, '29.1', 7))
        self.assertTrue(watch.Watcher.matches(otp_result(SHA, PINNED, 7), 'otp', SHA, '', 7))
        self.assertTrue(watch.Watcher.matches(ide_result(SHA, '263.1.2', 7, '263.1.2'), 'ide', SHA, '263.1.2', 7))
        for result, kind in [(otp_result(SHA, '29.1', 7, '29.1.1'), 'otp'),  # another input
                             (otp_result(SHA, '29.1.1', 7, '29.1'), 'otp'),  # another version installed
                             (otp_result(SHA2, '29.1', 7, '29.1'), 'otp'),  # another commit
                             (otp_result(SHA, '29.1', 8, '29.1'), 'otp'),  # another run
                             (otp_result(SHA, '29.1', 7, '29.1'), 'ide'),  # another job
                             (otp_result(SHA, PINNED, 7), 'otp')]:  # the baseline
            self.assertFalse(watch.Watcher.matches(result, kind, SHA, '29.1', 7), result)
        self.assertFalse(watch.Watcher.matches(otp_result(SHA, '29.1', 7, '29.1'), 'otp', SHA, '', 7))


class FeedTest(unittest.TestCase):
    def test_a_hostile_version_string_in_a_feed_is_ignored(self):
        hostile = ['%d.0`@everyone' % (MAJOR + 1), '%d.0\n<!--' % (MAJOR + 1), '%d.0 [x](http://e)' % (MAJOR + 1)]
        world = World([PINNED, '%d.0' % (MAJOR + 1)], [(BASE_IDE, '2026.3'), ('263.9.9`@x', '2026.3.9')])
        world.otp_feed += 'OTP-%s.1`@everyone x\n' % PINNED
        world.ide_feed['IIU'] += [{'build': h, 'version': h} for h in hostile] + [
            {'build': '263.9999.1', 'version': '2026.3`@everyone'}, 'not an object']
        code, out = cycle(world)
        self.assertEqual(code, 0, out)
        self.assertNotIn('@', out.replace('github-actions[bot]', ''))
        self.assertIn('Erlang/OTP: 1 entries without a valid version were ignored', out)
        self.assertIn('IntelliJ IDEA: 5 entries without a valid version were ignored', out)
        base = world.run_of({'ref': SHA})
        world.finish(base, {'otp': otp_result(SHA, PINNED, base), 'ide': ide_result(SHA, BASE_IDE, base)})
        code, out = cycle(world, T0 + HOUR)
        self.assertEqual(code, 0, out)
        self.assertNotIn('@', out.replace('github-actions[bot]', ''))
        state = world.state()
        self.assertNotIn('263.9.9`@x', state['seen']['ide'])
        self.assertIn('263.9999.1', state['seen']['ide'])
        self.assertIsNone(entry(state, 'ide', '263.9999.1')['name'])  # its hostile release version is dropped
        self.assertNotIn('@', world.state_issue()['body'])

    def test_feed_parsing(self):
        world = World([PINNED, '%d.0-rc1' % (MAJOR + 2)], [(BASE_IDE, '2026.3'), ('263.1.2', '2026.2.0.1')])
        versions, rejected = watch.read_otp_feed(world)
        self.assertEqual((sorted(versions), rejected), (sorted([PINNED, '%d.0-rc1' % (MAJOR + 2)]), 0))
        builds, _ = watch.read_ide_feed(world)
        self.assertEqual(builds, {BASE_IDE: '2026.3', '263.1.2': None})
        self.assertEqual(sorted(['28.0', '28.0-rc2', '28.0-rc1', '27.3.4.9'], key=watch.order),
                         ['27.3.4.9', '28.0-rc1', '28.0-rc2', '28.0'])

    def test_a_source_that_cannot_be_read_never_looks_like_no_versions(self):
        world = World([PINNED, '%d.0' % (MAJOR + 1)], [(BASE_IDE, '2026.3')])
        world.feed_status['otp'] = 503
        code, out = cycle(world)
        self.assertEqual(code, 1)
        self.assertIn('::error::Erlang/OTP', out)
        self.assertEqual(world.state()['pending'], ['ide', 'otp'])
        world.feed_status = {}
        world.otp_feed = 'maint\nmaster\n'
        code, out = cycle(world, T0 + HOUR)
        self.assertEqual(code, 1)
        self.assertEqual(world.state()['pending'], ['ide', 'otp'])
        world.otp_feed = 'OTP-%s x\nOTP-%d.0 x\n' % (PINNED, MAJOR + 1)
        world.feed_status['ide'] = 200
        world.ide_feed = {'unexpected': []}
        code, out = cycle(world, T0 + 2 * HOUR)
        self.assertEqual(code, 1)
        self.assertEqual(world.state()['pending'], ['ide'])


class CompareTest(unittest.TestCase):
    def test_the_application_version_and_the_position_are_not_part_of_the_key(self):
        base = otp_result(SHA, PINNED, 1, errors=[corpus_error('lib/stdlib-6.2.2.3/src/x.erl', 'e', 10, 2)])
        result = otp_result(SHA, '29.0', 2, errors=[corpus_error('lib/stdlib-7.0/src/x.erl', 'e', 40, 7)])
        self.assertEqual(watch.compare('otp', result, base)['sections'], [])
        result = otp_result(SHA, '29.0', 2, errors=[corpus_error('lib/stdlib-7.0/src/x.erl', 'e', 40, 7),
                                                    corpus_error('lib/stdlib-7.0/src/x.erl', 'e', 50, 7)])
        self.assertEqual(watch.compare('otp', result, base)['sections'],
                         [('corpus', 'parser errors in the Erlang/OTP sources', ['lib/stdlib-7.0/src/x.erl: e'])])
        self.assertEqual(watch.without_app_version('lib/stdlib-6.2.2.3/src/x.erl'), 'lib/stdlib/src/x.erl')
        self.assertEqual(watch.without_app_version('erts-17.0/preloaded/src/init.erl'), 'erts/preloaded/src/init.erl')

    def test_occurrences_are_counted(self):
        base = otp_result(SHA, PINNED, 1, errors=[corpus_error('a-1/x.erl', 'e')])
        result = otp_result(SHA, '29.0', 2, errors=[corpus_error('a-2/x.erl', 'e', n) for n in range(4)])
        self.assertEqual(watch.compare('otp', result, base)['sections'][0][2], ['a-2/x.erl: e (3 times)'])

    def test_a_trimmed_corpus_list_is_compared_in_full(self):
        shared = [corpus_error('lib/a-1/src/f%d.erl' % i, 'e') for i in range(60)]
        new = [corpus_error('lib/b-1/src/g%d.erl' % i, 'n') for i in range(67)]
        full = shared + new
        base = otp_result(SHA, PINNED, 1, errors=shared)
        result = otp_result(SHA, '29.1.2', 2, errors=full)
        self.assertTrue(result['checks']['corpus']['errors']['trimmed'])
        comparison = watch.compare('otp', result, base, {'result': full})
        self.assertEqual(comparison['notes'], [])
        self.assertEqual(len(comparison['sections'][0][2]), 67)
        comparison = watch.compare('otp', result, base, {'result': 'the artifact is gone'})
        self.assertEqual(len(comparison['sections'][0][2]), 40)
        self.assertIn('the full list of parser errors of the version could not be read: the artifact is gone',
                      comparison['notes'])

    def test_the_full_corpus_report_is_read_from_the_logs_artifact(self):
        world = World([PINNED, '%d.0' % (MAJOR + 1)], [(BASE_IDE, '2026.3')])
        cycle(world)
        base = world.run_of({'ref': SHA})
        world.finish(base, {'otp': otp_result(SHA, PINNED, base, errors=BASE_ERRORS),
                            'ide': ide_result(SHA, BASE_IDE, base)})
        version = '%d.0' % (MAJOR + 1)
        run_id = world.run_of({'otp': version, 'ref': SHA})
        errors = [corpus_error('lib/a-1/src/f%d.erl' % i, 'e') for i in range(127)]
        world.finish(run_id, {'otp': otp_result(SHA, version, run_id, version, errors=errors)},
                     logs={'errors': errors})
        cycle(world, T0 + HOUR)
        body = world.version_issues()[0]['body']
        self.assertIn('**corpus, parser errors in the Erlang/OTP sources**: 127, the first 30 listed', body)
        self.assertNotIn('incomplete', body)
        self.assertFalse(entry(world.state(), 'otp', version)['partial'])

    def test_other_trimmed_lists_make_the_comparison_incomplete(self):
        tests = [{'class': 'C', 'name': 't%d' % i, 'message': 'm'} for i in range(150)]
        base = ide_result(SHA, BASE_IDE, 1, failed_tests=tests[:120])
        result = ide_result(SHA, '263.8.1', 2, failed_tests=tests)
        comparison = watch.compare('ide', result, base)
        self.assertIn('failed tests: the version has 150, the baseline 120; only the first 100 of each were compared',
                      comparison['notes'])
        self.assertEqual(comparison['sections'][0][2][0], '150 failed tests, 120 in the baseline; the ones beyond '
                                                          'the first 100 were not compared')

    def test_checks_and_keys(self):
        base = ide_result(SHA, BASE_IDE, 1, verifier=['old problem'])
        result = ide_result(SHA, '263.8.1', 2, verifier=['old problem', 'new problem'])
        result['checks']['releasedPlugin'] = {'status': 'not-run', 'reason': 'the release has no archive'}
        base['checks']['releasedPlugin'] = {'status': 'passed', 'reports': []}
        result['checks']['compile'] = {'status': 'failed', 'errors': listed([{'file': 'A.java', 'line': 3,
                                                                              'message': 'cannot find symbol'}])}
        base['checks']['compile'] = {'status': 'failed', 'errors': listed([{'file': 'A.java', 'line': 9,
                                                                            'message': 'cannot find symbol'}])}
        sections = watch.compare('ide', result, base)['sections']
        self.assertEqual(sections, [
            ('verifyPlugin', 'Plugin Verifier problems of the built plugin', ['compatibility-problems: new problem']),
            ('releasedPlugin', 'status', ['releasedPlugin: passed in the baseline, not-run here: the release has no '
                                          'archive'])])
        skipped = otp_result(SHA, '29.0', 2)
        skipped['checks']['compilationTests']['skippedTests'] = listed([{'class': 'R', 'name': 'testRebar'}])
        self.assertEqual(watch.compare('otp', skipped, otp_result(SHA, PINNED, 1))['sections'],
                         [('compilationTests', 'skipped compilation tests', ['R.testRebar'])])
        unknown = otp_result(SHA, '30.0', 2, known=False)
        self.assertEqual(watch.compare('otp', unknown, otp_result(SHA, PINNED, 1))['sections'][0][0], 'knownRelease')

    def test_a_hostile_string_in_a_result_is_neutralised_in_the_issue(self):
        hostile = 'x```\n# @maintainer see [here](https://evil)\u202e `' + 'y' * 1000
        base = otp_result(SHA, PINNED, 1)
        result = otp_result(SHA, '29.0', 2, errors=[corpus_error('lib/a-1/' + hostile, hostile)])
        comparison = watch.compare('otp', result, base, {})
        entry_ = dict(watch.ENTRY_DEFAULTS, kind='otp', version='29.0', sha=SHA, run=2)
        body = watch.issue_body(entry_, comparison, 'https://github.com/o/r/actions/runs/2',
                                'https://github.com/o/r/actions/runs/1')
        self.assertNotIn('@', body)
        self.assertNotIn('\u202e', body)
        fences = [line for line in body.splitlines() if '`' in line and line.strip().startswith('```')]
        self.assertEqual(fences, ['```', '```'])
        problem = body.split('```\n')[1].split('\n')[0]
        self.assertEqual(len(problem), watch.ISSUE_TEXT_CHARS)
        self.assertTrue(problem.endswith('\u2026'))


@unittest.skipIf(yaml is None, 'PyYAML is not installed')
class WorkflowTest(unittest.TestCase):
    def setUp(self):
        with open(WATCH_YML, encoding='utf-8') as f:
            self.text = f.read()
        self.workflow = yaml.safe_load(self.text)
        self.triggers = self.workflow[True]  # PyYAML reads the key on as True.

    def test_triggers(self):
        cron = self.triggers['schedule'][0]['cron'].split()
        self.assertEqual(int(cron[0]) % 2, 1)
        self.assertEqual(cron[1:], ['*/6', '*', '*', '*'])
        dry_run = self.triggers['workflow_dispatch']['inputs']['dry_run']
        self.assertEqual((dry_run['type'], dry_run['default']), ('boolean', False))
        self.assertEqual(sorted(self.triggers['pull_request']['paths']),
                         ['.github/workflows/compat-watch.yml', 'scripts/compat/**'])

    def test_permissions(self):
        self.assertEqual(self.workflow['permissions'], {'contents': 'read'})
        jobs = self.workflow['jobs']
        writers = [job for job in jobs.values() if 'write' in job.get('permissions', {}).values()]
        self.assertEqual(len(writers), 1)
        self.assertEqual(writers[0]['permissions'], {'contents': 'read', 'actions': 'write', 'issues': 'write'})
        self.assertEqual(writers[0]['if'], "github.event_name != 'pull_request'")
        readers = [job for job in jobs.values() if job is not writers[0]]
        self.assertEqual([job['permissions'] for job in readers], [{'contents': 'read'}])
        self.assertEqual(readers[0]['if'], "github.event_name == 'pull_request'")
        self.assertIn('--dry-run', ''.join(step.get('run', '') for step in readers[0]['steps']))
        self.assertEqual(writers[0]['concurrency'], {'group': 'compat-watch', 'cancel-in-progress': False})

    def test_no_secrets_and_pinned_actions(self):
        self.assertNotIn('secrets.', self.text)
        for job in self.workflow['jobs'].values():
            for step in job['steps']:
                if 'uses' in step:
                    self.assertRegex(step['uses'], r'^actions/[a-z-]+@v[0-9]+$')
                self.assertNotIn('${{', step.get('run', ''))

    def test_checkout_has_what_the_tests_and_the_script_read(self):
        for job in self.workflow['jobs'].values():
            checkout = next(step for step in job['steps'] if step.get('uses', '').startswith('actions/checkout@'))
            paths = checkout['with']['sparse-checkout'].split()
            self.assertIn('scripts/compat', paths)
            self.assertIn('.github/workflows', paths)
            self.assertFalse(checkout['with']['persist-credentials'])


if __name__ == '__main__':
    unittest.main()
