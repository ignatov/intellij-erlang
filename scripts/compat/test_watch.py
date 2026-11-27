"""Unit tests of watch.py: python3 -m unittest discover -s scripts/compat -p 'test_*.py' -v

The feeds and the GitHub REST API are faked (World); nothing goes to the network."""

import copy
import datetime
import http.server
import io
import json
import os
import re
import tempfile
import threading
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
BUILD_GRADLE = os.path.join(REPOSITORY, 'build.gradle')
CEILING = watch.read_ide_ceiling(BUILD_GRADLE)

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
        self.created = {}
        self.next_run = 1000
        self.next_artifact = 1
        self.dispatched = []
        self.dispatch_status = 200
        self.crash_on_dispatch = None
        self.writes = []
        self.issue_queries = []
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

    def add_artifact(self, run_id, name, members, created=None, artifact_id=None):
        artifact_id = artifact_id or self.next_artifact
        self.artifacts.setdefault(run_id, []).append((artifact_id, name, zipped(members)))
        self.created[artifact_id] = created or watch.iso(self.now)
        self.next_artifact = max(self.next_artifact, artifact_id) + 1

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
            found = [{'id': i, 'name': n, 'created_at': self.created[i]}
                     for i, n, _ in self.artifacts.get(int(match.group(1)), [])
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
            self.issue_queries.append(query)
            found = [i for i in self.issues
                     if ('labels' not in query or query['labels'] in [label['name'] for label in i['labels']])
                     and ('creator' not in query or query['creator'] == i['user']['login'])]
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


def cycle(world, now=None, dry_run=False, build_gradle=BUILD_GRADLE):
    """One cycle of the watcher at `now`; (exit code, output)."""
    world.now = now or world.now
    out = io.StringIO()
    argv = ['--gradle-yml', GRADLE_YML, '--build-gradle', build_gradle] + (['--dry-run'] if dry_run else [])
    with unittest.mock.patch.dict(os.environ, {'GITHUB_REPOSITORY': 'owner/repo'}):
        code = watch.main(argv, http=world, now=world.now, out=out)
    return code, out.getvalue()


def with_baseline(world, errors=(), logs=None):
    """The first two cycles of `world`: the first starts only the baseline, which completes with the parser errors
    `errors`, and the second starts the versions. The run of the baseline."""
    cycle(world)
    base = world.run_of({'ref': SHA})
    world.finish(base, {'otp': otp_result(SHA, PINNED, base, errors=errors), 'ide': ide_result(SHA, BASE_IDE, base)},
                 logs=logs)
    cycle(world, T0 + HOUR / 2)
    return base


def entry(state, kind, version):
    return next(e for e in state['entries'] if e['kind'] == kind and e['version'] == version)


class Seeded(unittest.TestCase):
    """A world in which the first cycle ran and the baseline of SHA, which is all that it started, has finished."""

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
        self.assertEqual(entry(state, 'otp', '%d.0.1' % (MAJOR + 1))['state'], 'queued')
        self.assertNotIn('%d.0' % (MAJOR + 1), [e['version'] for e in state['entries']])
        # The IDE waits for the branch, which the baseline names.
        self.assertFalse([e for e in state['entries'] if e['kind'] == 'ide'])
        self.assertEqual(state['seen']['ide'], [])
        # Only the baseline is started: the versions wait until it is complete.
        self.assertEqual([i for _, i in self.world.dispatched], [{'ref': SHA}])
        self.assertIn('1 versions wait for the baseline of %s' % SHA, self.first)
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
        # The baseline is complete: the version is started on its commit.
        self.assertEqual([i for _, i in self.world.dispatched], [{'ref': SHA}, {'otp': '%d.0.1' % (MAJOR + 1), 'ref': SHA}])
        self.assertEqual(entry(state, 'otp', '%d.0.1' % (MAJOR + 1))['state'], 'running')

    def test_a_dry_run_writes_nothing(self):
        world = World(self.OTP, self.IDE)
        code, out = cycle(world, dry_run=True)
        self.assertEqual(code, 0, out)
        self.assertEqual(world.writes, [])
        self.assertIn('[dry run] would start compat.yml on master with the inputs {"ref": "%s"}' % SHA, out)
        self.assertIn('[dry run] would create the state issue at the end of the cycle', out)
        self.assertNotIn('would end red', out)

    def test_a_dry_run_lists_why_a_cycle_would_end_red_and_exits_with_zero(self):
        # A feed that failed, and three baselines in a row without a complete result.
        world = World(self.OTP, self.IDE)
        for hour in range(4):
            cycle(world, T0 + hour * HOUR)
            for run_id, _ in world.dispatched:
                world.runs[run_id]['status'] = 'completed'
        world.feed_status['otp'] = 503
        writes = len(world.writes)
        code, out = cycle(world, T0 + 4 * HOUR, dry_run=True)
        self.assertEqual((code, len(world.writes)), (0, writes), out)
        self.assertNotIn('::error::', out)
        reasons = out[out.index('[dry run] would end red because:\n'):].split('\n')[1:3]
        self.assertTrue(reasons[0].startswith('- Erlang/OTP: %s: HTTP 503; no Erlang/OTP version' % watch.OTP_FEED), out)
        self.assertTrue(reasons[1].startswith('- the last 3 baseline runs gave no complete result'), out)
        # The same cycle is red when it is not a dry run.
        code, out = cycle(world, T0 + 4 * HOUR)
        self.assertEqual(code, 1, out)
        self.assertIn('::error::Erlang/OTP: ', out)
        # A dry run that cannot work still fails: here the state cannot be read.
        world.state_issue()['body'] = 'no state'
        code, out = cycle(world, T0 + 5 * HOUR, dry_run=True)
        self.assertEqual(code, 1, out)
        self.assertIn('::error::#1: the state issue has no state block', out)

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
            # The cycle in which the version becomes unchecked is red, after the state is written.
            self.assertEqual(code, 1 if attempt == 2 else 0, out)
            e = entry(world.state(), 'otp', self.next)
            if attempt < 2:
                self.assertEqual((e['state'], e['tries']), ('running', attempt + 2), out)
        e = entry(world.state(), 'otp', self.next)
        self.assertEqual((e['state'], e['verdict'], len(e['runs'])), ('done', 'unchecked', 3))
        self.assertIn('::error::Erlang/OTP %s: unchecked' % self.next, out)
        self.assertEqual(world.version_issues(), [])
        body = world.state_issue()['body']
        for run_id in e['runs']:
            self.assertIn('/actions/runs/%d' % run_id, body)

    def test_a_run_that_never_finishes_is_an_attempt(self):
        world = self.world
        cycle(world, T0 + 6 * HOUR)
        self.assertEqual(entry(world.state(), 'otp', self.next)['tries'], 1)
        cycle(world, T0 + 8 * HOUR)
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

    def test_ide_branches_above_supported_until_build_are_not_watched(self):
        world = self.world
        above = '%d.100.1' % (CEILING + 1)
        world.ide_feed['IIU'].insert(0, {'build': above, 'version': None, 'type': 'eap'})
        code, out = cycle(world, T0 + 2 * HOUR)
        self.assertEqual(code, 0, out)
        self.assertIn('1 builds of branches above %d (supportedUntilBuild) are not watched' % CEILING, out)
        state = world.state()
        self.assertNotIn(above, state['seen']['ide'])
        self.assertFalse(any(e['version'] == above for e in state['entries']))
        self.assertNotIn({'ide': above, 'ref': SHA}, [i for _, i in world.dispatched])
        # Once the bound is raised, the build is new and is checked.
        with tempfile.TemporaryDirectory() as directory:
            raised = os.path.join(directory, 'build.gradle')
            with open(raised, 'w', encoding='utf-8') as f:
                f.write("def supportedUntilBuild = '%d.*'\n" % (CEILING + 1))
            cycle(world, T0 + 3 * HOUR, build_gradle=raised)
        self.assertIn({'ide': above, 'ref': SHA}, [i for _, i in world.dispatched])

    def test_a_build_gradle_without_supported_until_build_stops_the_cycle(self):
        with tempfile.TemporaryDirectory() as directory:
            broken = os.path.join(directory, 'build.gradle')
            with open(broken, 'w', encoding='utf-8') as f:
                f.write("def supportedUntilBuild = '263.12'\n")
            dispatched = len(self.world.dispatched)
            code, out = cycle(self.world, T0 + 2 * HOUR, build_gradle=broken)
        self.assertEqual(code, 1)
        self.assertIn("has no line def supportedUntilBuild = '<branch>.*'", out)
        self.assertEqual(len(self.world.dispatched), dispatched)

    def test_a_baseline_whose_result_is_gone_is_run_again(self):
        world = self.world
        world.artifacts.pop(self.baseline_run)
        self.finish_otp(self.next)
        code, out = cycle(world, T0 + 2 * HOUR)
        self.assertEqual(code, 0, out)
        # The cycle starts a new baseline and nothing else; the lost baseline cost the version no attempt.
        state = world.state()
        e = entry(state, 'otp', self.next)
        self.assertEqual((e['state'], e['tries']), ('queued', 0))
        self.assertEqual(([b['state'] for b in state['baselines']], state['failures']), (['running'], 0))
        self.assertEqual([i for _, i in world.dispatched[2:]], [{'ref': SHA}])
        base = world.dispatched[-1][0]
        world.finish(base, {'otp': otp_result(SHA, PINNED, base, errors=BASE_ERRORS),
                            'ide': ide_result(SHA, BASE_IDE, base)})
        cycle(world, T0 + 3 * HOUR)
        self.assertEqual([i for _, i in world.dispatched[3:]], [{'otp': self.next, 'ref': SHA}])
        run_id = world.dispatched[-1][0]
        world.finish(run_id, {'otp': otp_result(SHA, self.next, run_id, self.next, errors=BASE_ERRORS)})
        cycle(world, T0 + 4 * HOUR)
        e = entry(world.state(), 'otp', self.next)
        self.assertEqual((e['verdict'], e['tries']), ('same', 1))

    def test_an_old_baseline_is_run_again_before_a_version_is_started(self):
        world = self.world
        self.finish_otp(self.next)
        cycle(world, T0 + 2 * HOUR)
        later = '%d.0.2' % (MAJOR + 1)
        world.otp_feed += 'OTP-%s x y\n' % later
        started = len(world.dispatched)
        code, out = cycle(world, T0 + watch.BASELINE_MAX_AGE + 3 * HOUR)
        self.assertEqual(code, 0, out)
        self.assertEqual([i for _, i in world.dispatched[started:]], [{'ref': SHA}])
        self.assertEqual(entry(world.state(), 'otp', later)['state'], 'queued')
        base = world.dispatched[-1][0]
        world.finish(base, {'otp': otp_result(SHA, PINNED, base, errors=BASE_ERRORS),
                            'ide': ide_result(SHA, BASE_IDE, base)})
        cycle(world, T0 + watch.BASELINE_MAX_AGE + 4 * HOUR)
        self.assertEqual([i for _, i in world.dispatched[started + 1:]], [{'otp': later, 'ref': SHA}])

    def test_a_version_is_started_only_for_a_commit_with_a_complete_baseline(self):
        world = self.world
        self.finish_otp(self.next)
        cycle(world, T0 + 2 * HOUR)
        started = len(world.dispatched)
        # The default branch moves on. While the complete baseline is fresh, a version is checked on its commit.
        world.head = SHA2
        second, third, fourth = ['%d.0.%d' % (MAJOR + 1, n) for n in (2, 3, 4)]
        world.otp_feed += 'OTP-%s x y\n' % second
        cycle(world, T0 + 3 * HOUR)
        self.assertEqual([i for _, i in world.dispatched[started:]], [{'otp': second, 'ref': SHA}])
        self.finish_otp(second)
        # After that the head commit gets its baseline, and the cycle starts nothing else.
        world.otp_feed += 'OTP-%s x y\n' % third
        code, out = cycle(world, T0 + watch.BASELINE_FRESH + HOUR)
        self.assertEqual(code, 0, out)
        self.assertEqual([i for _, i in world.dispatched[started + 1:]], [{'ref': SHA2}])
        self.assertEqual(entry(world.state(), 'otp', third)['state'], 'queued')
        # While that baseline runs, neither a version nor a second baseline is started.
        world.otp_feed += 'OTP-%s x y\n' % fourth
        world.head = 'c' * 40
        code, out = cycle(world, T0 + watch.BASELINE_FRESH + 2 * HOUR)
        self.assertEqual(code, 0, out)
        self.assertEqual(len(world.dispatched), started + 2)
        self.assertIn('1 versions wait for the baseline of %s' % SHA2, out)
        base = world.dispatched[-1][0]
        world.finish(base, {'otp': otp_result(SHA2, PINNED, base, errors=BASE_ERRORS),
                            'ide': ide_result(SHA2, BASE_IDE, base)})
        cycle(world, T0 + watch.BASELINE_FRESH + 3 * HOUR)
        self.assertEqual([i for _, i in world.dispatched[started + 2:]], [{'otp': fourth, 'ref': SHA2}])
        self.assertEqual(entry(world.state(), 'otp', third)['verdict'], 'superseded')

    def test_the_artifact_that_was_created_last_is_read(self):
        world = self.world
        # The run was run again before it was collected. The ids of artifacts are not in the order of creation.
        run_id = world.run_of({'otp': self.next, 'ref': SHA})
        world.runs[run_id]['status'] = 'completed'
        first = otp_result(SHA, self.next, run_id, self.next, status='incomplete')
        world.add_artifact(run_id, 'compat-otp', {'compat-otp.json': first}, '2026-11-15T01:30:00Z', artifact_id=90)
        world.add_artifact(run_id, 'compat-otp', {'compat-otp.json': first}, 'not a time', artifact_id=95)
        second = otp_result(SHA, self.next, run_id, self.next, errors=BASE_ERRORS)
        world.add_artifact(run_id, 'compat-otp', {'compat-otp.json': second}, '2026-11-15T01:40:00Z', artifact_id=80)
        code, out = cycle(world, T0 + 2 * HOUR)
        self.assertEqual(code, 0, out)
        e = entry(world.state(), 'otp', self.next)
        self.assertEqual((e['verdict'], e['tries']), ('same', 1), out)
        self.assertLess(watch.created('2026-11-15T01:30:00Z'), watch.created('2026-11-15T01:30:00.5Z'))
        self.assertLess(watch.created(None), watch.created('2026-11-15T01:30:00Z'))


class SecondCycleTest(Seeded):
    """After the second cycle: the baseline of SHA is complete and the run of self.next is in flight."""

    def setUp(self):
        super().setUp()
        self.next = '%d.0.1' % (MAJOR + 1)
        cycle(self.world, T0 + HOUR)

    def add_version(self, version, hour):
        self.world.otp_feed += 'OTP-%s x y\n' % version
        return cycle(self.world, T0 + hour * HOUR)

    def failing(self, condition, status):
        """Makes the fake API answer `status` to the requests for which `condition` (method, path) holds."""
        handle = self.world.handle
        self.world.handle = lambda method, path, query, body: (
            watch.Response(status) if condition(method, path) else handle(method, path, query, body))

    # The state issue

    def test_renaming_unlabelling_or_closing_the_state_issue_does_not_seed_again(self):
        world = self.world
        issue = world.state_issue()
        issue.update(title='Compatibility watch (state, do not edit)', labels=[], state='closed')
        started = len(world.dispatched)
        code, out = cycle(world, T0 + 2 * HOUR)
        self.assertEqual(code, 0, out)
        self.assertIn('State: issue #%d' % issue['number'], out)
        self.assertEqual(len(world.issues), 1)
        self.assertEqual(len(world.dispatched), started)
        self.assertIn({'creator': 'github-actions[bot]', 'state': 'all', 'per_page': '100', 'page': '1'},
                      world.issue_queries)

    def test_the_state_issue_is_also_found_in_the_listing_of_its_label(self):
        handle = self.world.handle
        self.world.handle = lambda method, path, query, body: (
            self.world.json([]) if 'creator' in (query or {}) else handle(method, path, query, body))
        started = len(self.world.dispatched)
        code, out = cycle(self.world, T0 + 2 * HOUR)
        self.assertEqual(code, 0, out)
        self.assertIn('State: issue #1', out)
        self.assertEqual((len(self.world.issues), len(self.world.dispatched)), (1, started))

    def test_two_state_issues_stop_the_cycle(self):
        world = self.world
        world.add_issue('Another title', world.state_issue()['body'], 'other')
        world.otp_feed += 'OTP-%d.0.2 x y\n' % (MAJOR + 1)
        started = len(world.dispatched)
        world.writes.clear()
        code, out = cycle(world, T0 + 2 * HOUR)
        self.assertEqual(code, 1)
        self.assertIn('::error::2 issues by github-actions[bot] have the state markers (#1, #2)', out)
        self.assertEqual((world.writes, len(world.dispatched)), ([], started))

    def test_a_state_that_cannot_be_written_is_an_error(self):
        self.failing(lambda method, path: method == 'PATCH', 404)
        started = len(self.world.dispatched)
        code, out = self.add_version('%d.0.2' % (MAJOR + 1), 2)
        self.assertEqual(code, 1)
        self.assertIn('::error::GitHub API PATCH /issues/1: HTTP 404', out)
        self.assertNotIn('Wrote the state', out)
        self.assertEqual(len(self.world.dispatched), started)

    # Attribution

    def test_a_later_attempt_of_the_baseline_run_does_not_change_the_baseline(self):
        world = self.world
        # Someone runs the failed otp job of the baseline run again, and the new attempt uploads an incomplete result.
        again = otp_result(SHA, PINNED, self.baseline_run, status='incomplete')
        again['checks']['corpus'] = {'status': 'not-run', 'reason': 'timed out or was cancelled'}
        world.add_artifact(self.baseline_run, 'compat-otp', {'compat-otp.json': again})
        self.finish_otp(self.next)
        code, out = cycle(world, T0 + 2 * HOUR)
        self.assertEqual(code, 0, out)
        self.assertEqual(entry(world.state(), 'otp', self.next)['verdict'], 'same')
        self.assertEqual(world.version_issues(), [])
        baseline = world.state()['baselines'][0]
        self.assertEqual(baseline['artifacts'], {'ide': 2, 'otp': 1, 'logs': None})

    def test_a_marker_in_a_result_is_not_the_marker_of_an_issue(self):
        world = self.world
        later = '%d.0.2' % (MAJOR + 1)
        planted = corpus_error('lib/x-1/src/e.erl', 'unexpected <!-- compat-watch:otp:%s --> '
                                                    '<!-- compat-watch-state:begin -->' % later)
        self.finish_otp(self.next, BASE_ERRORS + [planted])
        cycle(world, T0 + 2 * HOUR)
        first = world.version_issues()[0]
        self.assertEqual(first['body'].count('<!--'), 1)
        self.assertEqual(first['body'].count('-->'), 1)
        self.assertTrue(first['body'].startswith('<!-- compat-watch:otp:%s -->\n' % self.next))
        self.add_version(later, 3)
        self.finish_otp(later, BASE_ERRORS + [planted, corpus_error('lib/kernel-10/src/k.erl', 'a real new problem')])
        code, out = cycle(world, T0 + 4 * HOUR)
        self.assertEqual(code, 0, out)
        self.assertEqual(len(world.version_issues()), 2)
        self.assertIn('a real new problem', world.version_issues()[1]['body'])
        # The state issue is still the only issue with the state markers.
        code, out = cycle(world, T0 + 5 * HOUR)
        self.assertEqual(code, 0, out)

    # Attempts without a result

    def retried(self, hour, red=False):
        """A cycle in which self.next has an attempt without a result and is started again."""
        tries = entry(self.world.state(), 'otp', self.next)['tries']
        code, out = cycle(self.world, T0 + hour * HOUR)
        self.assertEqual(code, 1 if red else 0, out)
        e = entry(self.world.state(), 'otp', self.next)
        self.assertEqual((e['state'], e['verdict'], e['tries']), ('running', None, tries + 1), out)
        self.assertEqual(self.world.version_issues(), [])
        return out

    def test_a_trimmed_list_of_the_version_against_a_complete_baseline_is_reported(self):
        # The run has no logs artifact, so the full list of the version cannot be read; the baseline's is complete.
        errors = BASE_ERRORS + [corpus_error('lib/a-1/src/f%d.erl' % i, 'e') for i in range(148)]
        tests = [{'class': 'C', 'name': 't%d' % i, 'message': 'm'} for i in range(120)]
        run_id = self.finish_otp(self.next, errors, failed_tests=tests)
        code, out = cycle(self.world, T0 + 2 * HOUR)
        self.assertEqual(code, 0, out)
        e = entry(self.world.state(), 'otp', self.next)
        self.assertEqual((e['verdict'], e['tries'], e['runs']), ('new problems', 1, [run_id]))
        body = self.world.version_issues()[0]['body']
        self.assertIn('**compilationTests, failed compilation tests**: 100 distinct, 100 occurrences; the result file '
                      'lists only the first 100 of 120\n', body)
        self.assertIn('**corpus, parser errors in the Erlang/OTP sources**: 98 distinct, 98 occurrences; the result '
                      'file lists only the first 100 of 150; its full list could not be read: the run has no artifact '
                      'compat-otp-logs\n', body)
        self.assertEqual(len(self.world.state()['reported']['otp'][str(MAJOR + 1)]['keys']), 198)

    def test_a_trimmed_baseline_without_the_full_report_never_gives_the_same(self):
        version = '%d.0' % (MAJOR + 1)
        errors = [corpus_error('lib/a-1/src/f%03d.erl' % i, 'e') for i in range(150)]
        for logs in ({'errors': errors}, None):
            # The baseline has 150 errors; with `logs` its full list can be read.
            world = World([PINNED, version], Seeded.IDE)
            with_baseline(world, errors, logs)
            # The same total, but the last 50 are new; the run has no logs artifact.
            run_id = world.run_of({'otp': version, 'ref': SHA})
            new = errors[:100] + [corpus_error('lib/b-1/src/g%d.erl' % i, 'new') for i in range(50)]
            world.finish(run_id, {'otp': otp_result(SHA, version, run_id, version, errors=new)})
            code, out = cycle(world, T0 + HOUR)
            self.assertEqual(code, 0, out)
            e = entry(world.state(), 'otp', version)
            self.assertEqual((e['state'], e['verdict'], e['tries']), ('running', None, 2), out)
            self.assertIn('the comparison of run %d is not complete' % run_id, out)
            self.assertEqual(world.version_issues(), [])
            # With the full report of the version the 50 new ones are found, if the baseline's can be read too.
            run_id = e['run']
            world.finish(run_id, {'otp': otp_result(SHA, version, run_id, version, errors=new)}, logs={'errors': new})
            cycle(world, T0 + 2 * HOUR)
            if logs:
                self.assertIn('50 distinct, 50 occurrences', world.version_issues()[0]['body'])
            else:
                self.assertEqual(entry(world.state(), 'otp', version)['tries'], 3)
                self.assertEqual(world.version_issues(), [])

    def test_an_unknown_schema_version_is_an_attempt_without_a_result_and_red(self):
        run_id = self.world.run_of({'otp': self.next, 'ref': SHA})
        result = otp_result(SHA, self.next, run_id, self.next, errors=BASE_ERRORS)
        result['schemaVersion'] = compat.SCHEMA_VERSION + 1
        result['checks'] = {'parser': {'status': 'failed', 'problems': listed([corpus_error('a.erl', 'new')])}}
        self.world.finish(run_id, {'otp': result})
        out = self.retried(2, red=True)
        self.assertIn('::error::the result compat-otp of run %d has a schemaVersion that watch.py does not know'
                      % run_id, out)

    def test_a_result_that_is_nested_too_deeply_is_an_attempt_without_a_result(self):
        run_id = self.world.run_of({'otp': self.next, 'ref': SHA})
        self.world.runs[run_id]['status'] = 'completed'
        self.world.add_artifact(run_id, 'compat-otp', {'compat-otp.json': '[' * 200000})
        self.assertIn('run %d has no result artifact compat-otp' % run_id, self.retried(2))

    def test_a_result_that_cannot_be_read_in_eight_cycles_is_an_attempt_without_a_result(self):
        run_id = self.finish_otp(self.next)
        broken = ['/actions/artifacts/%d/zip' % i for i, _, _ in self.world.artifacts[run_id]]
        self.failing(lambda method, path: path in broken, 500)
        for n in range(watch.MAX_COLLECT_ERRORS - 1):
            code, out = cycle(self.world, T0 + (2 + n) * HOUR)
            self.assertEqual(code, 1)
            e = entry(self.world.state(), 'otp', self.next)
            self.assertEqual((e['state'], e['tries'], e['errors'], e['run']), ('running', 1, n + 1, run_id), out)
        out = self.retried(2 + watch.MAX_COLLECT_ERRORS, red=True)
        self.assertIn('its result could not be read in 8 cycles in a row', out)
        self.assertEqual(entry(self.world.state(), 'otp', self.next)['errors'], 0)

    # One issue for each problem of a major release

    def test_problems_that_an_issue_of_the_major_release_reported_are_not_reported_again(self):
        world = self.world
        first = BASE_ERRORS + [corpus_error('lib/ssl-12/src/x.erl', 'first', n) for n in range(3)]
        self.finish_otp(self.next, first)
        cycle(world, T0 + 2 * HOUR)
        issue = world.version_issues()[0]
        self.assertIn('**corpus, parser errors in the Erlang/OTP sources**: 1 distinct, 3 occurrences', issue['body'])
        self.assertNotIn('Earlier issue', issue['body'])
        reported = world.state()['reported']['otp'][str(MAJOR + 1)]
        self.assertEqual((reported['issue'], len(reported['keys'])), (issue['number'], 1))
        self.assertRegex(reported['keys'][0], '^[0-9a-f]{10}$')
        # The next patch release has the same problem: no issue, the verdict links the earlier one.
        second = '%d.0.2' % (MAJOR + 1)
        self.add_version(second, 3)
        self.finish_otp(second, [corpus_error('lib/ssl-12.1/src/x.erl', 'first', 70)] + BASE_ERRORS)
        code, out = cycle(world, T0 + 4 * HOUR)
        self.assertEqual(code, 0, out)
        e = entry(world.state(), 'otp', second)
        self.assertEqual((e['state'], e['verdict'], e['issue']), ('done', 'known problems', issue['number']))
        self.assertEqual(len(world.version_issues()), 1)
        self.assertIn('| Erlang/OTP | %s | 2026-11-15 | known problems |' % second, world.state_issue()['body'])
        # The one after it has one more: its issue lists only that one and names the earlier issue.
        third = '%d.0.3' % (MAJOR + 1)
        self.add_version(third, 5)
        self.finish_otp(third, first + [corpus_error('lib/ssl-12.2/src/y.erl', 'second')])
        cycle(world, T0 + 6 * HOUR)
        self.assertEqual(len(world.version_issues()), 2)
        body = world.version_issues()[1]['body']
        self.assertIn('- Earlier issue of Erlang/OTP %d: #%d. The problems shown there are not repeated here.'
                      % (MAJOR + 1, issue['number']), body)
        self.assertIn('lib/ssl-12.2/src/y.erl: second', body)
        self.assertNotIn('first', body)
        reported = world.state()['reported']['otp'][str(MAJOR + 1)]
        self.assertEqual((reported['issue'], len(reported['keys'])), (world.version_issues()[1]['number'], 2))
        # Another major release has its own issue for the same problem.
        other = '%d.0' % (MAJOR + 2)
        self.add_version(other, 7)
        self.finish_otp(other, first)
        cycle(world, T0 + 8 * HOUR)
        self.assertEqual(len(world.version_issues()), 3)

    def test_a_result_of_an_unexpected_shape_is_an_attempt_without_a_result(self):
        def with_(**changes):
            result = otp_result(SHA, self.next, 0, self.next, errors=BASE_ERRORS)
            result.update(changes)
            return result
        for result in [with_(resolved='x'), with_(inputs='x'), with_(resolved={'commit': SHA, 'otp': 'x'})]:
            self.assertFalse(watch.Watcher.matches(result, 'otp', SHA, self.next, 0), result)
        watch.Watcher.matches(with_(run='x'), 'otp', SHA, self.next, 0)  # no run id to check, and no crash
        # A result without its checks has nothing to compare: it is not the same as the baseline.
        for hour, checks in enumerate([[1], {'corpus': {'status': 'passed'}, 'knownRelease': 'passed'}]):
            run_id = entry(self.world.state(), 'otp', self.next)['run']
            self.world.finish(run_id, {'otp': with_(checks=checks, run={'runId': run_id})})
            self.assertIn('has no check compilationTests', self.retried(2 + hour))
        # A list that is not one: the comparison finds nothing to read there and does not crash.
        odd = ide_result(SHA, '263.8.1', 2)
        odd['checks']['verifyPlugin'] = {'status': 'passed', 'reports': 5}
        odd['checks']['test'] = {'status': 'passed', 'failedTests': {'items': 'x', 'total': []}}
        self.assertEqual(watch.compare('ide', odd, ide_result(SHA, BASE_IDE, 1)), {'sections': [], 'notes': []})

    def test_an_unexpected_failure_while_collecting_does_not_stop_the_cycle(self):
        self.finish_otp(self.next)
        with unittest.mock.patch.object(watch, 'compare', side_effect=TypeError('text from a result @x')):
            code, out = self.add_version('%d.0' % (MAJOR + 2), 2)
        self.assertEqual(code, 1)
        self.assertIn('::error::Erlang/OTP %s: unexpected TypeError' % self.next, out)
        self.assertNotIn('@x', out)
        # The cycle went on, and the failure is counted like a result that cannot be read.
        self.assertEqual(self.world.dispatched[-1][1], {'otp': '%d.0' % (MAJOR + 2), 'ref': SHA})
        e = entry(self.world.state(), 'otp', self.next)
        self.assertEqual((e['state'], e['errors']), ('running', 1))

    def test_the_runs_of_a_version_are_its_own(self):
        world = self.world
        version = '%d.0' % (MAJOR + 2)
        world.dispatch_status = 422
        self.add_version(version, 2)
        self.assertEqual(entry(world.state(), 'otp', version)['state'], 'queued')
        world.dispatch_status = 200
        code, out = cycle(world, T0 + 3 * HOUR)
        self.assertEqual(code, 0, out)
        run_id = world.run_of({'otp': version, 'ref': SHA})
        state = world.state()
        self.assertEqual(entry(state, 'otp', version)['runs'], [run_id])
        self.assertEqual([e['version'] for e in state['entries'] if run_id in e['runs']], [version])
        self.assertEqual(world.state_issue()['body'].count('/actions/runs/%d' % run_id), 1)

    def checked(self, version, hour, errors, red=False):
        """`version` appears, is checked with the parser errors `errors` and collected; the output of that cycle."""
        self.add_version(version, hour)
        run_id = self.world.run_of({'otp': version, 'ref': SHA})
        self.world.finish(run_id, {'otp': otp_result(SHA, version, run_id, version, errors=errors)},
                          logs={'errors': errors})
        code, out = cycle(self.world, T0 + (hour + 1) * HOUR)
        self.assertEqual(code, 1 if red else 0, out)
        return out

    def test_only_the_problems_that_an_issue_shows_count_as_reported(self):
        world = self.world
        errors = BASE_ERRORS + [corpus_error('lib/a-1/src/f%d.erl' % i, 'e') for i in range(450)]
        run_id = world.run_of({'otp': self.next, 'ref': SHA})
        world.finish(run_id, {'otp': otp_result(SHA, self.next, run_id, self.next, errors=errors)},
                     logs={'errors': errors})
        cycle(world, T0 + 2 * HOUR)
        body = world.version_issues()[0]['body']
        self.assertIn('**corpus, parser errors in the Erlang/OTP sources**: 450 distinct, 450 occurrences\n', body)
        self.assertIn('<details><summary>170 more</summary>', body)
        self.assertIn('250 more are not shown: an issue shows at most 200 problems of a check', body)
        opened, folded = body.split('```\n')[1], body.split('```\n')[3]
        self.assertEqual((opened.count('\n'), folded.count('\n')), (watch.ISSUE_ITEMS, 170))
        line = world.state()['reported']['otp'][str(MAJOR + 1)]
        self.assertEqual((len(line['keys']), line['full']), (200, False))
        # The next version has the same problems: its issue shows the ones that the first one did not show.
        out = self.checked('%d.0.2' % (MAJOR + 1), 3, errors)
        self.assertEqual(len(world.version_issues()), 2, out)
        body = world.version_issues()[1]['body']
        self.assertIn('250 distinct, 250 occurrences\n', body)
        self.assertIn('50 more are not shown', body)
        self.assertNotIn('lib/a-1/src/f0.erl', body)
        self.assertIn('lib/a-1/src/f399.erl', body)
        line = world.state()['reported']['otp'][str(MAJOR + 1)]
        self.assertEqual((len(line['keys']), line['full']), (400, False))

    def test_a_line_keeps_at_most_400_keys_and_is_saturated_after_the_issue_that_passes_them(self):
        world = self.world
        major, other = str(MAJOR + 1), str(MAJOR + 2)
        errors = BASE_ERRORS + [corpus_error('lib/a-1/src/f%d.erl' % i, 'e') for i in range(450)]
        run_id = world.run_of({'otp': self.next, 'ref': SHA})
        world.finish(run_id, {'otp': otp_result(SHA, self.next, run_id, self.next, errors=errors)},
                     logs={'errors': errors})
        cycle(world, T0 + 2 * HOUR)
        self.checked('%s.0.2' % major, 3, errors)
        # The third issue shows the last 50, which takes the line past its bound: it is still filed.
        # The cycle in which the line becomes saturated is red and says so; the later ones are not.
        out = self.checked('%s.0.3' % major, 5, errors, red=True)
        self.assertEqual(len(world.version_issues()), 3, out)
        third = world.version_issues()[2]['number']
        self.assertIn('::error::Erlang/OTP %s is saturated: its issues have shown more than the 400 problems that the '
                      'state keeps. Its later versions with new problems are recorded as known problems with the link '
                      'of #%d' % (major, third), out)
        line = world.state()['reported']['otp'][major]
        self.assertEqual((len(line['keys']), line['full'], line['issue']), (watch.KEYS_PER_LINE, True, third))
        # After it the versions of the line get no issue, whatever they have: not the 50 that the state did not keep,
        # and not problems that no issue has shown.
        more = errors + [corpus_error('lib/b-1/src/g%d.erl' % i, 'other') for i in range(5)]
        for n, found in enumerate([errors, more, more]):
            out = self.checked('%s.0.%d' % (major, 4 + n), 7 + 2 * n, found)
            e = entry(world.state(), 'otp', '%s.0.%d' % (major, 4 + n))
            self.assertEqual((e['verdict'], e['issue']), ('known problems', third), out)
        self.assertIn('new problems, but #%d has shown more than the 400' % third, out)
        self.assertEqual(len(world.version_issues()), 3)
        # The bound of one line is not the bound of another: the next major release has its own issues.
        self.checked('%s.0' % other, 13, more)
        self.assertEqual(len(world.version_issues()), 4)
        self.assertEqual(world.state()['reported']['otp'][other]['full'], False)
        # A version of the line with the same problems as the baseline clears its set ...
        self.checked('%s.0.7' % major, 15, BASE_ERRORS)
        state = world.state()
        self.assertEqual(entry(state, 'otp', '%s.0.7' % major)['verdict'], 'same')
        self.assertEqual(sorted(state['reported']['otp']), [other])
        # ... and the problems of a later one are reported again.
        self.checked('%s.0.8' % major, 17, more)
        self.assertEqual(len(world.version_issues()), 5)
        self.assertNotIn('Earlier issue', world.version_issues()[4]['body'])
        # Every row of the table is still there.
        self.assertEqual(len(world.state()['entries']), 11)

    def test_the_same_result_of_an_older_version_does_not_clear_the_problems_of_a_newer_one(self):
        world = self.world
        major = MAJOR + 1
        older, reported, later, last = self.next, '%d.1' % major, '%d.2' % major, '%d.3' % major
        new = BASE_ERRORS + [corpus_error('lib/x-1/src/n%d.erl' % i, 'bad') for i in range(5)]
        # The run of the older version is still in flight when the newer one appears and is reported.
        self.add_version(reported, 5)
        first = world.run_of({'otp': older, 'ref': SHA})
        world.finish(first, {'otp': otp_result(SHA, older, first, older, errors=BASE_ERRORS, status='incomplete')})
        run_id = world.run_of({'otp': reported, 'ref': SHA})
        world.finish(run_id, {'otp': otp_result(SHA, reported, run_id, reported, errors=new)})
        cycle(world, T0 + 11 * HOUR)
        issue = world.version_issues()[0]['number']
        # The second run of the older version gives the same problems as the baseline, one cycle later.
        second = [r for r, i in world.dispatched if i == {'otp': older, 'ref': SHA}][-1]
        self.assertNotEqual(first, second)
        world.finish(second, {'otp': otp_result(SHA, older, second, older, errors=BASE_ERRORS)})
        cycle(world, T0 + 17 * HOUR)
        state = world.state()
        self.assertEqual(entry(state, 'otp', older)['verdict'], 'same')
        self.assertEqual(len(state['reported']['otp'][str(major)]['keys']), 5)
        # So the next version with the problems of the reported one gets no second issue.
        self.checked(later, 23, new)
        e = entry(world.state(), 'otp', later)
        self.assertEqual((e['verdict'], e['issue'], len(world.version_issues())), ('known problems', issue, 1))
        # The same result of a version that is not older than those clears the line.
        self.checked(last, 29, BASE_ERRORS)
        self.assertEqual(world.state()['reported']['otp'], {})

    def test_a_stored_time_in_the_future_is_read_as_the_start_of_the_cycle(self):
        world = World([PINNED, '%d.0' % (MAJOR + 1)], Seeded.IDE)
        cycle(world)
        issue = world.state_issue()
        self.assertIn('"at":"%s"' % watch.iso(T0), issue['body'])
        issue['body'] = issue['body'].replace('"at":"%s"' % watch.iso(T0), '"at":"2099-01-01T00:00:00Z"')
        code, out = cycle(world, T0 + HOUR)
        self.assertEqual(code, 0, out)
        self.assertIn('The state has the time 2099-01-01T00:00:00Z for the baseline of %s, which is later than the '
                      'start of this cycle: it is read as the start of this cycle' % SHA, out)
        self.assertEqual(world.state()['baselines'][0]['at'], watch.iso(T0 + HOUR))
        # Its run, which never finishes, times out as one that was started in that cycle.
        code, out = cycle(world, T0 + HOUR + watch.RUN_TIMEOUT)
        self.assertEqual(world.state()['baselines'][0]['state'], 'running', out)
        code, out = cycle(world, T0 + 2 * HOUR + watch.RUN_TIMEOUT)
        self.assertIn('did not finish within', out)
        self.assertEqual(world.state()['failures'], 1)
        # The time of a version too.
        world = World([PINNED, '%d.0' % (MAJOR + 1)], Seeded.IDE)
        with_baseline(world)
        issue = world.state_issue()
        started = '"at":"%s"' % watch.iso(T0 + HOUR / 2)
        self.assertEqual(issue['body'].count(started), 1)
        issue['body'] = issue['body'].replace(started, '"at":"2099-01-01T00:00:00Z"')
        code, out = cycle(world, T0 + HOUR)
        self.assertIn('The state has the time 2099-01-01T00:00:00Z for %d.0, which is later' % (MAJOR + 1), out)
        self.assertEqual(entry(world.state(), 'otp', '%d.0' % (MAJOR + 1))['at'], watch.iso(T0 + HOUR))

    def test_an_older_version_that_appears_later_is_superseded(self):
        started = len(self.world.dispatched)
        older = '%d.0.0.5' % (MAJOR + 1)
        code, out = self.add_version(older, 2)
        self.assertEqual(code, 0, out)
        self.assertEqual(entry(self.world.state(), 'otp', older)['verdict'], 'superseded')
        self.assertEqual(len(self.world.dispatched), started)


class BaselineAttemptsTest(unittest.TestCase):
    VERSION = '%d.0' % (MAJOR + 1)

    def finish(self, world, complete):
        for run_id, inputs in world.dispatched:
            if world.runs[run_id]['status'] == 'completed':
                continue
            sha = inputs['ref']
            if 'otp' in inputs:
                world.finish(run_id, {'otp': otp_result(sha, inputs['otp'], run_id, inputs['otp'], errors=BASE_ERRORS)})
            else:
                world.finish(run_id, {'otp': otp_result(sha, PINNED, run_id, errors=BASE_ERRORS),
                                      'ide': ide_result(sha, BASE_IDE, run_id,
                                                        status='passed' if complete else 'incomplete')})

    def test_after_three_failed_baselines_one_is_started_in_24_hours_and_every_cycle_is_red(self):
        world = World([PINNED, self.VERSION], Seeded.IDE)
        codes = []
        for hour in range(6):
            code, out = cycle(world, T0 + hour * HOUR)
            codes.append(code)
            self.finish(world, complete=False)
        # Red from the cycle in which the third baseline in a row failed; no version was ever started.
        self.assertEqual(codes, [0, 0, 0, 1, 1, 1], out)
        self.assertIn('::error::the last 3 baseline runs gave no complete result, so no version is checked', out)
        self.assertEqual([i for _, i in world.dispatched], [{'ref': SHA}] * watch.MAX_ATTEMPTS)
        state = world.state()
        self.assertEqual(([b['state'] for b in state['baselines']], state['failures']), (['failed'], 3))
        e = entry(state, 'otp', self.VERSION)
        self.assertEqual((e['state'], e['verdict'], e['tries']), ('queued', None, 0))
        # The count is of baselines in a row on whatever commits: a new commit does not start one sooner.
        world.head = SHA2
        code, out = cycle(world, T0 + 6 * HOUR)
        self.assertEqual((code, len(world.dispatched)), (1, 3), out)
        # One baseline is started 24 hours after the last one, and the cycles stay red.
        code, out = cycle(world, T0 + 2 * HOUR + watch.BASELINE_PAUSE)
        self.assertEqual((code, [i for _, i in world.dispatched[3:]]), (1, [{'ref': SHA2}]), out)
        self.finish(world, complete=False)
        for hour in (3, 9, 15, 21):
            code, out = cycle(world, T0 + hour * HOUR + watch.BASELINE_PAUSE)
            self.assertEqual((code, len(world.dispatched)), (1, 4), out)
        self.assertEqual(world.state()['failures'], 4)
        code, out = cycle(world, T0 + 3 * HOUR + 2 * watch.BASELINE_PAUSE)
        self.assertEqual((code, len(world.dispatched)), (1, 5), out)
        # A complete baseline ends it: the cycle is green and the version is started, with its first attempt.
        self.finish(world, complete=True)
        code, out = cycle(world, T0 + 4 * HOUR + 2 * watch.BASELINE_PAUSE)
        self.assertEqual(code, 0, out)
        self.assertEqual(world.state()['failures'], 0)
        self.assertEqual([i for _, i in world.dispatched[5:]], [{'otp': self.VERSION, 'ref': SHA2}])
        self.finish(world, complete=True)
        code, out = cycle(world, T0 + 5 * HOUR + 2 * watch.BASELINE_PAUSE)
        self.assertEqual(code, 0, out)
        e = entry(world.state(), 'otp', self.VERSION)
        self.assertEqual((e['verdict'], e['tries']), ('same', 1))

    def test_a_baseline_without_a_version_to_check_is_not_started_without_end(self):
        world = World([PINNED], Seeded.IDE)
        for hour in range(8):
            code, out = cycle(world, T0 + hour * HOUR)
            self.finish(world, complete=False)
        self.assertEqual(code, 1)
        self.assertEqual(len(world.dispatched), watch.MAX_ATTEMPTS)

    def test_a_failed_baseline_between_complete_ones_is_not_red(self):
        world = World([PINNED, self.VERSION], Seeded.IDE)
        for hour, complete in enumerate([False, False, True]):
            code, out = cycle(world, T0 + hour * HOUR)
            self.assertEqual(code, 0, out)
            self.finish(world, complete)
        code, out = cycle(world, T0 + 3 * HOUR)
        self.assertEqual((code, world.state()['failures']), (0, 0), out)
        self.assertEqual(world.dispatched[-1][1], {'otp': self.VERSION, 'ref': SHA})


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
        world.add_issue('Compatibility: Erlang/OTP %d.0' % (MAJOR + 1),
                        '<!-- compat-watch:otp:%d.0 -->' % (MAJOR + 1), 'compatibility', user={'login': 'mallory'})
        with_baseline(world)
        # The issue of another author with the title, the label and the markers is not the state.
        self.assertEqual(world.state_issue()['number'], 3)
        self.assertEqual(world.issues[0]['body'], state)
        self.assertEqual(world.state()['pending'], [])
        run_id = world.run_of({'otp': '%d.0' % (MAJOR + 1), 'ref': SHA})
        world.finish(run_id, {'otp': otp_result(SHA, '%d.0' % (MAJOR + 1), run_id, '%d.0' % (MAJOR + 1),
                                                known=False)})
        cycle(world, T0 + HOUR)
        mine = [i for i in world.version_issues() if i['user'] == BOT]
        self.assertEqual(len(mine), 1)
        self.assertEqual(world.issues[1]['body'], '<!-- compat-watch:otp:%d.0 -->' % (MAJOR + 1))

    def test_the_issue_of_a_version_is_found_after_a_crash(self):
        world = World([PINNED, '%d.0' % (MAJOR + 1)], [(BASE_IDE, '2026.3')])
        with_baseline(world)
        world.add_issue('Compatibility: Erlang/OTP %d.0' % (MAJOR + 1), '<!-- compat-watch:otp:%d.0 -->\n'
                        'filed before the crash' % (MAJOR + 1), 'compatibility')
        run_id = world.run_of({'otp': '%d.0' % (MAJOR + 1), 'ref': SHA})
        world.finish(run_id, {'otp': otp_result(SHA, '%d.0' % (MAJOR + 1), run_id, '%d.0' % (MAJOR + 1),
                                                known=False)})
        cycle(world, T0 + HOUR)
        self.assertEqual(len(world.version_issues()), 1)
        self.assertEqual(entry(world.state(), 'otp', '%d.0' % (MAJOR + 1))['issue'], 2)
        # The problem that the issue of the crashed cycle showed is reported.
        self.assertEqual(len(world.state()['reported']['otp'][str(MAJOR + 1)]['keys']), 1)

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

    def test_no_table_row_is_dropped_for_keys_and_the_keys_of_the_oldest_line_go_first(self):
        def row(kind, version, seen):
            return dict(watch.ENTRY_DEFAULTS, kind=kind, version=version, seen=seen, state='done', verdict='same',
                        sha=SHA, at='2026-11-15T00:17:00Z', run=10 ** 11, runs=[10 ** 11, 10 ** 11 + 1], tries=2)
        state = watch.new_state(T0)
        state['floor'] = {'ide': 263, 'otp': MAJOR}
        state['seen']['otp'] = ['%d.%d.%d' % (MAJOR, i, j) for i in range(30) for j in range(20)]
        state['entries'] = ([row('otp', '%d.0.1' % (MAJOR + 2), '2026-11-14'), row('otp', '%d.0' % MAJOR, '2026-11-10'),
                             row('ide', '263.1.1', '2026-11-12'), row('otp', '%d.0' % (MAJOR + 2), '2026-11-01')]
                            + [row('ide', '271.%d.1' % i, '2026-11-13') for i in range(200)])
        keys = ['%010x' % i for i in range(3000)]
        full = watch.KEYS_PER_LINE
        state['reported']['otp'] = {str(MAJOR - 1): {'issue': 5, 'keys': keys[:10], 'full': False},
                                    str(MAJOR): {'issue': 7, 'keys': keys[:full], 'full': False},
                                    str(MAJOR + 1): {'issue': 8, 'keys': keys[1000:1000 + full], 'full': False},
                                    str(MAJOR + 2): {'issue': 9, 'keys': keys[2000:2000 + full], 'full': True}}
        state['reported']['ide'] = {'263': {'issue': 6, 'keys': keys[:full], 'full': False},
                                    '271': {'issue': 4, 'keys': keys[:full], 'full': False}}
        watcher = watch.Watcher(watch.GitHub(None, 'owner/repo'), None, {'OTP_VERSION': PINNED}, T0, out=io.StringIO())
        watcher.state = state
        watcher.prune()
        body = watch.render_state(state, 'https://github.com', 'owner/repo')
        self.assertLess(len(body), watch.STATE_BODY_TARGET)
        parsed = watch.parse_state(body)
        self.assertEqual(parsed['seen'], state['seen'])
        # The rows beyond the newest 60 went first, and no other row went for the keys.
        self.assertEqual(len(parsed['entries']), watch.TABLE_ROWS)
        self.assertEqual(body.count('\n| '), watch.TABLE_ROWS + 1)
        # A major release that is not watched any more is dropped.
        reported = parsed['reported']['otp']
        self.assertEqual(sorted(reported), [str(MAJOR), str(MAJOR + 1), str(MAJOR + 2)])
        # Then the keys of the lines whose newest version is the oldest: the one without a row, then by the date.
        # Such a line is saturated, so that its problems are not reported a second time.
        self.assertEqual(reported[str(MAJOR + 1)], {'issue': 8, 'keys': [], 'full': True})
        self.assertEqual(reported[str(MAJOR)], {'issue': 7, 'keys': [], 'full': True})
        self.assertEqual(parsed['reported']['ide']['263'], {'issue': 6, 'keys': [], 'full': True})
        self.assertEqual(parsed['reported']['ide']['271'], {'issue': 4, 'keys': keys[:full], 'full': False})
        self.assertEqual(reported[str(MAJOR + 2)], {'issue': 9, 'keys': keys[2000:2000 + full], 'full': True})
        # The lines that became saturated make the cycle red and are named with the reason, once.
        self.assertEqual([message.split(':')[0] for message in watcher.errors],
                         ['Erlang/OTP %d is saturated' % (MAJOR + 1), 'Erlang/OTP %d is saturated' % MAJOR,
                          'IntelliJ IDEA 263 is saturated'])
        self.assertIn('is saturated: the state had to shrink below 40000 characters. Its later versions with new '
                      'problems are recorded as known problems with the link of #8', watcher.errors[0])
        watcher.prune()
        self.assertEqual(len(watcher.errors), 3)
        # A line that is saturated already and loses its keys is not named again.
        state = watch.new_state(T0)
        state['seen']['otp'] = ['%d.%d.%d' % (MAJOR, i, j) for i in range(60) for j in range(60)]
        state['reported']['otp'] = {str(MAJOR): {'issue': 7, 'keys': keys[:full], 'full': True}}
        watcher.state = state
        watcher.prune()
        self.assertEqual((state['reported']['otp'][str(MAJOR)]['keys'], len(watcher.errors)), ([], 3))
        # Without the pressure of the size nothing is dropped.
        state = watch.new_state(T0)
        state['reported']['otp'] = {str(MAJOR): {'issue': 7, 'keys': keys[:full], 'full': False}}
        watcher.state = state
        watcher.prune()
        self.assertEqual(state['reported']['otp'][str(MAJOR)], {'issue': 7, 'keys': keys[:full], 'full': False})

    def test_the_state_rejects_values_the_watcher_never_writes(self):
        state = watch.new_state(T0)
        state['seen']['otp'] = ['27.0`@x']
        body = watch.render_state(watch.new_state(T0), 'https://github.com', 'owner/repo')
        self.assertEqual(watch.parse_state(body)['pending'], ['ide', 'otp'])
        with self.assertRaises(watch.WatchError):
            watch.parse_state(body.replace('"otp":[]', '"otp":["27.0`@x"]'))
        # Values of a wrong type are an invalid state too, not a crash.
        for old, new in [('"pending":["ide","otp"]', '"pending":[[]]'), ('"entries":[]', '"entries":[{"kind":[]}]'),
                         ('"baselines":[]', '"baselines":[{"sha":5,"at":"x","state":"failed"}]'),
                         ('"failures":0', '"failures":true'), ('"failures":0', '"failures":-1'),
                         ('"entries":[]', '"entries":%s' % ('[' * 100000))]:
            self.assertIn(old, body)
            with self.assertRaises(watch.WatchError):
                watch.parse_state(body.replace(old, new))
        with self.assertRaises(watch.WatchError):
            watch.parse_state(body.replace('"reported":{"ide":{}',
                                           '"reported":{"ide":{"263":{"issue":1,"keys":["@x"],"full":false}}'))
        many = json.dumps(['%010x' % i for i in range(watch.KEYS_PER_LINE + 1)], separators=(',', ':'))
        for keys, valid in ((many, False), (many.replace('"0000000000",', ''), True)):
            changed = body.replace('"reported":{"ide":{}',
                                   '"reported":{"ide":{"263":{"issue":1,"keys":%s,"full":true}}' % keys)
            if valid:
                self.assertEqual(len(watch.parse_state(changed)['reported']['ide']['263']['keys']), 400)
            else:
                with self.assertRaises(watch.WatchError):
                    watch.parse_state(changed)


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

    def test_a_number_too_long_to_convert_is_not_a_version(self):
        huge = '9' * 5000
        self.assertTrue(compat.INPUT_PATTERNS['otp'].fullmatch(huge))
        self.assertFalse(watch.valid_version('otp', huge))
        self.assertFalse(watch.valid_version('otp', '%d%s' % (MAJOR + 1, '.1' * 30)))
        self.assertFalse(watch.valid_version('ide', '263.' + huge))
        # The major release is a key of the state, where it has at most 9 digits.
        self.assertFalse(watch.valid_version('otp', '1234567890.0'))
        self.assertTrue(watch.valid_version('otp', '123456789.0-rc1'))
        self.assertFalse(watch.valid_ide_name('2026.' + huge))
        world = World([PINNED, '%d.0' % (MAJOR + 1)], Seeded.IDE)
        world.otp_feed += 'OTP-%s x y\nOTP-%d.%s x y\n' % (huge, MAJOR + 1, huge)
        code, out = cycle(world)
        self.assertEqual(code, 0, out)
        self.assertIn('Erlang/OTP: 2 entries without a valid version were ignored', out)
        self.assertEqual([e['version'] for e in world.state()['entries'] if e['state'] == 'queued'],
                         ['%d.0' % (MAJOR + 1)])

    def test_a_feed_that_is_nested_too_deeply_stops_only_its_kind(self):
        world = World([PINNED, '%d.0' % (MAJOR + 1)], Seeded.IDE)
        request = world.request

        def nested(method, url, **kwargs):
            return watch.Response(200, b'[' * 200000) if url == watch.IDE_FEED else request(method, url, **kwargs)
        world.request = nested
        code, out = cycle(world)
        self.assertEqual(code, 1)
        self.assertIn('::error::IntelliJ IDEA: %s is not a JSON object' % watch.IDE_FEED, out)
        # The cycle went on: Erlang/OTP was discovered, the baseline was started and the state was written.
        self.assertEqual(world.state()['pending'], ['ide'])
        self.assertEqual(entry(world.state(), 'otp', '%d.0' % (MAJOR + 1))['state'], 'queued')
        self.assertEqual([i for _, i in world.dispatched], [{'ref': SHA}])

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


class CutFeedTest(unittest.TestCase):
    """A feed whose response is cut off, read by the real HTTP client from a local server."""

    VERSION = '%d.0' % (MAJOR + 1)

    def serve(self, respond):
        class Handler(http.server.BaseHTTPRequestHandler):
            protocol_version = 'HTTP/1.1'

            def do_GET(self):
                respond(self)
                self.wfile.flush()
                self.close_connection = True

            def log_message(self, *args):
                pass

        server = http.server.HTTPServer(('127.0.0.1', 0), Handler)
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        self.addCleanup(thread.join)
        self.addCleanup(server.server_close)
        self.addCleanup(server.shutdown)
        return 'http://127.0.0.1:%d/feed' % server.server_port

    def world(self, kind, url):
        """A world whose feed of `kind` is read from `url` once `world.cut` is set."""
        with unittest.mock.patch.dict(os.environ, {'no_proxy': '127.0.0.1', 'NO_PROXY': '127.0.0.1'}):
            client = watch.Http()
        world = World([PINNED, self.VERSION], Seeded.IDE)
        world.cut = False
        request = world.request

        def cut(method, target, **kwargs):
            if world.cut and target == {'otp': watch.OTP_FEED, 'ide': watch.IDE_FEED}[kind]:
                return client.request('GET', url)
            return request(method, target, **kwargs)
        world.request = cut
        return world

    def test_a_chunked_feed_that_is_cut_off_stops_only_its_kind(self):
        def respond(handler):
            handler.send_response(200)
            handler.send_header('Transfer-Encoding', 'chunked')
            handler.end_headers()
            handler.wfile.write(b'1000\r\n{"IIU": [{"build": "263.9999.1", "version": "2026.3.9"}, {"build": "263.99')
        world = self.world('ide', self.serve(respond))
        cycle(world)
        base = world.run_of({'ref': SHA})
        world.finish(base, {'otp': otp_result(SHA, PINNED, base), 'ide': ide_result(SHA, BASE_IDE, base)})
        world.cut = True
        code, out = cycle(world, T0 + HOUR)
        self.assertEqual(code, 1, out)
        self.assertIn('::error::IntelliJ IDEA: GET http://127.0.0.1:', out)
        self.assertIn('IncompleteRead', out)
        self.assertIn('; no IntelliJ IDEA version is discovered in this cycle\n', out)
        # The cycle went on: the run of the other kind was started and the state was written.
        self.assertEqual([i for _, i in world.dispatched[1:]], [{'otp': self.VERSION, 'ref': SHA}])
        state = world.state()
        self.assertEqual(entry(state, 'otp', self.VERSION)['state'], 'running')
        # Nothing of the cut body was recorded.
        self.assertEqual((state['pending'], state['seen']['ide']), (['ide'], []))
        self.assertEqual([e['version'] for e in state['entries'] if e['kind'] == 'ide'], [])

    def test_a_feed_shorter_than_its_content_length_stops_only_its_kind(self):
        # The connection is closed inside the name OTP-<major>.0-rc1: what came is the name of a release.
        cut_off = '%d.0' % (MAJOR + 3)

        def respond(handler):
            handler.send_response(200)
            handler.send_header('Content-Length', '100000')
            handler.end_headers()
            handler.wfile.write(('OTP-%s a b c\nOTP-%s' % (PINNED, cut_off)).encode())
        world = self.world('otp', self.serve(respond))
        with_baseline(world)
        world.ide_feed['IIU'].append({'build': '263.8000.1', 'version': '2026.3.1', 'type': 'release'})
        started = len(world.dispatched)
        world.cut = True
        code, out = cycle(world, T0 + HOUR)
        self.assertEqual(code, 1, out)
        self.assertIn('the response was cut off after %d of 100000 bytes; no Erlang/OTP version is discovered in this '
                      'cycle\n' % len('OTP-%s a b c\nOTP-%s' % (PINNED, cut_off)), out)
        self.assertEqual([i for _, i in world.dispatched[started:]], [{'ide': '263.8000.1', 'ref': SHA}])
        state = world.state()
        self.assertEqual(entry(state, 'ide', '263.8000.1')['state'], 'running')
        self.assertNotIn(cut_off, state['seen']['otp'])
        self.assertEqual([e['version'] for e in state['entries'] if e['kind'] == 'otp' and e['version'] == cut_off], [])
        # A complete response with a Content-Length is read as before.
        body = b'OTP-27.0 a b c\n'

        def complete(handler):
            handler.send_response(200)
            handler.send_header('Content-Length', str(len(body)))
            handler.end_headers()
            handler.wfile.write(body)
        with unittest.mock.patch.dict(os.environ, {'no_proxy': '127.0.0.1', 'NO_PROXY': '127.0.0.1'}):
            self.assertEqual(watch.Http().request('GET', self.serve(complete)).data, body)


class CompareTest(unittest.TestCase):
    def test_the_application_version_and_the_position_are_not_part_of_the_key(self):
        base = otp_result(SHA, PINNED, 1, errors=[corpus_error('lib/stdlib-6.2.2.3/src/x.erl', 'e', 10, 2)])
        result = otp_result(SHA, '29.0', 2, errors=[corpus_error('lib/stdlib-7.0/src/x.erl', 'e', 40, 7)])
        self.assertEqual(watch.compare('otp', result, base)['sections'], [])
        result = otp_result(SHA, '29.0', 2, errors=[corpus_error('lib/stdlib-7.0/src/x.erl', 'e', 40, 7),
                                                    corpus_error('lib/stdlib-7.0/src/x.erl', 'e', 50, 7)])
        self.assertEqual(watch.lines_of(watch.compare('otp', result, base)['sections']),
                         [('corpus', 'parser errors in the Erlang/OTP sources', ['lib/stdlib-7.0/src/x.erl: e'])])
        self.assertEqual(watch.without_app_version('lib/stdlib-6.2.2.3/src/x.erl'), 'lib/stdlib/src/x.erl')
        self.assertEqual(watch.without_app_version('erts-17.0/preloaded/src/init.erl'), 'erts/preloaded/src/init.erl')

    def test_the_list_of_expected_tokens_is_not_part_of_the_key_of_a_parser_error(self):
        got = "<attribute>, callback, define, %sspec or type expected, got 'if'"
        files = ['lib/a-1/src/x.erl', 'lib/a-1/src/y.erl']
        old = [corpus_error(file, got % 'nominal, ') for file in files]
        new = [corpus_error(file, got % 'native, nominal, ') for file in files]
        nothing = otp_result(SHA, PINNED, 1)
        sections = [watch.compare('otp', otp_result(SHA, '29.0', 2, '29.0', errors=errors), nothing)['sections']
                    for errors in (old, new)]
        # Two results that differ only in the list give the same keys, and are the same as each other.
        self.assertEqual([[p.key for p in found[0].problems] for found in sections],
                         [[p.key for p in sections[0][0].problems]] * 2)
        self.assertEqual(len({p.key for p in sections[0][0].problems}), 2)
        self.assertEqual(watch.compare('otp', otp_result(SHA, '29.0', 2, '29.0', errors=new),
                                       otp_result(SHA, PINNED, 1, errors=old)), {'sections': [], 'notes': []})
        # An issue shows the whole description.
        self.assertEqual(watch.lines_of(sections[1])[0][2][0], 'lib/a-1/src/x.erl: ' + got % 'native, nominal, ')
        # The token that was found is part of the key, and other descriptions stay whole.
        key = lambda description: watch.corpus_key(corpus_error('lib/a-1/src/x.erl', description))
        self.assertEqual(key(got % ''), ('lib/a/src/x.erl', "expected, got 'if'"))
        self.assertNotEqual(key(got % ''), key(got.replace("'if'", "'case'") % ''))
        for whole in ('<expression> expected', "'maybe' unexpected", "expected, got 'if'", 'x expected, got'):
            self.assertEqual(key(whole), ('lib/a/src/x.erl', whole))

    def test_a_failed_check_with_nothing_listed_is_never_the_same(self):
        test = {'class': 'A', 'name': 't', 'kind': 'failure', 'message': 'm'}

        def ide(**checks):
            result = ide_result(SHA, BASE_IDE, 1)
            result['checks'].update(checks)
            return result

        def notes(result, base):
            compared = watch.compare('ide', result, base)
            self.assertEqual(compared['sections'], [])
            return compared['notes']
        failed = {'status': 'failed', 'exitCode': 1, 'failedTests': listed([test]), 'skippedTests': listed([])}
        died = {'status': 'failed', 'exitCode': 1, 'tests': 0, 'failedTests': listed([]), 'skippedTests': listed([]),
                'reason': 'Gradle failed without a recognized finding; see logTail'}
        # The test task fails in the baseline with a failed test, and here without any test result.
        self.assertEqual(notes(ide(test=died), ide(test=failed)),
                         ['test: failed here with nothing listed to compare, failed in the baseline'])
        # The Plugin Verifier fails in the baseline with a problem, and here without a report.
        report = {'failingLevels': ['COMPATIBILITY_PROBLEMS'], 'problems': {'compatibility-problems': listed(['p'])}}
        self.assertEqual(notes(ide(verifyPlugin={'status': 'failed', 'exitCode': 1, 'reports': []}),
                               ide(verifyPlugin={'status': 'failed', 'exitCode': 1, 'reports': [report]})),
                         ['verifyPlugin: failed here with nothing listed to compare, failed in the baseline'])
        # The check did not run in the baseline, which has no exit code for it.
        blocked = {'status': 'not-run', 'reason': 'the plugin does not compile against this IDE', 'blockedBy': 'compile'}
        self.assertEqual(notes(ide(test=died), ide(test=blocked)),
                         ['test: failed here with nothing listed to compare, not-run in the baseline'])
        # The check ran in the baseline and did not run here.
        self.assertEqual(notes(ide(test=blocked), ide(test=failed)),
                         ['test: not-run here with nothing listed to compare, failed in the baseline'])
        # Not a note: the same failed test as in the baseline, a check that did not run in either, and a release
        # that the baseline does not know either.
        self.assertEqual(notes(ide(test=failed), ide(test=failed)), [])
        self.assertEqual(notes(ide(test=blocked), ide(test=blocked)), [])
        unknown = otp_result(SHA, '30.0', 1, known=False)
        self.assertEqual(watch.compare('otp', unknown, unknown), {'sections': [], 'notes': []})
        # With a problem to report, the version gets its issue.
        compared = watch.compare('ide', ide(test=died, verifyPlugin={'status': 'failed', 'exitCode': 1, 'reports': [report]}),
                                 ide(test=failed))
        self.assertEqual((len(compared['sections']), compared['notes']), (1, []))

    def test_occurrences_are_counted(self):
        base = otp_result(SHA, PINNED, 1, errors=[corpus_error('a-1/x.erl', 'e')])
        result = otp_result(SHA, '29.0', 2, errors=[corpus_error('a-2/x.erl', 'e', n) for n in range(4)])
        sections = watch.compare('otp', result, base)['sections']
        self.assertEqual(watch.lines_of(sections)[0][2], ['a-2/x.erl: e (3 times)'])
        self.assertEqual([(len(s.problems), sum(p.count for p in s.problems)) for s in sections], [(1, 3)])

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
        self.assertIsNone(comparison['sections'][0].cut)
        # Without the full list the listed ones that the baseline, whose list is complete, does not have are new.
        comparison = watch.compare('otp', result, base, {'result': 'the artifact is gone'})
        self.assertEqual(comparison['notes'], [])
        self.assertEqual(len(comparison['sections'][0].problems), 40)
        self.assertEqual(comparison['sections'][0].cut, 'the result file lists only the first 100 of 127; its full '
                                                        'list could not be read: the artifact is gone')

    def test_the_full_corpus_report_is_read_from_the_logs_artifact(self):
        world = World([PINNED, '%d.0' % (MAJOR + 1)], [(BASE_IDE, '2026.3')])
        with_baseline(world, BASE_ERRORS)
        version = '%d.0' % (MAJOR + 1)
        run_id = world.run_of({'otp': version, 'ref': SHA})
        errors = [corpus_error('lib/a-1/src/f%d.erl' % i, 'e') for i in range(127)]
        world.finish(run_id, {'otp': otp_result(SHA, version, run_id, version, errors=errors)},
                     logs={'errors': errors})
        cycle(world, T0 + HOUR)
        body = world.version_issues()[0]['body']
        self.assertIn('**corpus, parser errors in the Erlang/OTP sources**: 127 distinct, 127 occurrences\n', body)
        self.assertIn('<details><summary>97 more</summary>', body)
        self.assertNotIn('not shown', body)
        self.assertEqual(entry(world.state(), 'otp', version)['verdict'], 'new problems')

    def test_a_trimmed_list_of_the_baseline_makes_the_comparison_incomplete(self):
        tests = [{'class': 'C', 'name': 't%d' % i, 'message': 'm'} for i in range(150)]
        base = ide_result(SHA, BASE_IDE, 1, failed_tests=tests[:120])
        result = ide_result(SHA, '263.8.1', 2, failed_tests=tests)
        comparison = watch.compare('ide', result, base)
        self.assertEqual(comparison['notes'], ['failed tests: the baseline has 120 and its result lists only the '
                                               'first 100'])
        self.assertEqual(comparison['sections'], [])

    def test_a_trimmed_list_of_the_version_is_compared_by_its_listed_items(self):
        tests = [{'class': 'C', 'name': 't%03d' % i, 'message': 'm'} for i in range(150)]
        result = ide_result(SHA, '263.8.1', 2, failed_tests=tests)
        comparison = watch.compare('ide', result, ide_result(SHA, BASE_IDE, 1, failed_tests=tests[90:]))
        self.assertEqual(comparison['notes'], [])
        section = comparison['sections'][0]
        self.assertEqual((section.title, len(section.problems)), ('failed tests', 90))
        self.assertEqual(section.cut, 'the result file lists only the first 100 of 150')
        # None of the listed ones is new, but there are more than the baseline has in all: that is one problem.
        comparison = watch.compare('ide', result, ide_result(SHA, BASE_IDE, 1, failed_tests=tests[:100]))
        self.assertEqual(comparison['notes'], [])
        self.assertEqual(watch.lines_of(comparison['sections']), [
            ('test', 'failed tests',
             ['150 in all, 100 in the baseline: at least 50 more, none of them among the listed ones'])])

    def test_every_list_of_a_check_is_compared(self):
        base = otp_result(SHA, PINNED, 1, errors=BASE_ERRORS)
        base['checks']['corpus'].update(exitCode=0, slowFiles=listed([{'file': 'lib/a-1/src/s.erl', 'millis': 11000}]))
        result = otp_result(SHA, '29.0', 2, '29.0', errors=BASE_ERRORS)
        slow = [{'file': 'lib/a-2/src/s.erl', 'millis': 20000}, {'file': 'lib/stdlib-7.0/src/big.erl', 'millis': 600000}]
        result['checks']['corpus'].update(exitCode=0, slowFiles=listed(slow))
        # The baseline's check has failed already, so its status says nothing; the lists do.
        self.assertEqual(base['checks']['corpus']['status'], 'failed')
        self.assertEqual(watch.lines_of(watch.compare('otp', result, base)['sections']), [
            ('corpus', 'files of the Erlang/OTP sources that the parser is slow on',
             ['lib/stdlib-7.0/src/big.erl: 600000 ms'])])
        # A parser exception too; its file is compared without the version of the application.
        raised = [{'file': 'lib/a-1/src/s.erl', 'exception': 'java.lang.StackOverflowError'}]
        base['checks']['corpus']['exceptions'] = listed(raised)
        result['checks']['corpus'].update(slowFiles=listed([]), exceptions=listed([
            {'file': 'lib/a-2/src/s.erl', 'exception': 'java.lang.StackOverflowError'},
            {'file': 'lib/a-2/src/s.erl', 'exception': 'java.lang.IllegalStateException: x'}]))
        self.assertEqual(watch.lines_of(watch.compare('otp', result, base)['sections']), [
            ('corpus', 'parser exceptions in the Erlang/OTP sources',
             ['lib/a-2/src/s.erl: java.lang.IllegalStateException: x'])])
        # A skipped test of the IDE job too.
        ide_base, ide = ide_result(SHA, BASE_IDE, 1), ide_result(SHA, '263.8.1', 2, '263.8.1')
        ide_base['checks']['test']['skippedTests'] = listed([{'class': 'A', 'name': 'testOld'}])
        ide['checks']['test']['skippedTests'] = listed([{'class': 'A', 'name': 'testOld'},
                                                        {'class': 'A', 'name': 'testNew'}])
        self.assertEqual(watch.lines_of(watch.compare('ide', ide, ide_base)['sections']),
                         [('test', 'skipped tests', ['A.testNew'])])
        # Every list that compat.py writes with compat.trimmed is in LISTS, or is a list of the Plugin Verifier.
        # skippedFiles is not compared: it lists the files that the corpus check skips on purpose, as not Erlang
        # sources, and never fails the check.
        with open(os.path.join(os.path.dirname(os.path.abspath(compat.__file__)), 'compat.py'), encoding='utf-8') as f:
            written = set(re.findall(r"\[?'(\w+)'\]?(?: =|:) trimmed\(", f.read()))
        self.assertEqual(written, {'errors', 'failedTests', 'skippedTests', 'exceptions', 'slowFiles', 'skippedFiles'})

    def test_an_exit_code_that_is_zero_in_the_baseline_only_is_a_problem(self):
        base = otp_result(SHA, PINNED, 1, errors=BASE_ERRORS)
        base['checks']['corpus']['exitCode'] = 0
        result = otp_result(SHA, '29.0', 2, '29.0', errors=BASE_ERRORS)
        result['checks']['corpus'].update(exitCode=1, reason='the test task failed after the report was written')
        self.assertEqual(watch.lines_of(watch.compare('otp', result, base)['sections']), [
            ('corpus', 'exit code', ['corpus: exit code 1 here, 0 in the baseline: the test task failed after the '
                                     'report was written'])])
        # Not when the baseline's is not zero either, or when the check has none.
        base['checks']['corpus']['exitCode'] = 1
        self.assertEqual(watch.compare('otp', result, base)['sections'], [])
        del base['checks']['corpus']['exitCode']
        self.assertEqual(watch.compare('otp', result, base)['sections'], [])
        # With the exit code to show for it, the status is not a second problem.
        passed = otp_result(SHA, PINNED, 1)
        passed['checks']['compilationTests']['exitCode'] = 0
        failed = otp_result(SHA, '29.0', 2, '29.0')
        failed['checks']['compilationTests'].update(status='failed', exitCode=1, reason='Gradle failed')
        self.assertEqual(watch.lines_of(watch.compare('otp', failed, passed)['sections']), [
            ('compilationTests', 'exit code', ['compilationTests: exit code 1 here, 0 in the baseline: Gradle failed'])])

    def test_a_release_that_the_baseline_does_not_know_either_is_not_new(self):
        base = otp_result(SHA, '30.0', 1, known=False)
        self.assertEqual(watch.compare('otp', otp_result(SHA, '30.1', 2, '30.1', known=False), base),
                         {'sections': [], 'notes': []})
        # The same failure for another release is new.
        sections = watch.compare('otp', otp_result(SHA, '31.0', 2, '31.0', known=False), base)['sections']
        self.assertEqual(watch.lines_of(sections), [('knownRelease', 'the release is unknown to the plugin', [
            'Erlang/OTP 31: ErlangSdkRelease constant missing, no testData/mockSdk-R31'])])

    def test_checks_and_keys(self):
        base = ide_result(SHA, BASE_IDE, 1, verifier=['old problem'])
        result = ide_result(SHA, '263.8.1', 2, verifier=['old problem', 'new problem'])
        result['checks']['releasedPlugin'] = {'status': 'not-run', 'reason': 'the release has no archive'}
        base['checks']['releasedPlugin'] = {'status': 'passed', 'reports': []}
        result['checks']['compile'] = {'status': 'failed', 'errors': listed([{'file': 'A.java', 'line': 3,
                                                                              'message': 'cannot find symbol'}])}
        base['checks']['compile'] = {'status': 'failed', 'errors': listed([{'file': 'A.java', 'line': 9,
                                                                            'message': 'cannot find symbol'}])}
        sections = watch.lines_of(watch.compare('ide', result, base)['sections'])
        self.assertEqual(sections, [
            ('releasedPlugin', 'status', ['releasedPlugin: passed in the baseline, not-run here: the release has no '
                                          'archive']),
            ('verifyPlugin', 'Plugin Verifier problems of the built plugin', ['compatibility-problems: new problem'])])
        skipped = otp_result(SHA, '29.0', 2)
        skipped['checks']['compilationTests']['skippedTests'] = listed([{'class': 'R', 'name': 'testRebar'}])
        self.assertEqual(watch.lines_of(watch.compare('otp', skipped, otp_result(SHA, PINNED, 1))['sections']),
                         [('compilationTests', 'skipped compilation tests', ['R.testRebar'])])
        unknown = otp_result(SHA, '30.0', 2, known=False)
        self.assertEqual(watch.compare('otp', unknown, otp_result(SHA, PINNED, 1))['sections'][0][0], 'knownRelease')

    def test_a_check_that_does_not_apply_is_no_problem(self):
        base = ide_result(SHA, BASE_IDE, 1)
        result = ide_result(SHA, '262.1.1', 2)
        base['checks']['releasedPlugin'] = {'status': 'passed', 'reports': []}
        result['checks']['releasedPlugin'] = {'status': 'not-run', 'notApplicable': True,
                                              'reason': 'the IDE build is below the since-build 262.4852'}
        self.assertEqual(watch.compare('ide', result, base), {'sections': [], 'notes': []})

    def test_only_the_gated_levels_of_the_plugin_verifier_are_problems(self):
        def check(status, problems, failing=()):
            lists = {name: listed(lines) for name, lines in problems.items()}
            return {'status': status, 'reports': [{'ide': 'IU-263.1', 'failingLevels': list(failing),
                                                   'problems': lists}]}
        self.assertEqual(sorted(list(watch.VERIFIER_CATEGORIES) + ['MISSING_DEPENDENCIES']),
                         sorted(compat.VERIFIER_FAILURE_LEVELS))
        base, result = ide_result(SHA, BASE_IDE, 1), ide_result(SHA, '263.8.1', 2)
        base['checks']['verifyPlugin'] = check('passed', {'deprecated-usages': ['d1'], 'internal-api-usages': ['i1']})
        result['checks']['verifyPlugin'] = check('passed', {
            'deprecated-usages': ['d1', 'd2'], 'internal-api-usages': ['i1', 'i2'], 'experimental-api-usages': ['e1'],
            'non-extendable-api-usages': ['n1'], 'compatibility-warnings': ['w1']})
        self.assertEqual(watch.compare('ide', result, base), {'sections': [], 'notes': []})
        result['checks']['verifyPlugin'] = check('failed', {
            'deprecated-usages': ['d1', 'd2'], 'compatibility-problems': ['c1'], 'override-only-usages': ['o1'],
            'invalid-plugin': ['p1']}, ['COMPATIBILITY_PROBLEMS', 'OVERRIDE_ONLY_API_USAGES', 'INVALID_PLUGIN'])
        self.assertEqual(watch.lines_of(watch.compare('ide', result, base)['sections']), [
            ('verifyPlugin', 'Plugin Verifier problems of the built plugin',
             ['compatibility-problems: c1', 'invalid-plugin: p1', 'override-only-usages: o1'])])
        # Missing dependencies have no list in the result: the level itself is the problem.
        result['checks']['verifyPlugin'] = check('failed', {'deprecated-usages': ['d2']}, ['MISSING_DEPENDENCIES'])
        self.assertEqual(watch.lines_of(watch.compare('ide', result, base)['sections']), [
            ('verifyPlugin', 'Plugin Verifier problems of the built plugin', ['verdict: MISSING_DEPENDENCIES'])])
        # A trimmed list that is not compared does not make the comparison incomplete.
        result['checks']['verifyPlugin'] = check('passed', {'deprecated-usages': ['d%d' % i for i in range(150)]})
        self.assertEqual(watch.compare('ide', result, base), {'sections': [], 'notes': []})

    def test_a_trimmed_list_is_never_compared_by_its_first_items(self):
        # The baseline has 150 errors; the version has the same without the first 10, so the first 100 of each differ.
        errors = [corpus_error('lib/a-1/src/f%03d.erl' % i, 'e') for i in range(150)]
        base = otp_result(SHA, PINNED, 1, errors=errors)
        result = otp_result(SHA, '29.0', 2, errors=errors[10:])
        unread = {'result': 'no artifact', 'baseline': 'no artifact'}
        self.assertEqual(watch.compare('otp', result, base, unread), {'sections': [], 'notes': [
            'the full list of parser errors of the baseline could not be read: no artifact']})
        # With the baseline's full list only, none of the listed ones is new and the version has fewer in all.
        comparison = watch.compare('otp', result, base, {'result': 'no artifact', 'baseline': errors})
        self.assertEqual((comparison['sections'], len(comparison['notes'])), ([], 1))
        self.assertEqual(watch.compare('otp', result, base, {'result': errors[10:], 'baseline': errors}),
                         {'sections': [], 'notes': []})

    def test_a_hostile_string_in_a_result_is_neutralised_in_the_issue(self):
        hostile = 'x```\n# @maintainer see [here](https://evil)\u202e `' + 'y' * 1000
        base = otp_result(SHA, PINNED, 1)
        result = otp_result(SHA, '29.0', 2, errors=[corpus_error('lib/a-1/' + hostile, hostile)])
        comparison = watch.compare('otp', result, base, {})
        entry_ = dict(watch.ENTRY_DEFAULTS, kind='otp', version='29.0', sha=SHA, run=2)
        body, shown = watch.issue_body(entry_, comparison['sections'], 'https://github.com/o/r/actions/runs/2',
                                       'https://github.com/o/r/actions/runs/1')
        self.assertEqual(shown, [comparison['sections'][0].problems[0].key])
        self.assertNotIn('@', body)
        self.assertNotIn('\u202e', body)
        self.assertEqual(watch.safe('a <!-- compat-watch:otp:29.1 --> <!----> b'),
                         'a <! -- compat-watch:otp:29.1 -- > <! ---- > b')
        fences = [line for line in body.splitlines() if '`' in line and line.strip().startswith('```')]
        self.assertEqual(fences, ['```', '```'])
        problem = body.split('```\n')[1].split('\n')[0]
        self.assertEqual(len(problem), watch.ISSUE_TEXT_CHARS)
        self.assertTrue(problem.endswith('\u2026'))

    def test_an_issue_shows_a_bounded_number_of_problems_and_names_the_rest(self):
        def found(check, title, count, text='x'):
            return watch.Section(check, title, [watch.problem(check, title, i, '%s%d %s' % (title, i, text))
                                                for i in range(count)])
        entry_ = dict(watch.ENTRY_DEFAULTS, kind='otp', version='29.0', sha=SHA, run=2)
        sections = [found('corpus', 'errors', 150), found('corpus', 'slow', 100), found('compilationTests', 'failed', 5)]
        body, shown = watch.issue_body(entry_, sections, 'run', 'baseline', earlier=7)
        # Of a check 30 in the open and 200 in all, whatever its lists; another check has its own numbers.
        self.assertEqual(len(shown), 205)
        self.assertEqual(shown, [p.key for p in sections[0].problems] + [p.key for p in sections[1].problems[:50]]
                         + [p.key for p in sections[2].problems])
        blocks = body.split('```\n')
        self.assertEqual([block.count('\n') for block in blocks[1::2]], [30, 120, 50, 5])
        self.assertEqual(body.count('<details><summary>'), 2)
        self.assertEqual(body.count('</details>'), 2)
        self.assertIn('50 more are not shown: an issue shows at most 200 problems of a check and 55000 characters of '
                      'them. A later version of Erlang/OTP 29 that has them reports them.', body)
        self.assertEqual(body.count('more are not shown'), 1)
        # The budget of the body: long lines end the lists sooner, and the body stays below the limit of an issue.
        long = [found('corpus', 'errors', 500, 'y' * 400), found('compilationTests', 'failed', 500, 'z' * 400),
                found('knownRelease', 'release', 1, 'w' * 400)]
        body, shown = watch.issue_body(entry_, long, 'run', 'baseline')
        self.assertLess(len(body), watch.ISSUE_BODY_BUDGET + 5000)
        self.assertLess(len(body), watch.BODY_LIMIT)
        self.assertEqual(len(shown), watch.ISSUE_BODY_BUDGET // (watch.ISSUE_TEXT_CHARS + 1))
        self.assertIn('**knownRelease, release**: 1 distinct, 1 occurrences\n\n1 more are not shown', body)
        self.assertNotIn(long[2].problems[0].key, shown)


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
        # Never on pull_request, only in this repository and only on its default branch.
        self.assertEqual(writers[0]['if'], "${{ github.event_name != 'pull_request' "
                         "&& github.repository == 'ignatov/intellij-erlang' && (github.event_name == 'schedule' "
                         "|| github.ref == format('refs/heads/{0}', github.event.repository.default_branch)) }}")
        readers = [job for job in jobs.values() if job is not writers[0]]
        self.assertEqual([job['permissions'] for job in readers],
                         [{'contents': 'read', 'actions': 'read', 'issues': 'read'}])
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
            # Cone mode, so that build.gradle, a file of the root, is checked out.
            self.assertNotIn(checkout['with'].get('sparse-checkout-cone-mode'), (False, 'false'))
            self.assertFalse(checkout['with']['persist-credentials'])


if __name__ == '__main__':
    unittest.main()
