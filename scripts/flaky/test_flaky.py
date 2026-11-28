import io
import json
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import flaky  # noqa: E402

LOG = '''\ufeff2026-11-27T03:23:20.4738345Z ##[group]Run ./gradlew --no-daemon clean test
2026-11-27T03:33:37.2259834Z ErlangUsageBuildListenerTest > testRebarBuildIsCounted FAILED
2026-11-27T03:33:37.2260000Z     java.lang.NullPointerException: Cannot invoke `getAsInt()`
2026-11-27T03:33:37.2260001Z         at org.intellij.erlang.statistics.ErlangUsageBuildListenerTest.testRebarBuildIsCounted(ErlangUsageBuildListenerTest.java:81)
2026-11-27T03:33:37.2260002Z
2026-11-27T03:34:27.9306588Z 2639 tests completed, 1 failed, 1 skipped
2026-11-27T03:34:28.2287020Z > Task :test FAILED
'''
TEST = 'ErlangUsageBuildListenerTest.testRebarBuildIsCounted'
PATH = 'tests/org/intellij/erlang/statistics/ErlangUsageBuildListenerTest.java'
FILES = {'ErlangUsageBuildListenerTest': PATH}


def job(job_id, name, conclusion, started='2026-11-27T03:23:10Z'):
    return {'id': job_id, 'name': name, 'conclusion': conclusion, 'started_at': started}


def run(run_id=7, attempts=2, event='pull_request', branch='factory-release', conclusion='success'):
    return {'id': run_id, 'run_attempt': attempts, 'event': event, 'head_branch': branch, 'status': 'completed',
            'conclusion': conclusion}


def counts():
    return dict.fromkeys(flaky.COUNTS, 0)


class FakeGitHub:
    def __init__(self, jobs, logs, issues=()):
        self._jobs = jobs
        self._logs = logs
        self._issues = list(issues)
        self.calls = []

    def jobs(self, run_id, attempt):
        return self._jobs[(run_id, attempt)]

    def log(self, job_id):
        return self._logs.get(job_id)

    def issues(self):
        return self._issues

    def ensure_label(self):
        self.calls.append(('label',))

    def create(self, title, body):
        self.calls.append(('create', title, body))
        self._issues.append({'number': 100 + len(self._issues), 'state': 'open', 'body': body})
        return self._issues[-1]['number']

    def update(self, number, fields):
        self.calls.append(('update', number, fields))
        for issue in self._issues:
            if issue['number'] == number:
                issue.update(fields)

    def comment(self, number, body):
        self.calls.append(('comment', number, body))


def rerun_github(issues=()):
    return FakeGitHub({(7, 1): [job(11, 'build', 'failure'), job(12, 'test-windows-latest', 'success')],
                       (7, 2): [job(21, 'build', 'success'), job(22, 'test-windows-latest', 'success')]},
                      {11: LOG}, issues)


class ParseTest(unittest.TestCase):
    def test_failed_test_with_its_exception(self):
        (test, excerpt), = flaky.parse_failures(LOG)
        self.assertEqual(TEST, test)
        self.assertEqual(2, len(excerpt))
        # a backtick would end a code span of the issue
        self.assertEqual("java.lang.NullPointerException: Cannot invoke 'getAsInt()'", excerpt[0])
        self.assertTrue(excerpt[1].startswith('    at org.intellij.erlang.statistics.'))

    def test_task_lines_and_odd_names_are_no_tests(self):
        log = '> Task :test FAILED\nA > b c FAILED\n@ignatov > test FAILED\nA > testB FAILED and more\n'
        self.assertEqual([], flaky.parse_failures(log))

    def test_excerpt_is_cut(self):
        log = 'A > testB FAILED\n' + ('    ' + 'x' * 500 + '\n') * 40
        (_, excerpt), = flaky.parse_failures(log)
        self.assertEqual(flaky.EXCERPT_LINES, len(excerpt))
        self.assertEqual(flaky.EXCERPT_WIDTH, len(excerpt[0]))


class ObserveTest(unittest.TestCase):
    def test_job_that_passed_in_another_attempt(self):
        found = flaky.observe(rerun_github(), run(), counts())
        self.assertEqual([[7, 1, 11, 'build', flaky.RERUN, '2026-11-27T03:23:10Z']], [o.stored() for o in found])
        self.assertEqual(TEST, found[0].test)

    def test_pull_request_failure_without_a_pass_does_not_count(self):
        github = FakeGitHub({(7, 1): [job(11, 'build', 'failure')], (7, 2): [job(21, 'build', 'failure')]},
                            {11: LOG, 21: LOG})
        self.assertEqual([], flaky.observe(github, run(conclusion='failure'), counts()))

    def test_master_counts_the_last_attempt_only(self):
        github = FakeGitHub({(7, 1): [job(11, 'test-macos-latest', 'failure')],
                             (7, 2): [job(21, 'test-macos-latest', 'failure')]}, {11: LOG, 21: LOG})
        found = flaky.observe(github, run(event='push', branch='master', conclusion='failure'), counts())
        self.assertEqual([(2, 21, flaky.MASTER)], [(o.attempt, o.job, o.kind) for o in found])

    def test_jobs_that_are_skipped_are_counted(self):
        many = ''.join('A > test%d FAILED\n' % index for index in range(flaky.MAX_FAILED_PER_JOB + 1))
        github = FakeGitHub({(7, 1): [job(11, 'a', 'failure'), job(12, 'b', 'failure'), job(13, 'c', 'failure'),
                                      job(14, 'ide-${{ matrix.ide }}', 'failure')],
                             (7, 2): [job(21, 'a', 'success'), job(22, 'b', 'success'), job(23, 'c', 'success')]},
                            {11: 'Could not resolve all files\n', 12: many})
        seen = counts()
        self.assertEqual([], flaky.observe(github, run(), seen))
        self.assertEqual({'logs not read': 1, 'failed jobs without a failed test': 1,
                          'failed jobs with too many failed tests': 1},
                         {name: count for name, count in seen.items() if count})

    def test_candidates(self):
        self.assertTrue(flaky.is_candidate(run()))
        self.assertTrue(flaky.is_candidate(run(attempts=1, event='push', branch='master', conclusion='failure')))
        self.assertFalse(flaky.is_candidate(run(attempts=1, conclusion='failure')))
        self.assertFalse(flaky.is_candidate(run(attempts=1, event='push', branch='master')))
        self.assertFalse(flaky.is_candidate(dict(run(), status='in_progress')))


class PlanTest(unittest.TestCase):
    def setUp(self):
        self.found = flaky.observe(rerun_github(), run(), counts())

    def plan(self, issues, found=None, max_new=5, max_updates=10, seen=None):
        return flaky.plan(self.found if found is None else found, issues, FILES, max_new, max_updates,
                          counts() if seen is None else seen)

    def issue(self, state='open', reason=None, closed_at=None, number=5):
        return {'number': number, 'state': state, 'state_reason': reason, 'closed_at': closed_at,
                'body': flaky.render_body(TEST, PATH, self.found, [])}

    def later(self, at='2026-11-28T10:00:00Z'):
        return flaky.Observation(TEST, 8, 1, 31, 'build', flaky.RERUN, at, ['boom'])

    def test_new_issue_and_its_body(self):
        action, = self.plan([])
        self.assertEqual(('new', 'Flaky test: ' + TEST), (action.kind, action.title))
        body = action.body()
        self.assertTrue(body.startswith('<!-- flaky-test: %s -->\n<!-- flaky-test-state: ' % TEST))
        self.assertIn('job [build](https://github.com/ignatov/intellij-erlang/actions/runs/7/job/11), attempt 1', body)
        self.assertIn("```text\njava.lang.NullPointerException: Cannot invoke 'getAsInt()'\n", body)
        self.assertEqual([o.stored() for o in self.found], [o.stored() for o in flaky.read_state(body)])

    def test_known_failure_changes_nothing(self):
        self.assertEqual([], self.plan([self.issue()]))

    def test_new_failure_updates_the_open_issue(self):
        action, = self.plan([self.issue()], self.found + [self.later()])
        self.assertEqual(('update', 5, [31]), (action.kind, action.number, [o.job for o in action.new]))
        self.assertEqual([11, 31], [o.job for o in flaky.read_state(action.body())])

    def test_closed_issue_is_reopened_only_by_a_later_failure(self):
        closed = self.issue('closed', 'completed', '2026-11-28T00:00:00Z')
        early = self.later('2026-11-27T23:00:00Z')
        self.assertEqual([], self.plan([closed], self.found + [early]))
        action, = self.plan([closed], self.found + [early, self.later()])
        self.assertEqual(('reopen', [31]), (action.kind, [o.job for o in action.new]))

    def test_issue_closed_as_not_planned_stays_closed(self):
        seen = counts()
        closed = self.issue('closed', 'not_planned', '2026-11-28T00:00:00Z')
        self.assertEqual([], self.plan([closed], self.found + [self.later()], seen=seen))
        self.assertEqual(1, seen['failures of tests whose issue is closed as not planned'])

    def test_open_issue_wins_over_a_closed_one(self):
        closed = self.issue('closed', 'completed', '2026-11-26T00:00:00Z', number=9)
        action, = self.plan([closed, self.issue(number=5)], self.found + [self.later()])
        self.assertEqual(('update', 5), (action.kind, action.number))

    def test_test_that_only_a_pull_request_has(self):
        seen = counts()
        other = flaky.Observation('NewTest.testIt', 8, 1, 31, 'build', flaky.RERUN, '2026-11-28T10:00:00Z')
        self.assertEqual([], self.plan([], [other], seen=seen))
        self.assertEqual(1, seen['failures of tests that are not in this checkout'])

    def test_caps(self):
        seen = counts()
        self.assertEqual([], self.plan([], max_new=0, seen=seen))
        self.assertEqual(1, seen['changes left for the next run'])

    def test_state_that_is_not_ours_is_dropped(self):
        body = ('<!-- flaky-test: A.testB -->\n<!-- flaky-test-state: {"seen":[[1,1,2,"build","rerun",'
                '"2026-11-27T03:23:10Z"],[1,1,3,"[x](http://evil)","rerun","2026-11-27T03:23:10Z"],[1,true,4,"b",'
                '"rerun","2026-11-27T03:23:10Z"],"x",[1,1,5,"b","other","2026-11-27T03:23:10Z"]]} -->')
        self.assertEqual([2], [o.job for o in flaky.read_state(body)])
        self.assertEqual([], flaky.read_state('<!-- flaky-test: A.testB -->\n<!-- flaky-test-state: {"seen":1} -->'))


class Http:
    """GitHub over HTTP: the runs, the jobs of run 7, the log of job 11 behind a redirect, and the issues."""

    def __init__(self):
        self.issues = []
        self.requests = []

    def request(self, method, url, headers, data=None):
        self.requests.append((method, url, headers.get('Authorization')))
        base = 'https://api.github.com/repos/ignatov/intellij-erlang'

        def answer(value, status=200):
            return status, {}, json.dumps(value).encode()

        if url.startswith(base + '/actions/workflows/gradle.yml/runs?created=%3E%3D2026-11-25T12%3A00%3A00Z'):
            return answer({'workflow_runs': [run(), run(run_id=8, attempts=1)]})
        if url.startswith(base + '/actions/runs/7/attempts/'):
            return answer({'jobs': rerun_github().jobs(7, int(url.split('/attempts/')[1].split('/')[0]))})
        if url == base + '/actions/jobs/11/logs':
            return 302, {'Location': 'https://storage.example/log?sig=1'}, b''
        if url == 'https://storage.example/log?sig=1':
            return 200, {}, LOG.encode()
        if method == 'GET' and url.startswith(base + '/issues?labels=flaky-test&state=all'):
            return answer(self.issues)
        if method == 'POST' and url == base + '/labels':
            return answer({}, 422)
        if method == 'POST' and url == base + '/issues':
            issue = dict(json.loads(data), number=41, state='open')
            self.issues.append(issue)
            return answer(issue, 201)
        return 404, {}, b''


class MainTest(unittest.TestCase):
    def main(self, http, *argv):
        out = io.StringIO()
        with tempfile.TemporaryDirectory() as root:
            os.makedirs(os.path.join(root, os.path.dirname(PATH)))
            open(os.path.join(root, PATH), 'w').close()
            now = flaky.datetime.datetime(2026, 11, 28, 12, 0, tzinfo=flaky.datetime.timezone.utc)
            status = flaky.main(list(argv), {'GITHUB_TOKEN': 'secret'}, http, out, now, root)
        return status, out.getvalue()

    def test_dry_run_writes_nothing(self):
        http = Http()
        status, text = self.main(http, '--dry-run')
        self.assertEqual(0, status, text)
        self.assertIn('Would do: new issue: %s, 1 new failure(s)' % TEST, text)
        self.assertIn('`%s` in `%s` fails on CI' % (TEST, PATH), text)
        self.assertEqual({'GET'}, {method for method, _, _ in http.requests})
        self.assertNotIn('secret', text)

    def test_run_files_the_issue_once_and_keeps_the_token_from_the_storage_link(self):
        http = Http()
        status, text = self.main(http, '--run')
        self.assertEqual(0, status, text)
        self.assertEqual(['Flaky test: ' + TEST], [issue['title'] for issue in http.issues])
        self.assertEqual(['flaky-test', 'Bug'], http.issues[0]['labels'])
        self.assertEqual([None], [token for _, url, token in http.requests if url.startswith('https://storage.')])
        status, text = self.main(http, '--run')
        self.assertEqual(0, status, text)
        self.assertIn('Nothing to change.', text)
        self.assertEqual(1, len(http.issues))

    def test_failed_read_ends_the_run(self):
        http = Http()
        http.request = lambda method, url, headers, data=None: (500, {}, b'')
        status, text = self.main(http, '--run')
        self.assertEqual((1, 'error: GET /actions/workflows/gradle.yml/runs: HTTP 500\n'), (status, text))


class ExecuteTest(unittest.TestCase):
    def test_reopen_writes_the_body_first_and_then_one_comment(self):
        found = flaky.observe(rerun_github(), run(), counts())
        later = flaky.Observation(TEST, 8, 1, 31, 'build', flaky.MASTER, '2026-11-28T10:00:00Z', ['boom'])
        github = FakeGitHub({}, {})
        action = flaky.Action('reopen', TEST, PATH, found + [later], [later], 5)
        flaky.execute(github, [action], io.StringIO())
        (update, number, fields), (comment, commented, text) = github.calls
        self.assertEqual(('update', 5, 'comment', 5), (update, number, comment, commented))
        self.assertEqual(('open', 'reopened'), (fields['state'], fields['state_reason']))
        self.assertIn('```text\nboom\n```', fields['body'])
        self.assertTrue(text.startswith('Reopened: the test failed again after this issue was closed.'))
        self.assertIn('attempt 1: on a push to master', text)


if __name__ == '__main__':
    unittest.main()
