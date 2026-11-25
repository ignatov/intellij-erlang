"""Tests of the dispatch steps of the factory workflows: python3 -m unittest discover -s scripts/factory -p 'test_*.py' -v

The steps are bash in the workflow files (those jobs never check out this repository). Each test takes the run script
of a step from its workflow file, as GitHub would run it, and runs it with bash against a fake gh and a fake sleep.
"""

import datetime
import os
import re
import shutil
import subprocess
import tempfile
import textwrap
import unittest

REPOSITORY = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
WORKFLOWS = os.path.join(REPOSITORY, '.github', 'workflows')

# (workflow, step, the command that the step retries)
DISPATCHES = [
    ('factory-timer.yml', 'Start the next tick', 'workflow run factory-tick.yml'),
    ('factory-wake.yml', 'Start a tick', 'workflow run factory-tick.yml'),
    ('factory-tick.yml', 'Arm the timer for the next tick', 'workflow run factory-timer.yml'),
    ('factory-rescue.yml', 'Start a tick when the clock stopped', 'workflow run factory-tick.yml'),
]
RESCUE = ('factory-rescue.yml', 'Start a tick when the clock stopped')

# The fake gh: call N appends its arguments to calls.log and answers with answers/N, whose first line is the exit
# status and whose rest goes to stderr on a failure, to stdout on success. Without answers/N it succeeds silently.
FAKE_GH = r'''#!/bin/bash
dir="$FAKE_DIR"
n=$(( $(wc -l < "$dir/calls.log") + 1 ))
printf '%s\n' "$*" >> "$dir/calls.log"
[ -f "$dir/answers/$n" ] || exit 0
status=$(head -n 1 "$dir/answers/$n")
if [ "$status" = 0 ]; then tail -n +2 "$dir/answers/$n"; else tail -n +2 "$dir/answers/$n" >&2; fi
exit "$status"
'''
FAKE_SLEEP = '#!/bin/bash\nprintf \'%s\\n\' "$1" >> "$FAKE_DIR/sleeps.log"\n'


def read(path):
    with open(path, encoding='utf-8') as f:
        return f.read()


def step(workflow, name):
    """The run script and the plain env values of a step, as GitHub gives them to bash."""
    lines = read(os.path.join(WORKFLOWS, workflow)).split('\n')
    start = lines.index(next(line for line in lines if re.fullmatch(r' *- name: ' + re.escape(name), line)))
    indent = len(lines[start]) - len(lines[start].lstrip()) + 2  # the keys of the step
    body, env, i = None, {}, start + 1
    while i < len(lines) and (not lines[i].strip() or len(lines[i]) - len(lines[i].lstrip()) >= indent):
        line = lines[i]
        if line == ' ' * indent + 'env:':
            i += 1
            while i < len(lines) and len(lines[i]) - len(lines[i].lstrip()) > indent:
                m = re.fullmatch(r' +([A-Z_]+): (.*)', lines[i])
                if m and '${{' not in m.group(2):
                    env[m.group(1)] = m.group(2).strip("'")
                i += 1
            continue
        if line == ' ' * indent + 'run: |':
            block = []
            i += 1
            while i < len(lines) and (not lines[i].strip() or len(lines[i]) - len(lines[i].lstrip()) > indent):
                block.append(lines[i])
                i += 1
            body = textwrap.dedent('\n'.join(block)).rstrip('\n') + '\n'
            continue
        i += 1
    if body is None:
        raise AssertionError('no run script in the step %r of %s' % (name, workflow))
    return body, env


def retry_function(script):
    m = re.search(r'^retry\(\) \{\n.*?^\}\n', script, re.S | re.M)
    if not m:
        raise AssertionError('no retry function')
    return m.group(0)


def iso(minutes_ago):
    t = datetime.datetime.now(datetime.timezone.utc) - datetime.timedelta(minutes=minutes_ago)
    return t.strftime('%Y-%m-%dT%H:%M:%SZ')


class StepRun:
    def __init__(self, workflow, name, answers, env=None):
        self.dir = tempfile.mkdtemp(prefix='factory-dispatch-')
        os.mkdir(os.path.join(self.dir, 'answers'))
        os.mkdir(os.path.join(self.dir, 'bin'))
        open(os.path.join(self.dir, 'calls.log'), 'w').close()
        open(os.path.join(self.dir, 'sleeps.log'), 'w').close()
        for n, (status, text) in enumerate(answers, 1):
            with open(os.path.join(self.dir, 'answers', str(n)), 'w') as f:
                f.write('%d\n%s\n' % (status, text))
        for tool, text in (('gh', FAKE_GH), ('sleep', FAKE_SLEEP)):
            path = os.path.join(self.dir, 'bin', tool)
            with open(path, 'w') as f:
                f.write(text)
            os.chmod(path, 0o755)
        script, plain = step(workflow, name)
        run_env = {'PATH': os.path.join(self.dir, 'bin') + os.pathsep + os.environ['PATH'], 'FAKE_DIR': self.dir,
                   'GH_TOKEN': 'fake', 'REPO': 'owner/repo', 'PACE': 'active'}
        run_env.update(plain)
        run_env.update(env or {})
        self.waits = plain.get('WAITS')
        # How GitHub runs a step: "bash -e {0}" without a shell, "bash --noprofile --norc -eo pipefail {0}" with bash.
        shell = ['bash', '--noprofile', '--norc', '-eo', 'pipefail'] if 'shell: bash' in read(
            os.path.join(WORKFLOWS, workflow)) else ['bash', '-e']
        path = os.path.join(self.dir, 'step.sh')
        with open(path, 'w') as f:
            f.write(script)
        p = subprocess.run(shell + [path], env=run_env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                           universal_newlines=True, timeout=60)
        self.status, self.output = p.returncode, p.stdout
        self.calls = read(os.path.join(self.dir, 'calls.log')).splitlines()
        self.sleeps = [int(s) for s in read(os.path.join(self.dir, 'sleeps.log')).split()]
        shutil.rmtree(self.dir)


class DispatchRetryTest(unittest.TestCase):
    def run_dispatch(self, workflow, name, failures):
        answers = list(failures)
        if workflow == RESCUE[0]:  # the list of the tick's runs comes first: a stopped clock
            answers.insert(0, (0, '[{"status": "completed", "updatedAt": "%s"}]' % iso(90)))
        run = StepRun(workflow, name, answers)
        if workflow == RESCUE[0]:
            self.assertIn('run list', run.calls.pop(0))
        return run

    def test_every_copy_of_the_function_is_the_same(self):
        functions = {w: retry_function(step(w, n)[0]) for w, n, _ in DISPATCHES}
        self.assertEqual(1, len(set(functions.values())), sorted(functions))

    def test_the_waits(self):
        for workflow, name, _ in DISPATCHES:
            waits = step(workflow, name)[1]['WAITS']
            expected = '15 30 60 120 240' if workflow == 'factory-tick.yml' else '15 30 60 120 240 480'
            self.assertEqual(expected, waits, workflow)

    def test_success_at_once(self):
        for workflow, name, command in DISPATCHES:
            with self.subTest(workflow):
                run = self.run_dispatch(workflow, name, [])
                self.assertEqual(0, run.status, run.output)
                self.assertEqual(1, len(run.calls), run.calls)
                self.assertIn(command, run.calls[0])
                self.assertIn('--ref master', run.calls[0])
                self.assertEqual([], run.sleeps)

    def test_server_errors_are_retried_until_success(self):
        for workflow, name, command in DISPATCHES:
            with self.subTest(workflow):
                run = self.run_dispatch(workflow, name, [
                    (1, 'could not create workflow dispatch event: HTTP 500 (https://api.github.com/x)'),
                    (1, 'HTTP 502: Bad Gateway (https://api.github.com/x)'),
                    (1, 'error connecting to api.github.com'),
                    (1, 'HTTP 429: Too Many Requests'),
                    (1, 'HTTP 403: You have exceeded a secondary rate limit.')])
                self.assertEqual(0, run.status, run.output)
                self.assertEqual(6, len(run.calls))
                self.assertTrue(all(command in c for c in run.calls), run.calls)
                self.assertEqual([15, 30, 60, 120, 240], run.sleeps)
                self.assertIn('HTTP 500', run.output)

    def test_a_client_error_is_final(self):
        for workflow, name, _ in DISPATCHES:
            for error in ('HTTP 401: Bad credentials (https://api.github.com/x)',
                          'could not create workflow dispatch event: HTTP 422: Unexpected inputs provided',
                          'HTTP 404: Not Found', 'could not find any workflows named factory-tick.yml'):
                with self.subTest(workflow + ': ' + error):
                    run = self.run_dispatch(workflow, name, [(1, error)])
                    self.assertEqual(1, len(run.calls))
                    self.assertEqual([], run.sleeps)
                    self.assertIn('A final error: no retry.', run.output)
                    if workflow == 'factory-tick.yml':  # a warning only: the state is committed already
                        self.assertEqual(0, run.status, run.output)
                        self.assertIn('::warning::The timer could not be armed', run.output)
                    else:
                        self.assertEqual(1, run.status, run.output)
                        self.assertIn('::error::', run.output)

    def test_it_gives_up_after_the_last_wait(self):
        for workflow, name, _ in DISPATCHES:
            with self.subTest(workflow):
                waits = [int(w) for w in step(workflow, name)[1]['WAITS'].split()]
                run = self.run_dispatch(workflow, name, [(1, 'HTTP 503: Service Unavailable')] * (len(waits) + 3))
                self.assertEqual(len(waits) + 1, len(run.calls))
                self.assertEqual(waits, run.sleeps)
                self.assertEqual(0 if workflow == 'factory-tick.yml' else 1, run.status, run.output)


class RescueTest(unittest.TestCase):
    def rescue(self, runs_answer):
        return StepRun(RESCUE[0], RESCUE[1], [runs_answer])

    def runs(self, *runs):
        return 0, '[' + ', '.join('{"status": "%s", "updatedAt": "%s"}' % r for r in runs) + ']'

    def test_a_stopped_clock_starts_a_tick(self):
        run = self.rescue(self.runs(('completed', iso(41)), ('completed', iso(55))))
        self.assertEqual(0, run.status, run.output)
        self.assertEqual(2, len(run.calls), run.calls)
        self.assertIn('run list -R owner/repo -w factory-tick.yml', run.calls[0])
        self.assertEqual('workflow run factory-tick.yml -R owner/repo --ref master -f live=true', run.calls[1])
        self.assertIn('the clock stopped. Starting a tick.', run.output)

    def test_the_newest_end_counts_whatever_the_order(self):
        run = self.rescue(self.runs(('completed', iso(90)), ('completed', iso(12))))
        self.assertEqual(1, len(run.calls), run.calls)
        self.assertIn('the clock runs', run.output)

    def test_a_recent_tick_starts_nothing(self):
        run = self.rescue(self.runs(('completed', iso(38))))
        self.assertEqual(0, run.status, run.output)
        self.assertEqual(1, len(run.calls), run.calls)
        self.assertIn('the clock runs', run.output)

    def test_a_queued_or_running_tick_starts_nothing(self):
        for status in ('queued', 'in_progress', 'pending', 'waiting', 'requested'):
            with self.subTest(status):
                run = self.rescue(self.runs(('completed', iso(120)), (status, iso(100))))
                self.assertEqual(0, run.status, run.output)
                self.assertEqual(1, len(run.calls), run.calls)
                self.assertIn('A tick is queued or running.', run.output)

    def test_no_run_at_all_starts_a_tick(self):
        run = self.rescue((0, '[]'))
        self.assertEqual(2, len(run.calls), run.calls)
        self.assertIn('No tick since never', run.output)

    def test_a_failed_list_starts_nothing_and_does_not_fail(self):
        for answer in ((1, 'HTTP 500: Internal Server Error'), (0, 'not json'), (0, '[{"status": "completed"}]')):
            with self.subTest(answer):
                run = self.rescue(answer)
                self.assertEqual(0, run.status, run.output)
                self.assertEqual(1, len(run.calls), run.calls)
                self.assertIn('::warning::', run.output)


if __name__ == '__main__':
    unittest.main()
