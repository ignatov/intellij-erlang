#!/usr/bin/env bash
# Usage: run-check.sh NAME COMMAND [ARGS...]
# Runs one check of the compatibility check (.github/workflows/compat.yml). It writes $COMPAT_LOGS/NAME.started, copies
# the output to $COMPAT_LOGS/NAME.log, writes the exit code to $COMPAT_LOGS/NAME.exit and to the step output `exit`, and
# always succeeds: compat.py turns the outcome into the result, so a check that finds problems does not end the job
# early. A check with NAME.started and no NAME.exit did not finish: its step timed out or was killed.
set -uo pipefail
name=$1
shift
logs=${COMPAT_LOGS:?COMPAT_LOGS is not set}
mkdir -p "$logs"
: > "$logs/$name.started"
"$@" 2>&1 | tee "$logs/$name.log"
code=${PIPESTATUS[0]}
echo "$code" > "$logs/$name.exit"
if [[ -n "${GITHUB_OUTPUT:-}" ]]; then
  echo "exit=$code" >> "$GITHUB_OUTPUT"
fi
echo "Check $name exited with $code"
exit 0
