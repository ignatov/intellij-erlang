#!/usr/bin/env bash
# Which changed lines of src/ the test classes of a pull request execute.
#
#   scripts/coverage/pr-coverage.sh [--clean] [<base>]
#
# <base> is the commit to compare with: by default the merge base of origin/master and HEAD. The script runs only the
# test classes that the pull request adds or changes, with `test -Pcoverage`, and prints the result
# (scripts/coverage/prcoverage.py). Its files are in build/coverage-pr/: plan.json, result.json, result.txt, gradle.log.
# It reports and never fails: the exit status is 0 also when tests fail or nothing could be measured.
# --clean runs `clean` first; use it after "Duplicate method name "$$$getFont$$$"" (#1402).
set -u
cd "$(dirname "$0")/../.."

clean=""
if [[ "${1:-}" == "--clean" ]]; then
  clean="clean"
  shift
fi
base="${1:-}"
if [[ -z "$base" ]]; then
  base="$(git merge-base origin/master HEAD 2>/dev/null)" || base=""
fi
out=build/coverage-pr
if [[ -n "$clean" ]]; then
  # Before the files of this run are written: clean deletes build/.
  ./gradlew --no-daemon clean > /dev/null 2>&1 || true
fi
mkdir -p "$out"
# The files of an earlier run must not be read as the result of this one.
rm -f "$out/result.txt" "$out/result.json" "$out/plan.json" "$out/classes.txt" "$out/changes.diff" "$out/gradle.log"
if [[ -z "$base" ]] || ! git rev-parse --verify --quiet "$base^{commit}" > /dev/null; then
  text="changed lines: not measured, because the base commit '${1:-merge base of origin/master}' is not known."
  echo "$text" | tee "$out/result.txt"
  python3 -c 'import json, sys; json.dump({"text": sys.argv[1]}, open(sys.argv[2], "w", encoding="utf-8"))' \
    "$text" "$out/result.json"
  exit 0
fi
git diff --find-renames -U0 --no-color "$base" HEAD > "$out/changes.diff"

python3 scripts/coverage/prcoverage.py plan --diff "$out/changes.diff" --json "$out/plan.json" > "$out/classes.txt" || true
report=build/reports/jacoco/coverageReport/coverageReport.xml
status=""
results=""
if grep -q . "$out/classes.txt"; then
  # A report left from an earlier run must not be read as the result of this one.
  [[ -f "$report" ]] && mv -f "$report" "$report.old"
  filters=()
  while IFS= read -r class; do
    [[ -n "$class" ]] || continue
    filters+=(--tests "$class")
    # A result of an earlier run must not be read as the result of this one.
    rm -f "build/test-results/test/TEST-$class.xml"
  done < "$out/classes.txt"
  echo "Running with coverage: $(tr '\n' ' ' < "$out/classes.txt")"
  ./gradlew --no-daemon --no-build-cache test -Pcoverage "${filters[@]}" > "$out/gradle.log" 2>&1
  status=$?
  # What ran and what failed comes from these files, not from the plan.
  results=build/test-results/test
else
  report=""
fi

python3 scripts/coverage/prcoverage.py report --diff "$out/changes.diff" --plan "$out/plan.json" \
  ${report:+--report "$report"} ${status:+--gradle-status "$status"} ${results:+--test-results "$results"} \
  --json "$out/result.json" | tee "$out/result.txt"
exit 0
