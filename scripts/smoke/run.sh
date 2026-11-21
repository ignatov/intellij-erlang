#!/usr/bin/env bash
# Usage: scripts/smoke/run.sh [PHASE...]
# The machine half of the release smoke. CI runs it in the job release-smoke of .github/workflows/gradle.yml on every
# push to master; the same command runs on Linux and macOS. Run it from the root of the checkout.
# The phases, in their order:
#   inputs     download the pinned inputs (inputs.sh)
#   build      build the plugin archive with ./gradlew buildPlugin; only when SMOKE_ZIP is not set
#   archive    check the contents of the archive
#   inspect    for each IDE in SMOKE_IDES: check that the IDE declares every extension point the plugin uses; then for
#              each project in SMOKE_PROJECTS install the archive in a fresh sandbox of the IDE, run the headless
#              inspections on a fresh copy of the project and compare the problems with
#              scripts/smoke/expected-<project>.txt; the IDEs run at the same time
#   parser     parse the corpus with ErlangCorpusParsingTest and compare it with scripts/smoke/parser-baseline.txt
#   report     print the summary, and add it to the job summary on GitHub Actions
# Without a phase, all of them run; a failed phase does not stop the next ones, and the exit code is 1 if one failed.
# Other phases, not in the default list; each writes a committed file that the checks compare with:
#   parser-baseline   parse the corpus and write scripts/smoke/parser-baseline.txt (needs the inputs phase first):
#                     scripts/smoke/run.sh inputs parser-baseline
#   inspect-baseline  run the inspect phase and write scripts/smoke/expected-<project>.txt from its problems (needs the
#                     inputs phase and an archive first): scripts/smoke/run.sh inputs build inspect-baseline
# Environment:
#   JAVA_HOME           JDK 25; on macOS, when it is not set, /usr/libexec/java_home -v 25
#   SMOKE_WORK          the work directory, default build/smoke
#   SMOKE_ZIP           the plugin archive to check; default: build/distributions/intellij-erlang-<version>.zip of build
#   SMOKE_BUILD_NUMBER  the -PbuildNumber of the archive, default buildNumber of gradle.properties
#   SMOKE_IDES          IntelliJ IDEA versions, as -PplatformVersion takes them, default "2026.2 2026.3"
#   SMOKE_PROJECTS      default "smoke_app cowboy"
#   SMOKE_TIMEOUT       seconds for one inspect run, default 600
set -uo pipefail

here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
cd "$repo"
if [[ -z "${JAVA_HOME:-}" && -x /usr/libexec/java_home ]]; then
  JAVA_HOME=$(/usr/libexec/java_home -v 25) || exit 1
  export JAVA_HOME
fi
work=${SMOKE_WORK:-build/smoke}
mkdir -p "$work/results" "$work/logs"
work=$(cd "$work" && pwd)
property() { sed -n "s/^$1 *= *//p" gradle.properties | tr -d '[:space:]'; }
build_number=${SMOKE_BUILD_NUMBER:-$(property buildNumber)}
version="$(property version).$build_number"
zip=${SMOKE_ZIP:-$repo/build/distributions/intellij-erlang-$version.zip}
ides=${SMOKE_IDES:-2026.2 2026.3}
projects=${SMOKE_PROJECTS:-smoke_app cowboy}
smoke() { python3 "$here/smoke.py" "$@"; }

phase_inputs() {
  "$here/inputs.sh" "$work/inputs"
}

phase_build() {
  ./gradlew --no-daemon buildPlugin -PbuildNumber="$build_number" > "$work/logs/build.log" 2>&1
  local rc=$?
  tail -5 "$work/logs/build.log"
  [[ $rc == 0 && -f "$zip" ]] || { echo "::error::No plugin archive $zip, see $work/logs/build.log"; return 1; }
}

phase_archive() {
  smoke archive "$zip" --repo "$repo" --build-number "$build_number" --result "$work/results/archive.json"
}

# Resolves IntelliJ IDEA $1 through the Gradle build, as the compatibility check does, into $work/ide-$1.json.
ide_info() {
  COMPAT_IDE=$1 "$repo/scripts/compat/gradle.sh" -q compatIdeInfo > "$work/logs/ide-$1.log" 2>&1 \
    && cp build/compat/ide.json "$work/ide-$1.json"
}

project_source() {
  case $1 in
    smoke_app) echo "$repo/testData/smoke/smoke_app" ;;
    cowboy) echo "$work/inputs/cowboy" ;;
    *) echo "unknown project $1" >&2; return 1 ;;
  esac
}

phase_inspect() {
  local ide project rc=0 pids=()
  for ide in $ides; do
    if ! ide_info "$ide"; then
      cat "$work/logs/ide-$ide.log"
      for project in $projects; do
        smoke fail inspect "$project $ide" "IntelliJ IDEA $ide could not be resolved; see $work/logs/ide-$ide.log" \
          --result "$work/results/inspect-$ide-$project.json"
      done
      rc=1
      continue
    fi
    smoke extensions --ide "$work/ide-$ide.json" --zip "$zip" --result "$work/results/extensions-$ide.json" || rc=1
    (
      code=0
      for project in $projects; do
        smoke inspect --ide "$work/ide-$ide.json" --ide-version "$ide" --zip "$zip" --sdk "$work/inputs/otp" \
          --project "$project" --source "$(project_source "$project")" --expected "$here/expected-$project.txt" \
          --inputs-id "$(cat "$work/inputs/corpus-id")" --out "$work/insp/$ide-$project" \
          --timeout "${SMOKE_TIMEOUT:-600}" --result "$work/results/inspect-$ide-$project.json" || code=1
      done
      exit $code
    ) > "$work/logs/inspect-$ide.log" 2>&1 &
    pids+=($!)
  done
  for pid in "${pids[@]}"; do
    wait "$pid" || rc=1
  done
  for ide in $ides; do
    [[ ! -f "$work/logs/inspect-$ide.log" ]] || cat "$work/logs/inspect-$ide.log"
  done
  return $rc
}

parse_corpus() {
  local report="$work/corpus-report.json" code
  [[ ! -e "$report" ]] || mv "$report" "$report.old"
  smoke corpus --out "$work/corpus" --smoke-app "$repo/testData/smoke/smoke_app" --cowboy "$work/inputs/cowboy" \
    --otp "$work/inputs/otp" || return 1
  CORPUS_DIR="$work/corpus" CORPUS_REPORT="$report" "$repo/scripts/compat/gradle.sh" test \
    --tests org.intellij.erlang.parser.ErlangCorpusParsingTest --rerun > "$work/logs/parser-gradle.log" 2>&1
  code=$?
  grep -E '^Parsed [0-9]+ files|BUILD (SUCCESSFUL|FAILED)' "$work/logs/parser-gradle.log"
  smoke parser "$report" "$here/parser-baseline.txt" --corpus-id "$(cat "$work/inputs/corpus-id")" \
    --gradle-exit "$code" --result "$work/results/parser.json" "$@"
}

phase_parser() {
  parse_corpus
}

phase_parser-baseline() {
  parse_corpus --update
}

phase_inspect-baseline() {
  local project rc=0
  phase_inspect
  for project in $projects; do
    smoke expectations "$work" --project "$project" --file "$here/expected-$project.txt" \
      --inputs-id "$(cat "$work/inputs/corpus-id")" || rc=1
  done
  return $rc
}

phase_report() {
  smoke report "$work" > "$work/summary.md"
  local rc=$?
  cat "$work/summary.md"
  [[ -z "${GITHUB_STEP_SUMMARY:-}" ]] || cat "$work/summary.md" >> "$GITHUB_STEP_SUMMARY"
  return $rc
}

if (( $# == 0 )); then
  set -- inputs build archive inspect parser report
  [[ -z "${SMOKE_ZIP:-}" ]] || set -- inputs archive inspect parser report
fi
status=0
for phase in "$@"; do
  if ! declare -F "phase_$phase" > /dev/null; then
    echo "::error::Unknown phase $phase"
    exit 2
  fi
  echo "== $phase $(date '+%H:%M:%S')"
  start=$SECONDS
  "phase_$phase"
  code=$?
  echo "== $phase exit $code in $(( SECONDS - start )) s"
  (( code == 0 )) || status=1
done
exit $status
