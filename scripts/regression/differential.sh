#!/usr/bin/env bash
# Usage: scripts/regression/differential.sh BASE_REF CANDIDATE_REF CORPUS OUT
# The regression check: builds the plugin at BASE_REF and at CANDIDATE_REF (git refs of this repository, for example the
# last release tag and master), runs RegressionDiffTest over every .erl and .hrl file under CORPUS with each build, and
# writes OUT/base.jsonl, OUT/candidate.jsonl, the report OUT/report.json and its summary OUT/summary.md (diffreport.py,
# with the allowlist known.json). Headless: the platform test framework only. Both sides run on the same IDE.
# The probe is the RegressionDiffTest of this checkout; it is copied into the worktree of each side, so a ref that does
# not have it (an older release) is checked with the same probe. Each side builds in its own worktree under OUT/work.
# Environment:
#   FORKS           test JVMs per side, each takes 1/FORKS of the corpus (default 2); each gets its own IDE system dir
#   HEAP            heap per test JVM (default 3g)
#   PLATFORM        -PplatformVersion of both sides (default 2026.2)
#   PHASES          default lex,parse,format,highlight
#   SIDES           "base candidate" (default), or one of them
#   PARALLEL        1 (default) runs both sides at the same time, so their timings are comparable; 0 one after the other
#   BUDGET_MINUTES  no file is started later than this many minutes after the start; 0 (default) is no limit. The
#                   report then compares the files that both sides did and says it is incomplete
#   KNOWN           the allowlist, default known.json next to this script
#   KEEP            1 keeps the worktrees in OUT/work
# Exit code: 0 when both sides wrote records and the report was written, whatever it found; 1 otherwise.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
base_ref=${1:?BASE_REF}; cand_ref=${2:?CANDIDATE_REF}
corpus=$(cd "${3:?CORPUS}" && pwd) || exit 1
out=${4:?OUT}
mkdir -p "$out/work" "$out/logs"; out=$(cd "$out" && pwd)
forks=${FORKS:-2}; heap=${HEAP:-3g}; platform=${PLATFORM:-2026.2}; phases=${PHASES:-lex,parse,format,highlight}
sides=${SIDES:-base candidate}; parallel=${PARALLEL:-1}; known=${KNOWN:-$here/known.json}
probe="$repo/tests/org/intellij/erlang/regression/RegressionDiffTest.java"
[[ -f "$probe" ]] || { echo "no probe $probe"; exit 1; }
now_ms() { python3 -c 'import time; print(int(time.time() * 1000))'; }
deadline=0
if (( ${BUDGET_MINUTES:-0} > 0 )); then deadline=$(( $(now_ms) + BUDGET_MINUTES * 60000 )); fi

init="$out/work/init.gradle"
cat > "$init" <<EOF
allprojects {
  tasks.withType(Test).configureEach {
    maxHeapSize = '$heap'
    maxParallelForks = $forks
    System.properties.each { k, v -> if (k.toString().startsWith('diff.')) systemProperty k.toString(), v.toString() }
    outputs.upToDateWhen { false }
  }
}
EOF

prepare() { # side ref
  local wt="$out/work/$1" pkg
  if [[ ! -d "$wt" ]]; then
    git -C "$repo" worktree add --detach "$wt" "$2" > "$out/logs/$1-worktree.log" 2>&1 || return 1
  fi
  pkg="$wt/tests/org/intellij/erlang/regression"
  mkdir -p "$pkg"
  cp "$probe" "$pkg/"
  for ((i = 0; i < forks; i++)); do
    cat > "$pkg/RegressionDiffShard${i}Test.java" <<EOF
package org.intellij.erlang.regression;

public class RegressionDiffShard${i}Test extends RegressionDiffTest {
  @Override
  protected String shardSpec() {
    return "$i/$forks";
  }
}
EOF
  done
}

run_side() { # side ref
  local wt="$out/work/$1" task=":test" rc n
  prepare "$1" "$2" || { echo "$1: no worktree for $2, see $out/logs/$1-worktree.log"; return 1; }
  # A ref from before the regression check has no regressionDiff task, and its test task does not exclude the probe.
  grep -q "register('regressionDiff')" "$wt/build.gradle" && task=":regressionDiff"
  for ((i = 0; i < forks; i++)); do : > "$out/$1.jsonl.$i-$forks"; done  # no records of an earlier run
  (cd "$wt" && ./gradlew --no-daemon --no-build-cache -I "$init" -PplatformVersion="$platform" "$task" \
     --tests 'org.intellij.erlang.regression.RegressionDiffShard*' \
     -Ddiff.corpus="$corpus" -Ddiff.out="$out/$1.jsonl" -Ddiff.phases="$phases" -Ddiff.forkDirs=true \
     -Ddiff.deadline="$deadline") > "$out/logs/$1.log" 2>&1
  rc=$?
  for ((i = 0; i < forks; i++)); do cat "$out/$1.jsonl.$i-$forks" 2> /dev/null; done > "$out/$1.jsonl"
  n=$(grep -c '"file"' "$out/$1.jsonl" 2> /dev/null); n=${n:-0}
  echo "$1 ($2, task $task): gradle exit $rc, $n files, log $out/logs/$1.log"
  [[ $n -gt 0 ]]
}

rc=0; pids=()
for side in $sides; do
  ref=$base_ref; [[ $side == candidate ]] && ref=$cand_ref
  if [[ $parallel == 1 ]]; then
    run_side "$side" "$ref" & pids+=($!)
  else
    run_side "$side" "$ref" || rc=1
  fi
done
for p in ${pids[@]+"${pids[@]}"}; do wait "$p" || rc=1; done

if [[ -s "$out/base.jsonl" && -s "$out/candidate.jsonl" ]]; then
  python3 "$here/diffreport.py" "$out/base.jsonl" "$out/candidate.jsonl" "$out/report.json" --known "$known" \
    --summary "$out/summary.md" --shards "$forks" \
    --base-ref "$base_ref $(git -C "$repo" rev-parse --short "$base_ref^{commit}")" \
    --cand-ref "$cand_ref $(git -C "$repo" rev-parse --short "$cand_ref^{commit}")" || rc=1
elif [[ "$sides" == "base candidate" ]]; then
  rc=1
fi
if [[ "${KEEP:-0}" != 1 ]]; then
  for side in $sides; do
    if [[ -d "$out/work/$side" ]]; then git -C "$repo" worktree remove --force "$out/work/$side"; fi
  done
fi
exit $rc
