#!/usr/bin/env bash
# Usage: install-rebar3.sh VERSION SHA256
# Downloads the rebar3 escript VERSION, checks it against SHA256 and writes its path to REBAR3_PATH in $GITHUB_ENV.
# A copy of the "Install rebar3" step of the compilation-tests job in gradle.yml, whose pins compat.py reads.
# A failed download or checksum fails the step. Whether rebar3 runs on the installed Erlang/OTP is the check `rebar3`
# of run-check.sh: if it does not, compat.py makes that a finding of compilationTests.
set -euo pipefail
REBAR3_VERSION=$1
REBAR3_SHA256=$2
here=$(cd "$(dirname "$0")" && pwd)
rebar3="${RUNNER_TEMP:-/tmp}/rebar3/rebar3"
mkdir -p "$(dirname "$rebar3")"
curl -fsSL --retry 3 -o "$rebar3" "https://github.com/erlang/rebar3/releases/download/$REBAR3_VERSION/rebar3"
echo "$REBAR3_SHA256  $rebar3" | sha256sum -c -
chmod +x "$rebar3"
"$here/run-check.sh" rebar3 escript "$rebar3" version
if [[ -n "${GITHUB_ENV:-}" ]]; then
  echo "REBAR3_PATH=$rebar3" >> "$GITHUB_ENV"
fi
