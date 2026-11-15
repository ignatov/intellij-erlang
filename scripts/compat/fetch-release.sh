#!/usr/bin/env bash
# Usage: fetch-release.sh DIRECTORY
# Downloads the plugin archive of the latest GitHub release of $GITHUB_REPOSITORY (the build published to JetBrains
# Marketplace) into DIRECTORY and describes it in $COMPAT_LOGS/released.json. If there is none or the download fails,
# the description says why and the step output `archive` stays empty. Needs GH_TOKEN; always succeeds.
set -uo pipefail
dir=$1
logs=${COMPAT_LOGS:?COMPAT_LOGS is not set}
mkdir -p "$dir" "$logs"
meta="$logs/released.json"
fail() {
  python3 -c 'import json, sys; print(json.dumps({"error": sys.argv[1]}))' "$1" > "$meta"
  echo "::warning::Released plugin not checked: $1"
  exit 0
}
tag=$(gh release view --repo "$GITHUB_REPOSITORY" --json tagName --jq .tagName 2>&1) || fail "no latest release: $tag"
gh release download "$tag" --repo "$GITHUB_REPOSITORY" --pattern 'intellij-erlang-*.zip' --dir "$dir" --clobber 2>&1 \
  || fail "cannot download the plugin archive of release $tag"
shopt -s nullglob
zips=("$dir"/intellij-erlang-*.zip)
(( ${#zips[@]} == 1 )) || fail "release $tag has ${#zips[@]} plugin archives, expected 1"
python3 -c 'import json, os, sys; print(json.dumps({"tag": sys.argv[1], "asset": os.path.basename(sys.argv[2])}))' \
  "$tag" "${zips[0]}" > "$meta"
echo "Release $tag: ${zips[0]}"
if [[ -n "${GITHUB_OUTPUT:-}" ]]; then
  echo "archive=${zips[0]}" >> "$GITHUB_OUTPUT"
fi
