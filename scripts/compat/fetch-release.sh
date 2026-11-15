#!/usr/bin/env bash
# Usage: fetch-release.sh DIRECTORY GRADLE_PROPERTIES
# Downloads the plugin archive of the published plugin into DIRECTORY and describes it in $COMPAT_LOGS/released.json.
# The published plugin is the release of $GITHUB_REPOSITORY tagged v<version>.<N>, with <version> from
# GRADLE_PROPERTIES and the largest N (compat.py release-tag), not GitHub's "latest" release. If there is none or the
# download fails, the description says why and the step output `archive` stays empty, so that the check releasedPlugin
# does not run and the job is red. Needs GH_TOKEN; always succeeds.
set -uo pipefail
dir=$1
properties=$2
here=$(cd "$(dirname "$0")" && pwd)
logs=${COMPAT_LOGS:?COMPAT_LOGS is not set}
mkdir -p "$dir" "$logs"
meta="$logs/released.json"
fail() {
  python3 -c 'import json, sys; print(json.dumps({"error": sys.argv[1]}))' "$1" > "$meta"
  echo "::warning::Released plugin not checked: $1"
  exit 0
}
releases=$(gh release list --repo "$GITHUB_REPOSITORY" --exclude-drafts --limit 1000 --json tagName 2>&1) \
  || fail "cannot list the releases: $releases"
tag=$(printf '%s' "$releases" | python3 "$here/compat.py" release-tag --gradle-properties "$properties" 2>&1) \
  || fail "no published plugin: ${tag#::error::}"
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
