#!/usr/bin/env bash
# Usage: gradle.sh TASKS-AND-ARGS...
# Runs ./gradlew of the current directory (the checked-out plugin) as the compatibility check does: without a daemon,
# with plain console output, with compat.init.gradle, and against the IDE in $COMPAT_IDE (-PplatformVersion) when set.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
args=(--no-daemon --console=plain --init-script "$here/compat.init.gradle")
if [[ -n "${COMPAT_IDE:-}" ]]; then
  args+=("-PplatformVersion=$COMPAT_IDE")
fi
exec ./gradlew "${args[@]}" "$@"
