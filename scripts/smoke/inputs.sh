#!/usr/bin/env bash
# Usage: inputs.sh DIRECTORY
# Puts the pinned inputs of the release smoke into DIRECTORY, once: Erlang/OTP (the Erlang SDK of the inspected projects
# and the stdlib and kernel sources of the parser check) in DIRECTORY/otp, and cowboy in DIRECTORY/cowboy. Writes
# DIRECTORY/corpus-id, the name of the corpus that parser-baseline.txt is for. Every download is checked: the
# Erlang/OTP archive by its sha256, cowboy by its commit. Another run reuses what is there. Works on Linux and macOS.
set -euo pipefail

# The same Erlang/OTP release as the compilation-tests job of gradle.yml. It is the Ubuntu 24.04 build of builds.hex.pm;
# the smoke does not run it, it reads its sources and its version files, so the same archive serves macOS too. The sha256
# is the one that builds.hex.pm lists in builds.txt.
OTP_VERSION=27.3.4.9
OTP_SHA256=a9864f486e575e5e1c8e507641168bc31dbb43d7cfa8e124d5e8b67a6f6248d2
OTP_URL="https://builds.hex.pm/builds/otp/amd64/ubuntu-24.04/OTP-$OTP_VERSION.tar.gz"
# cowboy, the commit of the tag.
COWBOY_TAG=2.19.0
COWBOY_COMMIT=79e3fb02b31d47af6e69e8f3ba18fba291a3072a
COWBOY_URL=https://github.com/ninenines/cowboy.git

dir=${1:?usage: inputs.sh DIRECTORY}
mkdir -p "$dir"
dir=$(cd "$dir" && pwd)

sha256() {
  if command -v sha256sum > /dev/null; then sha256sum "$1" | cut -d' ' -f1; else shasum -a 256 "$1" | cut -d' ' -f1; fi
}

# Erlang/OTP. A directory is used only with its .pinned file, which is written last.
otp="$dir/otp"
if [[ "$(cat "$otp/.pinned" 2> /dev/null)" != "$OTP_VERSION $OTP_SHA256" ]]; then
  [[ ! -e "$otp" ]] || mv "$otp" "$dir/otp.old.$(date +%s)"
  archive="$dir/otp-$OTP_VERSION.tar.gz"
  if [[ ! -f "$archive" || "$(sha256 "$archive")" != "$OTP_SHA256" ]]; then
    curl -fsSL --retry 3 -o "$archive" "$OTP_URL"
  fi
  actual=$(sha256 "$archive")
  if [[ "$actual" != "$OTP_SHA256" ]]; then
    echo "::error::$OTP_URL has sha256 $actual, expected $OTP_SHA256"
    exit 1
  fi
  tmp="$dir/otp.tmp.$$"
  mkdir "$tmp"
  tar -xzf "$archive" -C "$tmp" --strip-components=1
  mv "$tmp" "$otp"
  echo "$OTP_VERSION $OTP_SHA256" > "$otp/.pinned"
fi
found=$(cat "$otp"/releases/*/OTP_VERSION)
if [[ "$found" != "$OTP_VERSION" ]]; then
  echo "::error::$otp is Erlang/OTP $found, expected $OTP_VERSION"
  exit 1
fi

# cowboy, without the history.
cowboy="$dir/cowboy"
if [[ "$(git -C "$cowboy" rev-parse HEAD 2> /dev/null)" != "$COWBOY_COMMIT" ]] \
    || [[ -n "$(git -C "$cowboy" status --porcelain 2> /dev/null)" ]]; then
  [[ ! -e "$cowboy" ]] || mv "$cowboy" "$dir/cowboy.old.$(date +%s)"
  tmp="$dir/cowboy.tmp.$$"
  git init -q "$tmp"
  # Three tries: a host that stays down still fails the step.
  for try in 1 2 3; do
    git -C "$tmp" fetch -q --depth 1 "$COWBOY_URL" "$COWBOY_COMMIT" && break
    if (( try == 3 )); then
      echo "::error::Cannot fetch $COWBOY_URL $COWBOY_COMMIT"
      exit 1
    fi
    sleep $(( try * 10 ))
  done
  git -C "$tmp" -c advice.detachedHead=false checkout -q FETCH_HEAD
  mv "$tmp" "$cowboy"
fi
found=$(git -C "$cowboy" rev-parse HEAD)
if [[ "$found" != "$COWBOY_COMMIT" ]]; then
  echo "::error::$cowboy is at $found, expected $COWBOY_COMMIT"
  exit 1
fi

echo "cowboy $COWBOY_TAG ${COWBOY_COMMIT:0:12}, Erlang/OTP $OTP_VERSION" > "$dir/corpus-id"
echo "Inputs in $dir: $(cat "$dir/corpus-id")"
