#!/usr/bin/env bash
# Usage: install-otp.sh VERSION
# Installs Erlang/OTP VERSION (as builds.hex.pm names it without OTP-, e.g. 27.3.4.9 or 28.0-rc1) into
# /usr/local/lib/erlang and links erl, erlc and escript into /usr/local/bin. A copy of the "Install Erlang/OTP" step of
# the compilation-tests job in gradle.yml, with the version as an argument: the archive's sha256 comes from builds.txt.
set -euo pipefail
OTP_VERSION=$1
if [[ ! "$OTP_VERSION" =~ ^[0-9]+(\.[0-9]+)*(-rc[0-9]+)?$ ]]; then
  echo "::error::'$OTP_VERSION' is not an Erlang/OTP version like 27.3.4.9, 29.1 or 30.0-rc1"
  exit 1
fi
major=${OTP_VERSION%%[.-]*}
. /etc/os-release
base="https://builds.hex.pm/builds/otp/$(dpkg --print-architecture)/ubuntu-$VERSION_ID"
sha256=$(curl -fsSL --retry 3 "$base/builds.txt" | awk -v build="OTP-$OTP_VERSION" '$1 == build { print $4 }')
if [ -z "$sha256" ]; then
  echo "::error::$base/builds.txt has no OTP-$OTP_VERSION"
  exit 1
fi
archive="${RUNNER_TEMP:-/tmp}/otp.tar.gz"
curl -fsSL --retry 3 -o "$archive" "$base/OTP-$OTP_VERSION.tar.gz"
echo "$sha256  $archive" | sha256sum -c -
root=/usr/local/lib/erlang
sudo mkdir -p "$root"
sudo tar -xzf "$archive" -C "$root" --strip-components=1
sudo "$root/Install" -minimal "$root"
for tool in erl erlc escript; do
  sudo ln -sf "$root/bin/$tool" "/usr/local/bin/$tool"
done
command -v erl erlc escript
release=$(erl -noshell -eval 'io:put_chars(erlang:system_info(otp_release)), halt().')
echo "Erlang/OTP $release at $(erl -noshell -eval 'io:put_chars(code:root_dir()), halt().')"
if [ "$release" != "$major" ]; then
  echo "::error::erl on PATH is Erlang/OTP $release, expected $major"
  exit 1
fi
