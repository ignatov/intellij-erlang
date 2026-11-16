#!/usr/bin/env python3
"""Writes the pinned ELP release manifest of the plugin, resources/elp/elp-release.json, for one ELP release tag.

The plugin downloads ELP only from this manifest: the tag, the ELP version and, per tarball, the OS, the CPU, the OTP release the
binary was built on, the size and the sha256 that GitHub reports as the asset's digest. Nothing is looked up at run time.

    python3 scripts/elp/elp_release_manifest.py --tag 2026-10-14 --version 1.1.0 --output resources/elp/elp-release.json

--version is what `elp version` prints for that release (the GitHub API does not know it). --releases-json reads a saved API
answer (one release or a list of releases) instead of asking api.github.com. The script fails when an asset has no sha256 digest,
when a tarball name does not parse, or when two tarballs are for the same OS, CPU and OTP release.
"""

import argparse
import json
import re
import sys
import urllib.request

REPOSITORY = "WhatsApp/erlang-language-platform"
LICENSE = "MIT OR Apache-2.0"

# elp-macos-aarch64-apple-darwin-otp-28.5.tar.gz
TARBALL = re.compile(r"^elp-(?P<os>linux|macos|windows)-(?P<target>(?P<arch>x86_64|aarch64)-[a-z0-9_]+(?:-[a-z0-9_]+)*)"
                     r"-otp-(?P<otp>\d+(?:\.\d+)*)\.tar\.gz$")
DIGEST = re.compile(r"^sha256:(?P<hex>[0-9a-f]{64})$")
TARGET_SUFFIX = {"linux": "-linux-gnu", "macos": "-apple-darwin", "windows": "-pc-windows-msvc"}
IGNORED_SUFFIXES = (".vsix",)


class ManifestError(Exception):
    pass


def fetch_release(tag):
    url = "https://api.github.com/repos/%s/releases/tags/%s" % (REPOSITORY, tag)
    request = urllib.request.Request(url, headers={"Accept": "application/vnd.github+json", "User-Agent": "elp-manifest-script"})
    with urllib.request.urlopen(request, timeout=60) as response:
        return json.load(response)


def select_release(data, tag):
    releases = data if isinstance(data, list) else [data]
    for release in releases:
        if release.get("tag_name") == tag:
            return release
    raise ManifestError("no release with tag %s" % tag)


def otp_key(otp):
    return tuple(int(part) for part in otp.split("."))


def build_manifest(release, version):
    tag = release["tag_name"]
    if not version or not version.strip():
        raise ManifestError("the ELP version is empty")
    assets = []
    seen = {}
    for asset in release.get("assets", []):
        name = asset.get("name", "")
        if name.endswith(IGNORED_SUFFIXES):
            continue
        match = TARBALL.match(name)
        if not match:
            raise ManifestError("asset name does not parse: %s" % name)
        os_name, arch, target, otp = match.group("os"), match.group("arch"), match.group("target"), match.group("otp")
        if not target.endswith(TARGET_SUFFIX[os_name]):
            raise ManifestError("unexpected target %s for %s in %s" % (target, os_name, name))
        if asset.get("state", "uploaded") != "uploaded":
            raise ManifestError("asset %s is not uploaded: %s" % (name, asset.get("state")))
        digest = DIGEST.match(asset.get("digest") or "")
        if not digest:
            raise ManifestError("asset %s has no sha256 digest: %r" % (name, asset.get("digest")))
        size = asset.get("size")
        if not isinstance(size, int) or size <= 0:
            raise ManifestError("asset %s has no size: %r" % (name, size))
        key = (os_name, arch, otp_key(otp)[0])
        if key in seen:
            raise ManifestError("two tarballs for %s %s OTP %d: %s and %s" % (key + (seen[key], name)))
        seen[key] = name
        assets.append({
            "file": name,
            "os": os_name,
            "arch": arch,
            "target": target,
            "otp": otp,
            "size": size,
            "sha256": digest.group("hex"),
            "executable": "elp.exe" if os_name == "windows" else "elp",
        })
    if not assets:
        raise ManifestError("release %s has no ELP tarballs" % tag)
    assets.sort(key=lambda a: (a["os"], a["arch"], otp_key(a["otp"])))
    return {
        "tag": tag,
        "version": version.strip(),
        "repository": "https://github.com/%s" % REPOSITORY,
        "downloadUrl": "https://github.com/%s/releases/download/%s/" % (REPOSITORY, tag),
        "license": LICENSE,
        "assets": assets,
    }


def render(manifest):
    return json.dumps(manifest, indent=2) + "\n"


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--tag", required=True, help="the ELP release tag, for example 2026-10-14")
    parser.add_argument("--version", required=True, help="what `elp version` prints for that release, for example 1.1.0")
    parser.add_argument("--releases-json", help="a saved GitHub API answer to read instead of api.github.com")
    parser.add_argument("--output", help="the manifest to write; standard output when omitted")
    args = parser.parse_args(argv)
    try:
        if args.releases_json:
            with open(args.releases_json, encoding="utf-8") as f:
                release = select_release(json.load(f), args.tag)
        else:
            release = select_release(fetch_release(args.tag), args.tag)
        text = render(build_manifest(release, args.version))
    except ManifestError as e:
        print("error: %s" % e, file=sys.stderr)
        return 1
    if args.output:
        with open(args.output, "w", encoding="utf-8", newline="\n") as f:
            f.write(text)
    else:
        sys.stdout.write(text)
    return 0


if __name__ == "__main__":
    sys.exit(main())
