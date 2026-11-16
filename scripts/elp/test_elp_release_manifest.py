import unittest

import elp_release_manifest as m

SHA = "0123456789abcdef" * 4


def asset(name, digest="sha256:" + SHA, size=100, **extra):
    return dict(name=name, digest=digest, size=size, **extra)


def release(*assets):
    return {"tag_name": "2026-10-14", "assets": list(assets)}


class BuildManifestTest(unittest.TestCase):
    def test_tarballs_are_parsed_and_vsix_ignored(self):
        manifest = m.build_manifest(release(
            asset("elp-windows-x86_64-pc-windows-msvc-otp-28.5.tar.gz"),
            asset("elp-macos-aarch64-apple-darwin-otp-29.1.tar.gz"),
            asset("elp-macos-aarch64-apple-darwin-otp-27.3.tar.gz"),
            asset("elp-macos-aarch64-apple-darwin-otp-27.3.vsix", digest=None),
        ), "1.1.0")
        self.assertEqual("2026-10-14", manifest["tag"])
        self.assertEqual("1.1.0", manifest["version"])
        self.assertEqual("https://github.com/WhatsApp/erlang-language-platform/releases/download/2026-10-14/",
                         manifest["downloadUrl"])
        self.assertEqual(["elp-macos-aarch64-apple-darwin-otp-27.3.tar.gz", "elp-macos-aarch64-apple-darwin-otp-29.1.tar.gz",
                          "elp-windows-x86_64-pc-windows-msvc-otp-28.5.tar.gz"], [a["file"] for a in manifest["assets"]])
        windows = manifest["assets"][2]
        self.assertEqual(("windows", "x86_64", "x86_64-pc-windows-msvc", "28.5", "elp.exe", SHA, 100),
                         (windows["os"], windows["arch"], windows["target"], windows["otp"], windows["executable"],
                          windows["sha256"], windows["size"]))
        self.assertEqual("elp", manifest["assets"][0]["executable"])

    def test_otp_without_minor_parses(self):
        manifest = m.build_manifest(release(asset("elp-linux-x86_64-unknown-linux-gnu-otp-29.tar.gz")), "1.0.0")
        self.assertEqual("29", manifest["assets"][0]["otp"])

    def test_missing_digest_fails(self):
        for digest in (None, "", "sha1:abc", "sha256:XYZ"):
            with self.assertRaises(m.ManifestError):
                m.build_manifest(release(asset("elp-linux-x86_64-unknown-linux-gnu-otp-29.1.tar.gz", digest=digest)), "1")

    def test_unparsable_name_fails(self):
        for name in ("elp-linux-riscv64-unknown-linux-gnu-otp-29.1.tar.gz", "elp-freebsd-x86_64-unknown-freebsd-otp-29.1.tar.gz",
                     "elp-linux-x86_64-unknown-linux-gnu-otp-29.1.zip", "elp-linux-x86_64-apple-darwin-otp-29.1.tar.gz",
                     "checksums.txt"):
            with self.assertRaises(m.ManifestError, msg=name):
                m.build_manifest(release(asset(name)), "1")

    def test_two_tarballs_for_one_otp_major_fail(self):
        with self.assertRaises(m.ManifestError):
            m.build_manifest(release(asset("elp-linux-x86_64-unknown-linux-gnu-otp-29.0.tar.gz"),
                                     asset("elp-linux-x86_64-unknown-linux-gnu-otp-29.1.tar.gz")), "1")

    def test_bad_size_or_state_or_empty_release_fails(self):
        name = "elp-linux-x86_64-unknown-linux-gnu-otp-29.1.tar.gz"
        for bad in (asset(name, size=0), asset(name, size=None), asset(name, state="starter")):
            with self.assertRaises(m.ManifestError):
                m.build_manifest(release(bad), "1")
        with self.assertRaises(m.ManifestError):
            m.build_manifest(release(asset("elp-x.vsix")), "1")
        with self.assertRaises(m.ManifestError):
            m.build_manifest(release(asset(name)), " ")

    def test_select_release_from_list(self):
        self.assertEqual("b", m.select_release([{"tag_name": "a"}, {"tag_name": "b"}], "b")["tag_name"])
        with self.assertRaises(m.ManifestError):
            m.select_release({"tag_name": "a"}, "b")


if __name__ == "__main__":
    unittest.main()
