/*
 * Copyright 2012-2026 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.elp.distribution;

import junit.framework.TestCase;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Stream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The pinned manifest {@code resources/elp/elp-release.json}: the release all ELP tests and recorded answers use, with one
 * well-formed tarball per OS, CPU and OTP build.
 */
public class ElpReleaseTest extends TestCase {
  private static final Pattern NAME =
    Pattern.compile("elp-(linux|macos|windows)-((x86_64|aarch64)-[a-z0-9_-]+)-otp-(\\d+(?:\\.\\d+)*)\\.tar\\.gz");

  public void testPinnedRelease() {
    ElpRelease release = ElpRelease.pinned();
    assertEquals("2026-10-14", release.tag());
    assertEquals("1.1.0", release.version());
    assertEquals("MIT OR Apache-2.0", release.license());
    assertEquals("https://github.com/WhatsApp/erlang-language-platform", release.repository());
    assertEquals("https://github.com/WhatsApp/erlang-language-platform/releases/download/2026-10-14/", release.downloadUrl());
    assertEquals(15, release.assets().size());
  }

  public void testEveryAssetIsWellFormed() {
    ElpRelease release = ElpRelease.pinned();
    for (ElpRelease.Asset asset : release.assets()) {
      String file = asset.file();
      assertTrue(file, asset.sha256().matches("[0-9a-f]{64}"));
      assertTrue(file, asset.size() > 0);
      Matcher matcher = NAME.matcher(file);
      assertTrue(file, matcher.matches());
      assertEquals(file, matcher.group(1), asset.os());
      assertEquals(file, matcher.group(2), asset.target());
      assertEquals(file, matcher.group(3), asset.arch());
      assertEquals(file, matcher.group(4), asset.otp());
      assertEquals(file, ElpRelease.WINDOWS.equals(asset.os()) ? "elp.exe" : "elp", asset.executable());
      assertEquals(file, file.substring(0, file.length() - ".tar.gz".length()), asset.directoryName());
      assertEquals(release.downloadUrl() + file, release.url(asset));
    }
  }

  public void testExactlyOneAssetPerOsCpuAndOtp() {
    Set<String> keys = new HashSet<>();
    for (ElpRelease.Asset asset : ElpRelease.pinned().assets()) {
      assertTrue(asset.file(), keys.add(asset.os() + "/" + asset.arch() + "/" + asset.otpMajor()));
    }
    Set<String> expected = new HashSet<>();
    for (String platform : List.of("linux/x86_64", "linux/aarch64", "macos/x86_64", "macos/aarch64", "windows/x86_64")) {
      for (int otp : new int[]{27, 28, 29}) expected.add(platform + "/" + otp);
    }
    assertEquals(expected, keys);
  }

  public void testSizeInMegabytes() {
    assertEquals("17.8 MB", asset("elp-macos-aarch64-apple-darwin-otp-28.5.tar.gz").sizeInMegabytes());
  }

  public void testBrokenManifestIsRefused() {
    String good = """
      {"tag": "t", "version": "1", "repository": "r", "downloadUrl": "u/", "license": "l", "assets": [
        {"file": "elp-linux-x86_64-unknown-linux-gnu-otp-29.1.tar.gz", "os": "linux", "arch": "x86_64",
         "target": "x86_64-unknown-linux-gnu", "otp": "29.1", "size": 10, "sha256": "%s", "executable": "elp"}]}""";
    String sha = "ab".repeat(32);
    assertEquals(1, parse(good.formatted(sha)).assets().size());
    for (String broken : List.of(good.formatted("AB".repeat(32)), good.formatted("abc"), good.formatted(sha).replace("\"size\": 10", "\"size\": 0"),
                                 good.formatted(sha).replace("\"otp\": \"29.1\"", "\"otp\": \"x\""),
                                 good.formatted(sha).replace("\"tag\": \"t\", ", ""),
                                 good.formatted(sha).replace(".tar.gz", ".zip"), "[]", "{\"assets\": []}")) {
      try {
        ElpRelease.parse(new ByteArrayInputStream(broken.getBytes(StandardCharsets.UTF_8)));
        fail("accepted: " + broken);
      }
      catch (IOException expected) {
        // refused
      }
    }
  }

  public void testPinnedManifestIsRead() {
    assertNull(ElpRelease.pinned().loadError());
  }

  /** A manifest that cannot be read gives a release without builds that says why; nothing is thrown. */
  public void testUnreadableManifestIsNotFatal() {
    ElpRelease missing = ElpRelease.read(() -> null);
    assertTrue(missing.assets().isEmpty());
    assertEquals("the plugin has no /elp/elp-release.json", missing.loadError());

    ElpRelease broken = ElpRelease.read(() -> new ByteArrayInputStream("{".getBytes(StandardCharsets.UTF_8)));
    assertTrue(broken.assets().isEmpty());
    assertTrue(broken.loadError(), broken.loadError().startsWith("IOException: The ELP manifest is not valid JSON"));

    ElpRelease noLibrary = ElpRelease.read(() -> {
      throw new NoClassDefFoundError("com/google/gson/JsonParser");
    });
    assertEquals("NoClassDefFoundError: com/google/gson/JsonParser", noLibrary.loadError());
    ElpAssetChoice choice = ElpAssetChoice.choose(noLibrary, new ElpPlatform("macos", "aarch64"), true, 28);
    assertNull(choice.asset());
    assertEquals(ElpAssetChoice.Reason.NO_RELEASE, choice.reason());
    assertEquals("No ELP build available: the plugin's ELP manifest cannot be read (NoClassDefFoundError: com/google/gson/JsonParser).",
                 choice.message());
  }

  /**
   * 2026.3 does not let plugins see Jackson 2, and the Plugin Verifier does not report a reference to it (it counts the IDE modules
   * that depend on it), so the compiled ELP classes are checked here.
   */
  public void testElpClassesDoNotReferenceJackson() throws Exception {
    URI uri = Objects.requireNonNull(ElpRelease.class.getResource("ElpRelease.class")).toURI();
    try (FileSystem jar = "jar".equals(uri.getScheme()) ? FileSystems.newFileSystem(uri, Map.of()) : null) {
      Path elp = Path.of(uri).getParent().getParent();
      assertTrue(elp.toString(), elp.endsWith("elp"));
      List<String> offenders;
      try (Stream<Path> files = Files.walk(elp)) {
        offenders = files.filter(f -> f.toString().endsWith(".class")).filter(f -> {
          try {
            return new String(Files.readAllBytes(f), StandardCharsets.ISO_8859_1).contains("com/fasterxml/jackson/");
          }
          catch (IOException e) {
            throw new AssertionError(e);
          }
        }).map(f -> elp.relativize(f).toString()).toList();
      }
      assertEquals(List.of(), offenders);
      assertTrue("the ELP classes are in " + elp, countClasses(elp) > 10);
    }
  }

  private static long countClasses(Path dir) throws IOException {
    try (Stream<Path> files = Files.walk(dir)) {
      return files.filter(f -> f.toString().endsWith(".class")).count();
    }
  }

  private static ElpRelease parse(String text) {
    try {
      return ElpRelease.parse(new ByteArrayInputStream(text.getBytes(StandardCharsets.UTF_8)));
    }
    catch (IOException e) {
      throw new AssertionError(e);
    }
  }

  static ElpRelease.Asset asset(String file) {
    return ElpRelease.pinned().assets().stream().filter(a -> a.file().equals(file)).findFirst().orElseThrow();
  }
}
