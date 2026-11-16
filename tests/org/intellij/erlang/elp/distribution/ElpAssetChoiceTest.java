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
import org.intellij.erlang.sdk.ErlangSdkRelease;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * The tarball of the pinned release for an OS, a CPU and the OTP release of the project's SDK: the newest build that is not newer
 * than the SDK, or the reason there is none.
 */
public class ElpAssetChoiceTest extends TestCase {
  private static final List<Integer> OTPS = Arrays.asList(25, 26, 27, 28, 29, 30, null);

  public void testChoiceTable() {
    List<ElpPlatform> platforms = List.of(new ElpPlatform("macos", "aarch64"), new ElpPlatform("macos", "x86_64"),
                                          new ElpPlatform("linux", "x86_64"), new ElpPlatform("linux", "aarch64"),
                                          new ElpPlatform("windows", "x86_64"), new ElpPlatform("windows", "aarch64"),
                                          new ElpPlatform("FreeBSD", "x86_64"),
                                          new ElpPlatform("linux", "x86_64", "/lib64/ld-linux-x86-64.so.2"));
    StringBuilder table = new StringBuilder();
    for (ElpPlatform platform : platforms) {
      table.append(platform.presentableName()).append(platform.missingLoader() != null ? " (no glibc)" : "").append(":");
      table.append(OTPS.stream().map(otp -> {
        ElpAssetChoice choice = ElpAssetChoice.choose(ElpRelease.pinned(), platform, otp != null, otp);
        String cell = choice.asset() != null ? choice.asset().otp() : String.valueOf(choice.reason());
        return " " + (otp != null ? otp : "none") + "=" + cell;
      }).collect(Collectors.joining()));
      table.append("\n");
    }
    assertEquals("""
                   macOS arm64: 25=OTP_TOO_OLD 26=OTP_TOO_OLD 27=27.3 28=28.5 29=29.1 30=29.1 none=NO_SDK
                   macOS x86_64: 25=OTP_TOO_OLD 26=OTP_TOO_OLD 27=27.3 28=28.5 29=29.1 30=29.1 none=NO_SDK
                   Linux x86_64: 25=OTP_TOO_OLD 26=OTP_TOO_OLD 27=27.3 28=28.5 29=29.1 30=29.1 none=NO_SDK
                   Linux aarch64: 25=OTP_TOO_OLD 26=OTP_TOO_OLD 27=27.3 28=28.5 29=29.1 30=29.1 none=NO_SDK
                   Windows x86_64: 25=OTP_TOO_OLD 26=OTP_TOO_OLD 27=27.3 28=28.5 29=29.1 30=29.1 none=NO_SDK
                   Windows aarch64: 25=UNSUPPORTED_CPU 26=UNSUPPORTED_CPU 27=UNSUPPORTED_CPU 28=UNSUPPORTED_CPU 29=UNSUPPORTED_CPU 30=UNSUPPORTED_CPU none=UNSUPPORTED_CPU
                   FreeBSD x86_64: 25=UNSUPPORTED_OS 26=UNSUPPORTED_OS 27=UNSUPPORTED_OS 28=UNSUPPORTED_OS 29=UNSUPPORTED_OS 30=UNSUPPORTED_OS none=UNSUPPORTED_OS
                   Linux x86_64 (no glibc): 25=NO_GLIBC 26=NO_GLIBC 27=NO_GLIBC 28=NO_GLIBC 29=NO_GLIBC 30=NO_GLIBC none=NO_GLIBC
                   """, table.toString());
  }

  public void testChosenAssetMatchesThePlatform() {
    ElpAssetChoice choice = ElpAssetChoice.choose(ElpRelease.pinned(), new ElpPlatform("linux", "aarch64"), true, 28);
    assertNotNull(choice.asset());
    assertEquals("elp-linux-aarch64-unknown-linux-gnu-otp-28.5.tar.gz", choice.asset().file());
    assertNull(choice.reason());
    assertEquals("", choice.message());
  }

  public void testMessages() {
    ElpRelease release = ElpRelease.pinned();
    assertEquals("The project uses OTP 26; ELP needs OTP 27 or newer.",
                 ElpAssetChoice.choose(release, new ElpPlatform("macos", "aarch64"), true, 26).message());
    assertEquals("The project has no Erlang SDK.", ElpAssetChoice.choose(release, new ElpPlatform("macos", "aarch64"), false, null).message());
    assertEquals(ElpAssetChoice.Reason.UNKNOWN_OTP, ElpAssetChoice.choose(release, new ElpPlatform("macos", "aarch64"), true, null).reason());
    assertEquals("The OTP release of the project's Erlang SDK is not known.",
                 ElpAssetChoice.choose(release, new ElpPlatform("macos", "aarch64"), true, null).message());
    assertEquals("ELP has no build for Windows aarch64.",
                 ElpAssetChoice.choose(release, new ElpPlatform("windows", "aarch64"), true, 28).message());
    assertEquals("ELP has no build for FreeBSD x86_64.", ElpAssetChoice.choose(release, new ElpPlatform("FreeBSD", "x86_64"), true, 28).message());
    assertEquals("ELP for Linux needs glibc; /lib/ld-linux-aarch64.so.1 is missing (musl?).",
                 ElpAssetChoice.choose(release, new ElpPlatform("linux", "aarch64", "/lib/ld-linux-aarch64.so.1"), true, 28).message());
  }

  public void testOtpMajor() {
    assertEquals(Integer.valueOf(27), ElpAssetChoice.otpMajor(new ErlangSdkRelease("27", "15.1")));
    assertEquals(Integer.valueOf(28), ElpAssetChoice.otpMajor(new ErlangSdkRelease("28.1.2", "16.1")));
    assertEquals(Integer.valueOf(16), ElpAssetChoice.otpMajor(new ErlangSdkRelease("R16B", "5.10.1")));
    assertEquals(Integer.valueOf(29), ElpAssetChoice.otpMajor(ErlangSdkRelease.V_29_0));
    assertNull(ElpAssetChoice.otpMajor(new ErlangSdkRelease("unknown", "1")));
    assertNull(ElpAssetChoice.otpMajor(null));
  }

  public void testGlibcLoaders() {
    assertEquals("/lib64/ld-linux-x86-64.so.2", ElpPlatform.glibcLoader("x86_64"));
    assertEquals("/lib/ld-linux-aarch64.so.1", ElpPlatform.glibcLoader("aarch64"));
    assertNull(ElpPlatform.glibcLoader("ARM32"));
  }
}
