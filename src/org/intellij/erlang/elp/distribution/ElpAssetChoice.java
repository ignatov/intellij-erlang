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

import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Comparator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Which tarball of the pinned release fits a machine and a project, or why none does.
 * <p>
 * ELP's documentation: the OTP number of a build is the release it was built on; ELP runs on that release or a newer one, so the
 * build for the closest older or equal OTP release is taken. A project on a newer OTP than every build gets the newest build.
 *
 * @param asset   the tarball, or {@code null} when none fits
 * @param reason  why none fits, or {@code null} when one does
 * @param message for the UI: why none fits, or empty when one does
 */
public record ElpAssetChoice(@Nullable ElpRelease.Asset asset, @Nullable Reason reason, @NotNull String message) {
  public enum Reason {
    /** The OS has no build (for example FreeBSD). */
    UNSUPPORTED_OS,
    /** The OS has no build for this CPU (for example Windows on ARM). */
    UNSUPPORTED_CPU,
    /** Linux without the glibc loader that the builds need (musl distributions, NixOS without nix-ld). */
    NO_GLIBC,
    /** The project has no Erlang SDK. */
    NO_SDK,
    /** The project has an Erlang SDK, but its OTP release is not known. */
    UNKNOWN_OTP,
    /** The project's OTP release is older than the oldest build. */
    OTP_TOO_OLD
  }

  private static final Pattern OTP_MAJOR = Pattern.compile("R?(\\d+).*");

  @NotNull
  public static ElpAssetChoice found(@NotNull ElpRelease.Asset asset) {
    return new ElpAssetChoice(asset, null, "");
  }

  @NotNull
  private static ElpAssetChoice none(@NotNull Reason reason, @NotNull String message) {
    return new ElpAssetChoice(null, reason, message);
  }

  /**
   * @param hasSdk   whether the project has an Erlang SDK
   * @param otpMajor the major OTP release of the project's SDK, see {@link #otpMajor}; {@code null} when it is not known
   */
  @NotNull
  public static ElpAssetChoice choose(@NotNull ElpRelease release, @NotNull ElpPlatform platform, boolean hasSdk,
                                      @Nullable Integer otpMajor) {
    List<ElpRelease.Asset> forOs = release.assets().stream().filter(a -> a.os().equals(platform.os())).toList();
    if (forOs.isEmpty()) return none(Reason.UNSUPPORTED_OS, "ELP has no build for " + platform.presentableName() + ".");
    List<ElpRelease.Asset> forPlatform = forOs.stream().filter(a -> a.arch().equals(platform.arch())).toList();
    if (forPlatform.isEmpty()) return none(Reason.UNSUPPORTED_CPU, "ELP has no build for " + platform.presentableName() + ".");
    if (platform.missingLoader() != null) {
      return none(Reason.NO_GLIBC, "ELP for Linux needs glibc; " + platform.missingLoader() + " is missing (musl?).");
    }
    if (!hasSdk) return none(Reason.NO_SDK, "The project has no Erlang SDK.");
    if (otpMajor == null) return none(Reason.UNKNOWN_OTP, "The OTP release of the project's Erlang SDK is not known.");
    int oldest = forPlatform.stream().mapToInt(ElpRelease.Asset::otpMajor).min().orElseThrow();
    return forPlatform.stream()
      .filter(a -> a.otpMajor() <= otpMajor)
      .max(Comparator.comparingInt(ElpRelease.Asset::otpMajor))
      .map(ElpAssetChoice::found)
      .orElseGet(() -> none(Reason.OTP_TOO_OLD, "The project uses OTP " + otpMajor + "; ELP needs OTP " + oldest + " or newer."));
  }

  /** @return the major OTP release, 27 for {@code 27.3.4} and 16 for {@code R16B}; {@code null} when there is none */
  @Nullable
  public static Integer otpMajor(@Nullable ErlangSdkRelease release) {
    if (release == null) return null;
    Matcher matcher = OTP_MAJOR.matcher(release.getOtpRelease());
    return matcher.matches() ? Integer.valueOf(matcher.group(1)) : null;
  }
}
