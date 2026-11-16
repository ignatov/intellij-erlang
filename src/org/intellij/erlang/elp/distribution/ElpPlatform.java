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

import com.intellij.openapi.util.SystemInfo;
import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import com.intellij.util.system.CpuArch;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.nio.file.Files;
import java.nio.file.Path;

/**
 * The OS and CPU an ELP build is chosen for, in the names of the manifest ({@link ElpRelease#MACOS}, {@link ElpRelease#X86_64}...).
 *
 * @param os            {@code macos}, {@code linux}, {@code windows}, or the {@code os.name} of another OS
 * @param arch          {@code x86_64}, {@code aarch64}, or the name of another CPU architecture
 * @param missingLoader on Linux, the glibc dynamic loader that ELP's builds need when it is not there (musl distributions such as
 *                      Alpine, or NixOS without nix-ld); {@code null} when it is there or on other systems
 */
public record ElpPlatform(@NotNull String os, @NotNull String arch, @Nullable String missingLoader) {
  public ElpPlatform(@NotNull String os, @NotNull String arch) {
    this(os, arch, null);
  }

  /** Looks for the glibc loader on Linux, so it touches the file system. */
  @NotNull
  @RequiresBackgroundThread
  public static ElpPlatform current() {
    String os = SystemInfo.isMac ? ElpRelease.MACOS : SystemInfo.isLinux ? ElpRelease.LINUX : SystemInfo.isWindows ? ElpRelease.WINDOWS
                                                                                                                  : SystemInfo.OS_NAME;
    String arch = CpuArch.isIntel64() ? ElpRelease.X86_64 : CpuArch.isArm64() ? ElpRelease.AARCH64 : CpuArch.CURRENT.name();
    String loader = ElpRelease.LINUX.equals(os) ? glibcLoader(arch) : null;
    return new ElpPlatform(os, arch, loader != null && !Files.exists(Path.of(loader)) ? loader : null);
  }

  /** @return the path of the glibc dynamic loader that a Linux binary for {@code arch} is linked against */
  @Nullable
  public static String glibcLoader(@NotNull String arch) {
    return switch (arch) {
      case ElpRelease.X86_64 -> "/lib64/ld-linux-x86-64.so.2";
      case ElpRelease.AARCH64 -> "/lib/ld-linux-aarch64.so.1";
      default -> null;
    };
  }

  /** For example {@code macOS arm64} or {@code Linux x86_64}. */
  @NotNull
  public String presentableName() {
    String osName = switch (os) {
      case ElpRelease.MACOS -> "macOS";
      case ElpRelease.LINUX -> "Linux";
      case ElpRelease.WINDOWS -> "Windows";
      default -> os;
    };
    String archName = ElpRelease.MACOS.equals(os) && ElpRelease.AARCH64.equals(arch) ? "arm64" : arch;
    return osName + " " + archName;
  }
}
