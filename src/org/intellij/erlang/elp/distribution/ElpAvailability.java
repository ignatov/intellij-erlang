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

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Whether a project has an ELP server, and if not, whether one can be downloaded: the answer of
 * {@link ElpDistribution#availability}.
 *
 * @param kind       see {@link Kind}
 * @param serverPath the {@code elp} binary that would be started for the project, or {@code null} when none would be
 * @param choice     the build of the pinned release for this machine and the project's OTP release, or why there is none; filled in
 *                   for every kind, so the settings can show it even when a custom path is in effect
 * @param release    the pinned release
 */
public record ElpAvailability(@NotNull Kind kind,
                              @Nullable String serverPath,
                              @NotNull ElpAssetChoice choice,
                              @NotNull ElpRelease release) {
  public enum Kind {
    /**
     * The path comes from the {@code erlang.elp.path} system property or from the custom path the user chose; {@code serverPath} is
     * {@code null} when that path is empty. Nothing is offered.
     */
    CONFIGURED,
    /** The downloaded server is chosen and its build for the project is installed: {@code serverPath} is that binary. */
    INSTALLED,
    /** The downloaded server is chosen and {@link ElpAssetChoice#asset()} of {@code choice} can be downloaded. */
    DOWNLOADABLE,
    /** The downloaded server is chosen and no build fits: {@code choice} says why. */
    UNAVAILABLE
  }

  public boolean hasServer() {
    return serverPath != null;
  }
}
