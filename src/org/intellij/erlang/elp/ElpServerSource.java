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

package org.intellij.erlang.elp;

import org.jetbrains.annotations.Nullable;

/**
 * Where the {@code elp} binary comes from. The {@code erlang.elp.path} system property wins over both.
 */
public enum ElpServerSource {
  /** The build of the pinned release that the plugin downloaded for the project's OTP release, see {@code ElpDistribution}. */
  DOWNLOADED,
  /** The path the user typed. */
  CUSTOM;

  /** @return the source with that name, or {@code null} for anything else */
  @Nullable
  public static ElpServerSource parse(@Nullable String name) {
    if (name == null) return null;
    for (ElpServerSource source : values()) {
      if (source.name().equalsIgnoreCase(name.trim())) return source;
    }
    return null;
  }
}
