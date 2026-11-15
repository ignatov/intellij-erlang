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

import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * The effective ELP settings: the stored ones, overridden by the {@code erlang.elp.path} and {@code erlang.elp.mode}
 * system properties when they are set. An empty path means that no ELP server is started.
 */
public record ElpConfiguration(@NotNull String path, @NotNull ElpMode mode) {
  public static final String PATH_PROPERTY = "erlang.elp.path";
  public static final String MODE_PROPERTY = "erlang.elp.mode";

  public boolean isEnabled() {
    return !path.isEmpty();
  }

  /**
   * @param propertyPath the {@code erlang.elp.path} system property; when it is set, even to an empty string, it wins
   * @param propertyMode the {@code erlang.elp.mode} system property; an unknown value is ignored
   */
  @NotNull
  public static ElpConfiguration resolve(@Nullable String storedPath,
                                         @Nullable String storedMode,
                                         @Nullable String propertyPath,
                                         @Nullable String propertyMode) {
    String path = StringUtil.notNullize(propertyPath != null ? propertyPath : storedPath).trim();
    ElpMode mode = ElpMode.parse(propertyMode);
    if (mode == null) mode = ElpMode.parse(storedMode);
    return new ElpConfiguration(path, mode != null ? mode : ElpMode.DIAGNOSTICS);
  }

  @NotNull
  public static ElpConfiguration resolve(@Nullable String storedPath, @Nullable String storedMode) {
    return resolve(storedPath, storedMode, System.getProperty(PATH_PROPERTY), System.getProperty(MODE_PROPERTY));
  }
}
