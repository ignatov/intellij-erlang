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
 * The effective ELP settings: the stored ones, overridden by the {@code erlang.elp.path}, {@code erlang.elp.mode} and
 * {@code erlang.elp.eqwalizer} system properties when they are set. An empty path means that no ELP server is started.
 *
 * @param eqwalizer whether the diagnostics of eqWAlizer, ELP's gradual type checker, are shown. Off by default: code that was
 *                  not written for eqWAlizer gets many type errors that the compiler does not report.
 */
public record ElpConfiguration(@NotNull String path, @NotNull ElpMode mode, boolean eqwalizer) {
  public static final String PATH_PROPERTY = "erlang.elp.path";
  public static final String MODE_PROPERTY = "erlang.elp.mode";
  public static final String EQWALIZER_PROPERTY = "erlang.elp.eqwalizer";

  public ElpConfiguration(@NotNull String path, @NotNull ElpMode mode) {
    this(path, mode, false);
  }

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
    return resolve(storedPath, storedMode, false, propertyPath, propertyMode, null);
  }

  /**
   * @param propertyEqwalizer the {@code erlang.elp.eqwalizer} system property: {@code true} or {@code false}, in any case,
   *                          wins over {@code storedEqwalizer}; any other value is ignored
   */
  @NotNull
  public static ElpConfiguration resolve(@Nullable String storedPath,
                                         @Nullable String storedMode,
                                         boolean storedEqwalizer,
                                         @Nullable String propertyPath,
                                         @Nullable String propertyMode,
                                         @Nullable String propertyEqwalizer) {
    String path = StringUtil.notNullize(propertyPath != null ? propertyPath : storedPath).trim();
    ElpMode mode = ElpMode.parse(propertyMode);
    if (mode == null) mode = ElpMode.parse(storedMode);
    Boolean eqwalizer = parseBoolean(propertyEqwalizer);
    return new ElpConfiguration(path, mode != null ? mode : ElpMode.PLUGIN, eqwalizer != null ? eqwalizer : storedEqwalizer);
  }

  @NotNull
  public static ElpConfiguration resolve(@Nullable String storedPath, @Nullable String storedMode, boolean storedEqwalizer) {
    return resolve(storedPath, storedMode, storedEqwalizer, System.getProperty(PATH_PROPERTY), System.getProperty(MODE_PROPERTY),
                   System.getProperty(EQWALIZER_PROPERTY));
  }

  /**
   * @return {@code true} or {@code false} for those words in any case, {@code null} for anything else
   */
  @Nullable
  public static Boolean parseBoolean(@Nullable String value) {
    if (value == null) return null;
    String trimmed = value.trim();
    if ("true".equalsIgnoreCase(trimmed)) return Boolean.TRUE;
    if ("false".equalsIgnoreCase(trimmed)) return Boolean.FALSE;
    return null;
  }
}
