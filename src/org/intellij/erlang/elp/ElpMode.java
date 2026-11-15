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

import java.util.Locale;

/**
 * Which engine answers in Erlang modules and headers once ELP is configured: the plugin's own resolve or ELP.
 */
public enum ElpMode {
  /** The plugin's PSI and resolve serve everything; ELP supplies only diagnostics and the quick fixes attached to them. */
  PLUGIN("Plugin"),
  /**
   * ELP serves the features it supports through the platform LSP client, and the plugin's counterparts of those features are
   * switched off while ELP is running for the file, see {@link ElpEngine}. Everything else stays with the plugin.
   */
  ELP("ELP");

  private final String myPresentableName;

  ElpMode(@NotNull String presentableName) {
    myPresentableName = presentableName;
  }

  @NotNull
  public String getPresentableName() {
    return myPresentableName;
  }

  /**
   * Also accepts the names of the prototype's modes: {@code DIAGNOSTICS} is {@link #PLUGIN} and {@code ALL} is {@link #ELP}.
   */
  @Nullable
  public static ElpMode parse(@Nullable String value) {
    if (StringUtil.isEmptyOrSpaces(value)) return null;
    String name = value.trim().toUpperCase(Locale.ROOT);
    return switch (name) {
      case "DIAGNOSTICS" -> PLUGIN;
      case "ALL" -> ELP;
      default -> {
        try {
          yield valueOf(name);
        }
        catch (IllegalArgumentException e) {
          yield null;
        }
      }
    };
  }
}
