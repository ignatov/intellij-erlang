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
 * Which LSP features ELP supplies.
 */
public enum ElpMode {
  /** Diagnostics and the quick fixes attached to them; the plugin's PSI owns everything else. */
  DIAGNOSTICS,
  /** Everything the platform LSP client supports, side by side with the plugin's own features. */
  ALL;

  @Nullable
  public static ElpMode parse(@Nullable String value) {
    if (StringUtil.isEmptyOrSpaces(value)) return null;
    try {
      return valueOf(value.trim().toUpperCase(Locale.ROOT));
    }
    catch (IllegalArgumentException e) {
      return null;
    }
  }
}
