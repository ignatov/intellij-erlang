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

package org.intellij.erlang.statistics;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;

/**
 * The "Usage Statistics" section of the Erlang External Tools settings page.
 */
public final class ErlangUsageStatisticsPanel extends JPanel {
  @NotNull
  static String prettyPrint(@NotNull String json) {
    StringBuilder result = new StringBuilder();
    int indent = 0;
    boolean inString = false;
    for (int i = 0; i < json.length(); i++) {
      char c = json.charAt(i);
      if (inString) {
        result.append(c);
        if (c == '\\' && i + 1 < json.length()) result.append(json.charAt(++i));
        else if (c == '"') inString = false;
        continue;
      }
      switch (c) {
        case '"' -> {
          inString = true;
          result.append(c);
        }
        case '{', '[' -> {
          indent++;
          result.append(c).append('\n').append("  ".repeat(indent));
        }
        case '}', ']' -> {
          indent--;
          result.append('\n').append("  ".repeat(Math.max(indent, 0))).append(c);
        }
        case ',' -> result.append(c).append('\n').append("  ".repeat(indent));
        case ':' -> result.append(": ");
        default -> result.append(c);
      }
    }
    return result.toString();
  }
}
