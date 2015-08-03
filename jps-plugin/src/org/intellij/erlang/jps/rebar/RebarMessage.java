/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.jps.rebar;

import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RebarMessage {
  private static final Pattern LOG_MESSAGE_PATTERN = Pattern.compile("(ERROR|WARN|INFO|DEBUG):(.+)");
  private final CompilerMessageCategory myCategory;
  private final String myDetails;

  private RebarMessage(@NotNull CompilerMessageCategory category, @NotNull String details) {
    this.myCategory = category;
    this.myDetails = details;
  }

  @Nullable
  public static RebarMessage create(@NotNull String message) {
    Matcher matcher = LOG_MESSAGE_PATTERN.matcher(StringUtil.trimTrailing(message));
    if (!matcher.matches()) return null;

    String type = matcher.group(1);
    String details = matcher.group(2);
    return new RebarMessage(findCategory(type), details);
  }

  @NotNull
  public CompilerMessageCategory getCategory() {
    return myCategory;
  }

  @NotNull
  public String getDetails() {
    return myDetails;
  }

  @NotNull
  private static CompilerMessageCategory findCategory(@NotNull String type) {
    if ("ERROR".equals(type)) {
      return CompilerMessageCategory.ERROR;
    }
    if ("WARN".equals(type)) {
      return CompilerMessageCategory.WARNING;
    }
    return CompilerMessageCategory.INFORMATION;
  }
}
