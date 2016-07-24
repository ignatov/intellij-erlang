/*
 * Copyright 2012-2014 Sergey Ignatov
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

package org.intellij.erlang.jps.builder;

import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ErlangCompilerError {
  static final Pattern COMPILER_MESSAGE_PATTERN = Pattern.compile("^((?:[a-zA-Z]:)?.+?):(?:(\\d+):)?(\\s*Warning:)?\\s*(.+)$");
  static final int PATH_MATCH_INDEX = 1;
  static final int LINE_MATCH_INDEX = 2;
  static final int WARNING_MATCH_INDEX = 3;
  static final int DETAILS_MATCH_INDEX = 4;

  private final String myErrorMessage;
  private final String myUrl;
  private final int myLine;
  private final CompilerMessageCategory myCategory;

  private ErlangCompilerError(@NotNull String errorMessage,
                              @NotNull String url,
                              int line,
                              @NotNull CompilerMessageCategory category) {
    this.myErrorMessage = errorMessage;
    this.myUrl = url;
    this.myLine = line;
    this.myCategory = category;
  }

  @NotNull
  public String getErrorMessage() {
    return myErrorMessage;
  }

  @NotNull
  public String getUrl() {
    return myUrl;
  }

  public int getLine() {
    return myLine;
  }

  @NotNull
  public CompilerMessageCategory getCategory() {
    return myCategory;
  }

  @Nullable
  public static ErlangCompilerError create(@NotNull String rootPath, @NotNull String erlcMessage) {
    Matcher matcher = COMPILER_MESSAGE_PATTERN.matcher(StringUtil.trimTrailing(erlcMessage));
    if (!matcher.matches()) return null;

    String relativeFilePath = FileUtil.toSystemIndependentName(matcher.group(PATH_MATCH_INDEX));
    File path = StringUtil.isEmpty(rootPath) ? new File(relativeFilePath) : new File(FileUtil.toSystemIndependentName(rootPath), relativeFilePath);
    if(!path.exists()) return null;

    String line = matcher.group(LINE_MATCH_INDEX);
    String warning = matcher.group(WARNING_MATCH_INDEX);
    String details = matcher.group(DETAILS_MATCH_INDEX);
    return createCompilerError(path.getPath(), line, warning, details);
  }

  @NotNull
  private static ErlangCompilerError createCompilerError(@NotNull String filePath,
                                                         @Nullable String line,
                                                         @Nullable String warning,
                                                         @NotNull String details) {
    int lineNumber = StringUtil.parseInt(line, -1);
    CompilerMessageCategory category = warning != null ? CompilerMessageCategory.WARNING : CompilerMessageCategory.ERROR;
    return new ErlangCompilerError(details, VfsUtilCore.pathToUrl(filePath), lineNumber, category);
  }
}
