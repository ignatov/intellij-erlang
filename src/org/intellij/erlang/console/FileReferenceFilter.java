/*
 * Copyright 2012-2013 Sergey Ignatov
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
package org.intellij.erlang.console;

import com.intellij.execution.filters.Filter;
import com.intellij.execution.filters.HyperlinkInfo;
import com.intellij.execution.filters.InvalidExpressionException;
import com.intellij.execution.filters.OpenFileHyperlinkInfo;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectScope;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class FileReferenceFilter implements Filter {
  public static final String PATH_MACROS = "$FILE_PATH$";
  public static final String LINE_MACROS = "$LINE$";
  public static final String COLUMN_MACROS = "$COLUMN$";

  private static final String FILE_PATH_REGEXP = "((?:\\p{Alpha}\\:)?[0-9 a-z_A-Z\\-\\\\./]+)";
  private static final String NUMBER_REGEXP = "([0-9]+)";

  private static final Pattern PATTERN_FILENAME = Pattern.compile("[/\\\\]?([^/\\\\]*?\\.erl)$");

  private final Pattern myPattern;
  private final Project myProject;
  private final int myFileMatchGroup;
  private final int myLineMatchGroup;
  private final int myColumnMatchGroup;

  public FileReferenceFilter(@NotNull Project project, @NonNls @NotNull String expression) {
    myProject = project;
    if (StringUtil.isEmpty(expression)) {
      throw new InvalidExpressionException("expression is empty");
    }
    int filePathIndex = expression.indexOf(PATH_MACROS);
    int lineIndex = expression.indexOf(LINE_MACROS);
    int columnIndex = expression.indexOf(COLUMN_MACROS);

    if (filePathIndex == -1) {
      throw new InvalidExpressionException("Expression must contain " + PATH_MACROS + " macros.");
    }
    final TreeMap<Integer,String> map = new TreeMap<Integer, String>();
    map.put(filePathIndex, PATH_MACROS);
    expression = StringUtil.replace(expression, PATH_MACROS, FILE_PATH_REGEXP);

    if (lineIndex != -1) {
      expression = StringUtil.replace(expression, LINE_MACROS, NUMBER_REGEXP);
      map.put(lineIndex, LINE_MACROS);
    }

    if (columnIndex != -1) {
      expression = StringUtil.replace(expression, COLUMN_MACROS, NUMBER_REGEXP);
      map.put(columnIndex, COLUMN_MACROS);
    }

    // The block below determines the registers based on the sorted map.
    int count = 0;
    for (final Integer integer : map.keySet()) {
      count++;
      final String s = map.get(integer);

      if (PATH_MACROS.equals(s)) {
        filePathIndex = count;
      }
      else if (LINE_MACROS.equals(s)) {
        lineIndex = count;
      }
      else if (COLUMN_MACROS.equals(s)) {
        columnIndex = count;
      }
    }

    myFileMatchGroup = filePathIndex;
    myLineMatchGroup = lineIndex;
    myColumnMatchGroup = columnIndex;
    myPattern = Pattern.compile(expression, Pattern.MULTILINE);
  }

  public Result applyFilter(@NotNull String line, int entireLength) {
    final Matcher matcher = myPattern.matcher(line);
    if (!matcher.find()) {
      return null;
    }
    final String filePath = matcher.group(myFileMatchGroup);
    final int fileLine = matchGroupToNumber(matcher, myLineMatchGroup);
    final int fileCol = matchGroupToNumber(matcher, myColumnMatchGroup);
    final int highlightStartOffset = entireLength - line.length() + matcher.start(0);
    final int highlightEndOffset = highlightStartOffset + (matcher.end(0) - matcher.start(0));
    final VirtualFile absolutePath = resolveAbsolutePath(filePath);
    final HyperlinkInfo hyperLink = (absolutePath != null
      ? new OpenFileHyperlinkInfo(myProject, absolutePath, fileLine, fileCol) : null);
    return new Result(highlightStartOffset, highlightEndOffset, hyperLink);
  }

  private static int matchGroupToNumber(@NotNull Matcher matcher, int matchGroup) {
    int number = 0;
    if (matchGroup != -1) {
      try {
        number = Integer.parseInt(matcher.group(matchGroup));
      } catch (NumberFormatException e) { // Ignore
      }
    }
    return (number > 0 ? number - 1 : 0);
  }

  @Nullable
  private VirtualFile resolveAbsolutePath(@NotNull String path) {
    final VirtualFile asIsFile = pathToVirtualFile(path);
    if (asIsFile != null) {
      return asIsFile;
    }
    final String projectBasedPath = path.startsWith(myProject.getBasePath())
      ? path : new File(myProject.getBasePath(), path).getAbsolutePath();
    final VirtualFile projectBasedFile = pathToVirtualFile(projectBasedPath);
    if (projectBasedFile != null) {
      return projectBasedFile;
    }
    final Matcher filenameMatcher = PATTERN_FILENAME.matcher(path);
    if (filenameMatcher.find()) {
      final String filename = filenameMatcher.group(1);
      final GlobalSearchScope projectScope = ProjectScope.getProjectScope(myProject);
      final PsiFile[] projectFiles = FilenameIndex.getFilesByName(myProject, filename, projectScope);
      if (projectFiles.length > 0) {
        return projectFiles[0].getVirtualFile();
      }
      final GlobalSearchScope libraryScope = ProjectScope.getLibrariesScope(myProject);
      final PsiFile[] libraryFiles = FilenameIndex.getFilesByName(myProject, filename, libraryScope);
      if (libraryFiles.length > 0) {
        return libraryFiles[0].getVirtualFile();
      }
    }
    return null;
  }

  @Nullable
  private static VirtualFile pathToVirtualFile(@NotNull String path) {
    final String normalizedPath = path.replace(File.separatorChar, '/');
    return LocalFileSystem.getInstance().findFileByPath(normalizedPath);
  }
}
