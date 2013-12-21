package org.intellij.erlang.jps.builder;

import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ErlangCompilerError {
  private static final Pattern COMPILER_MESSAGE_PATTERN = Pattern.compile("^(.+):(\\d+):(\\s*Warning:)?\\s*(.+)$");

  private final String errorMessage;
  private final String url;
  private final int line;
  private final CompilerMessageCategory category;

  private ErlangCompilerError(String errorMessage, String url, int line, CompilerMessageCategory category) {
    this.errorMessage = errorMessage;
    this.url = url;
    this.line = line;
    this.category = category;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

  public String getUrl() {
    return url;
  }

  public int getLine() {
    return line;
  }

  @Nullable
  public static ErlangCompilerError create(String rootPath, String erlcMessage) {
    Matcher matcher = COMPILER_MESSAGE_PATTERN.matcher(StringUtil.trimTrailing(erlcMessage));
    if (!matcher.matches()) return null;

    String relativeFilePath = FileUtil.toSystemIndependentName(matcher.group(1));
    String line = matcher.group(2);
    String warning = matcher.group(3);
    String details = matcher.group(4);

    String path = StringUtil.isEmpty(rootPath) ? relativeFilePath : new File(FileUtil.toSystemIndependentName(rootPath), relativeFilePath).getPath();
    int lineNumber = StringUtil.parseInt(line, -1);
    CompilerMessageCategory category = warning != null ? CompilerMessageCategory.WARNING : CompilerMessageCategory.ERROR;
    assert path != null;
    return new ErlangCompilerError(details, VfsUtilCore.pathToUrl(path), lineNumber, category);
  }

  public CompilerMessageCategory getCategory() {
    return category;
  }
}
