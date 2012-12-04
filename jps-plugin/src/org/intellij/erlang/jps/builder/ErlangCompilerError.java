package org.intellij.erlang.jps.builder;

import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtil;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * @author @nik
 */
public class ErlangCompilerError {
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
  public static ErlangCompilerError create(String rootPath, final String erlcMessage) {
    List<String> split = StringUtil.split(erlcMessage, ":");

    // a small hack for such messages: c:\User\test.erl:7:Warning:Message
    if (SystemInfo.isWindows) {
      String combine = split.get(0) + ":" + split.get(1);
      split.remove(0);
      split.remove(0);
      split.add(0, combine);
    }

    if (split.size() < 3) return null;

    String path = split.get(0);
    String line = split.get(1);

    String url = FileUtil.toSystemIndependentName(path);
    if (!StringUtil.startsWithIgnoreCase(path, rootPath)) {
      url = rootPath + "/" + url;
    }
    url = VfsUtil.pathToUrl(url);
//    if (!ApplicationManager.getApplication().isUnitTestMode() && VirtualFileManager.getInstance().findFileByUrl(url) == null) {
//      return null;
//    }

    boolean warning = StringUtil.equalsIgnoreCase(split.get(2).trim(), "warning");
    String messageForUser = StringUtil.replaceIgnoreCase(erlcMessage, path + ":" + line + (warning ? ": warning: " : ": "), "");
    return new ErlangCompilerError(messageForUser, url, StringUtil.parseInt(split.get(1), -1),
      warning ? CompilerMessageCategory.WARNING : CompilerMessageCategory.ERROR);
  }

  public CompilerMessageCategory getCategory() {
    return category;
  }
}
