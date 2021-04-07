package org.intellij.erlang;

import com.intellij.internal.statistic.fileTypes.FileTypeStatisticProvider;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileTypes.FileType;
import org.jetbrains.annotations.NotNull;

public final class ErlangFileTypeStatisticsProvider implements FileTypeStatisticProvider {
  private static final String PLUGIN_ID = "org.jetbrains.erlang";

  @NotNull
  @Override
  public String getPluginId() {
    return PLUGIN_ID;
  }

  @Override
  public boolean accept(@NotNull Editor editor, @NotNull FileType fileType) {
    return fileType instanceof ErlangFileType;
  }
}
