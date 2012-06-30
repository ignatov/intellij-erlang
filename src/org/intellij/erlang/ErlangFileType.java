package org.intellij.erlang;

import com.intellij.openapi.fileTypes.LanguageFileType;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

/**
 * @author ignatov
 */
public class ErlangFileType extends LanguageFileType {
  public static ErlangFileType INSTANCE = new ErlangFileType();
  public static ErlangFileType HRL = new HrlFilType();

  protected ErlangFileType() {
    super(ErlangLanguage.INSTANCE);
  }

  @NotNull
  @Override
  public String getName() {
    return "Erlang";
  }

  @NotNull
  @Override
  public String getDescription() {
    return "Erlang";
  }

  @NotNull
  @Override
  public String getDefaultExtension() {
    return "erl";
  }

  @Override
  public Icon getIcon() {
    return ErlangIcons.FILE;
  }

  @Override
  public boolean isJVMDebuggingSupported() {
    // turn off for now
    return false;
  }

  public static class HrlFilType extends ErlangFileType {
    @NotNull
    @Override
    public String getDefaultExtension() {
      return "hrl";
    }
  }
}
