package org.intellij.erlang;

import com.intellij.lang.Language;

public class ErlangLanguage extends Language {
  public static final ErlangLanguage INSTANCE = new ErlangLanguage();

  protected ErlangLanguage() {
    super("Erlang");
  }

  @Override
  public String getDisplayName() {
    return "Erlang language";
  }
}
