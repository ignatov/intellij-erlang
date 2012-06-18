package org.intellij.erlang.psi;

import com.intellij.psi.tree.IElementType;
import org.intellij.erlang.ErlangLanguage;

public class ErlangTokenType extends IElementType {
  public ErlangTokenType(String debug) {
    super(debug, ErlangLanguage.INSTANCE);
  }
}
