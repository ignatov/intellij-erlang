package org.intellij.erlang.psi;

import com.intellij.psi.tree.IElementType;
import org.intellij.erlang.ErlangLanguage;


public class ErlangCompositeElementType extends IElementType {
  public ErlangCompositeElementType(String debug) {
    super(debug, ErlangLanguage.INSTANCE);
  }
}
