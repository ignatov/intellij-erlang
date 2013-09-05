// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangType extends ErlangCompositeElement {

  @Nullable
  ErlangModuleRef getModuleRef();

  @Nullable
  ErlangQVar getQVar();

  @Nullable
  ErlangRecordRef getRecordRef();

  @Nullable
  ErlangTopType getTopType();

  @Nullable
  ErlangTypeRef getTypeRef();

  @Nullable
  PsiElement getFun();

}
