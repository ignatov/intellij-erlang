// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangTypeGuard extends ErlangCompositeElement {

  @Nullable
  ErlangQAtom getQAtom();

  @NotNull
  List<ErlangType> getTypeList();

  @Nullable
  PsiElement getParLeft();

  @Nullable
  PsiElement getParRight();

}
