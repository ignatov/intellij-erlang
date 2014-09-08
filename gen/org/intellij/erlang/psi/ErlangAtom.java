// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangAtom extends ErlangCompositeElement {

  @Nullable
  PsiElement getAtomName();

  String getName();

  @NotNull
  ErlangAtom setName(String newName);

  @NotNull
  PsiElement getNameIdentifier();

}
