// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangAtom extends ErlangCompositeElement {

  @Nullable
  PsiElement getAtomName();

  @NotNull String getName();

  @NotNull ErlangAtom setName(@NotNull String newName);

  @NotNull PsiElement getNameIdentifier();

}
