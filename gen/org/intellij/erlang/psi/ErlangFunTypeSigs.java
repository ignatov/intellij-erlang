// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;

public interface ErlangFunTypeSigs extends ErlangCompositeElement {

  @NotNull
  List<ErlangQAtom> getQAtomList();

  @NotNull
  List<ErlangTypeSig> getTypeSigList();

  @Nullable
  PsiElement getInteger();

  @Nullable
  PsiReference getReference();

}
