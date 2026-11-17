// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;

public interface ErlangRecordRef extends ErlangCompositeElement {

  @Nullable
  ErlangModuleRef getModuleRef();

  @NotNull
  ErlangQAtom getQAtom();

  @Nullable
  PsiElement getColon();

  @Nullable PsiReference getReference(@Nullable ErlangMacrosName o);

  @NotNull PsiReference getReference();

}
