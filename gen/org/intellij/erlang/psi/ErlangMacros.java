// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;

public interface ErlangMacros extends ErlangCompositeElement {

  @Nullable
  ErlangMacrosName getMacrosName();

  @NotNull
  PsiElement getQmark();

  @Nullable
  PsiReference getReference();

  @Nullable
  PsiReference getReference(@Nullable ErlangMacrosName o);

}
