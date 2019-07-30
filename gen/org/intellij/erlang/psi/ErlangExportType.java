// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;

public interface ErlangExportType extends ErlangType {

  @NotNull
  ErlangQAtom getQAtom();

  @Nullable
  PsiElement getOpArDiv();

  @Nullable
  PsiElement getInteger();

  @Nullable
  PsiReference getReference(@Nullable ErlangMacrosName o);

  @NotNull
  PsiReference getReference();

}
