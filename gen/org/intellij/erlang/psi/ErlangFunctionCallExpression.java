// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;

public interface ErlangFunctionCallExpression extends ErlangExpression {

  @NotNull
  ErlangArgumentList getArgumentList();

  @NotNull
  ErlangQAtom getQAtom();

  @Nullable
  PsiReference getReference();

  @NotNull
  PsiElement getNameIdentifier();

  int getTextOffset();

  @NotNull
  String getName();

}
