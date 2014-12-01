// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangTypedExpr extends ErlangNamedElement {

  @Nullable
  ErlangExpression getExpression();

  @NotNull
  ErlangQAtom getQAtom();

  @Nullable
  ErlangType getType();

  @Nullable
  PsiElement getColonColon();

  @Nullable
  PsiElement getOpEq();

  @NotNull
  String getName();

  @NotNull
  PsiElement setName(String newName);

  @NotNull
  PsiElement getNameIdentifier();

  int getTextOffset();

}
