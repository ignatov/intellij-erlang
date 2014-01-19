// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangConfigExpression extends ErlangExpression {

  @Nullable
  ErlangQAtom getQAtom();

  @Nullable
  ErlangQVar getQVar();

  @Nullable
  PsiElement getOpMinus();

  @Nullable
  PsiElement getOpPlus();

  @Nullable
  PsiElement getBnot();

  @Nullable
  PsiElement getChar();

  @Nullable
  PsiElement getFloat();

  @Nullable
  PsiElement getInteger();

  @Nullable
  PsiElement getNot();

}
