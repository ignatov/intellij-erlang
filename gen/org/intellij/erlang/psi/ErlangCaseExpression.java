// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangCaseExpression extends ErlangExpression {

  @NotNull
  ErlangCrClauses getCrClauses();

  @NotNull
  ErlangExpression getExpression();

  @NotNull
  PsiElement getCase();

  @NotNull
  PsiElement getEnd();

  @NotNull
  PsiElement getOf();

}
