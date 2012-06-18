// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangTryCatch extends ErlangCompositeElement {

  @NotNull
  List<ErlangExpression> getExpressionList();

  @NotNull
  List<ErlangTryClause> getTryClauseList();

  @Nullable
  PsiElement getAfter();

  @Nullable
  PsiElement getCatch();

  @Nullable
  PsiElement getEnd();

}
