// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;

public interface ErlangCaseExpression extends ErlangExpression, ErlangClauseOwner {

  @NotNull
  List<ErlangCrClause> getCrClauseList();

  @Nullable
  ErlangExpression getExpression();

  @NotNull
  PsiElement getCase();

  @Nullable
  PsiElement getEnd();

  @Nullable
  PsiElement getOf();

  boolean processDeclarations(PsiScopeProcessor processor, ResolveState state, PsiElement lastParent, PsiElement place);

}
