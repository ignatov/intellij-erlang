// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;

public interface ErlangListComprehension extends ErlangExpression {

  @NotNull
  List<ErlangExpression> getExpressionList();

  @NotNull
  PsiElement getBracketLeft();

  @NotNull
  PsiElement getBracketRight();

  @NotNull
  PsiElement getOrOr();

  boolean processDeclarations(PsiScopeProcessor processor, ResolveState state, PsiElement lastParent, PsiElement place);

}
