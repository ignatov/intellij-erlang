// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.ResolveState;

public interface ErlangListComprehension extends ErlangExpression {

  @NotNull
  ErlangExpression getExpression();

  @Nullable
  ErlangLcExprs getLcExprs();

  boolean processDeclarations(PsiScopeProcessor processor, ResolveState state, PsiElement lastParent, PsiElement place);

}
