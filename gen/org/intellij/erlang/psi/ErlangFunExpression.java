// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;

public interface ErlangFunExpression extends ErlangExpression {

  @Nullable
  ErlangFunClauses getFunClauses();

  @Nullable
  ErlangFunctionWithArity getFunctionWithArity();

  @Nullable
  ErlangFunctionWithArityVariables getFunctionWithArityVariables();

  @Nullable
  ErlangModuleRef getModuleRef();

  @Nullable
  ErlangQVar getQVar();

  @Nullable
  PsiElement getColon();

  @Nullable
  PsiElement getEnd();

  @NotNull
  PsiElement getFun();

  boolean processDeclarations(@NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place);

}
