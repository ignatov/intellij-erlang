// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangFunRefExpression extends ErlangExpression {

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

  @NotNull
  PsiElement getFun();

  //WARNING: processDeclarations(...) is skipped
  //matching processDeclarations(ErlangFunRefExpression, ...)
  //methods are not found in ErlangPsiImplUtil

}
