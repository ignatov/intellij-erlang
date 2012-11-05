// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangGlobalFunctionCallExpression extends ErlangExpression {

  @NotNull
  ErlangFunctionCallExpression getFunctionCallExpression();

  @Nullable
  ErlangModuleRef getModuleRef();

  @Nullable
  ErlangQAtom getQAtom();

}
