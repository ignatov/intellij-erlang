// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangPrefixExpression extends ErlangExpression {

  @NotNull
  ErlangExpression getExpression();

  @Nullable
  PsiElement getBnot();

  @Nullable
  PsiElement getNot();

}
