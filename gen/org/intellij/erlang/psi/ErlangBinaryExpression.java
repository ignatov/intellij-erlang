// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangBinaryExpression extends ErlangExpression {

  @NotNull
  List<ErlangBinElement> getBinElementList();

  @Nullable
  PsiElement getBinEnd();

  @NotNull
  PsiElement getBinStart();

}
