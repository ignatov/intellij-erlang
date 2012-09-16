// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangBinElement extends ErlangCompositeElement {

  @NotNull
  List<ErlangMaxExpression> getMaxExpressionList();

  @Nullable
  ErlangOptBitTypeList getOptBitTypeList();

  @Nullable
  PsiElement getBnot();

  @Nullable
  PsiElement getNot();

}
