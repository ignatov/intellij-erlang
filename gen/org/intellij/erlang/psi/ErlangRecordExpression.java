// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import com.intellij.psi.PsiReference;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public interface ErlangRecordExpression extends ErlangExpression {

  @NotNull
  ErlangExpression getExpression();

  @Nullable
  ErlangMacros getMacros();

  @Nullable
  ErlangRecordField getRecordField();

  @Nullable
  ErlangRecordRef getRecordRef();

  @Nullable
  ErlangRecordTuple getRecordTuple();

  @Nullable
  PsiReference getReference();

}
