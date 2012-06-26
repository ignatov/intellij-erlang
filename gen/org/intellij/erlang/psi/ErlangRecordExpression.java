// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;

public interface ErlangRecordExpression extends ErlangExpression {

  @Nullable
  ErlangExpression getExpression();

  @NotNull
  List<ErlangQAtom> getQAtomList();

  @Nullable
  ErlangRecordTuple getRecordTuple();

  @Nullable
  ErlangQAtom getAtomName();

  @Nullable
  PsiReference getReference();

}
