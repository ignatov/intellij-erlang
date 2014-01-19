// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static org.intellij.erlang.ErlangTypes.*;
import org.intellij.erlang.psi.*;

public class ErlangBinBaseTypeImpl extends ErlangTypeImpl implements ErlangBinBaseType {

  public ErlangBinBaseTypeImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitBinBaseType(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public ErlangQVar getQVar() {
    return findNotNullChildByClass(ErlangQVar.class);
  }

  @Override
  @NotNull
  public PsiElement getColon() {
    return findNotNullChildByType(ERL_COLON);
  }

  @Override
  @NotNull
  public PsiElement getInteger() {
    return findNotNullChildByType(ERL_INTEGER);
  }

}
