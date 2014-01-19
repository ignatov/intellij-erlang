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
import com.intellij.psi.PsiReference;

public class ErlangRecordFieldImpl extends ErlangCompositeElementImpl implements ErlangRecordField {

  public ErlangRecordFieldImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitRecordField(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangExpression getExpression() {
    return findChildByClass(ErlangExpression.class);
  }

  @Override
  @NotNull
  public List<ErlangQAtom> getQAtomList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangQAtom.class);
  }

  @Override
  @Nullable
  public PsiElement getDot() {
    return findChildByType(ERL_DOT);
  }

  @Override
  @Nullable
  public PsiElement getOpEq() {
    return findChildByType(ERL_OP_EQ);
  }

  @Override
  @Nullable
  public PsiElement getUniPattern() {
    return findChildByType(ERL_UNI_PATTERN);
  }

  @Nullable
  public PsiReference getReference() {
    return ErlangPsiImplUtil.getReference(this);
  }

  @Override
  @Nullable
  public ErlangQAtom getFieldNameAtom() {
    List<ErlangQAtom> p1 = getQAtomList();
    return p1.size() < 1 ? null : p1.get(0);
  }

}
