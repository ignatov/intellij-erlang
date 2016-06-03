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

public class ErlangMapEntryTypeImpl extends ErlangTypeImpl implements ErlangMapEntryType {

  public ErlangMapEntryTypeImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitMapEntryType(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<ErlangType> getTypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangType.class);
  }

  @Override
  @Nullable
  public PsiElement getAssoc() {
    return findChildByType(ERL_ASSOC);
  }

  @Override
  @Nullable
  public PsiElement getDotDotDot() {
    return findChildByType(ERL_DOT_DOT_DOT);
  }

  @Override
  @Nullable
  public PsiElement getMatch() {
    return findChildByType(ERL_MATCH);
  }

}
