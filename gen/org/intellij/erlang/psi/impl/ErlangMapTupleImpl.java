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

public class ErlangMapTupleImpl extends ErlangCompositeElementImpl implements ErlangMapTuple {

  public ErlangMapTupleImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitMapTuple(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<ErlangMapEntry> getMapEntryList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangMapEntry.class);
  }

  @Override
  @NotNull
  public PsiElement getCurlyLeft() {
    return notNullChild(findChildByType(ERL_CURLY_LEFT));
  }

  @Override
  @Nullable
  public PsiElement getCurlyRight() {
    return findChildByType(ERL_CURLY_RIGHT);
  }

  @Override
  @NotNull
  public PsiElement getRadix() {
    return notNullChild(findChildByType(ERL_RADIX));
  }

}
