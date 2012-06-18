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

public class ErlangIntTypeImpl extends ErlangCompositeElementImpl implements ErlangIntType {

  public ErlangIntTypeImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public PsiElement getInteger() {
    return findNotNullChildByType(ERL_INTEGER);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitIntType(this);
    else super.accept(visitor);
  }

}
