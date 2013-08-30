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

public class ErlangTopType100TImpl extends ErlangCompositeElementImpl implements ErlangTopType100T {

  public ErlangTopType100TImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitTopType100T(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangTopType100T getTopType100T() {
    return findChildByClass(ErlangTopType100T.class);
  }

  @Override
  @NotNull
  public ErlangType getType() {
    return findNotNullChildByClass(ErlangType.class);
  }

}
