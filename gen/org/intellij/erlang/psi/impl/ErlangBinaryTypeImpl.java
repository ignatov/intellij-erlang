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

public class ErlangBinaryTypeImpl extends ErlangCompositeElementImpl implements ErlangBinaryType {

  public ErlangBinaryTypeImpl(ASTNode node) {
    super(node);
  }

  @Override
  @Nullable
  public ErlangBinBaseType getBinBaseType() {
    return findChildByClass(ErlangBinBaseType.class);
  }

  @Override
  @Nullable
  public ErlangBinUnitType getBinUnitType() {
    return findChildByClass(ErlangBinUnitType.class);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitBinaryType(this);
    else super.accept(visitor);
  }

}
