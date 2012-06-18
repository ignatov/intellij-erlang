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

public class ErlangAttributeImpl extends ErlangCompositeElementImpl implements ErlangAttribute {

  public ErlangAttributeImpl(ASTNode node) {
    super(node);
  }

  @Override
  @Nullable
  public ErlangAtomAttribute getAtomAttribute() {
    return findChildByClass(ErlangAtomAttribute.class);
  }

  @Override
  @Nullable
  public ErlangCallbackSpec getCallbackSpec() {
    return findChildByClass(ErlangCallbackSpec.class);
  }

  @Override
  @Nullable
  public ErlangSpecification getSpecification() {
    return findChildByClass(ErlangSpecification.class);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitAttribute(this);
    else super.accept(visitor);
  }

}
