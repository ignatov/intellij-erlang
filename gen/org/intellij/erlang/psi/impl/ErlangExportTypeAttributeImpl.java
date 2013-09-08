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

public class ErlangExportTypeAttributeImpl extends ErlangCompositeElementImpl implements ErlangExportTypeAttribute {

  public ErlangExportTypeAttributeImpl(ASTNode node) {
    super(node);
  }

  @Override
  @Nullable
  public ErlangExportTypes getExportTypes() {
    return findChildByClass(ErlangExportTypes.class);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitExportTypeAttribute(this);
    else super.accept(visitor);
  }

}
