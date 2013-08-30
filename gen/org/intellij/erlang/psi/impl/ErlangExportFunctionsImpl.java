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

public class ErlangExportFunctionsImpl extends ErlangCompositeElementImpl implements ErlangExportFunctions {

  public ErlangExportFunctionsImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitExportFunctions(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<ErlangExportFunction> getExportFunctionList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangExportFunction.class);
  }

}
