package org.intellij.erlang.psi;

import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;

/**
* @author ignatov
*/
public class ErlangRecursiveVisitor extends ErlangVisitor {
  @Override
  public void visitCompositeElement(@NotNull ErlangCompositeElement o) {
    for (PsiElement psiElement : o.getChildren()) {
      if (psiElement instanceof ErlangCompositeElement) {
        psiElement.accept(this);
      }
    }
  }
}
