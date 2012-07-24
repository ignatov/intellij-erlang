package org.intellij.erlang.psi;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;

/**
 * @author ignatov
 */
public interface ErlangCompositeElement extends PsiElement {
  void setReference(PsiReference reference);
}
