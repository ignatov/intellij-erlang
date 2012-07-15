package org.intellij.erlang.psi.impl;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReferenceBase;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangQAtom;

/**
 * @author ignatov
 */
public abstract class ErlangAtomBasedReferenceImpl<T extends ErlangQAtom> extends PsiReferenceBase<T> {
  protected String myReferenceName;

  public ErlangAtomBasedReferenceImpl(T element, TextRange range, String name) {
    super(element, range);
    myReferenceName = name;
  }

  @Override
  public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
    myElement.getAtom().replace(ErlangElementFactory.createQAtomFromText(getElement().getProject(), newElementName));
    return myElement;
  }
}
