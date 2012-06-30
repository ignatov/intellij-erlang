package org.intellij.erlang.psi.impl;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReferenceBase;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangQAtom;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangRecordReferenceImpl<T extends ErlangQAtom> extends PsiReferenceBase<T> {
  private String myReferenceName;

  public ErlangRecordReferenceImpl(@NotNull T element, TextRange range, String name) {
    super(element, range);
    myReferenceName = name;
  }

  @Override
  public PsiElement resolve() {
    PsiFile containingFile = myElement.getContainingFile();
    return containingFile instanceof ErlangFile ? ((ErlangFile) containingFile).getRecord(myReferenceName) : null;
  }

  @NotNull
  @Override
  public Object[] getVariants() {
    return ArrayUtil.toObjectArray(ErlangPsiImplUtil.getRecordLookupElements(myElement.getContainingFile()));
  }

  @Override
  public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
    myElement.getAtom().replace(ErlangElementFactory.createQAtomFromText(getElement().getProject(), newElementName));
    return myElement;
  }
}
