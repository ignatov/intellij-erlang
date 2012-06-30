package org.intellij.erlang.psi.impl;

import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReferenceBase;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangQAtom;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * @author ignatov
 */
public class ErlangFunctionReferenceImpl<T extends ErlangQAtom> extends PsiReferenceBase<T> {
  private String myReferenceName;
  private int myArity;

  public ErlangFunctionReferenceImpl(@NotNull T element, TextRange range, String name, int arity) {
    super(element, range);
    myReferenceName = name;
    myArity = arity;
  }

  @Override
  public PsiElement resolve() {
    PsiFile containingFile = myElement.getContainingFile();
    return containingFile instanceof ErlangFile ? ((ErlangFile) containingFile).getFunction(myReferenceName, myArity) : null;
  }

  @NotNull
  @Override
  public Object[] getVariants() {
    List<LookupElement> lookupElements = ErlangPsiImplUtil.getFunctionLookupElements(myElement.getContainingFile());
    return ArrayUtil.toObjectArray(lookupElements);
  }

  @Override
  public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
    PsiElement atomFromText = ErlangElementFactory.createQAtomFromText(getElement().getProject(), newElementName);
    myElement.getAtom().replace(atomFromText);
    return myElement;
  }
}
