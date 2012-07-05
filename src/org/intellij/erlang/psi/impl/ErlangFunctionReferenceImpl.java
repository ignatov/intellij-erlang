package org.intellij.erlang.psi.impl;

import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReferenceBase;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangQAtom;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * @author ignatov
 */
public class ErlangFunctionReferenceImpl<T extends ErlangQAtom> extends PsiReferenceBase<T> {
  @Nullable
  private ErlangQAtom myModuleAtom;
  private String myReferenceName;
  private int myArity;

  public ErlangFunctionReferenceImpl(@NotNull T element, @Nullable ErlangQAtom moduleAtom, TextRange range, String name, int arity) {
    super(element, range);
    myModuleAtom = moduleAtom;
    myReferenceName = name;
    myArity = arity;
  }

  @Override
  public PsiElement resolve() {
    if (myModuleAtom != null) {
      return getExternalFunction(myModuleAtom.getText() + ".erl");
    }

    PsiFile containingFile = myElement.getContainingFile();
    return containingFile instanceof ErlangFile ? ((ErlangFile) containingFile).getFunction(myReferenceName, myArity) : null;
  }

  @Nullable
  private ErlangFunction getExternalFunction(@NotNull String name) {
    PsiFile[] files = FilenameIndex.getFilesByName(myElement.getProject(), name, GlobalSearchScope.projectScope(myElement.getProject()));
    List<ErlangFunction> result = new ArrayList<ErlangFunction>();
    for (PsiFile file : files) {
      if (file instanceof ErlangFile) {
        result.add(((ErlangFile) file).getFunction(myReferenceName, myArity));
      }
    }
    return result.isEmpty() ? null : result.get(0);
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
