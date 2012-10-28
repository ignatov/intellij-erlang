/*
 * Copyright 2012 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.psi.impl;

import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangQAtom;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * @author ignatov
 */
public class ErlangFunctionReferenceImpl<T extends ErlangQAtom> extends PsiPolyVariantReferenceBase<T> {
  @Nullable
  private final ErlangQAtom myModuleAtom;
  protected final String myReferenceName;
  private final int myArity;

  public ErlangFunctionReferenceImpl(@NotNull T element, @Nullable ErlangQAtom moduleAtom, TextRange range, String name, int arity) {
    super(element, range);
    myReferenceName = name;
    myModuleAtom = moduleAtom;
    myArity = arity;
  }

  @Override
  public PsiElement resolve() {
    if (myModuleAtom != null) {
      return getExternalFunction(getModuleFileName());
    }

    PsiFile containingFile = getElement().getContainingFile();
    return containingFile instanceof ErlangFile ? ((ErlangFile) containingFile).getFunction(myReferenceName, myArity) : null;
  }

  @NotNull
  @Override
  public ResolveResult[] multiResolve(boolean incompleteCode) {
    // todo: use incompleteCode

    Collection<ErlangFunction> result;
    if (myModuleAtom != null) {
      PsiFile[] files = FilenameIndex.getFilesByName(getElement().getProject(), getModuleFileName(),
        GlobalSearchScope.allScope(getElement().getProject()));
      result = new ArrayList<ErlangFunction>();
      for (PsiFile file : files) {
        if (file instanceof ErlangFile) {
          result.addAll(((ErlangFile) file).getFunctionsByName(myReferenceName));
        }
      }
    }
    else {
      PsiFile containingFile = getElement().getContainingFile();
      result = containingFile instanceof ErlangFile ? ((ErlangFile) containingFile).getFunctionsByName(myReferenceName) : ContainerUtil.<ErlangFunction>emptyList();
    }
    return PsiElementResolveResult.createResults(result);
  }

  @NotNull
  private String getModuleFileName() {
    if (myModuleAtom != null) {
      return myModuleAtom.getText() + ".erl";
    }
    return ".erl";
  }

  @Nullable
  private ErlangFunction getExternalFunction(@NotNull String moduleFileName) {
    PsiFile[] files = FilenameIndex.getFilesByName(getElement().getProject(), moduleFileName,
      GlobalSearchScope.allScope(getElement().getProject())); // todo: use module scope
    List<ErlangFunction> result = new ArrayList<ErlangFunction>();
    for (PsiFile file : files) {
      if (file instanceof ErlangFile) {
        result.add(((ErlangFile) file).getFunction(myReferenceName, myArity));
      }
    }
    return ContainerUtil.getFirstItem(result);
  }

  @NotNull
  @Override
  public Object[] getVariants() {
    List<LookupElement> lookupElements = ErlangPsiImplUtil.getFunctionLookupElements(getElement().getContainingFile(), true, null);
    return ArrayUtil.toObjectArray(lookupElements);
  }

  @Override
  public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
    PsiElement atom = getElement().getAtom();
    if (atom != null) {
      atom.replace(ErlangElementFactory.createQAtomFromText(getElement().getProject(), newElementName));
    }
    return getElement();
  }
}
