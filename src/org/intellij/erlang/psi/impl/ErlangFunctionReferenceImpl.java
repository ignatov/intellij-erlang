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

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.psi.*;
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
    if (suppressResolve()) return null; // for #132

    if (myModuleAtom != null) {
      final ErlangFunction explicitFunction = getExternalFunction(getModuleFileName());
      if (explicitFunction != null) {
        return explicitFunction;
      }
      else if (ErlangBifTable.isBif(myModuleAtom.getText(), myReferenceName, myArity)) {
        return getElement();
      }
      else {
        return null;
      }
    }

    PsiFile containingFile = getElement().getContainingFile();
    if (containingFile instanceof ErlangFile) {
      ErlangFile erlangFile = (ErlangFile) containingFile;

      ErlangFunction result = erlangFile.getFunction(myReferenceName, myArity);
      if (result != null) return result;

      final ErlangFunction implicitFunction = getExternalFunction("erlang.erl");
      if (implicitFunction != null) return implicitFunction;

      for (ErlangImportFunction importFunction : erlangFile.getImportedFunctions()) {
        PsiReference reference = importFunction.getReference();
        PsiElement resolve = reference != null ? reference.resolve() : null;
        if (resolve instanceof ErlangFunction) {
          ErlangFunction function = (ErlangFunction) resolve;
          if (function.getName().equals(myReferenceName) && function.getArity() == myArity) {
            return function;
          }
        }
      }
      return ContainerUtil.getFirstItem(ErlangPsiImplUtil.getErlangFunctionsFromIncludes((ErlangFile) containingFile, false, myReferenceName, myArity));
    }
    return null;
  }

  @NotNull
  @Override
  public ResolveResult[] multiResolve(boolean incompleteCode) {
    if (suppressResolve()) return ResolveResult.EMPTY_ARRAY; // for #132

    // todo: use incompleteCode
    if (resolve() != null) return ResolveResult.EMPTY_ARRAY;

    Collection<ErlangFunction> result;
    if (myModuleAtom != null) {
      result = getErlangFunctionsFromModule(getModuleFileName());
    }
    else {
      PsiFile containingFile = getElement().getContainingFile();
      if (containingFile instanceof ErlangFile) {
        ErlangFile erlangFile = (ErlangFile) containingFile;
        result = new ArrayList<ErlangFunction>();

        for (ErlangImportFunction importFunction : erlangFile.getImportedFunctions()) {
          PsiReference reference = importFunction.getReference();
          PsiElement resolve = reference != null ? reference.resolve() : null;
          if (resolve instanceof ErlangFunction && ((ErlangFunction) resolve).getName().equals(myReferenceName)) {
            result.add((ErlangFunction) resolve);
          }
        }

        result.addAll(erlangFile.getFunctionsByName(myReferenceName));
        result.addAll(getErlangFunctionsFromModule("erlang.erl"));
      }
      else {
        result = ContainerUtil.emptyList();
      }
    }
    return PsiElementResolveResult.createResults(result);
  }

  private Collection<ErlangFunction> getErlangFunctionsFromModule(String moduleFileName) {
    Collection<ErlangFunction> result;PsiFile[] files = FilenameIndex.getFilesByName(getElement().getProject(), moduleFileName,
      GlobalSearchScope.allScope(getElement().getProject()));
    result = new ArrayList<ErlangFunction>();
    for (PsiFile file : files) {
      if (file instanceof ErlangFile) {
        result.addAll(((ErlangFile) file).getFunctionsByName(myReferenceName));
      }
    }
    return result;
  }

  private boolean suppressResolve() {
    return PsiTreeUtil.getParentOfType(myElement, ErlangCallbackSpec.class) != null;
  }

  @NotNull
  private String getModuleFileName() {
    if (myModuleAtom != null) {
      return myModuleAtom.getText() + ".erl";
    }
    return ".erl";
  }

  @Override
  public boolean isReferenceTo(PsiElement element) {
    return getElement().getManager().areElementsEquivalent(resolve(), element);
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
    return ArrayUtil.toObjectArray(ErlangPsiImplUtil.getFunctionLookupElements(getElement().getContainingFile(), true, myModuleAtom));
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
