/*
 * Copyright 2012-2014 Sergey Ignatov
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

import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.psi.*;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangModuleIndex;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class ErlangFunctionReferenceImpl<T extends ErlangQAtom> extends PsiPolyVariantReferenceBase<T> {
  @Nullable
  private final ErlangQAtom myModuleAtom;
  protected final String myReferenceName;
  private final int myArity;

  public ErlangFunctionReferenceImpl(@NotNull T element, @Nullable ErlangQAtom moduleAtom, int arity) {
    super(element, ErlangPsiImplUtil.getTextRangeForReference(element));
    myReferenceName = ErlangPsiImplUtil.getNameIdentifier(element).getText();
    myModuleAtom = moduleAtom;
    myArity = arity;
  }

  @Override
  public PsiElement resolve() {
    if (suppressResolve()) return null; // for #132

    PsiFile file = getElement().getContainingFile().getOriginalFile();

    if (myModuleAtom != null) {
      ErlangFunction explicitFunction = getExternalFunction(getModuleFileName());
      if (explicitFunction != null) {
        return explicitFunction;
      }
      else if (ErlangBifTable.isBif(myModuleAtom.getText(), myReferenceName, myArity)) {
        return getElement();
      }
      else if (myReferenceName.equals(ErlangBifTable.MODULE_INFO) && (myArity == 1 || myArity == 0)) {
        return getElement();
      }
      else {
        return null;
      }
    }

    if (file instanceof ErlangFile) {
      ErlangFile erlangFile = (ErlangFile) file;

      ErlangFunction result = erlangFile.getFunction(myReferenceName, myArity);
      if (result != null) return result;

      ErlangFunction implicitFunction = getExternalFunction("erlang");
      if (implicitFunction != null) return implicitFunction;

      Module module = ModuleUtilCore.findModuleForPsiElement(erlangFile);
      Sdk sdk = module == null ? null : ModuleRootManager.getInstance(module).getSdk();
      ErlangSdkRelease release = sdk != null ? ErlangSdkType.getRelease(sdk) : null;

      if ((release == null || release.needBifCompletion("erlang")) && ErlangBifTable.isBif("erlang", myReferenceName, myArity)) {
        return getElement();
      }

      if (ErlangBifTable.isBif("", myReferenceName, myArity)) {
        return getElement();
      }

      for (ErlangImportFunction importFunction : erlangFile.getImportedFunctions()) {
        PsiReference reference = importFunction.getReference();
        PsiElement resolve = reference.resolve();
        if (resolve instanceof ErlangFunction) {
          ErlangFunction function = (ErlangFunction) resolve;
          if (function.getName().equals(myReferenceName) && function.getArity() == myArity) {
            return function;
          }
        }
      }
      return ContainerUtil.getFirstItem(ErlangPsiImplUtil.getErlangFunctionsFromIncludes((ErlangFile) file, false, myReferenceName, myArity));
    }
    return null;
  }

  @NotNull
  @Override
  public ResolveResult[] multiResolve(boolean incompleteCode) {
    if (suppressResolve()) return ResolveResult.EMPTY_ARRAY; // for #132

    // todo: use incompleteCode
    if (resolve() != null && !incompleteCode) return ResolveResult.EMPTY_ARRAY;

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
          PsiElement resolve = reference.resolve();
          if (resolve instanceof ErlangFunction && ((ErlangFunction) resolve).getName().equals(myReferenceName)) {
            result.add((ErlangFunction) resolve);
          }
        }

        result.addAll(erlangFile.getFunctionsByName(myReferenceName));
        result.addAll(getErlangFunctionsFromModule("erlang"));
      }
      else {
        result = ContainerUtil.emptyList();
      }
    }
    return PsiElementResolveResult.createResults(result);
  }

  private Collection<ErlangFunction> getErlangFunctionsFromModule(String moduleFileName) {
    Project project = getElement().getProject();
    Collection<ErlangFunction> result = new ArrayList<ErlangFunction>();
    for (ErlangFile file : ErlangModuleIndex.getFilesByName(project, moduleFileName, GlobalSearchScope.allScope(project))) {
      result.addAll(file.getFunctionsByName(myReferenceName));
    }
    return result;
  }

  private boolean suppressResolve() {
    return PsiTreeUtil.getParentOfType(myElement, ErlangCallbackSpec.class) != null;
  }

  @NotNull
  private String getModuleFileName() {
    return myModuleAtom != null ? ErlangPsiImplUtil.getName(myModuleAtom) : "";
  }

  @Override
  public boolean isReferenceTo(PsiElement element) {
    return getElement().getManager().areElementsEquivalent(resolve(), element);
  }

  @Nullable
  private ErlangFunction getExternalFunction(@NotNull String moduleFileName) {
    Project project = getElement().getProject();
    List<ErlangFunction> result = new ArrayList<ErlangFunction>();
    for (ErlangFile file : ErlangModuleIndex.getFilesByName(project, moduleFileName, GlobalSearchScope.allScope(project))) {
      ContainerUtil.addAllNotNull(result, file.getFunction(myReferenceName, myArity));
      ContainerUtil.addAllNotNull(result, ErlangPsiImplUtil.getErlangFunctionsFromIncludes(file, false, myReferenceName, myArity));
    }
    return ContainerUtil.getFirstItem(result);
  }

  @NotNull
  @Override
  public Object[] getVariants() {
    if (PsiTreeUtil.getParentOfType(myElement, ErlangExportFunction.class) != null) return EMPTY_ARRAY;
    return ArrayUtil.toObjectArray(ErlangPsiImplUtil.getFunctionLookupElements(getElement().getContainingFile(), true, myModuleAtom));
  }

  @Override
  public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
    ErlangPsiImplUtil.renameAtom(getElement().getAtom(), newElementName);
    return getElement();
  }

  public String getSignature() {
    return myReferenceName + "/" + myArity;
  }

  public String getName() {
    return myReferenceName;
  }

  public int getArity() {
    return myArity;
  }
}
