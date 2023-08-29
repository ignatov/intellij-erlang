/*
 * Copyright 2012-2019 Sergey Ignatov
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

import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.ObjectUtils;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.bif.ErlangOperatorTable;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class ErlangFunctionReferenceImpl extends ErlangPsiPolyVariantCachingReferenceBase<PsiElement> implements ErlangFunctionReference {
  @NotNull
  private final ErlangQAtom myQAtom;
  @Nullable
  private final ErlangQAtom myModuleAtom;
  private final String myReferenceName;
  private final int myArity;

  public ErlangFunctionReferenceImpl(@NotNull PsiElement owner,
                                     @NotNull ErlangQAtom qAtom,
                                     @Nullable ErlangQAtom moduleAtom,
                                     int arity) {
    super(owner, ErlangPsiImplUtil.getTextRangeForReference(owner, qAtom));
    myReferenceName = ErlangPsiImplUtil.getNameIdentifier(qAtom).getText();
    myQAtom = qAtom;
    myModuleAtom = moduleAtom;
    myArity = arity;
  }

  @Override
  public PsiElement resolveInner() {
    if (suppressResolve()) return null; // for #132

    if (myModuleAtom != null) {
      String moduleName = ErlangPsiImplUtil.getName(myModuleAtom);
      ErlangFunction explicitFunction = getExternalFunction(moduleName);
      boolean resolveToCallSite = explicitFunction == null && (
        ErlangBifTable.isBif(moduleName, myReferenceName, myArity) ||
        ErlangOperatorTable.canBeInvokedAsFunction(moduleName, myReferenceName, myArity) ||
        myReferenceName.equals(ErlangBifTable.MODULE_INFO) && (myArity == 1 || myArity == 0)
      );
      return resolveToCallSite ? getElement() : explicitFunction;
    }

    ErlangFile file = ObjectUtils.tryCast(getElement().getContainingFile(), ErlangFile.class);
    if (file == null) return null;

    ErlangFunction declaredFunction = file.getFunction(myReferenceName, myArity);
    if (declaredFunction != null) return declaredFunction;

    ErlangFunction fromImport = resolveImport(file.getImportedFunction(myReferenceName, myArity));
    if (fromImport != null) return fromImport;

    List<ErlangFunction> declaredInIncludes =
      ErlangPsiImplUtil.getErlangFunctionsFromIncludes(file, false, myReferenceName, myArity);
    if (!declaredInIncludes.isEmpty()) return ContainerUtil.getFirstItem(declaredInIncludes);

    List<ErlangImportFunction> importedInIncludes =
      ErlangPsiImplUtil.getImportsFromIncludes(file, false, myReferenceName, myArity);
    for (ErlangImportFunction importFromInclude : importedInIncludes) {
      ErlangFunction importedFunction = resolveImport(importFromInclude);
      if (importedFunction != null) return importedFunction;
    }

    if (!file.isNoAutoImport(myReferenceName, myArity)) {
      ErlangFunction implicitFunction = getExternalFunction("erlang");
      if (implicitFunction != null) return implicitFunction;

      ErlangSdkRelease release = ErlangSdkType.getRelease(file);
      if ((release == null || release.needBifCompletion("erlang")) &&
          ErlangBifTable.isBif("erlang", myReferenceName, myArity) ||
          ErlangBifTable.isBif("", myReferenceName, myArity)) return getElement();
    }

    return null;
  }

  @NotNull
  @Override
  public ResolveResult @NotNull [] multiResolve(boolean incompleteCode) {
    if (suppressResolve()) return ResolveResult.EMPTY_ARRAY; // for #132

    // todo: use incompleteCode
    if (resolve() != null && !incompleteCode) return ResolveResult.EMPTY_ARRAY;

    Collection<ErlangFunction> result;
    if (myModuleAtom != null) {
      result = getErlangFunctionsFromModule(ErlangPsiImplUtil.getName(myModuleAtom));
    }
    else {
      PsiFile containingFile = getElement().getContainingFile();
      if (containingFile instanceof ErlangFile erlangFile) {
        result = new ArrayList<>();

        for (ErlangImportFunction importFunction : erlangFile.getImportedFunctions()) {
          if (myReferenceName.equals(ErlangPsiImplUtil.getName(importFunction))) {
            ContainerUtil.addIfNotNull(result, resolveImport(importFunction));
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
    Collection<ErlangFunction> result = new ArrayList<>();
    for (ErlangFile file : ErlangModuleIndex.getFilesByName(project, moduleFileName, GlobalSearchScope.allScope(project))) {
      result.addAll(file.getFunctionsByName(myReferenceName));
    }
    return result;
  }

  private boolean suppressResolve() {
    return PsiTreeUtil.getParentOfType(myQAtom, ErlangCallbackSpec.class) != null;
  }

  @Override
  public boolean isReferenceTo(@NotNull PsiElement element) {
    return getElement().getManager().areElementsEquivalent(resolve(), element);
  }

  @Nullable
  private ErlangFunction getExternalFunction(@NotNull String moduleFileName) {
    Project project = getElement().getProject();
    List<ErlangFunction> result = new ArrayList<>();
    for (ErlangFile file : ErlangModuleIndex.getFilesByName(project, moduleFileName, GlobalSearchScope.allScope(project))) {
      ContainerUtil.addAllNotNull(result, file.getFunction(myReferenceName, myArity));
      ContainerUtil.addAllNotNull(result, ErlangPsiImplUtil.getErlangFunctionsFromIncludes(file, false, myReferenceName, myArity));
    }
    return ContainerUtil.getFirstItem(result);
  }

  @NotNull
  @Override
  public Object @NotNull [] getVariants() {
    if (PsiTreeUtil.getParentOfType(myQAtom, ErlangExportFunction.class) != null) return EMPTY_ARRAY;
    return ArrayUtil.toObjectArray(ErlangPsiImplUtil.getFunctionLookupElements(getElement().getContainingFile(), true, myModuleAtom));
  }

  @Override
  public PsiElement handleElementRename(@NotNull String newElementName) throws IncorrectOperationException {
    ErlangPsiImplUtil.renameAtom(myQAtom.getAtom(), newElementName);
    return getElement();
  }

  @Override
  public String getSignature() {
    return ErlangPsiImplUtil.createFunctionPresentation(myReferenceName, myArity);
  }

  @Override
  public String getName() {
    return myReferenceName;
  }

  @Override
  public int getArity() {
    return myArity;
  }

  @Nullable
  private static ErlangFunction resolveImport(@Nullable ErlangImportFunction importFunction) {
    PsiReference reference = importFunction != null ? importFunction.getReference() : null;
    PsiElement resolve = reference != null ? reference.resolve() : null;
    return ObjectUtils.tryCast(resolve, ErlangFunction.class);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof ErlangFunctionReferenceImpl)) return false;
    if (!super.equals(o)) return false;
    return !(myModuleAtom != null && !myModuleAtom.equals(((ErlangFunctionReferenceImpl) o).myModuleAtom));
  }

  @Override
  public int hashCode() {
    return myModuleAtom != null ? 31 * super.hashCode() + myModuleAtom.hashCode() : super.hashCode();
  }
}
