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

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.ObjectUtils;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.getNameIdentifier;
import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.getTextRangeForReference;

public class ErlangTypeReferenceImpl extends ErlangQAtomBasedReferenceImpl {
  @Nullable
  private final ErlangModuleRef myModuleRef;

  ErlangTypeReferenceImpl(@NotNull ErlangCompositeElement owner,
                          @NotNull ErlangQAtom element,
                          @Nullable ErlangModuleRef moduleRef) {
    super(owner, element, getTextRangeForReference(element), getNameIdentifier(element).getText());
    myModuleRef = moduleRef;
  }

  @Override
  public PsiElement resolveInner() {
    PsiFile containingFile = getPsiFile();
    if (containingFile instanceof ErlangFile) {
      ErlangTypeDefinition type = ((ErlangFile) containingFile).getType(myReferenceName);
      if (type != null) return type;
      return ContainerUtil.getFirstItem(ErlangPsiImplUtil.getErlangTypeFromIncludes((ErlangFile) containingFile, false, myReferenceName));
    }
    return null;
  }

  @NotNull
  private PsiFile getPsiFile() {
    PsiElement resolve = myModuleRef != null ? myModuleRef.getReference().resolve() : null;
    PsiFile moduleRefContainingFile = resolve != null ? resolve.getContainingFile() : null;
    return moduleRefContainingFile != null ? moduleRefContainingFile : myElement.getContainingFile();
  }

  @NotNull
  @Override
  public Object @NotNull [] getVariants() {
    ErlangExportTypeAttribute exportTypeAttribute = PsiTreeUtil.getParentOfType(myQAtom, ErlangExportTypeAttribute.class);
    ErlangFile moduleFile = null;
    if (myModuleRef != null) {
      PsiElement resolve = myModuleRef.getReference().resolve();
      moduleFile = resolve != null ? ObjectUtils.tryCast(resolve.getContainingFile(), ErlangFile.class) : null;
      if (moduleFile == null) return ArrayUtil.EMPTY_OBJECT_ARRAY;
    }
    PsiFile psiFile = moduleFile != null ? moduleFile : myElement.getContainingFile();
    boolean exportedOnly = moduleFile != null && !isSameModule(moduleFile);
    return ArrayUtil.toObjectArray(ErlangPsiImplUtil.getTypeLookupElements(psiFile, myModuleRef == null && exportTypeAttribute == null, exportTypeAttribute != null, exportedOnly));
  }

  private boolean isSameModule(@NotNull ErlangFile moduleFile) {
    PsiFile currentFile = myElement.getContainingFile().getOriginalFile();
    return moduleFile.getOriginalFile() == currentFile || ErlangPsiImplUtil.getIncludedFiles(moduleFile).contains(currentFile);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof ErlangTypeReferenceImpl)) return false;
    if (!super.equals(o)) return false;
    return !(myModuleRef != null && !myModuleRef.equals(((ErlangTypeReferenceImpl) o).myModuleRef));
  }

  @Override
  public int hashCode() {
    int result = super.hashCode();
    return myModuleRef != null ? 31 * result + myModuleRef.hashCode() : result;
  }
}
