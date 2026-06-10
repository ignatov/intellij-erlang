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

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.ObjectUtils;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangExportRecordAttribute;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangImportRecordAttribute;
import org.intellij.erlang.psi.ErlangModuleRef;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.psi.ErlangRecordDefinition;
import org.intellij.erlang.psi.ErlangRecordRef;
import org.jetbrains.annotations.NotNull;

public class ErlangRecordReferenceImpl extends ErlangQAtomBasedReferenceImpl {
  public ErlangRecordReferenceImpl(@NotNull PsiElement owner, @NotNull ErlangQAtom element) {
    super(owner, element, getTextRangeForRecordReference(owner, element), ErlangPsiImplUtil.getNameIdentifier(element).getText());
  }

  @Override
  public PsiElement resolveInner() {
    ErlangRecordRef recordRef = ObjectUtils.tryCast(myElement, ErlangRecordRef.class);
    if (recordRef != null) {
      ErlangImportRecordAttribute importRecordAttribute = PsiTreeUtil.getParentOfType(recordRef, ErlangImportRecordAttribute.class);
      if (importRecordAttribute != null) {
        return ErlangPsiImplUtil.resolveImportedRecord(importRecordAttribute, myReferenceName);
      }

      ErlangModuleRef moduleRef = recordRef.getModuleRef();
      if (moduleRef != null &&
          PsiTreeUtil.getParentOfType(recordRef, ErlangExportRecordAttribute.class) == null) {
        return ErlangPsiImplUtil.resolveRecordFromModule(moduleRef, myReferenceName);
      }
    }

    PsiFile containingFile = myElement.getContainingFile();
    if (containingFile instanceof ErlangFile) {
      ErlangRecordDefinition record = ((ErlangFile) containingFile).getRecord(myReferenceName);
      if (record != null) return record;

      record = ContainerUtil.getFirstItem(ErlangPsiImplUtil.getErlangRecordFromIncludes((ErlangFile) containingFile, false, myReferenceName));
      if (record != null) return record;

      return ErlangPsiImplUtil.getImportedRecord((ErlangFile) containingFile, myReferenceName);
    }
    return null;
  }

  @NotNull
  @Override
  public Object @NotNull [] getVariants() {
    ErlangRecordRef recordRef = ObjectUtils.tryCast(myElement, ErlangRecordRef.class);
    if (recordRef != null) {
      return ArrayUtil.toObjectArray(ErlangPsiImplUtil.getRecordLookupElements(recordRef));
    }
    return ArrayUtil.toObjectArray(ErlangPsiImplUtil.getRecordLookupElements(myElement.getContainingFile()));
  }

  @NotNull
  private static TextRange getTextRangeForRecordReference(@NotNull PsiElement owner, @NotNull ErlangQAtom atom) {
    return atom.getMacros() != null ? TextRange.from(0, 1) : ErlangPsiImplUtil.getTextRangeForReference(owner, atom);
  }
}
