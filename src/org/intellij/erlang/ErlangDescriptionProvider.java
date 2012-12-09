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

package org.intellij.erlang;

import com.intellij.psi.ElementDescriptionLocation;
import com.intellij.psi.ElementDescriptionProvider;
import com.intellij.psi.PsiElement;
import com.intellij.usageView.UsageViewLongNameLocation;
import com.intellij.usageView.UsageViewNodeTextLocation;
import com.intellij.usageView.UsageViewShortNameLocation;
import com.intellij.usageView.UsageViewTypeLocation;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangDescriptionProvider implements ElementDescriptionProvider {
  @Override
  public String getElementDescription(@NotNull PsiElement psiElement, @NotNull ElementDescriptionLocation location) {
    if (location == UsageViewNodeTextLocation.INSTANCE && psiElement instanceof ErlangNamedElement) {
      return getElementDescription(psiElement, UsageViewTypeLocation.INSTANCE) + " " +
        "'" + getElementDescription(psiElement, UsageViewShortNameLocation.INSTANCE) + "'";
    }
    if ((location == UsageViewShortNameLocation.INSTANCE || location == UsageViewLongNameLocation.INSTANCE) && psiElement instanceof ErlangNamedElement) {
      return ((ErlangNamedElement) psiElement).getName();
    }
    if (location == UsageViewTypeLocation.INSTANCE) {
      if (psiElement instanceof ErlangModule) return "Module";
      if (psiElement instanceof ErlangFunction) return "Function";
      if (psiElement instanceof ErlangRecordDefinition) return "Record";
      if (psiElement instanceof ErlangQVar) return "Variable";
      if (psiElement instanceof ErlangMacrosDefinition) return "Macros";
      if (psiElement instanceof ErlangTypedExpr) return "Record field";
      if (psiElement instanceof ErlangTypeDefinition) return "Type";
    }
    return null;
  }
}
