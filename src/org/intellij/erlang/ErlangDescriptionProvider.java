/*
 * Copyright 2012-2015 Sergey Ignatov
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

import com.intellij.codeInsight.highlighting.HighlightUsagesDescriptionLocation;
import com.intellij.ide.util.DeleteNameDescriptionLocation;
import com.intellij.ide.util.DeleteTypeDescriptionLocation;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.psi.ElementDescriptionLocation;
import com.intellij.psi.ElementDescriptionProvider;
import com.intellij.psi.PsiElement;
import com.intellij.refactoring.util.RefactoringDescriptionLocation;
import com.intellij.usageView.UsageViewLongNameLocation;
import com.intellij.usageView.UsageViewNodeTextLocation;
import com.intellij.usageView.UsageViewShortNameLocation;
import com.intellij.usageView.UsageViewTypeLocation;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

public class ErlangDescriptionProvider implements ElementDescriptionProvider {
  private final static Logger LOG = Logger.getInstance(ErlangDescriptionProvider.class);

  @Override
  public String getElementDescription(@NotNull PsiElement el, @NotNull ElementDescriptionLocation location) {
    if (!(el instanceof ErlangCompositeElement)) return null;

    if (location == UsageViewNodeTextLocation.INSTANCE
        && (el instanceof ErlangNamedElement
            || el instanceof ErlangQAtom
            || el instanceof ErlangTypeRef)) {
      return getElementDescription(el, UsageViewShortNameLocation.INSTANCE);
    }

    if (location == UsageViewShortNameLocation.INSTANCE
        || location == UsageViewLongNameLocation.INSTANCE
        || location instanceof DeleteNameDescriptionLocation
    ) {
      if (el instanceof ErlangNamedElement) return ((ErlangNamedElement) el).getName();
      if (el instanceof ErlangQAtom) return ErlangPsiImplUtil.getName((ErlangQAtom)el);
      if (el instanceof ErlangTypeRef) return el.getText();
      if (el instanceof ErlangAttribute) {
        ErlangSpecification spec = ((ErlangAttribute) el).getSpecification();
        if (spec != null) return spec.getName();
      }
    }

    if (location == HighlightUsagesDescriptionLocation.INSTANCE) {
      return getElementDescription(el, UsageViewShortNameLocation.INSTANCE);
    }

    if (location == UsageViewTypeLocation.INSTANCE
        || location == RefactoringDescriptionLocation.WITH_PARENT
        || location instanceof DeleteTypeDescriptionLocation
        || location instanceof UsageViewNodeTextLocation
    ) {
      if (el instanceof ErlangModule) return "module";
      else if (el instanceof ErlangFunction) return "function";
      else if (el instanceof ErlangRecordDefinition) return "record";
      else if (el instanceof ErlangQVar) return "variable";
      else if (el instanceof ErlangMacrosDefinition) return "macro";
      else if (el instanceof ErlangTypedExpr) return "record field";
      else if (el instanceof ErlangTypeDefinition) return "type";
      else if (el instanceof ErlangAttribute) return "attribute";
      else if (el instanceof ErlangQAtom
               || el instanceof ErlangAtom) return "atom";
      else if (el instanceof ErlangFunctionWithArity) return "function reference with arity";
      else if (el instanceof ErlangFunExpression) return "inline function definition";
      else if (el instanceof ErlangFunRefExpression) return "function reference";
      else if (el instanceof ErlangModuleRef) return "module reference";
      else if (el instanceof ErlangTypeRef) return "type";
    }

    LOG.error("ErlangDescriptionProvider: Unexpected element `%s`, class=`%s`, location=`%s`"
                .formatted(el.getText(), el.getClass(), location.getClass()));
    return "<unknown>";
  }
}
