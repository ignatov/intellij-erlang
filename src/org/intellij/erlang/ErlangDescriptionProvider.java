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
  public String getElementDescription(@NotNull PsiElement o, @NotNull ElementDescriptionLocation location) {
    if (!(o instanceof ErlangCompositeElement)) return null;
    if (location == UsageViewNodeTextLocation.INSTANCE && (o instanceof ErlangNamedElement || o instanceof ErlangQAtom || o instanceof ErlangTypeRef || o instanceof ErlangFunctionCallExpression)) {
      return getElementDescription(o, UsageViewShortNameLocation.INSTANCE);
    }
    if (location == UsageViewShortNameLocation.INSTANCE ||
        location == UsageViewLongNameLocation.INSTANCE ||
        location instanceof DeleteNameDescriptionLocation
    ) {
      if (o instanceof ErlangNamedElement) return ((ErlangNamedElement) o).getName();
      if (o instanceof ErlangQAtom) return ErlangPsiImplUtil.getName((ErlangQAtom)o);
      if (o instanceof ErlangTypeRef) return o.getText();
      if (o instanceof ErlangFunctionCallExpression) return o.getText();
      if (o instanceof ErlangAttribute) {
        ErlangSpecification spec = ((ErlangAttribute) o).getSpecification();
        if (spec != null) return spec.getName();
      }
    }
    if (location == HighlightUsagesDescriptionLocation.INSTANCE) {
      return getElementDescription(o, UsageViewShortNameLocation.INSTANCE);
    }
    if (location == UsageViewTypeLocation.INSTANCE ||
        location == RefactoringDescriptionLocation.WITH_PARENT ||
        location instanceof DeleteTypeDescriptionLocation
    ) {
      if (o instanceof ErlangModule) return "module";
      else if (o instanceof ErlangFunction) return "function";
      else if (o instanceof ErlangRecordDefinition) return "record";
      else if (o instanceof ErlangQVar) return "variable";
      else if (o instanceof ErlangMacrosDefinition) return "macro";
      else if (o instanceof ErlangTypedExpr) return "record field";
      else if (o instanceof ErlangTypeDefinition) return "type";
      else if (o instanceof ErlangAttribute) return "attribute";
      else if (o instanceof ErlangQAtom) return "atom";
      else if (o instanceof ErlangTypeRef) return "type";
      else if (o instanceof ErlangFunctionCallExpression) return "function";
      else if (o instanceof ErlangSpecification) return "specification";
    }
    LOG.error("Unexpected element " + o.getText() +
              ", class: " + o.getClass() +
              ", location: " + location.getClass());
    return "<unknown>";
  }
}
