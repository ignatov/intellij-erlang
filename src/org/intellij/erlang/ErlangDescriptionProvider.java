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
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.util.RefactoringDescriptionLocation;
import com.intellij.usageView.UsageViewLongNameLocation;
import com.intellij.usageView.UsageViewNodeTextLocation;
import com.intellij.usageView.UsageViewShortNameLocation;
import com.intellij.usageView.UsageViewTypeLocation;
import org.intellij.erlang.macrolayer.ErlangMacroLayer;
import org.intellij.erlang.macrolayer.ErlangMacroLayerHeaderTarget;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

public class ErlangDescriptionProvider implements ElementDescriptionProvider {
  private final static Logger LOG = Logger.getInstance(ErlangDescriptionProvider.class);

  @Override
  public String getElementDescription(@NotNull PsiElement o, @NotNull ElementDescriptionLocation location) {
    if (o instanceof ErlangMacroLayerHeaderTarget || ErlangMacroLayer.isNameMadeByMacro(o)) {
      return ErlangMacroLayer.describeLayerTarget(o, location);
    }
    if (!(o instanceof ErlangCompositeElement)) return null;
    // a macro use (?OTP_RELEASE, ?MODULE, an undefined macro): quick documentation takes it as its target when it
    // resolves to no -define; master logged "Unexpected element" for it
    if (o instanceof ErlangMacros || o instanceof ErlangMacrosName) {
      if (location == UsageViewTypeLocation.INSTANCE || location == RefactoringDescriptionLocation.WITH_PARENT ||
          location instanceof DeleteTypeDescriptionLocation) {
        return "macro";
      }
      return o.getText();
    }
    if (location == UsageViewNodeTextLocation.INSTANCE && (o instanceof ErlangNamedElement || o instanceof ErlangQAtom || o instanceof ErlangTypeRef || isFunctionReferenceOwner(o))) {
      return getElementDescription(o, UsageViewShortNameLocation.INSTANCE);
    }
    if (location == UsageViewShortNameLocation.INSTANCE ||
        location == UsageViewLongNameLocation.INSTANCE ||
        location instanceof DeleteNameDescriptionLocation
    ) {
      if (o instanceof ErlangNamedElement) return ((ErlangNamedElement) o).getName();
      if (o instanceof ErlangQAtom) return ErlangPsiImplUtil.getName((ErlangQAtom)o);
      if (o instanceof ErlangTypeRef) return o.getText();
      if (isFunctionReferenceOwner(o)) return describeFunctionReferenceOwner(o);
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
      else if (isFunctionReferenceOwner(o)) return "function";
      else if (o instanceof ErlangSpecification) return "specification";
    }
    // any other element or location: the platform's default description (master logged an error and said "<unknown>",
    // which the platform's presentation of a documentation or navigation target can reach with any Erlang element)
    LOG.debug("No description of " + o.getClass().getSimpleName() + " for " + location.getClass().getSimpleName());
    return null;
  }

  // name/arity, not the raw text of the owner, which keeps the arguments of a call and the line breaks and spaces of
  // the source (#1467); a spec gives its name only
  @NotNull
  private static String describeFunctionReferenceOwner(@NotNull PsiElement o) {
    PsiReference reference = o instanceof ErlangSpecFun ? null : o.getReference();
    if (reference instanceof ErlangFunctionReference ref && ref.getArity() >= 0) {
      // quoted only when the atom needs quotes, as erlc writes it: 'abs'(1) gives abs/1, erlang:'+'(1, 2) gives '+'/2
      return ErlangPsiImplUtil.toAtomName(ref.getName()) != null ? ref.getSignature() : ref.getName() + "/" + ref.getArity();
    }
    ErlangQAtom qAtom = PsiTreeUtil.getChildOfType(o, ErlangQAtom.class);
    return qAtom != null ? qAtom.getText() : o.getText();
  }

  private static boolean isFunctionReferenceOwner(@NotNull PsiElement o) {
    return o instanceof ErlangFunctionCallExpression ||
           o instanceof ErlangFunctionWithArity ||
           o instanceof ErlangExportFunction ||
           o instanceof ErlangImportFunction ||
           o instanceof ErlangCallbackFunction ||
           o instanceof ErlangSpecFun;
  }
}
