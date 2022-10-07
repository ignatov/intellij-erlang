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

package org.intellij.erlang.marker;

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzerSettings;
import com.intellij.codeInsight.daemon.LineMarkerInfo;
import com.intellij.codeInsight.daemon.LineMarkerProvider;
import com.intellij.codeInsight.daemon.impl.LineMarkersPass;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ObjectUtils;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.List;

public class ErlangMethodSeparatorProvider implements LineMarkerProvider {
  @Nullable
  @Override
  public LineMarkerInfo<?> getLineMarkerInfo(@NotNull PsiElement function) {
    return null;
  }

  @Override
  public void collectSlowLineMarkers(@NotNull List<? extends PsiElement> elements, @NotNull Collection<? super LineMarkerInfo<?>> result) {
    if (!DaemonCodeAnalyzerSettings.getInstance().SHOW_METHOD_SEPARATORS) {
      return;
    }

    for (PsiElement element : elements) {
      if (!(element instanceof LeafPsiElement)) continue;
      PsiElement atom = element.getParent();
      if (!(atom instanceof ErlangAtom)) continue;
      PsiElement parent = atom.getParent();
      if (!(parent instanceof ErlangQAtom)) continue;
      PsiElement clause = parent.getParent();
      if (!(clause instanceof ErlangFunctionClause)) continue;
      PsiElement function = clause.getParent();
      if (!(function instanceof ErlangFunction)) continue;

      PsiElement anchor = findAnchorElementForMethodSeparator((ErlangFunction) function);
      result.add(LineMarkersPass.createMethodSeparatorLineMarker(
        ObjectUtils.chooseNotNull(PsiTreeUtil.findChildOfAnyType(anchor, LeafPsiElement.class), anchor)
        , EditorColorsManager.getInstance()));
    }
  }

  @NotNull
  private static PsiElement findAnchorElementForMethodSeparator(@NotNull ErlangFunction function) {
    ErlangAttribute specAttribute = getSpecAttributeForFunction(function);
    PsiElement leftmostPossibleAnchor = function;
    PsiElement leftmostElement = function;
    while ((leftmostElement = leftmostElement.getPrevSibling()) != null) {
      if (leftmostElement instanceof PsiComment || leftmostElement == specAttribute) {
        leftmostPossibleAnchor = leftmostElement;
      }
      else if (!(leftmostElement instanceof PsiWhiteSpace)) break;
    }
    return leftmostPossibleAnchor;
  }

  @Nullable
  private static ErlangAttribute getSpecAttributeForFunction(@NotNull ErlangFunction function) {
    ErlangSpecification spec = function.findSpecification();
    ErlangFunTypeSigs signature = ErlangPsiImplUtil.getSignature(spec);
    ErlangSpecFun specFun = signature != null ? signature.getSpecFun() : null;
    PsiReference reference = specFun != null ? specFun.getReference() : null;
    boolean isSpecForPassedFunction = reference != null && reference.isReferenceTo(function);
    return isSpecForPassedFunction ? PsiTreeUtil.getParentOfType(spec, ErlangAttribute.class) : null;
  }
}
