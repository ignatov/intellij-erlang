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

package org.intellij.erlang.marker;

import com.intellij.codeHighlighting.Pass;
import com.intellij.codeInsight.daemon.LineMarkerInfo;
import com.intellij.codeInsight.daemon.LineMarkerProvider;
import com.intellij.codeInsight.daemon.impl.PsiElementListNavigator;
import com.intellij.icons.AllIcons;
import com.intellij.ide.util.DefaultPsiElementCellRenderer;
import com.intellij.openapi.editor.markup.GutterIconRenderer;
import com.intellij.psi.NavigatablePsiElement;
import com.intellij.psi.PsiElement;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.navigation.ErlangNavigationUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import java.text.MessageFormat;
import java.util.Collection;
import java.util.List;

public class ErlangBehaviourMarkerProvider implements LineMarkerProvider {
  @Override
  public LineMarkerInfo getLineMarkerInfo(@NotNull PsiElement element) {
    return null;
  }

  @Override
  public void collectSlowLineMarkers(@NotNull List<PsiElement> elements, @NotNull Collection<LineMarkerInfo> result) {
    for (PsiElement element : elements) {
      if (!(element instanceof ErlangAtom)) continue;
      PsiElement parent = element.getParent();
      if (!(parent instanceof ErlangQAtom)) continue;
      PsiElement clause = parent.getParent();
      if (!(clause instanceof ErlangFunctionClause)) continue;
      PsiElement function = clause.getParent();
      if (function instanceof ErlangFunction && ContainerUtil.getFirstItem(((ErlangFunction) function).getFunctionClauseList()) == clause) {
        List<ErlangCallbackSpec> prototypes = ErlangNavigationUtil.getCallbackSpecs((ErlangFunction) function);
        if (!prototypes.isEmpty()) {
          result.add(createImplementationMarker(element, (ErlangFunction) function, prototypes));
        }
      }
    }
  }

  private static LineMarkerInfo createImplementationMarker(PsiElement atom,
                                                           ErlangFunction function,
                                                           Collection<ErlangCallbackSpec> callbackSpecs) {
    String presentation = ErlangPsiImplUtil.createFunctionPresentation(function);

    List<NavigatablePsiElement> navigatables = ContainerUtil.newArrayList();
    for (ErlangCallbackSpec callbackSpec : callbackSpecs) {
      navigatables.add(ErlangNavigationUtil.getNavigatableSpecFun(presentation, callbackSpec));
    }

    return new LineMarkerInfo<>(
      atom,
      atom.getTextRange(),
      AllIcons.Gutter.ImplementingMethod,
      Pass.UPDATE_ALL,
      element -> "Implements callback '" + presentation + "'",
      (e, elt) -> {
        String title = MessageFormat.format("<html><body>Choose Overriding Callback of <b>{0}</b> ({1} callbacks found)</body></html>", presentation, callbackSpecs.size());
        PsiElementListNavigator.openTargets(
          e, navigatables.toArray(new NavigatablePsiElement[navigatables.size()]),
          title, title, new DefaultPsiElementCellRenderer()
        );
      },
      GutterIconRenderer.Alignment.RIGHT
    );
  }
}
