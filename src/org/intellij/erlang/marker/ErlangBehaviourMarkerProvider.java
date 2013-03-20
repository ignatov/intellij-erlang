/*
 * Copyright 2012-2013 Sergey Ignatov
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
import com.intellij.codeInsight.daemon.GutterIconNavigationHandler;
import com.intellij.codeInsight.daemon.LineMarkerInfo;
import com.intellij.codeInsight.daemon.LineMarkerProvider;
import com.intellij.codeInsight.daemon.impl.PsiElementListNavigator;
import com.intellij.icons.AllIcons;
import com.intellij.ide.util.DefaultPsiElementCellRenderer;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.editor.markup.GutterIconRenderer;
import com.intellij.psi.NavigatablePsiElement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.util.Function;
import org.intellij.erlang.ErlangIcons;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangAttrValImpl;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.event.MouseEvent;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

/**
 * @author ignatov
 */
public class ErlangBehaviourMarkerProvider implements LineMarkerProvider {

  @Override
  public LineMarkerInfo getLineMarkerInfo(@NotNull PsiElement element) {
    if (element instanceof ErlangFunction) {
      List<ErlangAttribute> prototypes = getPrototypes((ErlangFunction) element);
      if (!prototypes.isEmpty()) {
        return createImplementationMarker((ErlangFunction) element, prototypes);
      }
    }
    return null;
  }

  @Override
  public void collectSlowLineMarkers(@NotNull List<PsiElement> elements, @NotNull Collection<LineMarkerInfo> result) {
  }

  private static List<ErlangAttribute> getPrototypes(ErlangFunction function) {
    PsiFile file = function.getContainingFile();

    List<ErlangAttribute> prototypes = new LinkedList<ErlangAttribute>();

    String fullName = ErlangPsiImplUtil.createFunctionPresentation(function);
    if (file instanceof ErlangFile) {
      List<ErlangBehaviour> behaviours = ((ErlangFile) file).getBehaviours();
      for (ErlangBehaviour behaviour : behaviours) {
        ErlangModuleRef moduleRef = behaviour.getModuleRef();
        PsiReference reference = moduleRef != null ? moduleRef.getReference() : null;
        PsiElement resolve = reference != null ? reference.resolve() : null;
        PsiFile containingFile = resolve != null ? resolve.getContainingFile() : null;

        ErlangCallbackSpec callbackSpec = containingFile instanceof ErlangFile ? ((ErlangFile) containingFile).getCallbackByName(fullName) : null;
        PsiElement parent = callbackSpec != null ? callbackSpec.getParent() : null;
        if (parent instanceof ErlangAttribute) {
          prototypes.add((ErlangAttribute) parent);
        }
      }
    }
    return prototypes;
  }

  private static LineMarkerInfo createImplementationMarker(final ErlangFunction function,
                                                           final Collection<ErlangAttribute> prototypes) {
    final String presentation = ErlangPsiImplUtil.createFunctionPresentation(function);

    final List<NavigatablePsiElement> navigatables = new ArrayList<NavigatablePsiElement>();
    for (ErlangAttribute prototype : prototypes) {
      navigatables.add(
        new ErlangAttrValImpl(prototype.getNode()) {
          @Override
          public ItemPresentation getPresentation() {
            return new ItemPresentation() {
              @Nullable
              @Override
              public String getPresentableText() {
                return presentation;
              }

              @Nullable
              @Override
              public String getLocationString() {
                return getContainingFile().getName();
              }

              @Nullable
              @Override
              public Icon getIcon(boolean unused) {
                return ErlangIcons.CALLBACK;
              }
            };
          }

          @Nullable
          @Override
          public Icon getIcon(int flags) {
            return ErlangIcons.CALLBACK;
          }
        });
    }

    return new LineMarkerInfo<PsiElement>(
      function,
      function.getTextRange(),
      AllIcons.Gutter.ImplementingMethod,
      Pass.UPDATE_ALL,
      new Function<PsiElement, String>() {
        @Override
        public String fun(PsiElement element) {
          return "Implements callback '" + presentation + "'";
        }
      },
      new GutterIconNavigationHandler<PsiElement>() {
        @Override
        public void navigate(MouseEvent e, PsiElement elt) {
          String title = MessageFormat.format("<html><body>Choose Overriding Callback of <b>{0}</b> ({1} callbacks found)</body></html>", presentation, prototypes.size());
          PsiElementListNavigator.openTargets(
            e, navigatables.toArray(new NavigatablePsiElement[navigatables.size()]),
            title, title, new DefaultPsiElementCellRenderer()
          );
        }
      },
      GutterIconRenderer.Alignment.RIGHT
    );
  }


}
