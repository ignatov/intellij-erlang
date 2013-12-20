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

package org.intellij.erlang.navigation;

import com.intellij.codeInsight.daemon.impl.PsiElementListNavigator;
import com.intellij.ide.util.DefaultPsiElementCellRenderer;
import com.intellij.lang.LanguageCodeInsightActionHandler;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.NavigatablePsiElement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangCallbackSpec;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class ErlangGotoSuperHandler implements LanguageCodeInsightActionHandler {

  @Override
  public boolean isValidFor(Editor editor, PsiFile file) {
    return file instanceof ErlangFile;
  }

  @Override
  public void invoke(@NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file) {
    PsiElement focusedElement = file.findElementAt(editor.getCaretModel().getOffset());
    ErlangFunction function = focusedElement == null ? null : PsiTreeUtil.getParentOfType(focusedElement, ErlangFunction.class);

    if (function == null) return;

    List<ErlangCallbackSpec> callbackSpecs = ErlangNavigationUtil.getCallbackSpecs(function);
    String presentation = ErlangPsiImplUtil.createFunctionPresentation(function);

    PsiElementListNavigator.openTargets(editor, getNavigatables(presentation, callbackSpecs), "Go to callback specification", presentation, new DefaultPsiElementCellRenderer());
  }

  @Override
  public boolean startInWriteAction() {
    return false;
  }

  private static NavigatablePsiElement[] getNavigatables(String targetPresentation, List<ErlangCallbackSpec> callbackSpecs) {
    ArrayList<NavigatablePsiElement> navigatables = new ArrayList<NavigatablePsiElement>(callbackSpecs.size());

    for (ErlangCallbackSpec specFun : callbackSpecs) {
      ContainerUtil.addIfNotNull(navigatables, ErlangNavigationUtil.getNavigatableSpecFun(targetPresentation, specFun));
    }

    return navigatables.toArray(new NavigatablePsiElement[navigatables.size()]);
  }
}
