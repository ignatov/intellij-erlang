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

package org.intellij.erlang.rename;

import com.intellij.openapi.editor.Editor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.refactoring.listeners.RefactoringElementListener;
import com.intellij.refactoring.rename.RenamePsiElementProcessor;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.macrolayer.ErlangMacroLayer;
import org.intellij.erlang.console.ErlangConsoleView;
import org.intellij.erlang.psi.ErlangQVar;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Map;

public class ErlangRenameVariableProcessor extends RenamePsiElementProcessor {
  @Override
  public boolean canProcessElement(@NotNull PsiElement element) {
    return element instanceof ErlangQVar;
  }

  /** A free variable of a header macro body is renamed as from its uses (in the header and its includers). */
  @Override
  public @Nullable PsiElement substituteElementToRename(@NotNull PsiElement element, @Nullable Editor editor) {
    PsiElement target = ErlangMacroLayer.renameTargetOfMacroToken(element);
    if (target == null) ErlangRenameAtomProcessor.refuseUntilTheMacroLayerIsReady(element, editor);
    return target;
  }

  /** The macro layer's collisions of a variable rename (see {@link ErlangMacroLayerRenameProcessor#variableRenameCollisions}). */
  @Override
  public void findCollisions(@NotNull PsiElement element, @NotNull String newName, @NotNull Map<? extends PsiElement, String> allRenames,
                             @NotNull List<UsageInfo> result) {
    super.findCollisions(element, newName, allRenames, result);
    ErlangMacroLayerRenameProcessor.variableRenameCollisions(element, newName, result);
  }

  @Override
  public void renameElement(@NotNull PsiElement element,
                            @NotNull String newName,
                            UsageInfo @NotNull [] usages,
                            @Nullable RefactoringElementListener listener) throws IncorrectOperationException {
    boolean variableNameIsInvalid;
    try {
      PsiElement qVarFromText = ErlangElementFactory.createQVarFromText(element.getProject(), newName);
      //noinspection ConstantConditions
      variableNameIsInvalid = qVarFromText == null;
    } catch (Exception any) {
      variableNameIsInvalid = true;
    }
    if (variableNameIsInvalid) {
      throw new IncorrectOperationException("Invalid variable name");
    }
    if (ErlangConsoleView.isConsoleVariable(element)) {
      for (UsageInfo usage : usages) {
        PsiReference reference = usage.getReference();
        if (reference != null) reference.handleElementRename(newName);
      }
      return;
    }
    super.renameElement(element, newName, usages, listener);
  }
}
