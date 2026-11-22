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
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.listeners.RefactoringElementListener;
import com.intellij.refactoring.rename.RenamePsiElementProcessor;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.macrolayer.ErlangMacroLayer;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangMacrosBody;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.console.ErlangConsoleView;
import org.intellij.erlang.psi.ErlangQVar;
import org.intellij.erlang.psi.impl.ErlangVariableReferenceImpl;
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

  /**
   * With the macro layer: (1) a free variable of a -define body of this file: a function that uses the macro
   * already has a variable with the new name (the body would then bind or match it); (2) the variable to rename is a
   * use that only a macro binds (its own reference resolves only through the macro layer): it was taken as the target
   * before the macro layer could answer, so the rename would edit only it.
   */
  @Override
  public void findCollisions(@NotNull PsiElement element, @NotNull String newName, @NotNull Map<? extends PsiElement, String> allRenames,
                             @NotNull List<UsageInfo> result) {
    super.findCollisions(element, newName, allRenames, result);
    if (!(element instanceof ErlangQVar var) || !ErlangMacroLayer.isEnabled()) return;
    ErlangMacrosDefinition def = PsiTreeUtil.getParentOfType(var, ErlangMacrosDefinition.class);
    PsiFile file = var.getContainingFile();
    if (def != null && def.getName() != null && PsiTreeUtil.isAncestor(def.getMacrosBody(), var, false)) {
      if (file instanceof ErlangFile ef && !ErlangMacroLayer.isMacroParameter(def, var.getText())) {
        ErlangMacroLayerRenameProcessor.variableCollisions(ef, def.getName(), newName, element, result);
      }
      return;
    }
    PsiReference ref = var.getReference();
    if (PsiTreeUtil.getParentOfType(var, ErlangMacrosBody.class) == null && ref instanceof ErlangVariableReferenceImpl v &&
        isBoundElsewhereByMacro(var, v.resolveOnlyThroughMacroLayer())) {
      result.add(ErlangMacroLayerRenameProcessor.collision(var, element, "Variable " + var.getText() + " is bound by a macro, but the " +
                                                                         "rename started before the macro expansion of the file was ready: try again"));
    }
  }

  private static boolean isBoundElsewhereByMacro(@NotNull ErlangQVar var, ResolveResult @NotNull [] layerAnswers) {
    if (layerAnswers.length == 0) return false;
    for (ResolveResult r : layerAnswers) if (var.isEquivalentTo(r.getElement()) || var == r.getElement()) return false;
    return true;
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
