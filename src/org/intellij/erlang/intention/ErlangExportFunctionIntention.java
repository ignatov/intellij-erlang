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

package org.intellij.erlang.intention;

import com.intellij.codeInsight.daemon.HighlightDisplayKey;
import com.intellij.codeInsight.daemon.impl.analysis.HighlightingLevelManager;
import com.intellij.codeInsight.intention.preview.IntentionPreviewInfo;
import com.intellij.ide.PowerSaveMode;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.profile.codeInspection.InspectionProjectProfileManager;
import com.intellij.psi.PsiFile;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.inspection.ErlangUnusedFunctionInspection;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.quickfixes.ErlangExportFunctionFix;
import org.intellij.erlang.statistics.ErlangUsageCounters;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangExportFunctionIntention extends ErlangBaseNamedElementIntention {
  public ErlangExportFunctionIntention() {
    super("Export function", "Export function");
  }

  @Override
  public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
    if (!file.getManager().isInProject(file)) return false;
    ErlangFunction function = findFunction(file, editor.getCaretModel().getOffset());
    if (function != null && file instanceof ErlangFile) {
      return !((ErlangFile) file).getExportedFunctions().contains(function) &&
             ErlangExportFunctionFix.canBeExported((ErlangFile) file, function) &&
             !isOfferedByUnusedFunctionInspection((ErlangFile) file, function);
    }
    return false;
  }

  // The intention is available on the function name only, where the quick fix of the unused function inspection is offered too,
  // provided that inspections run for the file
  private static boolean isOfferedByUnusedFunctionInspection(@NotNull ErlangFile file, @NotNull ErlangFunction function) {
    if (PowerSaveMode.isEnabled()) return false;
    HighlightingLevelManager levelManager = HighlightingLevelManager.getInstance(file.getProject());
    if (!levelManager.shouldInspect(file) || levelManager.runEssentialHighlightingOnly(file)) return false;
    HighlightDisplayKey key = HighlightDisplayKey.find(ErlangUnusedFunctionInspection.SHORT_NAME);
    return key != null &&
           InspectionProjectProfileManager.getInstance(file.getProject()).getCurrentProfile().isToolEnabled(key, file) &&
           new ErlangUnusedFunctionInspection().isReported(file, function);
  }

  @Override
  public void invoke(@NotNull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
    ErlangUsageCounters.countIntention(getClass());
    ErlangFunction function = findFunction(file, editor.getCaretModel().getOffset());
    if (function != null) {
      new ErlangExportFunctionFix(function).invoke(project, file, editor, function, null);
    }
  }

  @Override
  public @NotNull IntentionPreviewInfo generatePreview(@NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file) {
    return IntentionPreviewInfo.EMPTY;
  }

  @Nullable
  private static ErlangFunction findFunction(PsiFile file, int offset) {
    return findElement(file, offset, ErlangFunction.class);
  }
}
