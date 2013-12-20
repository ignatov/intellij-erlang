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

package org.intellij.erlang.intention;

import com.intellij.codeInsight.intention.impl.BaseIntentionAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.quickfixes.ErlangExportFunctionFix;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangExportFunctionIntention extends BaseIntentionAction {
  @NotNull
  @Override
  public String getFamilyName() {
    return "Export function";
  }

  @NotNull
  @Override
  public String getText() {
    return "Export function";
  }

  @Override
  public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
    if (!file.getManager().isInProject(file)) return false;
    ErlangFunction function = findFunction(file, editor.getCaretModel().getOffset());
    if (function != null && file instanceof ErlangFile) {
      return !((ErlangFile) file).getExportedFunctions().contains(function);
    }
    return false;
  }

  @Override
  public void invoke(@NotNull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
    ErlangFunction function = findFunction(file, editor.getCaretModel().getOffset());
    if (function != null) {
      ErlangExportFunctionFix.processFunction(project, function);
    }
  }
  
  @Nullable
  private static ErlangFunction findFunction(PsiFile file, int offset) {
    PsiElement element = file.findElementAt(offset);
    ErlangFunction res = PsiTreeUtil.getParentOfType(element, ErlangFunction.class);
    if (res == null) return null;

    PsiElement name = res.getNameIdentifier();
    TextRange textRange = name.getTextRange();
    if (textRange == null || textRange.getEndOffset() < offset) return null;

    return res;
  }
}
