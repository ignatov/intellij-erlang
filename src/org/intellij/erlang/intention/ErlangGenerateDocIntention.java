/*
 * Copyright 2012-2020 Sergey Ignatov
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

import com.intellij.codeInsight.template.Template;
import com.intellij.codeInsight.template.TemplateManager;
import com.intellij.codeInsight.template.impl.ConstantNode;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.application.ErlangApplicationRunConfigurationType;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.quickfixes.ErlangGenerateSpecFix;
import org.intellij.erlang.types.ErlangExpressionType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class ErlangGenerateDocIntention extends ErlangBaseNamedElementIntention {
  protected ErlangGenerateDocIntention() {
    super("Generate Doc", "Generate Doc");
  }

  @Override
  public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
    if (!(file instanceof ErlangFile)) return false;
    ErlangFunction function = findFunction(file, editor.getCaretModel().getOffset());
    if (function == null) return false;
    return findFunctionComment(function) == null;
  }

  @Nullable
  private static PsiComment findFunctionComment(ErlangFunction function) {
    if (function == null) return null;
    for (PsiElement child = function.getPrevSibling(); child != null; child = child.getPrevSibling()) {
      if (child instanceof PsiComment) {
        PsiComment comment = (PsiComment) child;
        if (comment.getTokenType() == ErlangParserDefinition.ERL_FUNCTION_DOC_COMMENT) return comment;
        return null;
      }
      if (child instanceof ErlangAttribute) continue;
      if (child instanceof PsiWhiteSpace) continue;
      return null;
    }
    return null;
  }

  @Override
  public void invoke(@NotNull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
    if (!(file instanceof ErlangFile)) {
      throw new IncorrectOperationException("Only applicable to Erlang files.");
    }
    ErlangFunction function = findFunction(file, editor.getCaretModel().getOffset());
    if (function == null) {
      throw new IncorrectOperationException("Cursor should be placed on Erlang function.");
    }
    int textOffset = function.getTextOffset();
    Template template = createErlangDocTemplate(project);
    editor.getCaretModel().moveToOffset(textOffset);
    TemplateManager.getInstance(project).startTemplate(editor, template);
  }

  private static Template createErlangDocTemplate(Project project) {
    TemplateManager templateManager = TemplateManager.getInstance(project);
    Template template = templateManager.createTemplate("", "");
    template.setToReformat(true);
    template.addTextSegment("");
    template.addTextSegment("%%------------------------------------------------------------------------------\n");
    template.addTextSegment("%% @doc ");
    template.addEndVariable();
    template.addTextSegment("\n%% @end\n" +
                  "%%------------------------------------------------------------------------------\n");
    return template;
  }

  @Nullable
  private static ErlangFunction findFunction(PsiFile file, int offset) {
    return findElement(file, offset, ErlangFunction.class);
  }

}
