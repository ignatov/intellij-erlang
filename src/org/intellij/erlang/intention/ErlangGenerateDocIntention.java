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
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class ErlangGenerateDocIntention extends ErlangBaseNamedElementIntention {
  public static final String NAME = "Generate function @doc";

  protected ErlangGenerateDocIntention() {
    super(NAME, NAME);
  }

  @Override
  public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
    if (!(file instanceof ErlangFile)) return false;

    var function = findFunction(file, editor.getCaretModel().getOffset());

    if (function == null) return false;

    return findFunctionComment(function) == null;
  }

  @Nullable
  private static PsiComment findFunctionComment(ErlangFunction function) {
    if (function == null) return null;

    for (PsiElement child = function.getPrevSibling(); child != null; child = child.getPrevSibling()) {
      if (child instanceof PsiComment comment) {
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

    var function = findFunction(file, editor.getCaretModel().getOffset());

    if (function == null) {
      throw new IncorrectOperationException("Cursor should be placed on Erlang function.");
    }

    var textOffset = function.getTextOffset();
    var template = createErlangDocTemplate(project, function);

    editor.getCaretModel().moveToOffset(textOffset);
    TemplateManager.getInstance(project).startTemplate(editor, template);
  }

  /**
   * For a function node, return its arguments which can be casted to QVar and skip the incompatible ones.
   *
   * @param function The PSI node
   * @return List of argument PSI nodes
   */
  private static List<ErlangCompositeElement> getQVarArguments(ErlangFunction function) {
    var firstClause = function.getFirstClause();
    return firstClause.getArgumentDefinitionList()
                      .getArgumentDefinitionList()
                      .stream()
                      .map(argDef -> {
                        ErlangCompositeElement expr = argDef.getExpression();

                        if (expr instanceof ErlangAssignmentExpression assignmentExpr) {
                          var left = assignmentExpr.getLeft();
                          if (left instanceof ErlangMaxExpression) expr = left;
                          var right = assignmentExpr.getRight();
                          if (right instanceof ErlangMaxExpression) expr = right;
                        }

                        if (expr instanceof ErlangMaxExpression maxExpr) {
                          // Unwrap a maxExpression's first child
                          return maxExpr.getQVar();
                        }

                        return expr;
                      })
                      .toList();
  }

  private static @NotNull Template createErlangDocTemplate(Project project, ErlangFunction function) {
    var template = TemplateManager.getInstance(project).createTemplate("", "");

    template.setToReformat(true);

    // Add first line: %% @doc $Documentation$
    template.addTextSegment("");
    template.addTextSegment("%% @doc ");
    template.addVariable("DOC", new ConstantNode("Text"), true);
    template.addTextSegment("\n");

    // Insert: %% @param $ParamN$ for each parameter which is not a pattern
    var count = 1;

    for (var argExpr : getQVarArguments(function)) {
      if (argExpr instanceof ErlangQVar qVar) {
        template.addTextSegment("%% @param " + qVar.getName() + " ");
        template.addVariable("PARAM" + count, new ConstantNode("Describe " + qVar.getName()), true);
        template.addTextSegment("\n");
        count++;
      }
    }

    // Add final %% @returns $Return$
    template.addTextSegment("%% @returns ");
    template.addVariable("RETURN", new ConstantNode("What"), true);
    template.addTextSegment("\n");

    template.addEndVariable();
    return template;
  }

  @Nullable
  private static ErlangFunction findFunction(PsiFile file, int offset) {
    return findElement(file, offset, ErlangFunction.class);
  }
}
