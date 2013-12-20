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

package org.intellij.erlang.editor;

import com.intellij.codeInsight.editorActions.smartEnter.SmartEnterProcessor;
import com.intellij.codeInsight.template.*;
import com.intellij.codeInsight.template.impl.VariableNode;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.ScrollType;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.editor.highlighter.HighlighterIterator;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.formatter.FormatterUtil;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class ErlangClausesSmartEnterProcessor extends SmartEnterProcessor {
  @Override
  public boolean process(@NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file) {
    if (!(file instanceof ErlangFile)) return false;

    PsiDocumentManager.getInstance(project).commitAllDocuments();

    int offset = editor.getCaretModel().getOffset();
    HighlighterIterator iterator = ((EditorEx) editor).getHighlighter().createIterator(offset);
    boolean atEndOfDocument = offset == editor.getDocument().getTextLength();

    if (offset == 0) return false;
    if (!atEndOfDocument) iterator.retreat();
    if (iterator.atEnd()) return false;

    PsiElement elementAt = file.findElementAt(offset - 1);
    if (elementAt == null || elementAt.getNode().getElementType() != ErlangTypes.ERL_SEMI) return false;
    ASTNode sibling = FormatterUtil.getPreviousNonWhitespaceSibling(elementAt.getNode());
    if (sibling == null) return false;
    PsiElement psi = sibling.getPsi();

    if (psi instanceof ErlangFunctionClause) {
      return processFunctionClause(project, editor, offset, (ErlangFunctionClause) psi);
    }
    else if (psi instanceof ErlangCrClause) {
      return processCrClause(project, editor);
    }
    return false;
  }

  private static boolean processCrClause(@NotNull Project project, @NotNull Editor editor) {
    TemplateManager templateManager = TemplateManager.getInstance(project);
    Template template = templateManager.createTemplate("", "", "\n$variable$ ->$END$");
    Expression var = new MyTextExpressionNode("_");
    template.addVariable("variable", var, var, true);

    editor.getScrollingModel().scrollToCaret(ScrollType.RELATIVE);
    templateManager.startTemplate(editor, template);
    return true;
  }

  private static boolean processFunctionClause(@NotNull Project project, @NotNull Editor editor, int offset, @NotNull ErlangFunctionClause functionClause) {
    TemplateManager templateManager = TemplateManager.getInstance(project);
    ErlangQAtom qAtom = functionClause.getQAtom();

    List<ErlangArgumentDefinition> argumentDefinitionList = functionClause.getArgumentDefinitionList().getArgumentDefinitionList();

    String functionName = qAtom.getText();

    Template template = templateManager.createTemplate("", "");
    template.addTextSegment(functionName + "(");

    for (int i = 0; i < argumentDefinitionList.size(); i++) {
      if (i != 0) {
        template.addTextSegment(", ");
      }
      ErlangExpression expression = argumentDefinitionList.get(i).getExpression();

      Expression foo = new MyTextExpressionNode(expression.getText());
      template.addVariable("variable" + i, foo, foo, true);
    }

    template.addTextSegment(") ->");
    template.addEndVariable();

    editor.getScrollingModel().scrollToCaret(ScrollType.RELATIVE);
    editor.getDocument().insertString(offset, "\n");

    editor.getCaretModel().moveToOffset(offset + 1);
    editor.getScrollingModel().scrollToCaret(ScrollType.RELATIVE);

    templateManager.startTemplate(editor, template);
    return true;
  }

  private static class MyTextExpressionNode extends VariableNode {
    public MyTextExpressionNode(@NotNull String name) {
      super(name, null);
    }

    @Override
    public Result calculateResult(ExpressionContext context) {
      return new TextResult(getName());
    }
  }
}
