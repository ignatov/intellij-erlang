/*
 * Copyright 2013 Sergey Ignatov
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

import com.intellij.codeInsight.CodeInsightSettings;
import com.intellij.codeInsight.editorActions.TypedHandlerDelegate;
import com.intellij.codeInsight.template.Template;
import com.intellij.codeInsight.template.TemplateManager;
import com.intellij.codeInsight.template.impl.TextExpression;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.ScrollType;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.editor.highlighter.HighlighterIterator;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.formatter.FormatterUtil;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * @author ignatov
 */
public class ErlangSemicolonTypedHandler extends TypedHandlerDelegate {

  @Override
  public Result charTyped(char c, @NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file) {
    if (!(file instanceof ErlangFile)) return super.charTyped(c, project, editor, file);

    if (c != ';' || !CodeInsightSettings.getInstance().AUTOINSERT_PAIR_BRACKET) {
      return Result.CONTINUE;
    }
    process(project, editor, file);
    return Result.CONTINUE;
  }

  private static void process(@NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file) {
    if (!(file instanceof ErlangFile)) return;

    PsiDocumentManager.getInstance(project).commitAllDocuments();

    int offset = editor.getCaretModel().getOffset();
    HighlighterIterator iterator = ((EditorEx) editor).getHighlighter().createIterator(offset);
    boolean atEndOfDocument = offset == editor.getDocument().getTextLength();

    if (offset == 0) return;
    if (!atEndOfDocument) iterator.retreat();
    if (iterator.atEnd()) return;

    PsiElement elementAt = file.findElementAt(offset - 1);
    if (elementAt == null) return;
    ASTNode sibling = FormatterUtil.getPreviousNonWhitespaceSibling(elementAt.getNode());
    if (sibling == null) return;
    PsiElement psi = sibling.getPsi();

    if (psi instanceof ErlangFunctionClause) {
      ErlangFunctionClause nextClause = PsiTreeUtil.getNextSiblingOfType(psi, ErlangFunctionClause.class);
      String current = ErlangPsiImplUtil.createFunctionClausePresentation((ErlangFunctionClause) psi);
      String nextP = ErlangPsiImplUtil.createFunctionClausePresentation(nextClause);
      if (!nextP.equals(current)) {
        processFunctionClause(project, editor, offset, (ErlangFunctionClause) psi);
      }
    }
    else if (psi instanceof ErlangCrClause) {
      PsiElement parent = psi.getParent();
      
      if (parent instanceof ErlangCrClauses) {
        List<ErlangCrClause> crClauseList = ((ErlangCrClauses) parent).getCrClauseList();
        ErlangCrClause last = ContainerUtil.iterateAndGetLastItem(crClauseList);
        if (psi.equals(last)) {
          processCrClause(project, editor);          
        }
      }
    }
  }

  private static void processCrClause(Project project, Editor editor) {
    TemplateManager templateManager = TemplateManager.getInstance(project);
    Template template = templateManager.createTemplate("", "", "\n$variable$ ->$END$");
    TextExpression var = new TextExpression("_");
    template.addVariable("variable", var, var, true);

    editor.getScrollingModel().scrollToCaret(ScrollType.RELATIVE);
    templateManager.startTemplate(editor, template);
  }

  private static void processFunctionClause(@NotNull Project project, @NotNull Editor editor, int offset, @NotNull ErlangFunctionClause functionClause) {
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

      TextExpression foo = new TextExpression(expression.getText());
      template.addVariable("variable" + i, foo, foo, true);
    }

    template.addTextSegment(") ->");
    template.addEndVariable();

    editor.getScrollingModel().scrollToCaret(ScrollType.RELATIVE);
    editor.getDocument().insertString(offset, "\n");

    editor.getCaretModel().moveToOffset(offset + 1);

    templateManager.startTemplate(editor, template);
  }

  private static void reformat(@NotNull PsiElement atCaret) throws IncorrectOperationException {
    final TextRange range = atCaret.getTextRange();
    final PsiFile file = atCaret.getContainingFile();
    final PsiFile baseFile = file.getViewProvider().getPsi(file.getViewProvider().getBaseLanguage());
    CodeStyleManager.getInstance(atCaret.getProject()).reformatText(baseFile, range.getStartOffset(), range.getEndOffset());
  }
}
