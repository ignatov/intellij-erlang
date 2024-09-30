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

package org.intellij.erlang.eunit.generation;

import com.intellij.codeInsight.CodeInsightActionHandler;
import com.intellij.codeInsight.actions.CodeInsightAction;
import com.intellij.codeInsight.template.Expression;
import com.intellij.codeInsight.template.Template;
import com.intellij.codeInsight.template.TemplateManager;
import com.intellij.codeInsight.template.impl.ConstantNode;
import com.intellij.openapi.editor.CaretModel;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

public class ErlangUnitTestMethodAction extends CodeInsightAction implements CodeInsightActionHandler {
  private static void insertTestFunction(@NotNull Project project,
                                         @NotNull Editor editor,
                                         String name,
                                         boolean needNewline) {
    Template template = TemplateManager.getInstance(project).createTemplate("", "");
    Expression nameExpr = new ConstantNode(name);
    Expression expected = new ConstantNode("expected");
    Expression expr = new ConstantNode("expr");
    template.addTextSegment("\n" + (needNewline ? "\n" : ""));
    template.addVariable("name", nameExpr, nameExpr, true);
    template.addTextSegment("_test() ->\n");
    template.addTextSegment("?assertEqual(");
    template.addVariable(expected, true);
    template.addTextSegment(", ");
    template.addVariable(expr, true);
    template.addTextSegment(").");
    template.addEndVariable();
    template.setToIndent(true);
    template.setToReformat(true);
    TemplateManager.getInstance(project).startTemplate(editor, template, null);
  }

  @NotNull
  @Override
  protected CodeInsightActionHandler getHandler() {
    return this;
  }

  @Override
  public void invoke(@NotNull Project project, @NotNull Editor editor, @NotNull PsiFile psiFile) {
    PsiElement lastChild = psiFile.getLastChild();
    int endOffset = lastChild.getTextRange().getEndOffset();

    boolean needNewline = !(lastChild instanceof PsiWhiteSpace && StringUtil.countNewLines(lastChild.getText()) > 0);

    CaretModel caretModel = editor.getCaretModel();
    ErlangFunction function = PsiTreeUtil.getParentOfType(psiFile.findElementAt(caretModel.getOffset()), ErlangFunction.class);
    String name = function != null ? function.getName() : "name";
    caretModel.moveToOffset(endOffset);
    insertTestFunction(project, editor, name, needNewline);
  }

  @Override
  protected boolean isValidForFile(@NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file) {
    if (!(file instanceof ErlangFile)) return false;
    return ErlangPsiImplUtil.isEunitImported((ErlangFile) file);
  }
}
