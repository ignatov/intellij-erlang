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

package org.intellij.erlang;

import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.refactoring.InlineActionHandler;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.impl.source.codeStyle.CodeEditUtil;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.HelpID;
import com.intellij.refactoring.util.CommonRefactoringUtil;
import com.intellij.util.Query;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;

import java.util.Collection;

/**
 * @author ignatov
 */
public class ErlangInlineVariableHandler extends InlineActionHandler {
  private static final String REFACTORING_NAME = "Inline variable";

  @Override
  public boolean isEnabledForLanguage(Language l) {
    return l == ErlangLanguage.INSTANCE;
  }

  @Override
  public boolean canInlineElement(PsiElement element) {
    return element instanceof ErlangQVar;
  }

  @Override
  public void inlineElement(final Project project, final Editor editor, PsiElement element) {
    if (!(element instanceof ErlangQVar)) return;

    final PsiElement parent = element.getParent();
    if (!(parent instanceof ErlangMaxExpression)) return; // popup?
    final PsiElement assignment = parent.getParent();
    if (!(assignment instanceof ErlangAssignmentExpression)) {
      CommonRefactoringUtil.showErrorHint(project, editor, "Not in assignment expression.", REFACTORING_NAME, HelpID.INLINE_VARIABLE);
      return;
    }

    if (((ErlangAssignmentExpression) assignment).getLeft() != parent) return;

    final ErlangExpression right = ((ErlangAssignmentExpression) assignment).getRight();

    if (right == null) {
      CommonRefactoringUtil.showErrorHint(project, editor, "Should have an initializer.", REFACTORING_NAME, HelpID.INLINE_VARIABLE);
      return;
    }

    Query<PsiReference> search = ReferencesSearch.search(element);
    final Collection<PsiReference> all = search.findAll();

    CommandProcessor.getInstance().executeCommand(project, new Runnable() {
      public void run() {
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
          @Override
          public void run() {
            ErlangExpression rightWithoutParentheses = ErlangPsiImplUtil.getNotParenthesizedExpression(right);

            for (PsiReference psiReference : all) {
              PsiElement host = psiReference.getElement();
              PsiElement expr = host.getParent();
              ASTNode replacementNode = null;

              if (expr instanceof ErlangMaxExpression) {
                if (ErlangPsiImplUtil.getExpressionPrecedence(expr.getParent()) > ErlangPsiImplUtil.getExpressionPrecedence(rightWithoutParentheses)) {
                  replacementNode = expr.replace(ErlangPsiImplUtil.wrapWithParentheses(rightWithoutParentheses)).getNode();
                } else {
                  replacementNode = expr.replace(rightWithoutParentheses).getNode();
                }
              } else if (expr instanceof ErlangFunExpression) {
                replacementNode = host.replace(rightWithoutParentheses).getNode();
              } else if (expr instanceof ErlangGenericFunctionCallExpression) {
                replacementNode = substituteFunctionCall(project, host, rightWithoutParentheses).getNode();
              }

              if (replacementNode != null) CodeEditUtil.markToReformat(replacementNode, true);
            }

            PsiElement comma = PsiTreeUtil.getNextSiblingOfType(assignment, LeafPsiElement.class);
            if (comma != null && comma.getNode().getElementType() == ErlangTypes.ERL_COMMA) {
              comma.delete();
            }

            assignment.delete();
          }
        });
      }
    }, "Inline variable", null);
  }

  private static PsiElement substituteFunctionCall(Project project, PsiElement variable, ErlangExpression variableValue) {
    if (!(variableValue instanceof ErlangFunExpression)) return variable.replace(variableValue);

    ErlangFunExpression funExpression = (ErlangFunExpression) variableValue;

    if (null != funExpression.getFunClauses()) return variable.replace(variableValue);

    ErlangFunctionWithArity functionWithArity = funExpression.getFunctionWithArity();
    PsiElement function = functionWithArity != null ? functionWithArity.getQAtom() : null;

    if (function == null) return variable; //the condition is always false

    function = variable.replace(function);

    PsiElement parent = function.getParent();
    ErlangModuleRef moduleRef = funExpression.getModuleRef();
    PsiElement module = moduleRef != null ? moduleRef.getQAtom() : null;
    if (module == null) module = funExpression.getQVar();

    if (module == null || parent == null) return function;

    parent.addBefore(module, function);
    parent.addBefore(ErlangElementFactory.createLeafFromText(project, ":"), function);

    return parent;
  }

}
