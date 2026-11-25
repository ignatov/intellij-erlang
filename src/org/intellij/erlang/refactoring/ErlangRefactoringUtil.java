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

package org.intellij.erlang.refactoring;

import com.intellij.codeInsight.PsiEquivalenceUtil;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.SelectionModel;
import com.intellij.openapi.util.Pair;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiRecursiveElementVisitor;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.IntroduceTargetChooser;
import com.intellij.refactoring.RefactoringBundle;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ErlangRefactoringUtil {
  private ErlangRefactoringUtil() {
  }

  @NotNull
  public static List<ErlangExpressionTarget> getOccurrences(@NotNull final ErlangExpressionTarget pattern, @Nullable PsiElement context) {
    if (context == null) {
      return Collections.emptyList();
    }
    final List<ErlangExpression> patternOperands = pattern.isPrefix() ? pattern.getOperands() :
                                                   pattern.getExpression() instanceof ErlangAdditiveExpression chain ? chain.getExpressionList() :
                                                   null;
    final List<ErlangExpressionTarget> occurrences = new ArrayList<>();
    PsiRecursiveElementVisitor visitor = new PsiRecursiveElementVisitor() {
      public void visitElement(@NotNull PsiElement element) {
        if (patternOperands != null) {
          if (element instanceof ErlangAdditiveExpression chain && startsWith(chain, patternOperands)) {
            occurrences.add(ErlangExpressionTarget.prefix(chain, patternOperands.size()));
            List<ErlangExpression> operands = chain.getExpressionList();
            for (ErlangExpression operand : operands.subList(patternOperands.size(), operands.size())) {
              operand.accept(this);
            }
            return;
          }
        }
        else if (PsiEquivalenceUtil.areElementsEquivalent(element, pattern.getExpression())) {
          occurrences.add(ErlangExpressionTarget.of((ErlangExpression) element));
          return;
        }
        super.visitElement(element);
      }
    };
    context.accept(visitor);
    return occurrences;
  }

  private static boolean startsWith(@NotNull ErlangAdditiveExpression chain, @NotNull List<ErlangExpression> patternOperands) {
    List<ErlangExpression> operands = chain.getExpressionList();
    if (operands.size() < patternOperands.size()) return false;
    for (int i = 0; i < patternOperands.size(); i++) {
      if (!PsiEquivalenceUtil.areElementsEquivalent(operands.get(i), patternOperands.get(i))) return false;
      if (i > 0 && operatorBefore(operands.get(i)) != operatorBefore(patternOperands.get(i))) return false;
    }
    return true;
  }

  @Nullable
  private static IElementType operatorBefore(@NotNull ErlangExpression operand) {
    PsiElement operator = PsiTreeUtil.skipWhitespacesAndCommentsBackward(operand);
    return operator == null ? null : operator.getNode().getElementType();
  }

  @NotNull
  public static Pair<PsiElement, PsiElement> selectionToElements(@NotNull PsiFile file, @NotNull SelectionModel selectionModel) {
    PsiElement element1 = file.findElementAt(selectionModel.getSelectionStart());
    PsiElement element2 = file.findElementAt(selectionModel.getSelectionEnd() - 1);
    if (element1 instanceof PsiWhiteSpace) {
      element1 = file.findElementAt(element1.getTextRange().getEndOffset());
    }
    if (element2 instanceof PsiWhiteSpace) {
      element2 = file.findElementAt(element2.getTextRange().getStartOffset() - 1);
    }
    return Pair.create(element1, element2);
  }

  public static String shorten(@NotNull ErlangExpression o, @NotNull String defaultValue) {
    VariableTextBuilder visitor = new VariableTextBuilder();
    o.accept(visitor);
    return visitor.result(defaultValue);
  }

  @NotNull
  public static String shorten(@NotNull ErlangExpression o) { // maybe better to return List<String>
    VariableTextBuilder visitor = new VariableTextBuilder();
    o.accept(visitor);
    return visitor.result();
  }

  public static void smartIntroduce(@NotNull final Editor editor, 
                                    @NotNull PsiFile file, 
                                    @NotNull final Extractor extractor) {
    int offset = editor.getCaretModel().getOffset();
    PsiElement element = file.findElementAt(offset);
    if (!extractor.checkContext(file, editor, element)) return;
    List<ErlangExpressionTarget> targets = getIntroduceTargets(element);
    if (targets.size() == 1 || ApplicationManager.getApplication().isUnitTestMode()) {
      extractor.process(editor, targets.getFirst());
    }
    else if (targets.size() > 1) {
      IntroduceTargetChooser.showIntroduceTargetChooser(editor, targets, target -> extractor.process(editor, target),
                                                        RefactoringBundle.message("introduce.target.chooser.expressions.title"), -1);
    }
  }

  /**
   * The expressions around {@code element} to choose from, innermost first.
   */
  @NotNull
  public static List<ErlangExpressionTarget> getIntroduceTargets(@Nullable PsiElement element) {
    List<ErlangExpressionTarget> targets = new ArrayList<>();
    PsiElement child = null;
    while (element != null) {
      if (element instanceof ErlangClauseBody) {
        break;
      }
      else if (element instanceof ErlangExpression && !(element instanceof ErlangAssignmentExpression)) {
        boolean isQualifiedFunCall = element instanceof ErlangFunctionCallExpression &&
                                     element.getParent() instanceof ErlangGlobalFunctionCallExpression;
        if (!isQualifiedFunCall && !ErlangPsiImplUtil.inLeftPartOfAssignment(element, false)) {
          if (element instanceof ErlangAdditiveExpression chain && child != null) {
            addPrefixes(targets, chain, child);
          }
          targets.add(ErlangExpressionTarget.of((ErlangExpression) element));
        }
      }
      child = element;
      element = element.getParent();
    }
    return targets;
  }

  /**
   * Adds the prefixes of {@code chain} that contain {@code child}, its child, and are not the whole chain.
   */
  private static void addPrefixes(@NotNull List<ErlangExpressionTarget> targets,
                                  @NotNull ErlangAdditiveExpression chain,
                                  @NotNull PsiElement child) {
    List<ErlangExpression> operands = chain.getExpressionList();
    int before = 0;
    while (before < operands.size() && operands.get(before).getTextRange().getEndOffset() <= child.getTextRange().getStartOffset()) {
      before++;
    }
    for (int k = Math.max(before + 1, 2); k < operands.size(); k++) {
      targets.add(ErlangExpressionTarget.prefix(chain, k));
    }
  }

  public interface Extractor {
    boolean checkContext(@NotNull PsiFile file, @NotNull Editor editor, @Nullable PsiElement element);
    void process(@NotNull Editor editor, @NotNull ErlangExpressionTarget target);
  }
}
