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
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.Pass;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiRecursiveElementVisitor;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.IntroduceTargetChooser;
import com.intellij.util.SmartList;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ErlangRefactoringUtil {
  private static final Key<ErlangExpression> RUN_END = Key.create("ERLANG_LEADING_RUN_END");

  private ErlangRefactoringUtil() {
  }

  // The operand of an additive chain where a run of operands from the start of the chain up to element ends,
  // or null when the run is the whole chain
  @Nullable
  public static ErlangExpression getLeadingRunEnd(@NotNull ErlangAdditiveExpression chain, @NotNull PsiElement element) {
    while (element != null && element.getParent() != chain) element = element.getParent();
    if (element == null) return null;
    ErlangExpression end = element instanceof ErlangExpression e ? e : PsiTreeUtil.getNextSiblingOfType(element, ErlangExpression.class);
    return end == null || end == chain.getExpressionList().getLast() ? null : end;
  }

  // A non-physical expression with the text of the operands of the chain up to end
  @NotNull
  public static ErlangExpression createLeadingRun(@NotNull ErlangAdditiveExpression chain, @NotNull ErlangExpression end) {
    String text = chain.getText().substring(0, end.getStartOffsetInParent() + end.getTextLength());
    ErlangExpression run = ErlangElementFactory.createExpressionFromText(chain.getProject(), text);
    run.putUserData(RUN_END, end);
    return run;
  }

  // The physical operands of a leading run, or the expression itself
  @NotNull
  public static List<ErlangExpression> getOperands(@NotNull ErlangExpression expression) {
    ErlangExpression end = expression.getUserData(RUN_END);
    if (end == null) return new SmartList<>(expression);
    List<ErlangExpression> operands = ((ErlangAdditiveExpression) end.getParent()).getExpressionList();
    return new ArrayList<>(operands.subList(0, operands.indexOf(end) + 1));
  }

  // The physical element where a leading run is, or the element itself
  @NotNull
  public static PsiElement getPlace(@NotNull PsiElement element) {
    ErlangExpression end = element.getUserData(RUN_END);
    return end != null ? end.getParent() : element;
  }

  @NotNull
  public static TextRange getRange(@NotNull PsiElement element) {
    ErlangExpression end = element.getUserData(RUN_END);
    return end == null ? element.getTextRange() : new TextRange(end.getParent().getTextRange().getStartOffset(), end.getTextRange().getEndOffset());
  }

  // Puts the operands of a leading run under one physical node with the same text, so that it can be replaced
  @NotNull
  public static PsiElement materialize(@NotNull PsiElement element) {
    ErlangExpression end = element.getUserData(RUN_END);
    if (end == null) return element;
    PsiElement chain = end.getParent();
    PsiElement first = chain.getFirstChild();
    PsiElement run = chain.addBefore(element, first);
    chain.deleteChildRange(first, end);
    run.putUserData(RUN_END, null);
    return run;
  }

  // The operands and operators of an additive expression or a leading run
  @Nullable
  private static List<PsiElement> getRunParts(@NotNull PsiElement element) {
    ErlangExpression end = element.getUserData(RUN_END);
    PsiElement chain = end != null ? end.getParent() : element;
    if (!(chain instanceof ErlangAdditiveExpression)) return null;
    List<PsiElement> parts = new ArrayList<>();
    for (PsiElement child = chain.getFirstChild(); child != null; child = child.getNextSibling()) {
      if (!(child instanceof PsiWhiteSpace || child instanceof PsiComment)) parts.add(child);
      if (child == end) break;
    }
    return parts;
  }

  private static boolean startsWith(@NotNull List<PsiElement> parts, @NotNull List<PsiElement> prefix) {
    if (parts.size() < prefix.size()) return false;
    for (int i = 0; i < prefix.size(); i++) {
      PsiElement part = parts.get(i);
      PsiElement other = prefix.get(i);
      boolean same = part instanceof ErlangExpression ? PsiEquivalenceUtil.areElementsEquivalent(part, other) : part.textMatches(other);
      if (!same) return false;
    }
    return true;
  }

  @NotNull
  public static List<PsiElement> getOccurrences(@NotNull final PsiElement pattern, @Nullable PsiElement context) {
    if (context == null) {
      return Collections.emptyList();
    }
    final List<PsiElement> occurrences = new ArrayList<>();
    List<PsiElement> patternParts = getRunParts(pattern);
    PsiRecursiveElementVisitor visitor = new PsiRecursiveElementVisitor() {
      public void visitElement(@NotNull PsiElement element) {
        List<PsiElement> parts = patternParts != null && element instanceof ErlangAdditiveExpression ? getRunParts(element) : null;
        if (parts != null && startsWith(parts, patternParts)) {
          PsiElement end = parts.get(patternParts.size() - 1);
          occurrences.add(parts.size() == patternParts.size() ? element : createLeadingRun((ErlangAdditiveExpression) element, (ErlangExpression) end));
          return;
        }
        if (patternParts == null && PsiEquivalenceUtil.areElementsEquivalent(element, pattern)) {
          occurrences.add(element);
          return;
        }
        super.visitElement(element);
      }
    };
    context.accept(visitor);
    return occurrences;
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

  @NotNull
  static List<ErlangExpression> getIntroduceTargets(@Nullable PsiElement element) {
    List<ErlangExpression> expressions = new ArrayList<>();
    PsiElement previous = null;
    while (element != null) {
      if (element instanceof ErlangClauseBody) {
        break;
      }
      else if (element instanceof ErlangExpression && !(element instanceof ErlangAssignmentExpression)) {
        boolean isQualifiedFunCall = element instanceof ErlangFunctionCallExpression &&
                                     element.getParent() instanceof ErlangGlobalFunctionCallExpression;
        if (!isQualifiedFunCall && !ErlangPsiImplUtil.inLeftPartOfAssignment(element, false)) {
          ErlangExpression end = element instanceof ErlangAdditiveExpression chain && previous != null ? getLeadingRunEnd(chain, previous) : null;
          if (end != null) {
            List<ErlangExpression> operands = ((ErlangAdditiveExpression) element).getExpressionList();
            for (int i = Math.max(operands.indexOf(end), 1); i < operands.size() - 1; i++) {
              expressions.add(createLeadingRun((ErlangAdditiveExpression) element, operands.get(i)));
            }
          }
          expressions.add((ErlangExpression) element);
        }
      }
      previous = element;
      element = element.getParent();
    }
    return expressions;
  }

  public static void smartIntroduce(@NotNull final Editor editor, 
                                    @NotNull PsiFile file, 
                                    @NotNull final Extractor extractor) {
    int offset = editor.getCaretModel().getOffset();
    PsiElement element = file.findElementAt(offset);
    if (!extractor.checkContext(file, editor, element)) return;
    List<ErlangExpression> expressions = getIntroduceTargets(element);
    if (expressions.size() == 1 || ApplicationManager.getApplication().isUnitTestMode()) {
      extractor.process(editor, expressions.getFirst());
    }
    else if (expressions.size() > 1) {
      IntroduceTargetChooser.showChooser(editor, expressions,
        new Pass<ErlangExpression>() {
          @Override
          public void pass(@NotNull ErlangExpression expression) {
            extractor.process(editor, expression);
          }
        },
                                         PsiElement::getText, "Expressions", ErlangRefactoringUtil::getRange
      );
    }
  }

  public interface Extractor {
    boolean checkContext(@NotNull PsiFile file, @NotNull Editor editor, @Nullable PsiElement element);
    void process(@NotNull Editor editor, @NotNull ErlangExpression expression);
  }
}
