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
import com.intellij.openapi.util.Pass;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiRecursiveElementVisitor;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.IntroduceTargetChooser;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class ErlangRefactoringUtil {
  private ErlangRefactoringUtil() {
  }

  /**
   * A whole expression ({@code first == last}) or a leading run of the operands of an additive expression,
   * from its first operand to {@code last}. Such a run is an expression of its own: {@code A + B} in {@code A + B + C}.
   */
  public record Occurrence(@NotNull PsiElement first, @NotNull PsiElement last) {
    public boolean isRun() {
      return first != last;
    }

    @NotNull
    public TextRange getTextRange() {
      return TextRange.create(first.getTextRange().getStartOffset(), last.getTextRange().getEndOffset());
    }

    @NotNull
    public String getText() {
      return getTextRange().substring(first.getContainingFile().getText());
    }
  }

  @NotNull
  public static List<Occurrence> getOccurrences(@NotNull final Occurrence pattern, @Nullable PsiElement context) {
    if (context == null) {
      return Collections.emptyList();
    }
    final List<PsiElement> patternChildren = pattern.isRun() ? getRunChildren(pattern) :
                                             pattern.first() instanceof ErlangAdditiveExpression ? getSignificantChildren(pattern.first()) :
                                             null;
    final List<Occurrence> occurrences = new ArrayList<>();
    PsiRecursiveElementVisitor visitor = new PsiRecursiveElementVisitor() {
      public void visitElement(@NotNull PsiElement element) {
        if (!pattern.isRun() && PsiEquivalenceUtil.areElementsEquivalent(element, pattern.first())) {
          occurrences.add(new Occurrence(element, element));
          return;
        }
        if (patternChildren != null && element instanceof ErlangAdditiveExpression) {
          List<PsiElement> children = getSignificantChildren(element);
          if (startsWith(children, patternChildren)) {
            if (children.size() == patternChildren.size()) {
              occurrences.add(new Occurrence(element, element));
              return;
            }
            PsiElement last = children.get(patternChildren.size() - 1);
            occurrences.add(new Occurrence(children.getFirst(), last));
            for (PsiElement next = last.getNextSibling(); next != null; next = next.getNextSibling()) {
              next.accept(this);
            }
            return;
          }
        }
        super.visitElement(element);
      }
    };
    context.accept(visitor);
    return occurrences;
  }

  private static boolean startsWith(@NotNull List<PsiElement> children, @NotNull List<PsiElement> prefix) {
    if (children.size() < prefix.size()) return false;
    for (int i = 0; i < prefix.size(); i++) {
      if (!PsiEquivalenceUtil.areElementsEquivalent(children.get(i), prefix.get(i))) return false;
    }
    return true;
  }

  @NotNull
  private static List<PsiElement> getSignificantChildren(@NotNull PsiElement element) {
    return Arrays.asList(PsiEquivalenceUtil.getFilteredChildren(element, null, false));
  }

  @NotNull
  private static List<PsiElement> getRunChildren(@NotNull Occurrence run) {
    List<PsiElement> children = getSignificantChildren(run.first().getParent());
    return children.subList(children.indexOf(run.first()), children.indexOf(run.last()) + 1);
  }

  /**
   * Returns the last operand of the leading run of operands of {@code parent}, an additive expression,
   * that ends at {@code lastSelected}, or null when that run is not shorter than the whole expression.
   */
  @Nullable
  public static ErlangExpression getLeadingRunEnd(@Nullable PsiElement parent, @NotNull PsiElement lastSelected) {
    if (!(parent instanceof ErlangAdditiveExpression chain) || !PsiTreeUtil.isAncestor(chain, lastSelected, true)) return null;
    PsiElement child = PsiTreeUtil.findPrevParent(chain, lastSelected);
    List<ErlangExpression> operands = chain.getExpressionList();
    int k = 0;
    for (ErlangExpression operand : operands) {
      if (operand.getTextRange().getStartOffset() < child.getTextRange().getStartOffset()) k++;
    }
    return k >= 1 && k < operands.size() - 1 ? operands.get(k) : null;
  }

  @NotNull
  public static ErlangExpression getExpression(@NotNull Occurrence occurrence) {
    return occurrence.isRun() ? ErlangElementFactory.createExpressionFromText(occurrence.first().getProject(), occurrence.getText()) :
           (ErlangExpression) occurrence.first();
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
    List<ErlangExpression> expressions = new ArrayList<>();
    while (element != null) {
      if (element instanceof ErlangClauseBody) {
        break;
      }
      else if (element instanceof ErlangExpression && !(element instanceof ErlangAssignmentExpression)) {
        boolean isQualifiedFunCall = element instanceof ErlangFunctionCallExpression &&
                                     element.getParent() instanceof ErlangGlobalFunctionCallExpression;
        if (!isQualifiedFunCall && !ErlangPsiImplUtil.inLeftPartOfAssignment(element, false)) {
          expressions.add((ErlangExpression) element);
        }
      }
      element = element.getParent();
    }
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
                                         PsiElement::getText
      );
    }
  }

  public interface Extractor {
    boolean checkContext(@NotNull PsiFile file, @NotNull Editor editor, @Nullable PsiElement element);
    void process(@NotNull Editor editor, @NotNull ErlangExpression expression);
  }
}
