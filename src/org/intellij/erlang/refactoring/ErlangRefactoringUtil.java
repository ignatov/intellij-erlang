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
import com.intellij.lang.ASTFactory;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.SelectionModel;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.Pass;
import com.intellij.openapi.util.Ref;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiRecursiveElementVisitor;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.impl.source.tree.CompositeElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.IntroduceTargetChooser;
import org.intellij.erlang.ErlangTypes;
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
  public static List<PsiElement> getOccurrences(@NotNull final PsiElement pattern, @Nullable PsiElement context) {
    if (context == null) {
      return Collections.emptyList();
    }
    if (pattern instanceof ErlangAdditiveExpression additive) {
      splitChainsStartingWith(additive, context);
    }
    final List<PsiElement> occurrences = new ArrayList<>();
    PsiRecursiveElementVisitor visitor = new PsiRecursiveElementVisitor() {
      public void visitElement(@NotNull PsiElement element) {
        if (PsiEquivalenceUtil.areElementsEquivalent(element, pattern)) {
          occurrences.add(element);
          return;
        }
        super.visitElement(element);
      }
    };
    context.accept(visitor);
    return occurrences;
  }

  /**
   * A chain of additive operators such as {@code A + B + C} is one flat expression, while its leading operands
   * {@code A + B} are a subexpression too. Returns the leading operands of {@code parent} from {@code startOffset}
   * to {@code endOffset} as their own expression, or null when the range is not such a leading part.
   */
  @Nullable
  public static ErlangExpression leadingOperands(@Nullable PsiElement parent, int startOffset, int endOffset) {
    if (!(parent instanceof ErlangAdditiveExpression chain)) return null;
    List<ErlangExpression> operands = chain.getExpressionList();
    if (operands.getFirst().getTextRange().getStartOffset() != startOffset) return null;
    for (int count = 2; count < operands.size(); count++) {
      if (operands.get(count - 1).getTextRange().getEndOffset() == endOffset) {
        return splitLeadingOperands(chain, count);
      }
    }
    return null;
  }

  /**
   * Moves the first {@code count} operands of {@code chain} and the operators between them into a new additive
   * expression. The text does not change, but {@code chain} and everything in it are replaced with new elements.
   */
  @NotNull
  private static ErlangExpression splitLeadingOperands(@NotNull ErlangAdditiveExpression chain, int count) {
    Ref<PsiElement> result = Ref.create();
    CommandProcessor.getInstance().runUndoTransparentAction(() -> ApplicationManager.getApplication().runWriteAction(() -> {
      ASTNode copy = chain.getNode().copyElement();
      List<ErlangExpression> operands = ((ErlangAdditiveExpression) copy.getPsi()).getExpressionList();
      ASTNode first = operands.getFirst().getNode();
      ASTNode afterLast = operands.get(count - 1).getNode().getTreeNext();
      CompositeElement part = ASTFactory.composite(ErlangTypes.ERL_ADDITIVE_EXPRESSION);
      copy.addChild(part, first);
      part.addChildren(first, afterLast, null);
      chain.getNode().getTreeParent().replaceChild(chain.getNode(), copy);
      result.set(part.getPsi());
    }));
    return (ErlangExpression) result.get();
  }

  private static void splitChainsStartingWith(@NotNull ErlangAdditiveExpression pattern, @NotNull PsiElement context) {
    List<ErlangExpression> patternOperands = pattern.getExpressionList();
    int count = patternOperands.size();
    List<ErlangAdditiveExpression> chains = new ArrayList<>(PsiTreeUtil.findChildrenOfType(context, ErlangAdditiveExpression.class));
    Collections.reverse(chains);
    for (ErlangAdditiveExpression chain : chains) {
      List<ErlangExpression> operands = chain.getExpressionList();
      if (operands.size() <= count || PsiTreeUtil.isAncestor(chain, pattern, false)) continue;
      boolean same = true;
      for (int i = 0; i < count && same; i++) {
        same = PsiEquivalenceUtil.areElementsEquivalent(operands.get(i), patternOperands.get(i)) &&
               (i == count - 1 || operatorAfter(operands.get(i)) == operatorAfter(patternOperands.get(i)));
      }
      if (same) splitLeadingOperands(chain, count);
    }
  }

  @Nullable
  private static Object operatorAfter(@NotNull ErlangExpression operand) {
    PsiElement operator = PsiTreeUtil.skipWhitespacesAndCommentsForward(operand);
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
    splitChainsAround(file, offset);
    element = file.findElementAt(offset);
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

  /**
   * Splits the additive chains around {@code offset} so that each leading part of a chain that contains
   * {@code offset} is an expression of its own, as a tree of binary operators would have it.
   */
  private static void splitChainsAround(@NotNull PsiFile file, int offset) {
    PsiElement element = file.findElementAt(offset);
    while (element != null && !(element instanceof ErlangClauseBody)) {
      PsiElement parent = element.getParent();
      if (parent instanceof ErlangAdditiveExpression chain) {
        List<ErlangExpression> operands = chain.getExpressionList();
        int index = 0;
        int end = element.getTextRange().getEndOffset();
        while (index < operands.size() - 1 && operands.get(index).getTextRange().getEndOffset() < end) index++;
        if (Math.max(2, index + 1) < operands.size()) {
          splitLeadingOperands(chain, operands.size() - 1);
          element = file.findElementAt(offset);
          continue;
        }
      }
      element = parent;
    }
  }

  public interface Extractor {
    boolean checkContext(@NotNull PsiFile file, @NotNull Editor editor, @Nullable PsiElement element);
    void process(@NotNull Editor editor, @NotNull ErlangExpression expression);
  }
}
