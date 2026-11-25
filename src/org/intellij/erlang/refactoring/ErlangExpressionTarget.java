/*
 * Copyright 2012-2026 Sergey Ignatov
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

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.introduce.IntroduceTarget;
import org.intellij.erlang.psi.ErlangAdditiveExpression;
import org.intellij.erlang.psi.ErlangExpression;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * An expression to extract: a whole expression, or the first operands of an additive chain. The parser makes one element
 * of a chain like {@code A + B - C}, so its prefix {@code A + B}, which is an operand of the chain, has no element of its own.
 */
public final class ErlangExpressionTarget implements IntroduceTarget {
  private final ErlangExpression myExpression;
  private final int myPrefixOperands;

  private ErlangExpressionTarget(@NotNull ErlangExpression expression, int prefixOperands) {
    myExpression = expression;
    myPrefixOperands = prefixOperands;
  }

  @NotNull
  public static ErlangExpressionTarget of(@NotNull ErlangExpression expression) {
    return new ErlangExpressionTarget(expression, 0);
  }

  /**
   * The first {@code operands} operands of {@code chain}, at least two; the whole chain when they are all its operands.
   */
  @NotNull
  public static ErlangExpressionTarget prefix(@NotNull ErlangAdditiveExpression chain, int operands) {
    return operands >= chain.getExpressionList().size() ? of(chain) : new ErlangExpressionTarget(chain, Math.max(operands, 2));
  }

  /**
   * The shortest prefix of an additive chain, of at least two operands, that contains a selection from {@code first} to
   * {@code last}, if it is not the whole chain. An operator belongs to the prefix that ends with the operand after it.
   */
  @Nullable
  public static ErlangExpressionTarget prefixOfSelection(@NotNull PsiElement first, @NotNull PsiElement last) {
    if (!(PsiTreeUtil.findCommonParent(first, last) instanceof ErlangAdditiveExpression chain)) return null;
    List<ErlangExpression> operands = chain.getExpressionList();
    int end = last.getTextRange().getEndOffset();
    for (int i = 1; i < operands.size() - 1; i++) {
      if (operands.get(i).getTextRange().getEndOffset() >= end) return prefix(chain, i + 1);
    }
    return null;
  }

  /**
   * The prefixes of {@code chain}, of at least two operands and not the whole chain, that contain {@code child}, a child of
   * the chain, shortest first.
   */
  @NotNull
  public static List<ErlangExpressionTarget> prefixesContaining(@NotNull ErlangAdditiveExpression chain, @NotNull PsiElement child) {
    List<ErlangExpression> operands = chain.getExpressionList();
    int before = 0;
    while (before < operands.size() && operands.get(before).getTextRange().getEndOffset() <= child.getTextRange().getStartOffset()) {
      before++;
    }
    List<ErlangExpressionTarget> prefixes = new ArrayList<>();
    for (int k = Math.max(before + 1, 2); k < operands.size(); k++) {
      prefixes.add(prefix(chain, k));
    }
    return prefixes;
  }

  /**
   * The whole expression, or the chain whose operands the target is.
   */
  @NotNull
  public ErlangExpression getExpression() {
    return myExpression;
  }

  public boolean isPrefix() {
    return myPrefixOperands > 0;
  }

  /**
   * The operands of the prefix, or the whole expression alone.
   */
  @NotNull
  public List<ErlangExpression> getOperands() {
    if (!isPrefix()) return Collections.singletonList(myExpression);
    return ((ErlangAdditiveExpression) myExpression).getExpressionList().subList(0, myPrefixOperands);
  }

  @NotNull
  @Override
  public TextRange getTextRange() {
    TextRange range = myExpression.getTextRange();
    return isPrefix() ? TextRange.create(range.getStartOffset(), getOperands().getLast().getTextRange().getEndOffset()) : range;
  }

  @NotNull
  public String getText() {
    if (!isPrefix()) return myExpression.getText();
    return getTextRange().shiftLeft(myExpression.getTextRange().getStartOffset()).substring(myExpression.getText());
  }

  /**
   * Visits the expression, or the operands of the prefix and what is between them.
   */
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (!isPrefix()) {
      myExpression.accept(visitor);
      return;
    }
    List<ErlangExpression> operands = getOperands();
    for (PsiElement e = operands.getFirst(); e != null; e = e.getNextSibling()) {
      e.accept(visitor);
      if (e == operands.getLast()) break;
    }
  }

  /**
   * Replaces the operands of the prefix, and the operators between them, with {@code replacement}.
   */
  @NotNull
  public PsiElement replacePrefix(@NotNull PsiElement replacement) {
    List<ErlangExpression> operands = getOperands();
    PsiElement added = myExpression.addBefore(replacement, operands.getFirst());
    myExpression.deleteChildRange(operands.getFirst(), operands.getLast());
    return added;
  }

  @NotNull
  @Override
  public PsiElement getPlace() {
    return myExpression;
  }

  @NotNull
  @Override
  public String render() {
    return getText();
  }

  @Override
  public boolean equals(Object o) {
    return o instanceof ErlangExpressionTarget other && myExpression == other.myExpression && myPrefixOperands == other.myPrefixOperands;
  }

  @Override
  public int hashCode() {
    return 31 * myExpression.hashCode() + myPrefixOperands;
  }

  @Override
  public boolean isValid() {
    return myExpression.isValid();
  }
}
