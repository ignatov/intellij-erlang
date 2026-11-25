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

package org.intellij.erlang.editor;

import com.intellij.codeInsight.editorActions.ExtendWordSelectionHandlerBase;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangAdditiveExpression;
import org.intellij.erlang.psi.ErlangClauseBody;
import org.intellij.erlang.psi.ErlangExpression;
import org.intellij.erlang.psi.ErlangFile;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class ErlangWordSelectioner extends ExtendWordSelectionHandlerBase {
  @Override
  public boolean canSelect(PsiElement e) {
    return e.getContainingFile() instanceof ErlangFile;
  }

  @Override
  public List<TextRange> select(@NotNull PsiElement e, @NotNull CharSequence editorText, int cursorOffset, @NotNull Editor editor) {
    List<TextRange> select = super.select(e, editorText, cursorOffset, editor);
    if (e.getParent() instanceof ErlangAdditiveExpression chain && select != null) {
      return ContainerUtil.concat(select, leadingOperandRanges(chain, e));
    }
    PsiElement clause = e.getParent();
    if (clause instanceof ErlangClauseBody && select != null) {
      List<ErlangExpression> list = ((ErlangClauseBody) clause).getExpressionList();
      ErlangExpression first = ContainerUtil.getFirstItem(list);
      ErlangExpression last = ContainerUtil.getLastItem(list);

      if (first != null && last != null) {
        TextRange allExpressionsRange = TextRange.create(first.getTextRange().getStartOffset(), last.getTextRange().getEndOffset());
        return ContainerUtil.concat(select, expandToWholeLine(editorText, allExpressionsRange, false));
      }
    }
    return select;
  }

  @NotNull
  private static List<TextRange> leadingOperandRanges(@NotNull ErlangAdditiveExpression chain, @NotNull PsiElement e) {
    List<ErlangExpression> operands = chain.getExpressionList();
    int start = chain.getTextRange().getStartOffset();
    int end = e.getTextRange().getEndOffset();
    List<TextRange> ranges = new ArrayList<>();
    for (int i = 1; i < operands.size() - 1; i++) {
      int operandEnd = operands.get(i).getTextRange().getEndOffset();
      if (operandEnd >= end) ranges.add(TextRange.create(start, operandEnd));
    }
    return ranges;
  }
}
