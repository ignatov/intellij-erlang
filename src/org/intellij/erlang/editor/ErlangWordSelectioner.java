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

import com.intellij.codeInsight.editorActions.ExtendWordSelectionHandlerBase;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangClauseBody;
import org.intellij.erlang.psi.ErlangExpression;
import org.intellij.erlang.psi.ErlangFile;

import java.util.List;

public class ErlangWordSelectioner extends ExtendWordSelectionHandlerBase {
  @Override
  public boolean canSelect(PsiElement e) {
    return e.getContainingFile() instanceof ErlangFile;
  }

  @Override
  public List<TextRange> select(PsiElement e, CharSequence editorText, int cursorOffset, Editor editor) {
    List<TextRange> select = super.select(e, editorText, cursorOffset, editor);
    PsiElement clause = e.getParent();
    if (clause instanceof ErlangClauseBody) {
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
}
