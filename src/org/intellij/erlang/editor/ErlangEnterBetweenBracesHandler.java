/*
 * Copyright 2012-2015 Sergey Ignatov
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

import com.intellij.codeInsight.editorActions.enter.EnterBetweenBracesHandler;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.actionSystem.EditorActionHandler;
import com.intellij.openapi.util.Ref;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.psi.ErlangFile;
import org.jetbrains.annotations.NotNull;

public class ErlangEnterBetweenBracesHandler extends EnterBetweenBracesHandler {
  @Override
  public Result preprocessEnter(@NotNull PsiFile file, @NotNull Editor editor, @NotNull Ref<Integer> caretOffsetRef,
                                @NotNull Ref<Integer> caretAdvance, @NotNull DataContext dataContext,
                                EditorActionHandler originalHandler) {
    return !(file instanceof ErlangFile) ? Result.Continue :
      super.preprocessEnter(file, editor, caretOffsetRef, caretAdvance, dataContext, originalHandler);
  }

  @Override
  protected boolean isBracePair(char c1, char c2) {
    return super.isBracePair(c1, c2) || c1 == '[' && c2 == ']';
  }
}
