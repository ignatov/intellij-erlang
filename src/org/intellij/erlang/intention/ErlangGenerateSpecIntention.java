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

package org.intellij.erlang.intention;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.quickfixes.ErlangGenerateSpecFix;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangGenerateSpecIntention extends ErlangBaseNamedElementIntention {
  public ErlangGenerateSpecIntention() {
    super(ErlangGenerateSpecFix.NAME, ErlangGenerateSpecFix.NAME);
  }

  @Override
  public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
    if (!(file instanceof ErlangFile)) return false;
    ErlangFunction function = findFunction(file, editor.getCaretModel().getOffset());
    if (function == null) return false;
    return function.findSpecification() == null;
  }

  @Override
  public void invoke(@NotNull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
    if (!(file instanceof ErlangFile)) {
      throw new IncorrectOperationException("Only applicable to Erlang files.");
    }
    ErlangFunction function = findFunction(file, editor.getCaretModel().getOffset());
    if (function == null) {
      throw new IncorrectOperationException("Cursor should be placed on Erlang function.");
    }
    if (function.findSpecification() != null) {
      throw new IncorrectOperationException("Specification for this function already exists.");
    }
    ErlangGenerateSpecFix.generateSpec(editor, function);
  }

  @Nullable
  private static ErlangFunction findFunction(PsiFile file, int offset) {
    return findElement(file, offset, ErlangFunction.class);
  }
}
