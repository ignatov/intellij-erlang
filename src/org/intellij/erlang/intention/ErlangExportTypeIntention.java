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

import com.intellij.codeInsight.intention.preview.IntentionPreviewInfo;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangTypeDefinition;
import org.intellij.erlang.quickfixes.ErlangExportTypeFix;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangExportTypeIntention extends ErlangBaseNamedElementIntention {
  private static final String NAME = "Export type";

  public ErlangExportTypeIntention() {
    super(NAME, NAME);
  }

  @Override
  public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
    if (!file.getManager().isInProject(file) || !(file instanceof ErlangFile)) return false;
    ErlangTypeDefinition type = findType(file, editor.getCaretModel().getOffset());
    return type != null;
  }

  @Override
  public void invoke(@NotNull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
    ErlangTypeDefinition type = findType(file, editor.getCaretModel().getOffset());
    assert type != null;
    ErlangExportTypeFix.processType(project, type);
  }

  @Override
  public @NotNull IntentionPreviewInfo generatePreview(@NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file) {
    return IntentionPreviewInfo.EMPTY;
  }
  
  @Nullable
  private static ErlangTypeDefinition findType(PsiFile file, int offset) {
    return findElement(file, offset, ErlangTypeDefinition.class);
  }
}
