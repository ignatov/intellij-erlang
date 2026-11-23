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

package org.intellij.erlang.quickfixes;

import com.intellij.codeInsight.intention.FileModifier.SafeFieldForPreview;
import com.intellij.codeInsight.intention.preview.IntentionPreviewInfo;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.SmartPointerManager;
import com.intellij.psi.SmartPsiElementPointer;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangRecordDefinition;
import org.intellij.erlang.psi.ErlangRecordExpression;
import org.intellij.erlang.psi.ErlangTypedRecordFields;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.statistics.ErlangUsageCounters;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangIntroduceRecordFieldFix extends ErlangQuickFixBase {
  @SafeFieldForPreview
  @Nullable
  private final SmartPsiElementPointer<ErlangRecordDefinition> myDefinition;

  public ErlangIntroduceRecordFieldFix(@Nullable ErlangRecordDefinition definition) {
    myDefinition = definition != null ? SmartPointerManager.createPointer(definition) : null;
  }

  @NotNull
  @Override
  public String getFamilyName() {
    return "Introduce record field";
  }

  @Override
  public @NotNull PsiElement getElementToMakeWritable(@NotNull PsiFile currentFile) {
    ErlangRecordDefinition definition = myDefinition != null ? myDefinition.getElement() : null;
    PsiFile file = definition != null ? definition.getContainingFile() : null;
    return file != null ? file : currentFile;
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    ErlangUsageCounters.countQuickFix(project, getClass());
    PsiElement element = descriptor.getPsiElement();
    ErlangTypedRecordFields fields = getRecordFields(element);
    if (fields == null || !fields.getContainingFile().isWritable()) return;
    fields.replace(createNewFields(project, fields, element));
  }

  @Override
  public @NotNull IntentionPreviewInfo generatePreview(@NotNull Project project,
                                                       @NotNull ProblemDescriptor previewDescriptor) {
    PsiElement element = previewDescriptor.getPsiElement();
    ErlangTypedRecordFields fields = getRecordFields(element);
    if (fields == null) return IntentionPreviewInfo.EMPTY;
    PsiElement newFields = createNewFields(project, fields, element);
    PsiFile file = fields.getContainingFile();
    if (file == element.getContainingFile()) {
      fields.replace(newFields);
      return IntentionPreviewInfo.DIFF;
    }
    PsiElement definition = fields.getParent();
    String oldText = definition.getText();
    TextRange range = fields.getTextRange().shiftLeft(definition.getTextRange().getStartOffset());
    return new IntentionPreviewInfo.CustomDiff(file.getFileType(), file.getName(), oldText,
                                               range.replace(oldText, newFields.getText()));
  }

  @Nullable
  private static ErlangTypedRecordFields getRecordFields(@NotNull PsiElement element) {
    ErlangRecordExpression recordExpression = PsiTreeUtil.getParentOfType(element, ErlangRecordExpression.class);
    PsiReference reference = recordExpression != null ? recordExpression.getReferenceInternal() : null;
    PsiElement resolve = reference != null ? reference.resolve() : null;
    return resolve instanceof ErlangRecordDefinition definition ? definition.getTypedRecordFields() : null;
  }

  @NotNull
  private static PsiElement createNewFields(@NotNull Project project,
                                           @NotNull ErlangTypedRecordFields fields,
                                           @NotNull PsiElement element) {
    String replace = fields.getText().replaceFirst("\\{", "").replace("}", "");
    boolean empty = StringUtil.isEmptyOrSpaces(replace);

    String newFields = replace + (empty ? "" : ", ") + element.getText();
    return ErlangElementFactory.createRecordFieldsFromText(project, newFields);
  }
}
