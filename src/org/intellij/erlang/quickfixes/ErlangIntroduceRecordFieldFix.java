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

package org.intellij.erlang.quickfixes;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangRecordDefinition;
import org.intellij.erlang.psi.ErlangRecordExpression;
import org.intellij.erlang.psi.ErlangTypedRecordFields;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.jetbrains.annotations.NotNull;

public class ErlangIntroduceRecordFieldFix extends ErlangQuickFixBase {
  @NotNull
  @Override
  public String getFamilyName() {
    return "Introduce record field";
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    PsiElement element = descriptor.getPsiElement();
    ErlangRecordExpression recordExpression = PsiTreeUtil.getParentOfType(element, ErlangRecordExpression.class);
    if (recordExpression != null) {
      PsiReference reference = recordExpression.getReferenceInternal();
      PsiElement resolve = reference != null ? reference.resolve() : null;
      if (resolve != null) {
        ErlangTypedRecordFields fields = ((ErlangRecordDefinition) resolve).getTypedRecordFields();
        if (fields != null) {
          String replace = fields.getText().replaceFirst("\\{", "").replace("}", "");
          boolean empty = StringUtil.isEmptyOrSpaces(replace);

          String newFields = replace + (empty ? "" : " ,") + element.getText();
          PsiElement recordFieldsFromText = ErlangElementFactory.createRecordFieldsFromText(project, newFields);

          fields.replace(recordFieldsFromText);
        }
      }
    }
  }
}
