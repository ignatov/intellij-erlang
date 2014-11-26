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

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class ErlangRemoveDuplicateFunctionExportFix extends ErlangQuickFixBase {
  @NotNull
  @Override
  public String getFamilyName() {
    return "Remove duplicate export";
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    ErlangExportFunction function = PsiTreeUtil.getParentOfType(descriptor.getPsiElement(), ErlangExportFunction.class, false);
    if (function == null) return;
    PsiFile file = function.getContainingFile();
    if (!(file instanceof ErlangFile)) return;
    List<ErlangAttribute> attributes = ((ErlangFile) file).getAttributes();
    for (ErlangAttribute attribute : attributes) {
      ErlangExport export = attribute.getExport();
      if (export == null) continue;
      ErlangExportFunctions exportFunctions = export.getExportFunctions();
      if (exportFunctions == null) continue;
      if (exportFunctions.getChildren().length == 0) continue;
      StringBuilder newExport = new StringBuilder();
      for (ErlangExportFunction ef : exportFunctions.getExportFunctionList()) {
        if (ef == function || !ef.getText().equals(function.getText())) {
          if (newExport.length() > 0)
            newExport.append(", ");
          newExport.append(ef.getText());
        }
      }
      if (newExport.length() == 0) {
        if (attribute.getNextSibling() instanceof PsiWhiteSpace)
          attribute.getNextSibling().delete();
        attribute.delete();
      }
      else {
        attribute.replace(ErlangElementFactory.createExportFromText(project, newExport.toString()));
      }
    }
  }

}
