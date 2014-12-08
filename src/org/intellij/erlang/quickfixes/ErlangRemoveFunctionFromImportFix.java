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
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class ErlangRemoveFunctionFromImportFix  extends ErlangQuickFixBase {
  @NotNull
  @Override
  public String getFamilyName() {
    return "Remove from import";
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    ErlangFunction function = PsiTreeUtil.getParentOfType(descriptor.getPsiElement(), ErlangFunction.class, false);
    if (function == null) return;
    PsiFile file = function.getContainingFile();
    if (!(file instanceof ErlangFile)) return;
    List<ErlangAttribute> attributes = ((ErlangFile) file).getAttributes();
    for (ErlangAttribute attribute : attributes) {
      ErlangImportDirective importDirective = attribute.getImportDirective();
      if (importDirective == null || importDirective.getModuleRef() == null) continue;
      ErlangImportFunctions importFunctions = importDirective.getImportFunctions();
      if (importFunctions == null) continue;
      if (importFunctions.getImportFunctionList().size() == 0) continue;
      if (importFunctions.getImportFunctionList().size() == 1) {
        attribute.delete();
        continue;
      }
      StringBuilder newImport = new StringBuilder();
      for (ErlangImportFunction ef : importFunctions.getImportFunctionList()) {
        if (!ef.getText().equals(function.getName() + "/" + function.getArity())) {
          if (newImport.length() > 0)
            newImport.append(", ");
          newImport.append(ef.getText());
        }
      }
      String moduleName = importDirective.getModuleRef().getText();
      attribute.replace(ErlangElementFactory.createImportFromText(project, moduleName, newImport.toString()));
    }
  }

}
