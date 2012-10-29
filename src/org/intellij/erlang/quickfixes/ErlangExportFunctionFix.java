/*
 * Copyright 2012 Sergey Ignatov
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

import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * @author ignatov
 */
public class ErlangExportFunctionFix implements LocalQuickFix {
  @NotNull
  @Override
  public String getName() {
    return getFamilyName();
  }

  @NotNull
  @Override
  public String getFamilyName() {
    return "Export function";
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    ErlangFunction function = PsiTreeUtil.getParentOfType(descriptor.getPsiElement(), ErlangFunction.class);

    if (function != null) {
      PsiFile containingFile = function.getContainingFile();
      if (containingFile instanceof ErlangFile) {
        String exportText = ErlangPsiImplUtil.createFunctionPresentation(function);

        List<ErlangAttribute> attributes = ((ErlangFile) containingFile).getAttributes();
        for (ErlangAttribute attribute : attributes) {
          ErlangExport export = attribute.getExport();
          if (export != null) {
            ErlangExportFunctions exportFunctions = export.getExportFunctions();

            if (exportFunctions != null) {
              String s = exportFunctions.getText().replace("[", "").replace("]", "") + ", " + exportText;
              PsiElement newExportFunctions = ErlangElementFactory.createExportFromText(project, s);

              attribute.replace(newExportFunctions);
            }
            return;
          }
        }
        List<ErlangRecordDefinition> records = ((ErlangFile) containingFile).getRecords();
        List<ErlangFunction> functions = ((ErlangFile) containingFile).getFunctions();

        ErlangRecordDefinition firstRecord = ContainerUtil.getFirstItem(records);
        ErlangFunction firstFunction = ContainerUtil.getFirstItem(functions);

        ErlangNamedElement elementBefore = firstRecord != null ? firstRecord : firstFunction;

        if (elementBefore != null) {
          containingFile.addBefore(ErlangElementFactory.createExportFromText(project, exportText), elementBefore);
          containingFile.addBefore(ErlangElementFactory.createLeafFromText(project, "."), elementBefore);
          containingFile.addBefore(ErlangElementFactory.createLeafFromText(project, "\n\n"), elementBefore);
        }
      }
    }
  }
}
