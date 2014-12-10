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
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class ErlangRemoveFunctionFromImportFix  extends ErlangQuickFixBase {
  final boolean myOnlyCurrentImport;

  public ErlangRemoveFunctionFromImportFix(boolean onlyCurrentImport) {
    myOnlyCurrentImport = onlyCurrentImport;
  }

  public ErlangRemoveFunctionFromImportFix() {
    this(false);
  }

  @NotNull

  @Override
  public String getFamilyName() {
    return "Remove from import";
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    PsiElement function = PsiTreeUtil.getParentOfType(descriptor.getPsiElement(), ErlangFunction.class);
    if (function == null) {
       function = PsiTreeUtil.getParentOfType(descriptor.getPsiElement(), ErlangImportFunction.class, false);
    }
    if (function == null) return;
    PsiFile file = function.getContainingFile();
    if (!(file instanceof ErlangFile)) return;

    String fullName;
    if (function instanceof ErlangFunction) {
      fullName = ErlangPsiImplUtil.createFunctionPresentation((ErlangFunction) function);
    }
    else {
      fullName = ErlangPsiImplUtil.createFunctionPresentation((ErlangImportFunction) function);
    }
    if (fullName == null) return;

    if (myOnlyCurrentImport && function instanceof ErlangImportFunction) {
      ErlangAttribute currentImportAttribute = PsiTreeUtil.getParentOfType(function, ErlangAttribute.class);
      if (currentImportAttribute != null)
        removeFunctionFromImport(project, currentImportAttribute, fullName);
      return;
    }
    for (ErlangAttribute attribute : ((ErlangFile) file).getAttributes()) {
      removeFunctionFromImport(project, attribute, fullName);
    }
  }

  public static void removeFunctionFromImport(@NotNull Project project, @NotNull ErlangAttribute importAttribute, @NotNull String name) {
    ErlangImportDirective importDirective = importAttribute.getImportDirective();
    if (importDirective == null || importDirective.getModuleRef() == null) return;
    ErlangImportFunctions fns = importDirective.getImportFunctions();
    List<ErlangImportFunction> functions = fns == null ? ContainerUtil.<ErlangImportFunction>emptyList() : fns.getImportFunctionList();
    if (functions.isEmpty()) return;
    List<String> newImports = ContainerUtil.newArrayList();
    for (ErlangImportFunction f : functions) {
      String presentation = ErlangPsiImplUtil.createFunctionPresentation(f);
      if (presentation == null || !presentation.equals(name)) {
        newImports.add(f.getText());
      }
    }
    if (newImports.isEmpty()) {
      if (importAttribute.getNextSibling() instanceof PsiWhiteSpace)
        importAttribute.getNextSibling().delete();
      importAttribute.delete();
    }
    else {
      String moduleName = importDirective.getModuleRef().getText();
      importAttribute.replace(ErlangElementFactory.createImportFromText(project, moduleName, StringUtil.join(newImports, ", ")));
    }
  }

}
