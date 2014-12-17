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
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangFunctionCallExpression;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.jetbrains.annotations.NotNull;

public class ErlangSpecifyModulePrefixFix extends ErlangQuickFixBase {
  private final String myModuleName;

  public ErlangSpecifyModulePrefixFix(@NotNull String moduleName) {
    myModuleName = moduleName;
  }

  @NotNull
  @Override
  public String getFamilyName() {
    return "Specify erlang module";
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    ErlangFunctionCallExpression callExpr = PsiTreeUtil.getParentOfType(
      descriptor.getPsiElement(), ErlangFunctionCallExpression.class);
    if (callExpr == null) return;
    callExpr.replace(ErlangElementFactory.createFunctionWithModuleCallExpression(project, myModuleName, callExpr.getText()));
  }

}
