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

package org.intellij.erlang.inspection;

import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.psi.PsiFile;
import com.intellij.util.Processor;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class ErlangR17SyntaxInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;

    Module module = ModuleUtilCore.findModuleForPsiElement(file);
    Sdk sdk = module == null ? null : ModuleRootManager.getInstance(module).getSdk();
    ErlangSdkRelease release = sdk != null ? ErlangSdkType.getRelease(sdk) : null;
    if (release != null && release.isNewerThan(ErlangSdkRelease.R17)) return;

    file.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitFunExpression(@NotNull ErlangFunExpression funExpression) {
        super.visitFunExpression(funExpression);
        ErlangFunClauses funClauses = funExpression.getFunClauses();
        List<ErlangFunClause> funClauseList = funClauses != null ? funClauses.getFunClauseList() : null;
        if (ContainerUtil.isEmpty(funClauseList)) return;
        ContainerUtil.process(funClauseList, new Processor<ErlangFunClause>() {
          @Override
          public boolean process(ErlangFunClause funClause) {
            ErlangArgumentDefinition funName = funClause.getArgumentDefinition();
            if (funName != null) {
              problemsHolder.registerProblem(funName, "Named funs require Erlang/OTP 17.0");
            }
            return true;
          }
        });
      }

      @Override
      public void visitMapExpression(@NotNull ErlangMapExpression mapExpression) {
        super.visitMapExpression(mapExpression);
        problemsHolder.registerProblem(mapExpression, "Maps require Erlang/OTP 17.0");
      }
    });
  }
}
