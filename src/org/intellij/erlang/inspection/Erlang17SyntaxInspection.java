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

import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.util.Processor;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class Erlang17SyntaxInspection extends ErlangInspectionBase {
  @Override
  protected boolean canRunOn(@NotNull ErlangFile file) {
    ErlangSdkRelease release = ErlangSdkType.getRelease(file);
    return release == null || ErlangSdkRelease.V_17_0.isNewerThan(release);
  }

  @NotNull
  @Override
  protected ErlangVisitor buildErlangVisitor(@NotNull final ProblemsHolder holder, @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitFunExpression(@NotNull ErlangFunExpression funExpression) {
        ErlangFunClauses funClauses = funExpression.getFunClauses();
        List<ErlangFunClause> funClauseList = funClauses != null ? funClauses.getFunClauseList() : null;
        if (ContainerUtil.isEmpty(funClauseList)) return;
        ContainerUtil.process(funClauseList, new Processor<ErlangFunClause>() {
          @Override
          public boolean process(ErlangFunClause funClause) {
            ErlangArgumentDefinition funName = funClause.getArgumentDefinition();
            if (funName != null) {
              registerProblem(holder, funName, "Named funs require Erlang/OTP 17.0");
            }
            return true;
          }
        });
      }

      @Override
      public void visitMapExpression(@NotNull ErlangMapExpression mapExpression) {
        registerProblem(holder, mapExpression, "Maps require Erlang/OTP 17.0");
      }
    };
  }
}
