/*
 * Copyright 2012-2026 Sergey Ignatov
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
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.ErlangExpression;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangListComprehension;
import org.intellij.erlang.psi.ErlangVisitor;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

public class Erlang29SyntaxInspection extends ErlangInspectionBase {
  private static final String MULTI_VALUED_COMPREHENSION_MESSAGE =
    "Multi-valued comprehensions are only supported in Erlang 29 and newer versions";

  @Override
  protected boolean canRunOn(@NotNull ErlangFile file) {
    ErlangSdkRelease release = ErlangSdkType.getRelease(file);
    return release == null || ErlangSdkRelease.V_29_0.isNewerThan(release);
  }

  @NotNull
  @Override
  protected ErlangVisitor buildErlangVisitor(@NotNull final ProblemsHolder holder, @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitListComprehension(@NotNull ErlangListComprehension o) {
        int headExpressionCount = 0;
        for (ASTNode child : o.getNode().getChildren(null)) {
          if (child.getElementType() == ErlangTypes.ERL_OR_OR) {
            return;
          }

          PsiElement psi = child.getPsi();
          if (psi instanceof ErlangExpression) {
            headExpressionCount++;
            if (headExpressionCount > 1) {
              registerProblem(holder, psi, MULTI_VALUED_COMPREHENSION_MESSAGE);
            }
          }
        }
      }
    };
  }
}
