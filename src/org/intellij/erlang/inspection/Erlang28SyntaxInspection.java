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
import com.intellij.psi.tree.IElementType;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangLcExpression;
import org.intellij.erlang.psi.ErlangListComprehension;
import org.intellij.erlang.psi.ErlangVisitor;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

public class Erlang28SyntaxInspection extends ErlangInspectionBase {
  private static final String STRICT_MESSAGE = "Strict generators are only supported in Erlang 28 and newer versions";
  private static final String ZIP_MESSAGE = "Zip generators are only supported in Erlang 28 and newer versions";

  @Override
  protected boolean canRunOn(@NotNull ErlangFile file) {
    ErlangSdkRelease release = ErlangSdkType.getRelease(file);
    return release == null || ErlangSdkRelease.V_28_0.isNewerThan(release);
  }

  @NotNull
  @Override
  protected ErlangVisitor buildErlangVisitor(@NotNull final ProblemsHolder holder, @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitListComprehension(@NotNull ErlangListComprehension o) {
        // lc_exprs is inlined into the comprehension, so the zip '&&' separators and the
        // strict-generator lc_expression nodes are direct children. Only direct children are
        // inspected so nested comprehensions are reported by their own visit, not twice.
        for (ASTNode child : o.getNode().getChildren(null)) {
          IElementType type = child.getElementType();
          if (type == ErlangTypes.ERL_OP_AND_AND) {
            registerProblem(holder, child.getPsi(), ZIP_MESSAGE);
          }
          else if (child.getPsi() instanceof ErlangLcExpression) {
            for (ASTNode operator : child.getChildren(null)) {
              IElementType operatorType = operator.getElementType();
              if (operatorType == ErlangTypes.ERL_OP_LT_COLON_MINUS || operatorType == ErlangTypes.ERL_OP_LT_COLON_EQ) {
                registerProblem(holder, operator.getPsi(), STRICT_MESSAGE);
              }
            }
          }
        }
      }
    };
  }
}
