/*
 * Copyright 2012-2015 Sergey Ignatov
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
import org.intellij.erlang.bif.ErlangOperatorTable;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import java.util.Set;

public class ErlangIllegalGuardInspection extends ErlangInspectionBase {
  private static final String ERROR = "Illegal guard expression";
  //Source: http://www.erlang.org/doc/man/erlang.html
  private static final Set<String> BIFS_ALLOWED_IN_GUARDS = Set.of(
    "abs/1", "bit_size/1", "binary_part/3",
    "byte_size/1", "binary_part/2", "element/2",
    "float/1", "hd/1", "is_atom/1", "is_binary/1",
    "is_bitstring/1", "is_boolean/1", "is_float/1",
    "is_function/1", "is_function/2", "is_integer/1",
    "is_list/1", "is_map/1", "is_number/1",
    "is_pid/1", "is_port/1", "is_record/2",
    "is_record/3", "is_reference/1", "is_tuple/1",
    "length/1", "map_size/1", "node/1",
    "node/0", "round/1", "self/0",
    "size/1", "tl/1", "trunc/1", "tuple_size/1",
    "map_get/2", "is_map_key/2"
  );

  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder holder) {
    for (ErlangFunction function : file.getFunctions()) {
      for (ErlangFunctionClause clause : function.getFunctionClauseList()) {
        ErlangClauseGuard guard = clause.getClauseGuard();
        if (guard != null) {
          guard.accept(new GuardInspector(holder));
        }
        ErlangClauseBody clauseBody = clause.getClauseBody();
        if (clauseBody != null) {
          clauseBody.accept(new GuardDetector(holder));
        }
      }
    }
  }

  private static class GuardDetector extends ErlangRecursiveVisitor {
    private final ProblemsHolder myHolder;

    public GuardDetector(ProblemsHolder holder) {
      this.myHolder = holder;
    }

    @Override
    public void visitClauseGuard(@NotNull ErlangClauseGuard o) {
      ErlangGuard guard = o.getGuard();
      if (guard != null) {
        guard.accept(new GuardInspector(myHolder));
      }
    }

    @Override
    public void visitGuard(@NotNull ErlangGuard o) {
      o.accept(new GuardInspector(myHolder));
    }
  }

  private static class GuardInspector extends ErlangRecursiveVisitor {
    private final ProblemsHolder myHolder;

    public GuardInspector(ProblemsHolder holder) {
      this.myHolder = holder;
    }

    @Override
    public void visitAssignmentExpression(@NotNull ErlangAssignmentExpression o) {
      registerProblem(myHolder, o, ERROR);
    }

    @Override
    public void visitCaseExpression(@NotNull ErlangCaseExpression o) {
      registerProblem(myHolder, o, ERROR);
    }

    @Override
    public void visitReceiveExpression(@NotNull ErlangReceiveExpression o) {
      registerProblem(myHolder, o, ERROR);
    }

    @Override
    public void visitTryExpression(@NotNull ErlangTryExpression o) {
      registerProblem(myHolder, o, ERROR);
    }

    @Override
    public void visitListComprehension(@NotNull ErlangListComprehension o) {
      registerProblem(myHolder, o, ERROR);
    }

    @Override
    public void visitBeginEndExpression(@NotNull ErlangBeginEndExpression o) {
      registerProblem(myHolder, o, ERROR);
    }

    @Override
    public void visitSendExpression(@NotNull ErlangSendExpression o) {
      registerProblem(myHolder, o, ERROR);
    }

    @Override
    public void visitListOpExpression(@NotNull ErlangListOpExpression o) {
      registerProblem(myHolder, o, ERROR);
    }

    @Override
    public void visitCatchExpression(@NotNull ErlangCatchExpression o) {
      registerProblem(myHolder, o, ERROR);
    }

    @Override
    public void visitFunExpression(@NotNull ErlangFunExpression o) {
      registerProblem(myHolder, o, ERROR);
    }

    @Override
    public void visitAnonymousCallExpression(@NotNull ErlangAnonymousCallExpression o) {
      registerProblem(myHolder, o, ERROR);
    }

    @Override
    public void visitIfExpression(@NotNull ErlangIfExpression o) {
      registerProblem(myHolder, o, ERROR);
    }

    @Override
    public void visitGenericFunctionCallExpression(@NotNull ErlangGenericFunctionCallExpression o) {
      registerProblem(myHolder, o, ERROR);
    }

    @Override
    public void visitGlobalFunctionCallExpression(@NotNull ErlangGlobalFunctionCallExpression o) {
      String moduleName = ErlangPsiImplUtil.getName(o.getModuleRef().getQAtom());
      ErlangFunctionCallExpression function = o.getFunctionCallExpression();
      String functionName = function.getName();
      int functionArity = function.getArgumentList().getExpressionList().size();
      String functionPresentation = ErlangPsiImplUtil.createFunctionPresentation(functionName, functionArity);

      if (!"erlang".equals(moduleName)) {
        registerProblem(myHolder, o, ERROR);
      }
      else if (!BIFS_ALLOWED_IN_GUARDS.contains(functionPresentation) &&
               !ErlangOperatorTable.canBeInvokedAsFunction(moduleName, functionPresentation)) {
        registerProblem(myHolder, function, ERROR);
      }
      else {
        function.getArgumentList().accept(this);
      }
    }

    @Override
    public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
      if (o.getQAtom().getMacros() != null) {
        return;
      }
      String functionName = o.getName();
      int functionArity = o.getArgumentList().getExpressionList().size();
      String functionPresentation = ErlangPsiImplUtil.createFunctionPresentation(functionName, functionArity);

      if (((ErlangFile) o.getContainingFile()).getFunction(functionName, functionArity) != null) {
        registerProblem(myHolder, o, "Call to local/imported function " + functionPresentation + " is illegal in guard");
      }
      else if (!BIFS_ALLOWED_IN_GUARDS.contains(functionPresentation)) {
        registerProblem(myHolder, o, ERROR);
      }
      else {
        o.getArgumentList().accept(this);
      }
    }
  }
}