/*
 * Copyright 2012-2023 Sergey Ignatov
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

package org.intellij.erlang.psi;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Static helper methods to handle PSI tree nodes as specific node types.
 * TODO: Merge these methods into base classes for those nodes.
 */
public final class PsiExprUtil {
  private PsiExprUtil() {
  }

  /*
   * Under fair assumption that an assignment contains a variable and some expression, unwraps
   * the assignment expression into these two parts.
   * As this is used for naming generated type specs, precision is less important here, we only
   * need to extract a fairly relevant variable name and the expression part.
   * TODO: This goes into ErlangAssignmentExpression.
   */
  public static class AssignmentUnwrapResult {
    public AssignmentUnwrapResult(@Nullable ErlangQVar qVar, @NotNull ErlangExpression expr) {
      this.qVar = qVar;
      this.expression = expr;
    }

    public final @Nullable ErlangQVar qVar;
    public final ErlangExpression expression;

    // For logging
    public String getQVarString() {
      if (qVar == null) return "";
      return qVar.toString();
    }
  }

  /**
   * Given an Expression node, try to break an assignment into variable and value part
   *
   * @return Either both parts set in an AssignmentUnwrapResult, or only expr part set
   */
  public static AssignmentUnwrapResult extractQVarFromAssignment(ErlangExpression expression) {
    // Simple case the expression IS a QVar
    if (expression instanceof ErlangMaxExpression maxExpression) {
      var qVar = getQVar(maxExpression);
      if (qVar != null) {
        // unwrap as QVar=QVar, good enough for spec building
        return new AssignmentUnwrapResult(qVar, expression);
      }
    }

    if (expression instanceof ErlangAssignmentExpression assignmentExpression) {
      var right = assignmentExpression.getRight();
      var left = assignmentExpression.getLeft();

      if (right instanceof ErlangMaxExpression rightMaxExpr) {
        var rightQVar = getQVar(rightMaxExpr);
        if (rightQVar != null) return new AssignmentUnwrapResult(rightQVar, left);
      }
      else {
        if (right != null && left instanceof ErlangMaxExpression leftMaxExpr) {
          var leftQVar = getQVar(leftMaxExpr);
          if (leftQVar != null) return new AssignmentUnwrapResult(leftQVar, right);
        }
      }
    }

    return new AssignmentUnwrapResult(null, expression);
  }

  /**
   * Check whether an expression is QVar only
   *
   * @return QVar of the expression if it consists only of QVar
   */
  private static @Nullable ErlangQVar getQVar(@NotNull ErlangMaxExpression expression) {
    if (expression.getFirstChild() instanceof ErlangQVar qVar) {
      return qVar;
    }
    return null;
  }
}
