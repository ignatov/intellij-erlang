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

package org.intellij.erlang.types;

import com.intellij.openapi.util.Ref;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlTypeFactory {
  /**
   * Simple inference from expression type
   *
   * @return The inferred type
   */
  public static ErlType fromExpression(@Nullable ErlangExpression e) {
    if (e == null) return ErlSimpleType.ANY;

    final Ref<ErlType> result = Ref.create(ErlSimpleType.ANY);

    e.accept(new ErlangVisitor() {
      @Override
      public void visitParenthesizedExpression(@NotNull ErlangParenthesizedExpression o) {
        result.set(fromExpression(o.getExpression()));
      }

      @Override
      public void visitMultiplicativeExpression(@NotNull ErlangMultiplicativeExpression o) {
        var firstChild = o.getFirstChild();
        var operator = firstChild.getNextSibling();

        if (operator == null) return;

        var operatorElementType = operator.getNode().getElementType();

        if (operatorElementType == ErlangTypes.ERL_OP_AR_DIV) {
          result.set(ErlSimpleType.FLOAT);
        }
        else if (operatorElementType == ErlangTypes.ERL_OP_AR_MUL) {
          var leftType = fromExpression(o.getLeft());
          var rightType = fromExpression(o.getRight());

          if (leftType.equals(rightType)) {
            result.set(leftType);
          }
          else {
            result.set(ErlSimpleType.FLOAT);
          }
        }
      }

      @Override
      public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
        var reference = o.getReference();
        PsiElement element = reference.resolve();

        if (element instanceof ErlangFunction functionElement) {
          result.set(calculateFunctionType(functionElement));
        }
      }

      @Override
      public void visitBinaryExpression(@NotNull ErlangBinaryExpression o) {
        result.set(ErlSimpleType.BINARY);
      }

      @Override
      public void visitTupleExpression(@NotNull ErlangTupleExpression o) {
        result.set(ErlSimpleType.TUPLE);
      }

      @Override
      public void visitListExpression(@NotNull ErlangListExpression o) {
        result.set(ErlSimpleType.LIST);
      }

      @Override
      public void visitAdditiveExpression(@NotNull ErlangAdditiveExpression addExpr) {
        ErlType leftType = fromExpression(addExpr.getLeft());
        ErlType rightType = fromExpression(addExpr.getRight());

        if (leftType.equals(rightType)) result.set(leftType);
      }

      @Override
      public void visitMaxExpression(@NotNull ErlangMaxExpression o) {
        if (o.getInteger() != null) {
          result.set(ErlSimpleType.INTEGER);
        }
        else if (o.getFloat() != null) {
          result.set(ErlSimpleType.FLOAT);
        }
        else if (o.getQAtom() != null) {
          result.set(ErlSimpleType.ATOM);
        }
        else if (o.getChar() != null) result.set(ErlSimpleType.CHAR);
      }

      @Override
      public void visitStringLiteral(@NotNull ErlangStringLiteral o) {
        result.set(ErlSimpleType.STRING);
      }

      @Override
      public void visitFunExpression(@NotNull ErlangFunExpression o) {
        result.set(ErlSimpleType.FUN);
      }
    });
    return result.get();
  }

  /**
   * Create ErlType from a string representation ("integer" becomes INTEGER_TYPE), type params are ignored.
   *
   * @param text The string representation of the type
   * @return The type guessed or a null
   */
  public static @Nullable ErlType fromString(@Nullable String text) {
    if (text == null || text.equals("any")) return ErlSimpleType.ANY;

    if (text.equals("float")) return ErlSimpleType.FLOAT;
    if (text.equals("integer")) return ErlSimpleType.INTEGER;
    if (text.equals("number")) return ErlSimpleType.NUMBER;
    if (text.equals("binary")) return ErlSimpleType.BINARY;
    if (text.equals("list")) return ErlSimpleType.LIST;
    if (text.equals("[]") || text.equals("nil")) return ErlSimpleType.NIL;
    if (text.equals("string")) return ErlSimpleType.STRING;
    if (text.equals("tuple")) return ErlSimpleType.TUPLE;
    if (text.equals("atom")) return ErlSimpleType.ATOM;
    if (text.equals("char")) return ErlSimpleType.CHAR;
    if (text.equals("fun")) return ErlSimpleType.FUN;

    return null;
  }

  @NotNull
  public static ErlType calculateFunctionType(@NotNull ErlangFunction function) {
    var spec = function.findSpecification();

    // TODO: Infer type properly
    if (spec == null) return ErlSimpleType.ANY;

    var signature = ErlangPsiImplUtil.getSignature(spec);

    if (signature == null) return ErlSimpleType.ANY;

    for (ErlangTypeSig typeSig : signature.getTypeSigList()) {
      var funType = typeSig.getFunType();
      var typeClause = funType.getTopTypeClause();
      var type = PsiTreeUtil.getChildOfType(typeClause, ErlangType.class);
      var typeRef = type != null ? type.getTypeRef() : null;
      var text = typeRef != null ? typeRef.getText() : null;
      var expressionType = fromString(text);

      if (expressionType != null) return expressionType;
    }

    return ErlSimpleType.ANY;
  }
}
