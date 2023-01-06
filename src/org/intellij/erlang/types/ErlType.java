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

/**
 * Describes an Erlang type with optional type parameters.
 */
public class ErlType {
  public enum Kind {
    ANY,
    NONE, // empty type
    ATOM,
    BINARY,
    BIT_STRING, // also BIT_STRING is BINARY
    BOOLEAN,
    FLOAT,
    INTEGER,
    CHAR, // also CHAR is INTEGER
    NUMBER, // implies INTEGER|FLOAT
    LIST,
    STRING, // also STRING is LIST
    NIL, // also NIL is LIST
    MAP,
    PID,
    PORT,
    REF,
    TUPLE,
    FUN, // a generic function type, any arity, any return
  }

  public Kind kind;

  public static ErlType ANY_TYPE = new ErlType(Kind.ANY);
  public static ErlType NONE_TYPE = new ErlType(Kind.NONE);
  public static ErlType FLOAT_TYPE = new ErlType(Kind.FLOAT);
  public static ErlType INTEGER_TYPE = new ErlType(Kind.INTEGER);
  public static ErlType NUMBER_TYPE = new ErlType(Kind.NUMBER);
  public static ErlType BINARY_TYPE = new ErlType(Kind.BINARY);
  public static ErlType LIST_TYPE = new ErlType(Kind.LIST);
  public static ErlType NIL_TYPE = new ErlType(Kind.NIL);
  public static ErlType STRING_TYPE = new ErlType(Kind.STRING);
  public static ErlType TUPLE_TYPE = new ErlType(Kind.TUPLE);
  public static ErlType ATOM_TYPE = new ErlType(Kind.ATOM);
  public static ErlType CHAR_TYPE = new ErlType(Kind.CHAR);
  public static ErlType FUN_TYPE = new ErlType(Kind.FUN);

  public ErlType(Kind kind) {
    this.kind = kind;
  }

  @Override
  public String toString() {
    switch (kind) {
      case ANY -> {
        return "any()";
      }
      case NONE -> {
        return "none()";
      }
      case ATOM -> {
        return "atom()";
      }
      case BINARY -> {
        return "binary()";
      }
      case BIT_STRING -> {
        return "bit_string()";
      }
      case BOOLEAN -> {
        return "boolean()";
      }
      case FLOAT -> {
        return "float()";
      }
      case INTEGER -> {
        return "integer()";
      }
      case CHAR -> {
        return "char()";
      }
      case NUMBER -> {
        return "number()";
      }
      case LIST -> {
        return "list()"; // TODO: add type parameters
      }
      case STRING -> {
        return "string()";
      }
      case NIL -> {
        return "nil()";
      }
      case MAP -> {
        return "map()";
      }
      case PID -> {
        return "pid()";
      }
      case PORT -> {
        return "port()";
      }
      case REF -> {
        return "reference()";
      }
      case TUPLE -> {
        return "tuple()";
      }
      case FUN -> {
        return "fun()";
      }
      default -> {
        assert false : "Type kind=%s toString: Not implemented".formatted(kind);
        return "none()";
      }
    }
  }

  /**
   * Simple inference from expression type
   *
   * @return The inferred type
   */
  public static ErlType fromExpression(@Nullable ErlangExpression e) {
    if (e == null) return ANY_TYPE;

    final Ref<ErlType> result = Ref.create(ANY_TYPE);

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
          result.set(FLOAT_TYPE);
        }
        else if (operatorElementType == ErlangTypes.ERL_OP_AR_MUL) {
          var leftType = fromExpression(o.getLeft());
          var rightType = fromExpression(o.getRight());

          if (leftType.equals(rightType)) {
            result.set(leftType);
          }
          else {
            result.set(FLOAT_TYPE);
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
        result.set(BINARY_TYPE);
      }

      @Override
      public void visitTupleExpression(@NotNull ErlangTupleExpression o) {
        result.set(TUPLE_TYPE);
      }

      @Override
      public void visitListExpression(@NotNull ErlangListExpression o) {
        result.set(LIST_TYPE);
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
          result.set(INTEGER_TYPE);
        }
        else if (o.getFloat() != null) {
          result.set(FLOAT_TYPE);
        }
        else if (o.getQAtom() != null) {
          result.set(ATOM_TYPE);
        }
        else if (o.getChar() != null) result.set(CHAR_TYPE);
      }

      @Override
      public void visitStringLiteral(@NotNull ErlangStringLiteral o) {
        result.set(STRING_TYPE);
      }

      @Override
      public void visitFunExpression(@NotNull ErlangFunExpression o) {
        result.set(FUN_TYPE);
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
    if (text == null || text.equals("any")) return ANY_TYPE;

    if (text.equals("float")) return FLOAT_TYPE;
    if (text.equals("integer")) return INTEGER_TYPE;
    if (text.equals("number")) return NUMBER_TYPE;
    if (text.equals("binary")) return BINARY_TYPE;
    if (text.equals("list")) return LIST_TYPE;
    if (text.equals("[]") || text.equals("nil")) return NIL_TYPE;
    if (text.equals("string")) return STRING_TYPE;
    if (text.equals("tuple")) return TUPLE_TYPE;
    if (text.equals("atom")) return ATOM_TYPE;
    if (text.equals("char")) return CHAR_TYPE;
    if (text.equals("fun")) return FUN_TYPE;

    return null;
  }

  public boolean isSubtypeOf(ErlType other) {
    if (this.equals(other)) return true;
    if (other.kind == Kind.ANY) return true;
    if ((kind == Kind.FLOAT || kind == Kind.INTEGER) && other.kind == Kind.NUMBER) return true;
    if (kind == Kind.STRING && other.kind == Kind.LIST) return true;
    if (kind == Kind.NIL && other.kind == Kind.LIST) return true;

    return false;
  }

  @NotNull
  public static ErlType calculateFunctionType(@NotNull ErlangFunction function) {
    var spec = function.findSpecification();

    // TODO: Infer type properly
    if (spec == null) return ANY_TYPE;

    var signature = ErlangPsiImplUtil.getSignature(spec);

    if (signature == null) return ANY_TYPE;

    for (ErlangTypeSig typeSig : signature.getTypeSigList()) {
      var funType = typeSig.getFunType();
      var typeClause = funType.getTopTypeClause();
      var type = PsiTreeUtil.getChildOfType(typeClause, ErlangType.class);
      var typeRef = type != null ? type.getTypeRef() : null;
      String text = typeRef != null ? typeRef.getText() : null;
      ErlType expressionType = fromString(text);
      if (expressionType != null) return expressionType;
    }

    return ANY_TYPE;
  }

}
