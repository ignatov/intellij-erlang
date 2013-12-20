/*
 * Copyright 2012-2013 Sergey Ignatov
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
import com.intellij.psi.PsiReference;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.Convertor;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Map;

public abstract class ErlangExpressionType {
  public String getName() {
    return myName;
  }

  private final String myName;

  public ErlangExpressionType(String name) {
    myName = name;
  }

  @Override
  public String toString() {
    return "ErlangExpressionType{'" + myName + "'}";
  }

  public boolean accept(ErlangExpressionType type) {
    return equals(type);
  }

  public static class ErlangPrimitiveType extends ErlangExpressionType {
    public ErlangPrimitiveType(@NotNull String name) {
      super(name);
    }
  }

  public static final ErlangExpressionType FLOAT      = new ErlangPrimitiveType("FLOAT");
  public static final ErlangExpressionType FUN        = new ErlangPrimitiveType("FUN");
  public static final ErlangExpressionType INTEGER    = new ErlangPrimitiveType("INTEGER");
  public static final ErlangExpressionType CHAR       = new ErlangPrimitiveType("CHAR");
  public static final ErlangExpressionType IOLIST     = new ErlangPrimitiveType("IOLIST");
  public static final ErlangExpressionType TUPLE      = new ErlangPrimitiveType("TUPLE");
  public static final ErlangExpressionType ATOM       = new ErlangPrimitiveType("ATOM");
  public static final ErlangExpressionType BINARY     = new ErlangPrimitiveType("BINARY");
  public static final ErlangExpressionType BITSTRING  = new ErlangPrimitiveType("BITSTRING");
  public static final ErlangExpressionType STRING     = new ErlangPrimitiveType("STRING");
  public static final ErlangExpressionType PID        = new ErlangPrimitiveType("PID");
  public static final ErlangExpressionType PORT       = new ErlangPrimitiveType("PORT");
  public static final ErlangExpressionType REF        = new ErlangPrimitiveType("REF");
  public static final ErlangExpressionType TERM       = new ErlangPrimitiveType("TERM");
  public static final ErlangExpressionType BOOLEAN    = new ErlangPrimitiveType("BOOLEAN");
  public static final ErlangExpressionType UNKNOWN    = new ErlangPrimitiveType("UNKNOWN");
  public static final ErlangExpressionType LIST       = new ErlangPrimitiveType("LIST") {
    @Override
    public boolean accept(ErlangExpressionType type) {
      return super.accept(type) || type.equals(STRING);
    }
  };

  public static final Map<String , ErlangExpressionType> TYPE_MAP = ContainerUtil.newMapFromValues(
    ContainerUtil.<ErlangExpressionType>list(
      FLOAT, FUN, INTEGER, LIST, IOLIST, TUPLE, ATOM, BINARY, BITSTRING, STRING, PID, PORT, REF, TERM, BOOLEAN
    ).iterator(), new Convertor<ErlangExpressionType, String>() {
      @Override
      public String convert(ErlangExpressionType erlangExpressionType) {
        return erlangExpressionType.myName.toLowerCase();
      }
    });

  @NotNull
  public static ErlangExpressionType create(@Nullable ErlangExpression e) {
    if (e == null) return UNKNOWN;
    final Ref<ErlangExpressionType> ref = Ref.create(UNKNOWN);
    e.accept(new ErlangVisitor() {
      @Override
      public void visitParenthesizedExpression(@NotNull ErlangParenthesizedExpression o) {
        ref.set(create(o.getExpression()));
      }

      @Override
      public void visitMultiplicativeExpression(@NotNull ErlangMultiplicativeExpression o) {
        PsiElement firstChild = o.getFirstChild();
        PsiElement operator = firstChild.getNextSibling();
        if (operator == null) return;
        IElementType operatorElementType = operator.getNode().getElementType();
        if (operatorElementType == ErlangTypes.ERL_OP_AR_DIV) ref.set(FLOAT);
        else if (operatorElementType == ErlangTypes.ERL_OP_AR_MUL) {
          ErlangExpressionType leftType = create(o.getLeft());
          ErlangExpressionType rightType = create(o.getRight());
          if (leftType.equals(rightType)) ref.set(leftType);
          else ref.set(FLOAT);
        }
      }

      @Override
      public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
        PsiReference reference = o.getReference();
        PsiElement element = reference != null ? reference.resolve() : null ;
        if (element instanceof ErlangFunction) {
          ref.set(calculateFunctionType(((ErlangFunction) element)));
        }
      }

      @Override
      public void visitBinaryExpression(@NotNull ErlangBinaryExpression o) {
        ref.set(BINARY);
      }

      @Override
      public void visitTupleExpression(@NotNull ErlangTupleExpression o) {
        ref.set(TUPLE);
      }

      @Override
      public void visitListExpression(@NotNull ErlangListExpression o) {
        ref.set(LIST);
      }

      @Override
      public void visitAdditiveExpression(@NotNull ErlangAdditiveExpression o) {
        ErlangExpressionType leftType = create(o.getLeft());
        ErlangExpressionType rightType = create(o.getRight());
        if (leftType.equals(rightType)) ref.set(leftType);
      }

      @Override
      public void visitMaxExpression(@NotNull ErlangMaxExpression o) {
        if (o.getInteger() != null) ref.set(INTEGER);
        else if (o.getFloat() != null) ref.set(FLOAT);
        else if (o.getQAtom() != null) ref.set(ATOM);
        else if (o.getChar() != null) ref.set(CHAR);
      }

      @Override
      public void visitStringLiteral(@NotNull ErlangStringLiteral o) {
        ref.set(STRING);
      }

      @Override
      public void visitFunExpression(@NotNull ErlangFunExpression o) {
        ref.set(FUN);
      }
    });
    return ref.get();
  }

  @NotNull
  public static ErlangExpressionType calculateFunctionType(@NotNull ErlangFunction function) {
    ErlangSpecification spec = ErlangPsiImplUtil.getSpecification(function);
    if (spec == null) return UNKNOWN;
    ErlangFunTypeSigs signature = ErlangPsiImplUtil.getSignature(spec);
    if (signature == null) return UNKNOWN;
    for (ErlangTypeSig typeSig : signature.getTypeSigList()) {
      ErlangFunType funType = typeSig.getFunType();
      ErlangTopTypeClause typeClause = funType.getTopTypeClause();
      ErlangTopType topType = typeClause != null ? typeClause.getTopType() : null;
      ErlangType type = topType != null ? topType.getType() : null;
      ErlangTypeRef typeRef = type != null ? type.getTypeRef() : null;
      String text = typeRef != null ? typeRef.getText() : null;
      ErlangExpressionType expressionType = TYPE_MAP.get(text);
      if (expressionType != null) return expressionType;
    }
    return UNKNOWN;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    ErlangExpressionType that = (ErlangExpressionType) o;
    if (myName != null ? !myName.equals(that.myName) : that.myName != null) return false;
    return true;
  }

  @Override
  public int hashCode() {
    return myName != null ? myName.hashCode() : 0;
  }
}
