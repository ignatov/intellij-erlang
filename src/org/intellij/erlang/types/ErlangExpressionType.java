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

package org.intellij.erlang.types;

import com.intellij.openapi.util.Ref;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
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

  protected ErlangExpressionType(String name) {
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

  private static final ErlangExpressionType FLOAT      = new ErlangPrimitiveType("FLOAT");
  private static final ErlangExpressionType FUN        = new ErlangPrimitiveType("FUN");
  private static final ErlangExpressionType INTEGER    = new ErlangPrimitiveType("INTEGER");
  private static final ErlangExpressionType CHAR       = new ErlangPrimitiveType("CHAR");
  private static final ErlangExpressionType IOLIST     = new ErlangPrimitiveType("IOLIST");
  private static final ErlangExpressionType TUPLE      = new ErlangPrimitiveType("TUPLE");
  private static final ErlangExpressionType ATOM       = new ErlangPrimitiveType("ATOM");
  private static final ErlangExpressionType BINARY     = new ErlangPrimitiveType("BINARY");
  private static final ErlangExpressionType BITSTRING  = new ErlangPrimitiveType("BITSTRING");
  private static final ErlangExpressionType STRING     = new ErlangPrimitiveType("STRING");
  private static final ErlangExpressionType PID        = new ErlangPrimitiveType("PID");
  private static final ErlangExpressionType PORT       = new ErlangPrimitiveType("PORT");
  private static final ErlangExpressionType REF        = new ErlangPrimitiveType("REF");
  private static final ErlangExpressionType TERM       = new ErlangPrimitiveType("TERM");
  private static final ErlangExpressionType BOOLEAN    = new ErlangPrimitiveType("BOOLEAN");
  public static final ErlangExpressionType UNKNOWN    = new ErlangPrimitiveType("UNKNOWN");
  private static final ErlangExpressionType LIST       = new ErlangPrimitiveType("LIST") {
    @Override
    public boolean accept(ErlangExpressionType type) {
      return super.accept(type) || type.equals(STRING);
    }
  };

  public static final Map<String , ErlangExpressionType> TYPE_MAP = ContainerUtil.newMapFromValues(
    ContainerUtil.list(
      FLOAT, FUN, INTEGER, LIST, IOLIST, TUPLE, ATOM, BINARY, BITSTRING, STRING, PID, PORT, REF, TERM, BOOLEAN
    ).iterator(), erlangExpressionType -> erlangExpressionType.myName.toLowerCase());

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
          ref.set(calculateFunctionType((ErlangFunction) element));
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
    ErlangSpecification spec = function.findSpecification();
    if (spec == null) return UNKNOWN;
    ErlangFunTypeSigs signature = ErlangPsiImplUtil.getSignature(spec);
    if (signature == null) return UNKNOWN;
    for (ErlangTypeSig typeSig : signature.getTypeSigList()) {
      ErlangFunType funType = typeSig.getFunType();
      ErlangTopTypeClause typeClause = funType.getTopTypeClause();
      ErlangType type = PsiTreeUtil.getChildOfType(typeClause, ErlangType.class);
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
