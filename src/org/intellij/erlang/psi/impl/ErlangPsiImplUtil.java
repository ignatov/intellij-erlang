//package org.intellij.erlang.psi.impl;
//
//import com.intellij.jpa.ql.model.QlElement;
//import com.intellij.jpa.ql.model.QlEntity;
//import com.intellij.jpa.ql.psi.*;
//import com.intellij.openapi.util.TextRange;
//import com.intellij.psi.*;
//import com.intellij.psi.util.MethodSignature;
//import com.intellij.psi.util.PsiTreeUtil;
//import com.intellij.psi.util.PsiTypesUtil;
//import com.intellij.psi.util.PsiUtil;
//import com.intellij.util.ArrayUtil;
//import com.intellij.util.SmartList;
//import org.jetbrains.annotations.NotNull;
//import org.jetbrains.annotations.Nullable;
//
//import java.util.Collections;
//import java.util.List;
//
//public class ErlangPsiImplUtil {
//
//  public static final PsiArrayType PACKAGE_TYPE = new PsiArrayType(PsiType.VOID);
//
//  private ErlangPsiImplUtil() {
//  }
//
//  // QlReferenceExpression -------------------------------
//  @NotNull
//  public static PsiPolyVariantReference getReference(QlReferenceExpression o) {
//    return new ErlangReferenceImpl(o);
//  }
//
//  @Nullable
//  public static QlExpression getQualifier(QlReferenceExpression o) {
//    return PsiTreeUtil.findChildOfType(o, QlExpression.class);
//  }
//
//  @Nullable
//  public static Object resolve(QlReferenceExpression o) {
//    ResolveResult[] results = o.getReference().multiResolve(false);
//    ResolveResult result = results.length > 0 ? results[0] : null;
//    if (result == null) return null;
//    if (result instanceof QlReferenceImpl.QlResolveResult) return ((QlReferenceImpl.QlResolveResult)result).getPersistentElement();
//    return result.getElement();
//  }
//
//  @Nullable
//  public static PsiType getPsiType(QlReferenceExpression o) {
//    final Object target = o.resolve();
//    if (target == null) return null;
//
//    if (target instanceof QlAliasDefinition) {
//      final QlExpression expression = ((QlAliasDefinition)target).getExpression();
//      if (expression instanceof QlReferenceExpression) {
//        final Object o1 = ((QlReferenceExpression)expression).resolve();
//        if (o1 instanceof QlElement) {
//          return ((QlElement)o1).getPsiType();
//        }
//      }
//      return null;
//    }
//
//    if (target instanceof QlElement) {
//      return ((QlElement)target).getPsiType();
//    }
//
//    if (target instanceof QlInVariableExpression) {
//      final QlReferenceExpression referenceExpression = ((QlInVariableExpression)target).getReferenceExpression();
//      return referenceExpression == null ? null : referenceExpression.getPsiType();
//    }
//
//    if (target instanceof PsiField) {
//      return ((PsiField)target).getType();
//    }
//
//    if (target instanceof QlReferenceExpression) {
//      return (target.equals(o) && ((QlReferenceExpression)target).getText().equals("size")) ? // TODO: fixme
//             JavaPsiFacade.getInstance(o.getProject()).getElementFactory().createTypeFromText(CommonClassNames.JAVA_LANG_INTEGER, o) :
//             null;
//    }
//
//    if (target instanceof PsiPackage) { // incomplete query case
//      return PACKAGE_TYPE;
//    }
//
//    if (target instanceof PsiClass) { // incomplete query case
//      return PsiElementFactory.SERVICE.getInstance(o.getProject()).createType((PsiClass)target);
//    }
//
//    return null;
//  }
//
//  @NotNull
//  public static PsiElement getNavigationElement(QlReferenceExpression o) {
//    return o.getIdentifier();
//  }
//
//  // QlConstructorExpression -------------------------------
//  @Nullable
//  public static PsiReference getReference(QlConstructorExpression o) {
//    QlReferenceExpression expression = o.getReferenceExpression();
//    if (expression == null) return null;
//    return new PsiReferenceBase<QlConstructorExpression>(o, TextRange.from(expression.getStartOffsetInParent(), expression.getTextLength())) {
//      @Override
//      public PsiElement resolve() {
//        return resolveConstructor(getElement());
//      }
//
//      @NotNull
//      @Override
//      public Object[] getVariants() {
//        return ArrayUtil.EMPTY_OBJECT_ARRAY;
//      }
//    };
//  }
//}
