package org.intellij.erlang.psi.impl;

import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Function;
import com.intellij.util.PlatformIcons;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collections;
import java.util.List;

public class ErlangPsiImplUtil {
  private ErlangPsiImplUtil() {
  }

  @SuppressWarnings("UnusedParameters")
  public static boolean processDeclarations(@NotNull ErlangQVar o, @NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place) {
    return processor.execute(o, state);
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangQVar o) {
    return new ErlangVariableReferenceImpl(o, TextRange.from(0, o.getTextLength()));
  }

  @NotNull
  public static PsiReference getReference(@NotNull ErlangFunctionCallExpression o) {
    return new ErlangFunctionReferenceImpl<ErlangFunctionCallExpression>(
      o, TextRange.from(0, o.getExpression().getTextLength()),
      o.getExpression().getText(), o.getArgumentList().getExpressionList().size());
  }

  @NotNull
  public static PsiReference getReference(ErlangExportFunction o) {
    PsiElement arity = o.getInteger();
    return new ErlangFunctionReferenceImpl<ErlangExportFunction>(o, TextRange.from(0, o.getQAtom().getTextLength()),
      o.getQAtom().getText(), StringUtil.parseInt(arity == null ? "" : arity.getText(), -1));
  }

  public static PsiReference getReference(ErlangQAtom o) {
    return new ErlangFunctionReferenceImpl<ErlangQAtom>(o, TextRange.from(0, o.getAtom().getTextLength()), o.getAtom().getText(), -1);
  }

  // Utilities for processors

  static boolean inFunctionClause(PsiElement psiElement) {
    return PsiTreeUtil.getParentOfType(psiElement, ErlangFunctionClause.class) != null;
  }

  static boolean inDefinition(PsiElement psiElement) {
    return PsiTreeUtil.getParentOfType(psiElement, ErlangArgumentDefinition.class) != null;
  }

  static boolean isLeftPartOfAssignment(PsiElement psiElement) {
    ErlangAssignmentExpression assignmentExpression = PsiTreeUtil.getParentOfType(psiElement, ErlangAssignmentExpression.class);
    if (assignmentExpression == null) return false;
    return PsiTreeUtil.isAncestor(assignmentExpression.getLeft(), psiElement, false);
  }

  @NotNull
  static List<LookupElement> getFunctionLookupElements(@NotNull PsiFile containingFile) {
    if (containingFile instanceof ErlangFile) {
      return ContainerUtil.map(((ErlangFile) containingFile).getFunctions(), new Function<ErlangFunction, LookupElement>() {
        @Override
        public LookupElement fun(ErlangFunction function) {
          return LookupElementBuilder.create(function)
            .withIcon(PlatformIcons.FUNCTION_ICON).withTailText("/" + function.getArity());
        }
      });
    }
    return Collections.emptyList();
  }

  @NotNull
  public static String getName(ErlangFunction o) {
    return o.getAtomName().getAtom().getText();
  }

  @NotNull
  public static String getName(ErlangQVar o) {
    return o.getText();
  }

  public static int getArity(ErlangFunction o) {
    return o.getFunctionClauseList().get(0).getArgumentDefinitionList().size();
  }
}
