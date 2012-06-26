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
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangIcons;
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
            .setIcon(ErlangIcons.FUNCTION).setTailText("/" + function.getArity());
        }
      });
    }
    return Collections.emptyList();
  }

  @NotNull
  public static List<LookupElement> getRecordLookupElements(PsiFile containingFile) {
    if (containingFile instanceof ErlangFile) {
      return ContainerUtil.map(((ErlangFile) containingFile).getRecords(), new Function<ErlangRecordDefinition, LookupElement>() {
        @Override
        public LookupElement fun(ErlangRecordDefinition rd) {
          return LookupElementBuilder.create(rd).setIcon(ErlangIcons.RECORD);
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

  @NotNull
  public static String getName(ErlangRecordDefinition o) {
    ErlangQAtom atom = o.getQAtom();
    if (atom == null) return "";
    return atom.getText();
  }

  @NotNull
  public static PsiElement getNameIdentifier(ErlangRecordDefinition o) {
    ErlangQAtom atom = o.getQAtom();
    return atom != null ? atom : o;
  }

  public static int getTextOffset(ErlangRecordDefinition o) {
    return o.getNameIdentifier().getTextOffset();
  }

  @NotNull
  public static PsiElement getNameIdentifier(ErlangQVar o) {
    return o;
  }

  @NotNull
  public static PsiElement getNameIdentifier(ErlangFunction o) {
    return o.getAtomName();
  }

  @Nullable
  public static PsiReference getReference(ErlangRecordExpression o) {
    ErlangQAtom atom = o.getAtomName();
    if (atom == null) return null;
    return new ErlangRecordReferenceImpl<ErlangRecordExpression>(o,
      TextRange.from(atom.getStartOffsetInParent(), atom.getTextLength()), atom.getText());
  }
}
