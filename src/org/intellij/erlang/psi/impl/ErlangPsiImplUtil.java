package org.intellij.erlang.psi.impl;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import org.intellij.erlang.psi.ErlangExpression;
import org.intellij.erlang.psi.ErlangFunctionCallExpression;
import org.intellij.erlang.psi.ErlangQVar;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangPsiImplUtil {
  private ErlangPsiImplUtil() {
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangFunctionCallExpression o) {
    return new ErlangReferenceImpl<ErlangFunctionCallExpression>(o, TextRange.from(0, o.getExpression().getTextLength()));
  }

  @Nullable
  public static Object resolve(@NotNull ErlangFunctionCallExpression o) {
    return o.getReference().resolve();
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangQVar o) {
    return new ErlangVariableReferenceImpl(o, TextRange.from(0, o.getTextLength()));
  }

  @Nullable
  public static Object resolve(@NotNull ErlangQVar o) {
    return o.getReference().resolve();
  }

  public static boolean processDeclarations(@NotNull ErlangQVar o, @NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place) {
    return processor.execute(o, state);
  }
}
