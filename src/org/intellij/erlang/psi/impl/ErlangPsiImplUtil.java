package org.intellij.erlang.psi.impl;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiReference;
import org.intellij.erlang.psi.ErlangExpression;
import org.intellij.erlang.psi.ErlangFunctionCallExpression;
import org.jetbrains.annotations.Nullable;

public class ErlangPsiImplUtil {
  private ErlangPsiImplUtil() {
  }

  @Nullable
  public static PsiReference getReference(ErlangFunctionCallExpression o) {
    return new ErlangReferenceImpl<ErlangExpression>(o.getExpression(), TextRange.from(0, o.getExpression().getTextLength()));
  }

  @Nullable
  public static Object resolve(ErlangFunctionCallExpression o) {
    return o.getReference().resolve();
  }
}
