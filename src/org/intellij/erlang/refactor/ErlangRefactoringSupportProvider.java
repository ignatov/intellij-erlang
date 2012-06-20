package org.intellij.erlang.refactor;

import com.intellij.lang.refactoring.RefactoringSupportProvider;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangRefactoringSupportProvider extends RefactoringSupportProvider {
  @Override
  public boolean isAvailable(@NotNull PsiElement context) {
    return super.isAvailable(context);
  }
}
