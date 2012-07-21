package org.intellij.erlang.refactor;

import com.intellij.lang.refactoring.RefactoringSupportProvider;
import com.intellij.psi.PsiElement;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangRecordDefinition;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangRefactoringSupportProvider extends RefactoringSupportProvider {
  @Override
  public boolean isSafeDeleteAvailable(PsiElement element) {
    return element instanceof ErlangFunction || element instanceof ErlangRecordDefinition;
  }
}
