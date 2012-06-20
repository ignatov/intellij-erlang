package org.intellij.erlang.psi.impl;

import com.intellij.openapi.util.Key;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangArgumentDefinition;
import org.intellij.erlang.psi.ErlangAssignmentExpression;
import org.intellij.erlang.psi.ErlangQVar;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @author ignatov
 */
public class ErlangVarProcessor implements PsiScopeProcessor {
  private ErlangQVar result = null;
  private final String myRequestedName;
  private final PsiElement myOrigin;

  public ErlangVarProcessor(String requestedName, PsiElement origin) {
    myRequestedName = requestedName;
    myOrigin = origin;
  }

  @Override
  public boolean execute(@NotNull PsiElement psiElement, ResolveState resolveState) {
//    System.out.print(psiElement.getText() + " : ");
//    System.out.println(psiElement.hashCode());
    if (!psiElement.equals(myOrigin) && psiElement instanceof ErlangQVar && psiElement.getText().equals(myRequestedName)) {
      boolean b1 = PsiTreeUtil.getParentOfType(psiElement, ErlangArgumentDefinition.class) != null;
      if (b1 || b2(psiElement)) {
        //      final PsiElement var = ((ErlangQVar) psiElement).getVar();
        //      if (var != null && var.getText().equals(myRequestedName)) {
        result = (ErlangQVar) psiElement;
        return false;
        //      }
      }
    }
    return true;
  }

  private static boolean b2(PsiElement psiElement) {
    ErlangAssignmentExpression assignmentExpression = PsiTreeUtil.getParentOfType(psiElement, ErlangAssignmentExpression.class);
    if (assignmentExpression == null) return false;
    return PsiTreeUtil.isAncestor(assignmentExpression.getLeft(), psiElement, false);
  }

  @Override
  public <T> T getHint(@NotNull Key<T> tKey) {
    return null;
  }

  @Nullable
  public ErlangQVar getResult() {
    return result;
  }

  @Override
  public void handleEvent(Event event, @Nullable Object o) {
  }
}
