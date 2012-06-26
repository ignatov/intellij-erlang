package org.intellij.erlang.psi.impl;

import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.BaseScopeProcessor;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangFunctionClause;
import org.intellij.erlang.psi.ErlangQVar;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.inDefinition;
import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.isLeftPartOfAssignment;

/**
 * @author ignatov
 */
public class ErlangVarProcessor extends BaseScopeProcessor {
  private ErlangQVar result = null;
  private final String myRequestedName;
  private final PsiElement myOrigin;

  public ErlangVarProcessor(String requestedName, PsiElement origin) {
    myRequestedName = requestedName;
    myOrigin = origin;
  }

  @Override
  public boolean execute(@NotNull PsiElement psiElement, ResolveState resolveState) {
    ErlangFunctionClause clause = PsiTreeUtil.getParentOfType(myOrigin, ErlangFunctionClause.class);
    if (!psiElement.equals(myOrigin) && psiElement instanceof ErlangQVar && psiElement.getText().equals(myRequestedName)) {
      if (PsiTreeUtil.isAncestor(clause, psiElement, false) && (inDefinition(psiElement) || isLeftPartOfAssignment(psiElement))) {
        result = (ErlangQVar) psiElement;
        return false;
      }
    }
    return true;
  }

  @Nullable
  public ErlangQVar getResult() {
    return result;
  }
}
