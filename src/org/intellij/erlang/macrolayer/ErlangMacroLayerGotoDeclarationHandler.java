package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler;
import com.intellij.openapi.editor.Editor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangMacrosBody;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * Round 5: go to declaration from a name inside a {@code -define} body that master cannot resolve there (a header body
 * that names things of its includers, a fragment body): the layer resolves it through the expanded uses of the macro.
 */
public final class ErlangMacroLayerGotoDeclarationHandler implements GotoDeclarationHandler {
  @Override
  public PsiElement @Nullable [] getGotoDeclarationTargets(@Nullable PsiElement source, int offset, Editor editor) {
    if (source == null || !ErlangMacroLayer.isEnabled() || PsiTreeUtil.getParentOfType(source, ErlangMacrosBody.class) == null) return null;
    PsiReference ref = source.getContainingFile().findReferenceAt(offset);
    if (ref != null && (ref.resolve() != null || ref instanceof PsiPolyVariantReference poly && poly.multiResolve(false).length > 0)) {
      return null; // master's answer
    }
    List<PsiElement> targets = ErlangMacroLayer.resolveInMacroBody(source);
    return targets.isEmpty() ? null : targets.toArray(PsiElement.EMPTY_ARRAY);
  }
}
