package org.intellij.erlang;

import com.intellij.psi.*;
import com.intellij.util.ProcessingContext;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangReferenceContributor extends PsiReferenceContributor {
  @Override
  public void registerReferenceProviders(PsiReferenceRegistrar registrar) {
    registrar.registerReferenceProvider(ErlangPsiImplUtil.secondAtomInIsRecord(),
      new PsiReferenceProvider() {
        @NotNull
        @Override
        public PsiReference[] getReferencesByElement(@NotNull PsiElement element, @NotNull ProcessingContext context) {
          return element instanceof ErlangQAtom ? new PsiReference[]{ErlangPsiImplUtil.createRecordRef(((ErlangQAtom) element))} : PsiReference.EMPTY_ARRAY;
        }
      });
  }
}
