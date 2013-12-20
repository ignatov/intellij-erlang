package org.intellij.erlang;

import com.intellij.psi.*;
import com.intellij.util.ProcessingContext;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import static com.intellij.patterns.PlatformPatterns.psiElement;

public class ErlangReferenceContributor extends PsiReferenceContributor {
  private enum Kind {FUNCTION, MODULE, RECORD}

  @Override
  public void registerReferenceProviders(PsiReferenceRegistrar registrar) {
    register(registrar, Kind.RECORD,   "erlang", "is_record",  2, 1);
    register(registrar, Kind.RECORD,   "erlang", "is_record",  3, 1);
    register(registrar, Kind.MODULE,   "erlang", "spawn",      3, 0);
    register(registrar, Kind.FUNCTION, "erlang", "spawn",      3, 1);
    register(registrar, Kind.MODULE,   "erlang", "spawn_link", 3, 0);
    register(registrar, Kind.FUNCTION, "erlang", "spawn_link", 3, 1);
  }

  private static void register(PsiReferenceRegistrar registrar, final Kind kind, String module, String function, int arity, int position) {
    registrar.registerReferenceProvider(psiElement(ErlangQAtom.class).with(
      new ErlangPsiImplUtil.ErlangFunctionCallParameter<PsiElement>(function, module, arity, position)),
      new PsiReferenceProvider() {
        @NotNull
        @Override
        public PsiReference[] getReferencesByElement(@NotNull PsiElement element, @NotNull ProcessingContext context) {
          return element instanceof ErlangQAtom ? new PsiReference[]{createReference((ErlangQAtom) element)} : PsiReference.EMPTY_ARRAY;
        }

        private PsiReference createReference(ErlangQAtom element) {
          if (kind == Kind.MODULE)   return ErlangPsiImplUtil.createModuleReference(element);
          if (kind == Kind.FUNCTION) return ErlangPsiImplUtil.createFunctionReference(element);
          if (kind == Kind.RECORD)   return ErlangPsiImplUtil.createRecordRef(element);
          throw new UnsupportedOperationException("Unknown kind of reference: " + kind);
        }
      });
  }
}
