/*
 * Copyright 2012-2015 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang;

import com.intellij.psi.*;
import com.intellij.util.ProcessingContext;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil.ErlangFunctionCallNamedParameter;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil.ErlangFunctionCallParameter;
import org.jetbrains.annotations.NotNull;

import static com.intellij.patterns.PlatformPatterns.psiElement;

public class ErlangReferenceContributor extends PsiReferenceContributor {
  private enum Kind {FUNCTION, MODULE, RECORD}

  @Override
  public void registerReferenceProviders(@NotNull PsiReferenceRegistrar registrar) {
    registerRecords(registrar, "erlang", "is_record", 2, 1);
    registerRecords(registrar, "erlang", "is_record", 3, 1);

    ErlangFunctionCallNamedParameter<PsiElement> moduleParameter =
      new ErlangFunctionCallNamedParameter<PsiElement>("Module", "Mod", "_Module");
    registrar.registerReferenceProvider(psiElement(ErlangQAtom.class).with(moduleParameter), getProvider(Kind.MODULE));

    ErlangFunctionCallNamedParameter<PsiElement> funParameter =
      new ErlangFunctionCallNamedParameter<PsiElement>("Function", "Fun", "_Function");
    registrar.registerReferenceProvider(psiElement(ErlangQAtom.class).with(funParameter), getProvider(Kind.FUNCTION));
  }

  private static void registerRecords(PsiReferenceRegistrar r, String module, String fun, int arity, int position) {
    ErlangFunctionCallParameter<PsiElement> recordParameter =
      new ErlangFunctionCallParameter<PsiElement>(fun, module, arity, position);
    r.registerReferenceProvider(psiElement(ErlangQAtom.class).with(recordParameter), getProvider(Kind.RECORD));
  }

  private static PsiReferenceProvider getProvider(final Kind kind) {
    return new PsiReferenceProvider() {
      @NotNull
      @Override
      public PsiReference[] getReferencesByElement(@NotNull PsiElement element, @NotNull ProcessingContext context) {
        return element instanceof ErlangQAtom ? new PsiReference[]{createReference((ErlangQAtom) element)}
                                              : PsiReference.EMPTY_ARRAY;
      }

      private PsiReference createReference(ErlangQAtom element) {
        if (kind == Kind.MODULE) return ErlangPsiImplUtil.createModuleReference(element);
        if (kind == Kind.FUNCTION) return ErlangPsiImplUtil.createFunctionReference(element);
        if (kind == Kind.RECORD) return ErlangPsiImplUtil.createRecordRef(element);
        throw new UnsupportedOperationException("Unknown kind of reference: " + kind);
      }
    };
  }
}
