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

import com.intellij.patterns.PsiElementPattern;
import com.intellij.psi.*;
import com.intellij.util.ObjectUtils;
import com.intellij.util.ProcessingContext;
import org.intellij.erlang.psi.ErlangArgumentList;
import org.intellij.erlang.psi.ErlangFunctionCallExpression;
import org.intellij.erlang.psi.ErlangMaxExpression;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil.ErlangFunctionCallParameter;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil.ErlangFunctionCallModuleParameter;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil.ErlangFunctionCallFunctionParameter;
import org.jetbrains.annotations.NotNull;

import static com.intellij.patterns.PlatformPatterns.psiElement;

public class ErlangReferenceContributor extends PsiReferenceContributor {
  @Override
  public void registerReferenceProviders(@NotNull PsiReferenceRegistrar registrar) {
    //noinspection unchecked
    PsiElementPattern.Capture<ErlangQAtom> atomArgInFunctionCall = psiElement(ErlangQAtom.class)
      .withParents(ErlangMaxExpression.class, ErlangArgumentList.class, ErlangFunctionCallExpression.class);

    registerRecords(registrar, atomArgInFunctionCall, "erlang", "is_record", 2, 1);
    registerRecords(registrar, atomArgInFunctionCall, "erlang", "is_record", 3, 1);

    registrar.registerReferenceProvider(
      atomArgInFunctionCall.with(new ErlangFunctionCallModuleParameter()), new ModuleReferenceProvider());
    registrar.registerReferenceProvider(
      atomArgInFunctionCall.with(new ErlangFunctionCallFunctionParameter()), new FunctionReferenceProvider());
  }

  private static void registerRecords(@NotNull PsiReferenceRegistrar registrar,
                               @NotNull PsiElementPattern.Capture<ErlangQAtom> pattern, @NotNull String module,
                               @NotNull String function, int arity, int position) {
    registrar.registerReferenceProvider(
      pattern.with(new ErlangFunctionCallParameter<PsiElement>(function, module, arity, position)),
      new RecordReferenceProvider());
  }

  private static class ModuleReferenceProvider extends ReferenceProvider {
    @NotNull
    @Override
    protected PsiReference createReference(@NotNull ErlangQAtom atom) {
      return ErlangPsiImplUtil.createModuleReference(atom);
    }
  }

  private static class FunctionReferenceProvider extends ReferenceProvider {
    @NotNull
    @Override
    protected PsiReference createReference(@NotNull ErlangQAtom atom) {
      return ErlangPsiImplUtil.createFunctionReference(atom);
    }
  }

  private static class RecordReferenceProvider extends ReferenceProvider {
    @NotNull
    @Override
    protected PsiReference createReference(@NotNull ErlangQAtom atom) {
      return ErlangPsiImplUtil.createRecordRef(atom);
    }
  }

  private static abstract class ReferenceProvider extends PsiReferenceProvider {
    @NotNull
    @Override
    public PsiReference[] getReferencesByElement(@NotNull PsiElement element, @NotNull ProcessingContext context) {
      ErlangQAtom atom = ObjectUtils.tryCast(element, ErlangQAtom.class);
      return atom != null ? new PsiReference[]{createReference(atom)}
                          : PsiReference.EMPTY_ARRAY;
    }
    @NotNull
    protected abstract PsiReference createReference(@NotNull ErlangQAtom atom);
  }
}
