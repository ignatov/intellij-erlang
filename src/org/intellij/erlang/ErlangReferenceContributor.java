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
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil.ErlangFunctionCallArgument;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil.ErlangFunctionCallFunctionArgument;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil.ErlangFunctionCallModuleArgument;
import org.jetbrains.annotations.NotNull;

import static com.intellij.patterns.PlatformPatterns.psiElement;

public class ErlangReferenceContributor extends PsiReferenceContributor {
  @Override
  public void registerReferenceProviders(@NotNull PsiReferenceRegistrar registrar) {
    PsiElementPattern.Capture<ErlangQAtom> atom = psiElement(ErlangQAtom.class);

    PsiElementPattern.Capture<ErlangQAtom> atomArg = atom.withParents(ErlangMaxExpression.class,
                                                                      ErlangArgumentList.class,
                                                                      ErlangFunctionCallExpression.class);
    registrar.registerReferenceProvider(atomArg.with(isRecordArgument(2)), new RecordReferenceProvider());
    registrar.registerReferenceProvider(atomArg.with(isRecordArgument(3)), new RecordReferenceProvider());

    registrar.registerReferenceProvider(atom.with(new ErlangFunctionCallModuleArgument()), new ModuleReferenceProvider());
    registrar.registerReferenceProvider(atom.with(new ErlangFunctionCallFunctionArgument()), new FunctionReferenceProvider());
  }

  @NotNull
  private static ErlangFunctionCallArgument<ErlangQAtom> isRecordArgument(int position) {
    return new ErlangFunctionCallArgument<>("erlang", "is_record", position, 1);
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
}
