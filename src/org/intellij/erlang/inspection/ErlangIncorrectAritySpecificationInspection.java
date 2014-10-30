/*
 * Copyright 2012-2014 Sergey Ignatov
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
package org.intellij.erlang.inspection;

import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunTypeSigs;
import org.intellij.erlang.psi.ErlangSpecification;
import org.intellij.erlang.psi.ErlangTypeSig;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

public class ErlangIncorrectAritySpecificationInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder problemsHolder) {
    for (ErlangSpecification spec : file.getSpecifications()) {
      ErlangFunTypeSigs signature = spec.getSignature();
      PsiElement psiArity = signature != null ? signature.getSpecFun().getInteger() : null;
      int arity = psiArity != null ? ErlangPsiImplUtil.getArity(psiArity) : -1;
      if (arity != -1) {
        for (ErlangTypeSig typeSig : signature.getTypeSigList()) {
          if (typeSig.getFunType().getFunTypeArguments().getTopTypeList().size() != arity) {
            problemsHolder.registerProblem(spec, "Specification has the wrong arity '" + signature.getSpecFun().getText() + "'");
          }
        }
      }
    }
  }
}
