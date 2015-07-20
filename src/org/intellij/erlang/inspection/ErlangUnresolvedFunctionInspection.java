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

import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.quickfixes.ErlangCreateFunctionQuickFix;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangUnresolvedFunctionInspection extends ErlangInspectionBase {
  @NotNull
  @Override
  protected ErlangVisitor buildErlangVisitor(@NotNull final ProblemsHolder holder,
                                             @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
        PsiReference reference = o.getReference();
        if (reference instanceof ErlangFunctionReference && reference.resolve() == null) {
          if (o.getQAtom().getMacros() != null) return;
          ErlangFunctionReference r = (ErlangFunctionReference) reference;

          String name = r.getName();
          int arity = r.getArity();

          if (arity < 0) return;

          String signature = r.getSignature();

          PsiElement parent = o.getParent();
          if (parent instanceof ErlangGlobalFunctionCallExpression) {
            ErlangModuleRef moduleRef = ((ErlangGlobalFunctionCallExpression) parent).getModuleRef();
            if (moduleRef.getQAtom().getMacros() != null) return;
            signature = moduleRef.getText() + ":" + signature;
          }

          LocalQuickFix[] qfs = parent instanceof ErlangGenericFunctionCallExpression || parent instanceof ErlangGlobalFunctionCallExpression ?
            new LocalQuickFix[]{} :
            new LocalQuickFix[]{new ErlangCreateFunctionQuickFix(name, arity)};

          registerProblem(holder, o.getNameIdentifier(), "Unresolved function " + "'" + signature + "'", qfs);
        }
      }

      @Override
      public void visitSpecFun(@NotNull ErlangSpecFun o) {
        inspect(o, o.getQAtom(), o.getReference());
      }

      @Override
      public void visitFunctionWithArity(@NotNull ErlangFunctionWithArity o) {
        inspect(o, o.getQAtom(), o.getReference());
      }

      private void inspect(PsiElement what, ErlangQAtom target, @Nullable PsiReference reference) {
        if (PsiTreeUtil.getParentOfType(what, ErlangCallbackSpec.class) != null || target.getMacros() != null ||
          !(reference instanceof ErlangFunctionReference) || reference.resolve() != null) {
          return;
        }

        ErlangFunctionReference r = (ErlangFunctionReference) reference;
        if (r.getArity() < 0) return; //there is no need to inspect incomplete/erroneous code
        LocalQuickFix[] qfs = PsiTreeUtil.getNextSiblingOfType(what, ErlangModuleRef.class) != null ?
          new LocalQuickFix[]{} : new LocalQuickFix[]{new ErlangCreateFunctionQuickFix(r.getName(), r.getArity())};
        registerProblem(holder, target, "Unresolved function " + "'" + r.getSignature() + "'", qfs);
      }
    };
  }
}