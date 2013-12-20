/*
 * Copyright 2012-2013 Sergey Ignatov
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

import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangFunctionReferenceImpl;
import org.intellij.erlang.quickfixes.ErlangCreateFunctionQuickFix;
import org.jetbrains.annotations.NotNull;

public class ErlangUnresolvedFunctionInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;
    file.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
        super.visitFunctionCallExpression(o);        
        PsiReference reference = o.getReference();
        if (reference instanceof ErlangFunctionReferenceImpl && reference.resolve() == null) {
          if (o.getQAtom().getMacros() != null) return;
          ErlangFunctionReferenceImpl r = (ErlangFunctionReferenceImpl) reference;

          String name = r.getName();
          int arity = r.getArity();

          if (ErlangBifTable.isBif("erlang", name, arity)) return;

          String signature = r.getSignature();

          PsiElement parent = o.getParent();
          if (parent instanceof ErlangGlobalFunctionCallExpression) {
            ErlangModuleRef moduleRef = ((ErlangGlobalFunctionCallExpression) parent).getModuleRef();
            if (moduleRef != null) {
              if (moduleRef.getQAtom().getMacros() != null) return;
              String moduleName = moduleRef.getText();
              if (ErlangBifTable.isBif(moduleName, name, arity)) return;
              signature = moduleName + ":" + signature;
            }
          }

          LocalQuickFix[] qfs = parent instanceof ErlangGenericFunctionCallExpression || parent instanceof ErlangGlobalFunctionCallExpression ?
            new LocalQuickFix[]{} :
            new LocalQuickFix[]{new ErlangCreateFunctionQuickFix(name, arity)};

          problemsHolder.registerProblem(o.getNameIdentifier(), "Unresolved function " + "'" + signature + "'", qfs);
        }
      }

      @Override
      public void visitSpecFun(@NotNull ErlangSpecFun o) {
        super.visitSpecFun(o);
        PsiReference reference = o.getReference();
        if (reference instanceof ErlangFunctionReferenceImpl && reference.resolve() == null) {
          if (o.getQAtom().getMacros() != null) return;
          ErlangFunctionReferenceImpl r = (ErlangFunctionReferenceImpl) reference;
          LocalQuickFix[] qfs = getQuickFixes(o, new ErlangCreateFunctionQuickFix(r.getName(), r.getArity()));
          problemsHolder.registerProblem(o.getQAtom(), "Unresolved function " + "'" + r.getSignature() + "'", qfs);
        }
      }

      @Override
      public void visitFunctionWithArity(@NotNull ErlangFunctionWithArity o) {
        super.visitFunctionWithArity(o);
        PsiReference reference = o.getReference();
        if (reference instanceof ErlangFunctionReferenceImpl && reference.resolve() == null) {
          if (o.getQAtom().getMacros() != null) return;
          ErlangFunctionReferenceImpl r = (ErlangFunctionReferenceImpl) reference;
          LocalQuickFix[] qfs = getQuickFixes(o, new ErlangCreateFunctionQuickFix(r.getName(), r.getArity()));
          problemsHolder.registerProblem(o.getQAtom(), "Unresolved function " + "'" + r.getSignature() + "'", qfs);
        }
      }

      //prevents UnresolvedFunction messages in callback specifications
      @Override
      public void visitCallbackSpec(@NotNull ErlangCallbackSpec o) {
      }
    });
  }

  private static LocalQuickFix[] getQuickFixes(PsiElement o, ErlangCreateFunctionQuickFix fix) {
    return PsiTreeUtil.getNextSiblingOfType(o, ErlangModuleRef.class) != null ? new LocalQuickFix[]{} : new LocalQuickFix[]{fix};
  }
}
