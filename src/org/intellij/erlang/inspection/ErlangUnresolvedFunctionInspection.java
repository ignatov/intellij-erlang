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
import com.intellij.psi.SmartPointerManager;
import com.intellij.psi.SmartPsiElementPointer;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.apache.commons.lang3.ObjectUtils;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangCreateFunctionQuickFix;
import org.intellij.erlang.quickfixes.ErlangCreateFunctionQuickFix.DefaultFunctionTextProvider;
import org.intellij.erlang.quickfixes.ErlangCreateFunctionQuickFix.FunctionTextProvider;
import org.intellij.erlang.quickfixes.ErlangCreateRemoteFunctionQuickFix;
import org.intellij.erlang.refactoring.ErlangRefactoringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class ErlangUnresolvedFunctionInspection extends ErlangInspectionBase {
  @NotNull
  @Override
  protected ErlangVisitor buildErlangVisitor(@NotNull final ProblemsHolder holder,
                                             @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression call) {
        PsiReference reference = call.getReference();
        PsiElement parent = call.getParent();
        ErlangModuleRef moduleRef = parent instanceof ErlangGlobalFunctionCallExpression ?
                                    ((ErlangGlobalFunctionCallExpression) parent).getModuleRef() : null;
        if (call.getQAtom().getMacros() != null ||
            moduleRef != null && moduleRef.getQAtom().getMacros() != null ||
            !(reference instanceof ErlangFunctionReference) || reference.resolve() != null) {
          return;
        }

        String name = call.getName();
        int arity = call.getArgumentList().getExpressionList().size();
        String presentation = ErlangPsiImplUtil.createFunctionPresentation(name, arity);
        String fixMessage = "Create Function " + (moduleRef != null ? moduleRef.getText() + ":" : "") + presentation;

        LocalQuickFix fix = parent instanceof ErlangGenericFunctionCallExpression ? null :
                            moduleRef != null ? ErlangCreateRemoteFunctionQuickFix.create(
                              fixMessage, ErlangPsiImplUtil.resolveToFile(moduleRef), createTextProvider(call)) :
                            new ErlangCreateFunctionQuickFix(fixMessage, createTextProvider(call));
        LocalQuickFix[] fixes = fix != null ? new LocalQuickFix[]{fix} : LocalQuickFix.EMPTY_ARRAY;

        registerProblem(holder, call.getNameIdentifier(), "Unresolved function " + presentation, fixes);
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
            !(reference instanceof ErlangFunctionReference r) || reference.resolve() != null) {
          return;
        }

        int arity = r.getArity();
        String name = r.getName();
        if (arity < 0 || name == null) return;

        String functionPresentation = ErlangPsiImplUtil.createFunctionPresentation(name, arity);
        ErlangModuleRef moduleRef = what instanceof ErlangFunctionWithArity ?
                                    PsiTreeUtil.getPrevSiblingOfType(what, ErlangModuleRef.class) : null;
        LocalQuickFix fix;
        if (moduleRef != null && moduleRef.getQAtom().getMacros() == null) {
          String fixMessage = "Create Function " + moduleRef.getText() + ":" + functionPresentation;
          String atomName = ObjectUtils.defaultIfNull(ErlangPsiImplUtil.toAtomName(name), name);
          fix = ErlangCreateRemoteFunctionQuickFix.create(fixMessage, ErlangPsiImplUtil.resolveToFile(moduleRef),
                                                          new DefaultFunctionTextProvider(atomName, arity));
        }
        else {
          String fixMessage = "Create Function " + functionPresentation;
          fix = PsiTreeUtil.getNextSiblingOfType(what, ErlangModuleRef.class) != null ? null :
                new ErlangCreateFunctionQuickFix(fixMessage, name, arity);
        }
        LocalQuickFix[] qfs = fix != null ? new LocalQuickFix[]{fix} : LocalQuickFix.EMPTY_ARRAY;

        registerProblem(holder, target, "Unresolved function " + functionPresentation, qfs);
      }
    };
  }

  @NotNull
  private static FunctionTextProvider createTextProvider(@NotNull ErlangFunctionCallExpression call) {
    final SmartPsiElementPointer<ErlangFunctionCallExpression> myPointer =
      SmartPointerManager.getInstance(call.getProject()).createSmartPsiElementPointer(call);
    return new FunctionTextProvider() {
      @NotNull
      @Override
      public String getName() {
        ErlangFunctionCallExpression call = myPointer.getElement();
        assert call != null;
        String name = call.getName();
        return ObjectUtils.defaultIfNull(ErlangPsiImplUtil.toAtomName(name), name);
      }

      @NotNull
      @Override
      public List<String> getArguments() {
        ErlangFunctionCallExpression call = myPointer.getElement();
        assert call != null;
        List<ErlangExpression> expressions = call.getArgumentList().getExpressionList();
        return ContainerUtil.map(expressions, ErlangRefactoringUtil::shorten);
      }
    };
  }
}