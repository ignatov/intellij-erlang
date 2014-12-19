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

package org.intellij.erlang.quickfixes;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.List;

public abstract class ErlangRemoveFunctionFromImportFixBase extends ErlangQuickFixBase {
  @NotNull
  @Override
  public String getFamilyName() {
    return "Remove from import";
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    String fullName = getSignature(descriptor.getPsiElement());
    if (fullName == null) return;
    removeFunctionFromImport(getAttributesForProcessing(descriptor.getPsiElement()), fullName);
  }

  @Nullable
  protected abstract String getSignature(@NotNull PsiElement function);

  @NotNull
  protected abstract Collection<? extends ErlangAttribute> getAttributesForProcessing(@NotNull PsiElement descriptorElement);

  private static void removeFunctionFromImport(@NotNull Collection<? extends ErlangAttribute> attributes,
                                               @Nullable String name) {
    for (ErlangAttribute attribute : attributes) {
      ErlangImportDirective importDirective = attribute.getImportDirective();
      ErlangImportFunctions fns = importDirective != null && importDirective.getModuleRef() != null ? importDirective.getImportFunctions() : null;
      List<ErlangImportFunction> functions = fns == null ? ContainerUtil.<ErlangImportFunction>emptyList() : fns.getImportFunctionList();
      if (name == null || functions.isEmpty()) continue;

      for (int i = 0; i < functions.size(); ++i) {
        String presentation = ErlangPsiImplUtil.createFunctionPresentation(functions.get(i));
        if (presentation.equals(name)) {
          cutFunction(functions.get(i), i == functions.size() - 1);
        }
      }
      //noinspection unchecked
      if (PsiTreeUtil.getChildOfAnyType(fns, ErlangImportFunction.class, PsiComment.class) == null) {
        //noinspection ConstantConditions
        if (attribute.getNextSibling() instanceof PsiWhiteSpace) {
          attribute.getNextSibling().delete();
        }
        attribute.delete();
      }
    }
  }

  private static void cutFunction(@NotNull ErlangImportFunction function, boolean isLast) {
    removeComma(function, false);
    if (isLast) removeComma(function, true);
    function.delete();
  }

  private static void removeComma(@NotNull ErlangImportFunction function, boolean backward) {
    PsiElement maybeComma = backward
      ? PsiTreeUtil.skipSiblingsBackward(function, PsiWhiteSpace.class, PsiComment.class)
      : PsiTreeUtil.skipSiblingsForward(function, PsiWhiteSpace.class, PsiComment.class);
    if (maybeComma == null || maybeComma.getNode().getElementType() != ErlangTypes.ERL_COMMA) return;
    maybeComma.delete();
  }

  public static class ErlangRemoveFunctionFromAllImportsFix extends ErlangRemoveFunctionFromImportFixBase {
    @Nullable
    @Override
    protected String getSignature(@NotNull PsiElement function) {
      ErlangFunction f = PsiTreeUtil.getParentOfType(function, ErlangFunction.class);
      return f != null ? ErlangPsiImplUtil.createFunctionPresentation(f) : null;
    }

    @NotNull
    @Override
    protected Collection<? extends ErlangAttribute> getAttributesForProcessing(@NotNull PsiElement descriptorElement) {
      return ((ErlangFile) descriptorElement.getContainingFile()).getAttributes();
    }
  }
}
