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

package org.intellij.erlang.refactoring;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.safeDelete.NonCodeUsageSearchInfo;
import com.intellij.refactoring.safeDelete.SafeDeleteProcessorDelegateBase;
import com.intellij.refactoring.safeDelete.usageInfo.SafeDeleteReferenceSimpleDeleteUsageInfo;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.SmartList;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.ErlangExportFunction;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangSpecification;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.List;

public class ErlangSafeDeleteProcessor extends SafeDeleteProcessorDelegateBase {
  @Override
  public @Nullable Collection<? extends PsiElement> getElementsToSearch(@NotNull PsiElement element,
                                                                        @Nullable Module module,
                                                                        @NotNull Collection<? extends PsiElement> allElementsToDelete) {
    return allElementsToDelete;
  }


  @Override
  public boolean handlesElement(PsiElement element) {
    return element instanceof ErlangFunction;
  }

  @Nullable
  @Override
  public NonCodeUsageSearchInfo findUsages(@NotNull PsiElement element,
                                           @NotNull PsiElement @NotNull [] allElementsToDelete,
                                           @NotNull List<? super UsageInfo> result) {
    if (element instanceof ErlangFunction) {
      Collection<PsiReference> all = ReferencesSearch.search(element).findAll();
      for (PsiReference ref : all) {
        PsiElement refElement = ref.getElement();
        if (PsiTreeUtil.getParentOfType(refElement, ErlangSpecification.class) != null) continue;
        final boolean inExport = PsiTreeUtil.getParentOfType(refElement, ErlangExportFunction.class, false) != null;
        result.add(new SafeDeleteReferenceSimpleDeleteUsageInfo(refElement, element, inExport) {
          @Override
          public void deleteElement() throws IncorrectOperationException {
            PsiElement e = getElement();
            if (isSafeDelete() && inExport && e != null) {

              PsiElement prev = PsiTreeUtil.prevVisibleLeaf(e);
              if (isComma(prev)) {
                prev.delete();
              }
              else {
                PsiElement next = PsiTreeUtil.nextVisibleLeaf(e);
                if (isComma(next)) {
                  next.delete();
                }
              }
              e.delete();
            }
          }

          @Contract("null -> false")
          private static boolean isComma(@Nullable PsiElement leaf) {
            return leaf instanceof LeafPsiElement && ((LeafPsiElement) leaf).getElementType() == ErlangTypes.ERL_COMMA;
          }
        });
      }
    }
    return null;
  }

  @Nullable
  @Override
  public Collection<PsiElement> getAdditionalElementsToDelete(@NotNull PsiElement element,
                                                              @NotNull Collection<? extends PsiElement> allElementsToDelete,
                                                              boolean askUser) {
    if (element instanceof ErlangFunction) {
      ErlangSpecification specification = ((ErlangFunction) element).findSpecification();
      if (specification != null) {
        return new SmartList<>(specification.getParent());
      }
    }
    return null;
  }

  @Nullable
  @Override
  public Collection<String> findConflicts(@NotNull PsiElement element, @NotNull PsiElement @NotNull [] allElementsToDelete) {
    return null;
  }

  @Nullable
  @Override
  public UsageInfo[] preprocessUsages(@NotNull Project project, UsageInfo @NotNull [] usages) {
    return usages;
  }

  @Override public void prepareForDeletion(@NotNull PsiElement element) throws IncorrectOperationException { }
  @Override public boolean isToSearchInComments(PsiElement element) { return false; }
  @Override public void setToSearchInComments(PsiElement element, boolean enabled) { }
  @Override public boolean isToSearchForTextOccurrences(PsiElement element) { return false; }
  @Override public void setToSearchForTextOccurrences(PsiElement element, boolean enabled) { }
}
