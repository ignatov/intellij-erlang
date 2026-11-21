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

package org.intellij.erlang.search;

import com.intellij.find.findUsages.AbstractFindUsagesDialog;
import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.find.findUsages.FindUsagesHandlerFactory;
import com.intellij.find.findUsages.FindUsagesOptions;
import com.intellij.ide.DataManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.wm.IdeFocusManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ReferenceRange;
import com.intellij.psi.search.PsiSearchHelper;
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.search.UsageSearchContext;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.Processor;
import org.intellij.erlang.macrolayer.ErlangMacroLayer;
import org.intellij.erlang.psi.ErlangQAtom;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangFindUsagesHandlerFactory extends FindUsagesHandlerFactory {
  @Override
  public boolean canFindUsages(@NotNull PsiElement element) {
    return element instanceof ErlangQAtom || ErlangMacroLayer.isNameOfFunctionMadeByMacro(element);
  }

  @Nullable
  @Override
  public FindUsagesHandler createFindUsagesHandler(@NotNull final PsiElement element, boolean forHighlightUsages) {
    if (ErlangMacroLayer.isNameOfFunctionMadeByMacro(element)) return new FunctionMadeByMacroHandler(element);
    if (element instanceof ErlangQAtom) {
      return new FindUsagesHandler(element) {
        @NotNull
        @Override
        public AbstractFindUsagesDialog getFindUsagesDialog(boolean isSingleFile,
                                                            boolean toShowInNewTab,
                                                            boolean mustOpenInNewTab) {
          return new CommonFindUsagesDialog(element, getProject(), getFindUsagesOptions(DataManager.getInstance().getDataContext(IdeFocusManager.getInstance(getProject()).getFocusOwner())), toShowInNewTab, mustOpenInNewTab, isSingleFile, this);
        }
      };
    }
    return null;
  }

  /**
   * Round 5: the macro layer resolves a call of a function made by a macro (such as {@code foo()} after
   * {@code ?GEN(foo).}) to the atom name in the macro use. That leaf is not a named element, so the platform's reference
   * search has no word to look for. This handler searches the name and keeps the references that resolve to the leaf.
   */
  private static final class FunctionMadeByMacroHandler extends FindUsagesHandler {
    FunctionMadeByMacroHandler(@NotNull PsiElement element) {
      super(element);
    }

    @Override
    public boolean processElementUsages(@NotNull PsiElement element,
                                        @NotNull Processor<? super UsageInfo> processor,
                                        @NotNull FindUsagesOptions options) {
      if (!options.isUsages) return true;
      String name = ReadAction.compute(element::getText);
      SearchScope scope = ReadAction.compute(() -> options.searchScope.intersectWith(element.getUseScope()));
      return PsiSearchHelper.getInstance(getProject()).processElementsWithWord((occurrence, offsetInElement) -> {
        for (PsiReference ref : occurrence.getReferences()) {
          if (ReferenceRange.containsOffsetInElement(ref, offsetInElement) && ref.isReferenceTo(element) &&
              !processor.process(new UsageInfo(ref))) {
            return false;
          }
        }
        return true;
      }, scope, name, UsageSearchContext.IN_CODE, true);
    }
  }
}
