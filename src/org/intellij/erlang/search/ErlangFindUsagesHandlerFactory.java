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
import com.intellij.ide.DataManager;
import com.intellij.psi.PsiElement;
import org.intellij.erlang.psi.ErlangQAtom;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangFindUsagesHandlerFactory extends FindUsagesHandlerFactory {
  @Override
  public boolean canFindUsages(@NotNull PsiElement element) {
    return element instanceof ErlangQAtom;
  }

  @Nullable
  @Override
  public FindUsagesHandler createFindUsagesHandler(@NotNull final PsiElement element, boolean forHighlightUsages) {
    if (element instanceof ErlangQAtom) {
      return new FindUsagesHandler(element) {
        @NotNull
        @Override
        public AbstractFindUsagesDialog getFindUsagesDialog(boolean isSingleFile,
                                                            boolean toShowInNewTab,
                                                            boolean mustOpenInNewTab) {
          return new CommonFindUsagesDialog(element, getProject(), getFindUsagesOptions(DataManager.getInstance().getDataContext()), toShowInNewTab, mustOpenInNewTab, isSingleFile, this);
        }
      };
    }
    return null;
  }
}
