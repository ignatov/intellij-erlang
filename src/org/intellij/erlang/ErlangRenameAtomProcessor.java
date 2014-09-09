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

package org.intellij.erlang;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.refactoring.listeners.RefactoringElementListener;
import com.intellij.refactoring.rename.BindablePsiReference;
import com.intellij.refactoring.rename.RenamePsiElementProcessor;
import com.intellij.refactoring.rename.RenameUtil;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangRenameAtomProcessor extends RenamePsiElementProcessor {
  @Override
  public boolean canProcessElement(@NotNull PsiElement element) {
    return element instanceof ErlangQAtom;
  }

  @Override
  public void renameElement(PsiElement element, String newName, UsageInfo[] usages, @Nullable RefactoringElementListener listener) throws IncorrectOperationException {
    // see RenameUtil#doRenameGenericNamedElement
    boolean hasBindables = false;
    for (UsageInfo usage : usages) {
      if (!(usage.getReference() instanceof BindablePsiReference)) {
        RenameUtil.rename(usage, newName);
      } else {
        hasBindables = true;
      }
    }

    ErlangPsiImplUtil.renameQAtom((ErlangQAtom) element, newName);

    if (hasBindables) {
      for (UsageInfo usage : usages) {
        final PsiReference ref = usage.getReference();
        if (ref instanceof BindablePsiReference) {
          try {
            ref.bindToElement(element);
          }
          catch (IncorrectOperationException e) {//fall back to old scheme
            ref.handleElementRename(newName);
          }
        }
      }
    }
    if (listener != null) {
      listener.elementRenamed(element);
    }
  }
}
