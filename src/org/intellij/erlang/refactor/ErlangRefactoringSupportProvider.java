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

package org.intellij.erlang.refactor;

import com.intellij.lang.refactoring.RefactoringSupportProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.refactoring.RefactoringActionHandler;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.refactor.introduce.ErlangExtractFunctionHandler;
import org.intellij.erlang.refactor.introduce.ErlangIntroduceVariableHandler;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangRefactoringSupportProvider extends RefactoringSupportProvider {
  @Override
  public boolean isSafeDeleteAvailable(@NotNull PsiElement element) {
    return
      element instanceof ErlangFunction ||
      element instanceof ErlangRecordDefinition ||
      element instanceof ErlangMacrosDefinition ||
      element instanceof ErlangTypeDefinition;
  }

  @Nullable
  @Override
  public RefactoringActionHandler getIntroduceVariableHandler() {
    return new ErlangIntroduceVariableHandler();
  }

  @Override
  public boolean isInplaceRenameAvailable(@NotNull PsiElement element, PsiElement context) {
    return element instanceof ErlangNamedElement && element.getUseScope() instanceof LocalSearchScope;
  }

  @Override
  public boolean isMemberInplaceRenameAvailable(@NotNull PsiElement element, PsiElement context) {
    return element instanceof ErlangNamedElement;
  }

  @Nullable
  @Override
  public RefactoringActionHandler getExtractMethodHandler() {
    return new ErlangExtractFunctionHandler();
  }
}
