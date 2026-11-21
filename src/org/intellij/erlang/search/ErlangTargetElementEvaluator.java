/*
 * Copyright 2012-2015 Sergey Ignatov
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

import com.intellij.codeInsight.TargetElementEvaluatorEx;
import com.intellij.codeInsight.TargetElementEvaluatorEx2;
import com.intellij.codeInsight.TargetElementUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import org.intellij.erlang.macrolayer.ErlangMacroLayer;
import org.intellij.erlang.psi.ErlangQAtom;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangTargetElementEvaluator extends TargetElementEvaluatorEx2 implements TargetElementEvaluatorEx {
  /**
   * Round 6 (next), item 3a: the name that a macro makes is its own declaration, so the identifier highlighting with
   * the caret on it (the argument of {@code ?HGEN(hfoo).}, a name in a {@code -define} body) paints it and its uses.
   */
  @Override
  public @Nullable PsiElement getNamedElement(@NotNull PsiElement element) {
    return ErlangMacroLayer.isMadeNameDeclaration(element) ? element : null;
  }

  @Override
  public boolean isIdentifierPart(@NotNull PsiFile element, @NotNull CharSequence text, int offset) {
    return false;
  }

  @Override
  public boolean includeSelfInGotoImplementation(@NotNull PsiElement element) {
    return false;
  }

  @Nullable
  @Override
  public PsiElement getElementByReference(@NotNull PsiReference ref, int flags) {
    if ((flags & TargetElementUtil.ELEMENT_NAME_ACCEPTED) == 0) return null;
    // round 6 (next), N2: a macro with several possible definitions (conditional branches) has no single target, so go
    // to declaration (which does not accept the name) offers them all; rename (which does) starts from the first, and
    // renames the other branches' definitions too (ErlangRenameMacroProcessor)
    if ((ref instanceof org.intellij.erlang.psi.impl.ErlangMacrosReferenceImpl || ref instanceof org.intellij.erlang.psi.impl.ErlangBareMacroReference) &&
        ref.resolve() == null) {
      PsiElement first = org.intellij.erlang.psi.impl.ErlangMacroResolve.firstTarget(ref);
      if (first != null) return first;
    }
    PsiElement element = ref.getElement();
    return element instanceof ErlangQAtom && ref.resolve() == null ? element : null;
  }
}
