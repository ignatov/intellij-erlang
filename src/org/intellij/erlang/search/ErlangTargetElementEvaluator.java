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
import com.intellij.psi.impl.source.resolve.reference.impl.PsiMultiReference;
import org.intellij.erlang.macrolayer.ErlangMacroLayer;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.psi.impl.ErlangBareMacroReference;
import org.intellij.erlang.psi.impl.ErlangMacroResolve;
import org.intellij.erlang.psi.impl.ErlangMacrosReferenceImpl;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangTargetElementEvaluator extends TargetElementEvaluatorEx2 implements TargetElementEvaluatorEx {
  /**
   * The name that a macro makes is its own declaration, so the identifier highlighting with
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
    // a macro with several possible definitions (conditional branches) has no single target, so go to declaration
    // (which does not accept the name) offers them all; rename (which does) starts from the first, and renames the
    // other branches' definitions too (ErlangRenameMacroProcessor). At a use the platform hands over a multi-reference
    // (the macro and, for ?F(...), the call; with the layer off, the use and its name): the macro reference inside it.
    for (PsiReference r : ref instanceof PsiMultiReference multi ? multi.getReferences() : new PsiReference[]{ref}) {
      if (r instanceof ErlangMacrosReferenceImpl || r instanceof ErlangBareMacroReference) {
        PsiElement first = ErlangMacroResolve.firstTarget(r);
        if (first != null) return first;
      }
    }
    PsiElement element = ref.getElement();
    return element instanceof ErlangQAtom && ref.resolve() == null ? element : null;
  }
}
