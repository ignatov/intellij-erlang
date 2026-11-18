/*
 * Copyright 2012-2026 Sergey Ignatov
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

import com.intellij.openapi.util.Key;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.CachedValue;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.psi.util.PsiModificationTracker;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Tells whether two elements of a file are in different branches of the same
 * {@code -ifdef/-ifndef/-if ... -elif/-else ... -endif} block, so that at most one of them is compiled.
 */
final class ErlangConditionalBranches {
  private static final Key<CachedValue<ErlangConditionalBranches>> KEY = Key.create("erlang.conditional.branches");

  private record Branch(@NotNull PsiElement block, int index) {
  }

  private final ErlangFile myFile;
  private final Map<PsiElement, List<Branch>> myBranches;

  static ErlangConditionalBranches getInstance(@NotNull ErlangFile file) {
    return CachedValuesManager.getCachedValue(file, KEY,
                                             () -> CachedValueProvider.Result.create(
                                               new ErlangConditionalBranches(file),
                                               PsiModificationTracker.MODIFICATION_COUNT));
  }

  ErlangConditionalBranches(@NotNull ErlangFile file) {
    myFile = file;
    Map<PsiElement, List<Branch>> branches = new HashMap<>();
    List<Branch> stack = new ArrayList<>();
    for (PsiElement child = file.getFirstChild(); child != null; child = child.getNextSibling()) {
      if (child instanceof PsiWhiteSpace || child instanceof PsiComment) continue;
      branches.put(child, List.copyOf(stack));
      if (child instanceof ErlangAttribute attribute) {
        String directive = getDirective(attribute);
        if (directive == null) continue;
        switch (directive) {
          case "ifdef", "ifndef", "if" -> stack.add(new Branch(attribute, 0));
          case "elif", "else" -> {
            if (!stack.isEmpty()) {
              Branch last = stack.remove(stack.size() - 1);
              stack.add(new Branch(last.block(), last.index() + 1));
            }
          }
          case "endif" -> {
            if (!stack.isEmpty()) stack.remove(stack.size() - 1);
          }
          default -> {
          }
        }
      }
    }
    myBranches = Map.copyOf(branches);
  }

  boolean areExclusive(@NotNull PsiElement first, @NotNull PsiElement second) {
    List<Branch> firstBranches = myBranches.get(getTopLevelElement(first));
    List<Branch> secondBranches = myBranches.get(getTopLevelElement(second));
    if (firstBranches == null || secondBranches == null) return false;
    for (int i = 0; i < Math.min(firstBranches.size(), secondBranches.size()); i++) {
      Branch a = firstBranches.get(i);
      Branch b = secondBranches.get(i);
      if (a.block() != b.block()) return false;
      if (a.index() != b.index()) return true;
    }
    return false;
  }

  @Nullable
  private PsiElement getTopLevelElement(@NotNull PsiElement element) {
    PsiElement current = element;
    while (current != null && current.getParent() != myFile) {
      current = current.getParent();
    }
    return current;
  }

  @Nullable
  private static String getDirective(@NotNull ErlangAttribute attribute) {
    for (PsiElement child = attribute.getFirstChild(); child != null; child = child.getNextSibling()) {
      if (child instanceof PsiWhiteSpace || child instanceof PsiComment || child instanceof PsiErrorElement) continue;
      if (child.getNode().getElementType() == ErlangTypes.ERL_OP_MINUS) continue;
      if ("if".equals(child.getText())) return "if";
      if (child instanceof ErlangAtomAttribute atomAttribute) return atomAttribute.getName();
      if (child instanceof ErlangElseAtomAttribute) return "else";
      return child.getFirstChild() == null ? child.getText() : null;
    }
    return null;
  }
}
