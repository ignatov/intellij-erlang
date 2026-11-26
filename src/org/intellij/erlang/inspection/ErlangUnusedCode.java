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

import com.intellij.openapi.progress.ProgressManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.function.Predicate;

/**
 * Unused definitions as erlc finds them: a definition is used only when a use of it is reachable from the roots of the module,
 * so definitions that only unused definitions of the same kind use are unused too.
 */
final class ErlangUnusedCode {
  private ErlangUnusedCode() {
  }

  /**
   * Returns the candidates that no use reachable from a root reaches. A reference counts as the use by the candidate of the
   * {@code definitionClass} that contains it, a use of a candidate by itself does not count, and a reference outside any
   * candidate (an export, a spec, a record, another definition that is not a candidate) is a root.
   */
  static <T extends PsiElement> @NotNull Set<T> unreachable(@NotNull PsiFile file,
                                                            @NotNull Collection<T> candidates,
                                                            @NotNull Class<? extends T> definitionClass,
                                                            @NotNull Predicate<PsiElement> ignoredReference) {
    LocalSearchScope scope = new LocalSearchScope(file);
    Set<T> candidateSet = new HashSet<>(candidates);
    Map<T, List<T>> usedBy = new HashMap<>();
    Set<T> live = new HashSet<>();
    Deque<T> queue = new ArrayDeque<>();
    for (T candidate : candidateSet) {
      ProgressManager.checkCanceled();
      for (PsiReference reference : ReferencesSearch.search(candidate, scope).findAll()) {
        PsiElement element = reference.getElement();
        if (ignoredReference.test(element)) continue;
        T user = PsiTreeUtil.getParentOfType(element, definitionClass);
        if (user == candidate) continue;
        if (user == null || !candidateSet.contains(user)) {
          if (live.add(candidate)) queue.add(candidate);
          break;
        }
        usedBy.computeIfAbsent(user, k -> new ArrayList<>()).add(candidate);
      }
    }
    while (!queue.isEmpty()) {
      ProgressManager.checkCanceled();
      for (T used : usedBy.getOrDefault(queue.poll(), Collections.emptyList())) {
        if (live.add(used)) queue.add(used);
      }
    }
    Set<T> result = new LinkedHashSet<>(candidates);
    result.removeAll(live);
    return result;
  }
}
