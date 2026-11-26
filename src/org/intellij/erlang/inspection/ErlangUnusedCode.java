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
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

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
   * candidate (an export, a spec, a record, another definition that is not a candidate) is a root. An anchor, a candidate the
   * file may use where the search sees no reference, keeps what it uses used, but is itself used only when a root reaches it.
   */
  static <T extends PsiElement> @NotNull Set<T> unreachable(@NotNull PsiFile file,
                                                            @NotNull Collection<T> candidates,
                                                            @NotNull Class<? extends T> definitionClass,
                                                            @NotNull Predicate<PsiElement> ignoredReference,
                                                            @NotNull Predicate<T> anchor) {
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
    for (T candidate : candidateSet) {
      if (anchor.test(candidate)) queue.add(candidate);
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

  /**
   * The atoms that may name a definition where the reference search finds no use of it: the arguments of macro calls, the
   * bodies of macros, the name of a call with a macro among its arguments, and every atom of the included headers.
   */
  static @NotNull Set<String> namesUsedOutOfSight(@NotNull ErlangFile file) {
    Set<String> names = new HashSet<>();
    for (ErlangGenericFunctionCallExpression call : PsiTreeUtil.findChildrenOfType(file, ErlangGenericFunctionCallExpression.class)) {
      if (call.getMacros() != null || ContainerUtil.exists(call.getQAtomList(), q -> q.getMacros() != null)) addAtoms(call.getArgumentList(), names);
    }
    for (ErlangMacrosDefinition definition : PsiTreeUtil.findChildrenOfType(file, ErlangMacrosDefinition.class)) {
      addAtoms(definition.getMacrosBody(), names);
    }
    for (ErlangFunctionCallExpression call : PsiTreeUtil.findChildrenOfType(file, ErlangFunctionCallExpression.class)) {
      ErlangQAtom name = PsiTreeUtil.getChildOfType(call, ErlangQAtom.class);
      if (name != null && name.getMacros() != null) addAtoms(call.getArgumentList(), names);
      else if (PsiTreeUtil.findChildOfType(call.getArgumentList(), ErlangMacros.class) != null) names.add(call.getName());
    }
    Set<ErlangFile> headers = new HashSet<>();
    Deque<ErlangFile> queue = new ArrayDeque<>(List.of(file));
    while (!queue.isEmpty()) {
      ProgressManager.checkCanceled();
      for (ErlangFile header : ErlangPsiImplUtil.getDirectlyIncludedFiles(queue.poll())) {
        if (header != file && headers.add(header)) {
          addAtoms(header, names);
          queue.add(header);
        }
      }
    }
    return names;
  }

  private static void addAtoms(@Nullable PsiElement element, @NotNull Set<String> names) {
    if (element == null) return;
    PsiTreeUtil.processElements(element, e -> {
      if (e.getNode().getElementType() == ErlangTypes.ERL_ATOM_NAME) names.add(StringUtil.unquoteString(e.getText(), '\''));
      return true;
    });
  }
}
