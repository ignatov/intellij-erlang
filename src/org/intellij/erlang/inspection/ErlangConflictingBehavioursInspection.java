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

package org.intellij.erlang.inspection;

import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.openapi.util.Pair;
import com.intellij.psi.PsiElement;
import com.intellij.util.Function;
import com.intellij.util.ObjectUtils;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.MultiMap;
import org.intellij.erlang.psi.ErlangBehaviour;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangModule;
import org.intellij.erlang.psi.ErlangModuleRef;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.Set;

public class ErlangConflictingBehavioursInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder holder) {
    ErlangModule module = file.getModule();
    if (module == null) return;

    List<Pair<String, List<String>>> duplicates = computeCallbacksRequiredByMultipleModules(file);
    if (!duplicates.isEmpty()) {
      reportProblem(module, holder, duplicates);
    }
  }

  private void reportProblem(@NotNull ErlangModule module,
                             @NotNull ProblemsHolder problemsHolder,
                             @NotNull List<Pair<String, List<String>>> callbacksAndRequiringModules) {
    assert !callbacksAndRequiringModules.isEmpty();

    StringBuilder builder = new StringBuilder("Conflicting behaviours - ");
    String semicolon = "; ";
    String comma = ", ";
    for (Pair<String, List<String>> callbackAndRequiringModules : callbacksAndRequiringModules) {
      String callback = callbackAndRequiringModules.first;
      List<String> requiringModules = callbackAndRequiringModules.second;

      assert !requiringModules.isEmpty();

      builder.append("callback ").append(callback).append(" required by ");
      for (String requiringModule : requiringModules) {
        builder.append("'").append(requiringModule).append("'").append(comma);
      }
      builder.setLength(builder.length() - comma.length());

      builder.append(semicolon);
    }
    String message = builder.substring(0, builder.length() - semicolon.length());
    registerProblem(problemsHolder, module, message);
  }

  @NotNull
  private static List<Pair<String, List<String>>> computeCallbacksRequiredByMultipleModules(@NotNull ErlangFile file) {
    List<String> orderedCallbacks = ContainerUtil.newArrayList();
    final MultiMap<String, String> callbacksToOwners = MultiMap.create();

    Set<String> distinctBehaviours = ContainerUtil.newHashSet();
    for (ErlangBehaviour behaviour : file.getBehaviours()) {
      String behaviourName = behaviour.getName();
      if (!distinctBehaviours.add(behaviourName)) continue;

      ErlangModuleRef moduleRef = behaviour.getModuleRef();
      PsiElement m = moduleRef != null ? moduleRef.getReference().resolve() : null;
      ErlangFile behaviourModule = ObjectUtils.tryCast(m != null ? m.getContainingFile() : null, ErlangFile.class);
      if (behaviourModule == null) continue;

      for (String callback : behaviourModule.getCallbackMap().keySet()) {
        if (!callbacksToOwners.containsKey(callback)) {
          orderedCallbacks.add(callback);
        }
        callbacksToOwners.putValue(callback, behaviourName);
      }
    }

    return ContainerUtil.mapNotNull(orderedCallbacks, new Function<String, Pair<String, List<String>>>() {
      @Override
      public Pair<String, List<String>> fun(String callback) {
        Collection<String> callbackOwners = callbacksToOwners.get(callback);
        //noinspection ConstantConditions
        return callbackOwners.size() < 2 ? null :
               Pair.<String, List<String>>create(callback, ContainerUtil.newArrayList(callbackOwners));
      }
    });
  }
}
