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
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import org.intellij.erlang.psi.ErlangBehaviour;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangModuleRef;
import org.jetbrains.annotations.NotNull;

public class ErlangUnresolvedBehaviourInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder holder) {
    for (ErlangBehaviour behaviour : file.getBehaviours()) {
      PsiElement bracket = behaviour.getParLeft();
      PsiElement target = bracket != null ? bracket.getNextSibling() : null;
      if (target == null) continue;
      ErlangModuleRef moduleRef = behaviour.getModuleRef();
      PsiReference reference = moduleRef != null ? moduleRef.getReference() : null;
      PsiElement resolvedFile = reference != null ? reference.resolve() : null;
      if (resolvedFile == null) {
        registerProblem(holder, target, "Unresolved behaviour " + behaviour.getName());
      }
    }
  }
}
