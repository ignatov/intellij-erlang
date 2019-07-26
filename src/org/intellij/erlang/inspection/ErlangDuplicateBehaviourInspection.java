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

import com.intellij.codeInspection.LocalQuickFixBase;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangBehaviour;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangModule;
import org.jetbrains.annotations.NotNull;

import java.util.HashSet;
import java.util.Set;

public class ErlangDuplicateBehaviourInspection extends ErlangInspectionBase {
  public static final String FIX_MESSAGE = "Remove duplicate behaviours";

  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder holder) {
    ErlangModule module = file.getModule();
    if (module == null) return;

    Set<String> distinctBehaviourNames = new HashSet<>();
    Set<String> behaviourDuplicates = new HashSet<>();
    for (ErlangBehaviour behaviour : file.getBehaviours()) {
      String name = behaviour.getName();
      if (!distinctBehaviourNames.add(name)) {
        behaviourDuplicates.add(name);
      }
    }
    if (behaviourDuplicates.isEmpty()) return;

    StringBuilder builder = new StringBuilder("Duplicate behaviours - ");
    String separator = ", ";
    for (String name : behaviourDuplicates) {
      builder.append("'").append(name).append("'").append(separator);
    }
    String message = builder.substring(0, builder.length() - separator.length());
    registerProblem(holder, module, message, new RemoveDuplicateBehaviourFix());
  }

  private static class RemoveDuplicateBehaviourFix extends LocalQuickFixBase {
    protected RemoveDuplicateBehaviourFix() {
      super(FIX_MESSAGE);
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor problemDescriptor) {
      ErlangFile file = PsiTreeUtil.getParentOfType(problemDescriptor.getPsiElement(), ErlangFile.class);
      if (file == null) return;

      Set<String> distinctBehaviourNames = new HashSet<>();
      for (ErlangBehaviour behaviour : file.getBehaviours()) {
        if (!distinctBehaviourNames.add(behaviour.getName())) {
          PsiElement element = PsiTreeUtil.getParentOfType(behaviour, ErlangAttribute.class);
          if (element != null) {
            element.delete();
          }
        }
      }
    }
  }
}