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

package org.intellij.erlang.inspection;

import com.intellij.codeInspection.LocalQuickFixBase;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangCreateFunctionQuickFix;
import org.intellij.erlang.quickfixes.ErlangExportFunctionFix;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Map;

public class ErlangUndefinedCallbackFunctionInspection extends ErlangInspectionBase {
  public static final String FIX_MESSAGE = "Implement and export all callbacks";

  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder holder) {
    for (ErlangBehaviour behaviour : file.getBehaviours()) {
      ErlangModuleRef moduleRef = behaviour.getModuleRef();
      PsiElement module = moduleRef != null ? moduleRef.getReference().resolve() : null;
      ErlangFile behaviourFile = module instanceof ErlangModule ? (ErlangFile) module.getContainingFile() : null;
      PsiElement toHighlight = behaviour.getParLeft() != null ? behaviour.getParLeft().getNextSibling() : null;
      if (behaviourFile == null || toHighlight == null) continue;

      List<ErlangCallbackSpec> undefinedCallbacks = ContainerUtil.newArrayList();
      Map<String, ErlangCallbackSpec> callbackMap = behaviourFile.getCallbackMap();
      for (ErlangCallbackSpec spec : callbackMap.values()) {
        String name = ErlangPsiImplUtil.getCallbackSpecName(spec);
        int arity = ErlangPsiImplUtil.getCallbackSpecArity(spec);
        ErlangFunction function = name != null ? file.getFunction(name, arity) : null;
        if (function == null || !function.isExported()) {
          undefinedCallbacks.add(spec);
        }
      }

      if (!undefinedCallbacks.isEmpty()) {
        boolean multiple = undefinedCallbacks.size() != 1;
        StringBuilder builder = new StringBuilder();
        builder.append("Undefined or non-exported callback function").append(multiple ? "s" : "").append(": ");
        for (ErlangCallbackSpec spec : undefinedCallbacks) {
          builder.append("'").append(ErlangPsiImplUtil.createFunctionPresentationFromCallbackSpec(spec)).append("', ");
        }
        String message = builder.substring(0, builder.length() - 2);
        registerProblem(holder, toHighlight, message, new ErlangCreateAndExportCallbacksFix(behaviour.getName()));
      }
    }
  }

  private static class ErlangCreateAndExportCallbacksFix extends LocalQuickFixBase {
    private final String myBehaviourName;

    protected ErlangCreateAndExportCallbacksFix(@NotNull String behaviourName) {
      super(FIX_MESSAGE);
      myBehaviourName = behaviourName;
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor problemDescriptor) {
      ErlangFile file = PsiTreeUtil.getParentOfType(problemDescriptor.getPsiElement(), ErlangFile.class);
      ErlangBehaviour behaviour = findBehaviour(file, myBehaviourName);
      ErlangModuleRef moduleRef = behaviour != null ? behaviour.getModuleRef() : null;
      PsiElement module = moduleRef != null ? moduleRef.getReference().resolve() : null;
      ErlangFile behaviourFile = module instanceof ErlangModule ? (ErlangFile) module.getContainingFile() : null;
      if (behaviourFile == null) return;

      Map<String, ErlangCallbackSpec> callbackMap = behaviourFile.getCallbackMap();
      for (ErlangCallbackSpec spec : callbackMap.values()) {
        String name = ErlangPsiImplUtil.getCallbackSpecName(spec);
        int arity = ErlangPsiImplUtil.getCallbackSpecArity(spec);
        if (name == null) continue;
        if (file.getFunction(name, arity) == null) {
          new ErlangCreateFunctionQuickFix(name, arity).applyFix(project, problemDescriptor);
        }
        ErlangFunction newFunction = file.getFunction(name, arity);
        if (newFunction != null && !newFunction.isExported())  {
          new ErlangExportFunctionFix(newFunction).invoke(project, file, null, newFunction, null);
        }
      }
    }

    @Nullable
    private static ErlangBehaviour findBehaviour(@Nullable ErlangFile file, @NotNull String name) {
      if (file == null) return null;
      for (ErlangBehaviour behaviour : file.getBehaviours()) {
        if (name.equals(behaviour.getName())) return behaviour;
      }
      return null;
    }
  }
}