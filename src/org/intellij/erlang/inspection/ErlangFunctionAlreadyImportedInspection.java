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

import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiFile;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangRemoveFunctionFromImportFix;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Map;

public class ErlangFunctionAlreadyImportedInspection extends ErlangInspectionBase {

  @NotNull
  protected ErlangVisitor buildErlangVisitor(@NotNull final ProblemsHolder holder, @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitFile(@NotNull PsiFile file) {
        if (!(file instanceof ErlangFile)) return;
        Map<String, String> alreadyImported = ContainerUtil.newHashMap();
        for (ErlangAttribute attribute : ((ErlangFile)file).getAttributes()) {
          ErlangImportFunctions importFunctions =
            attribute.getImportDirective() != null ? attribute.getImportDirective().getImportFunctions() : null;
          if (importFunctions == null) continue;
          ErlangModuleRef moduleRef = attribute.getImportDirective().getModuleRef();
          if (moduleRef == null) continue;
          List<String> justImported = ContainerUtil.newArrayList();
          for (ErlangImportFunction f : importFunctions.getImportFunctionList()) {
            String functionPresentation = ErlangPsiImplUtil.createFunctionPresentation(f);
            if (functionPresentation == null) continue;
            if (alreadyImported.containsKey(functionPresentation)) {
              holder.registerProblem(f, "Function '" + functionPresentation + "' already imported from '" +
                  alreadyImported.get(functionPresentation) + "'",
                ProblemHighlightType.GENERIC_ERROR_OR_WARNING,
                new ErlangRemoveFunctionFromImportFix(true));
            }
            justImported.add(functionPresentation);
          }
          for (String s : justImported) {
            alreadyImported.put(s, moduleRef.getText());
          }
        }
      }
    };
  }

}