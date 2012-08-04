/*
 * Copyright 2012 Sergey Ignatov
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
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangQVar;
import org.intellij.erlang.psi.ErlangRecursiveVisitor;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.*;

/**
 * @author ignatov
 */
public class ErlangUnresolvedVariableInspection extends ErlangBaseInspection {
  @Nls
  @NotNull
  @Override
  public String getDisplayName() {
    return "Unresolved variable";
  }

  @NotNull
  @Override
  public String getShortName() {
    return "ErlangUnresolvedVariableInspection";
  }

  @Override
  protected void checkFile(PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;
    file.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitQVar(@NotNull ErlangQVar o) {
        if (inDefinition(o) || isLeftPartOfAssignment(o) || inAtomAttribute(o) || isMacros(o) || isForceSkipped(o) || inSpecification(o)) {
          return;
        }
        PsiReference reference = o.getReference();
        if (reference != null && reference.resolve() == null) {
          problemsHolder.registerProblem(o, "Unresolved variable " + "'" + o.getText() + "'");
        }
      }
    });
  }
}
