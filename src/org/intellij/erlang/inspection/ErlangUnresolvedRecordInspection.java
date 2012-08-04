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

import com.intellij.codeInspection.InspectionManager;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.psi.ErlangRecordExpression;
import org.intellij.erlang.psi.ErlangRecursiveVisitor;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangUnresolvedRecordInspection extends ErlangBaseInspection {
  @Nls
  @NotNull
  @Override
  public String getDisplayName() {
    return "Unresolved record";
  }

  @NotNull
  @Override
  public String getShortName() {
    return "ErlangUnresolvedRecordInspection";
  }

  @Override
  protected void checkFile(PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;
    file.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitRecordExpression(@NotNull ErlangRecordExpression o) {
        ErlangQAtom atomName = o.getAtomName();
        if (atomName != null) {
          PsiElement prevSibling = atomName.getPrevSibling();
          if (prevSibling != null && "#".equals(prevSibling.getText())) {
            o.getReference();
            PsiReference reference = atomName.getReference(); // todo: rewrite
            if (reference == null || reference.resolve() == null) {
              problemsHolder.registerProblem(atomName, "Unresolved record " + "'" + atomName.getText() + "'");
            }
          }
        }
      }
    });
  }
}
