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

import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Query;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunctionClause;
import org.intellij.erlang.psi.ErlangQVar;
import org.intellij.erlang.psi.ErlangRecursiveVisitor;
import org.intellij.erlang.quickfixes.ErlangRenameVariableFix;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.*;

/**
 * @author ignatov
 */
public class ErlangUnusedVariableInspection extends ErlangBaseInspection {
  @Nls
  @NotNull
  @Override
  public String getDisplayName() {
    return "Unused variable";
  }

  @NotNull
  @Override
  public String getShortName() {
    return "ErlangUnusedVariableInspection";
  }

  @Override
  protected void checkFile(PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;
    file.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitQVar(@NotNull ErlangQVar o) {
        PsiReference reference = o.getReference();
        PsiElement resolve = reference != null ? reference.resolve() : null;
        if (resolve == null && !isForceSkipped(o) && !isMacros(o) && ((inDefinition(o) && !inArgumentList(o)) || inAssignment(o))) {
          ErlangFunctionClause functionClause = PsiTreeUtil.getTopmostParentOfType(o, ErlangFunctionClause.class);
          if (functionClause == null) return;
          Query<PsiReference> search = ReferencesSearch.search(o, new LocalSearchScope(functionClause));
          if (search.findFirst() == null) {
            problemsHolder.registerProblem(o, "Unused variable " + "'" + o.getText() + "'", new ErlangRenameVariableFix());
          }
        }
      }
    });
  }
}
