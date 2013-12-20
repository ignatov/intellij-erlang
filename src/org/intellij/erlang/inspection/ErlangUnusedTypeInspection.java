/*
 * Copyright 2012-2013 Sergey Ignatov
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

import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.util.Query;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangRecursiveVisitor;
import org.intellij.erlang.psi.ErlangTypeDefinition;
import org.intellij.erlang.quickfixes.ErlangExportTypeFix;
import org.intellij.erlang.quickfixes.ErlangRemoveTypeFix;
import org.jetbrains.annotations.NotNull;

public class ErlangUnusedTypeInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;
    file.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitTypeDefinition(@NotNull ErlangTypeDefinition o) {
        Query<PsiReference> search = ReferencesSearch.search(o, new LocalSearchScope(o.getContainingFile()));
        if (search.findFirst() == null) {
          problemsHolder.registerProblem(o.getNameIdentifier(),
            "Unused function " + "'" + o.getName() + "'",
            ProblemHighlightType.LIKE_UNUSED_SYMBOL,
            new ErlangRemoveTypeFix(),
            new ErlangExportTypeFix());
        }
      }
    });
  }
}
