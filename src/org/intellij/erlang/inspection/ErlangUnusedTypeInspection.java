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

import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Query;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangTypeDefinition;
import org.intellij.erlang.psi.ErlangTypeRef;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.psi.impl.ErlangTypeReferenceImpl;
import org.intellij.erlang.quickfixes.ErlangExportTypeFix;
import org.intellij.erlang.quickfixes.ErlangRemoveTypeFix;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class ErlangUnusedTypeInspection extends ErlangInspectionBase {
  @Override
  protected boolean canRunOn(@NotNull ErlangFile file) {
    return file.getFileType() != ErlangFileType.HEADER;
  }

  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull final ProblemsHolder problemsHolder) {
    List<ErlangTypeReferenceImpl> headerReferences = null;
    for (ErlangTypeDefinition o : file.getTypes()) {
      Query<PsiReference> search = ReferencesSearch.search(o, new LocalSearchScope(o.getContainingFile()));
      if (search.findFirst() == null) {
        if (headerReferences == null) headerReferences = getTypeReferencesOfIncludedFiles(file);
        String name = o.getName();
        int arity = ErlangPsiImplUtil.getArity(o);
        if (name != null && headerReferences.stream().anyMatch(r -> r.isUnresolvedLocalUseOf(name, arity))) continue;
        problemsHolder.registerProblem(o.getNameIdentifier(),
          "Unused type " + "'" + o.getName() + "'",
          ProblemHighlightType.LIKE_UNUSED_SYMBOL,
          new ErlangRemoveTypeFix(),
          new ErlangExportTypeFix());
      }
    }
  }

  // erlc expands an include in place, so a type reference that a header leaves unresolved can name a type of the module
  @NotNull
  private static List<ErlangTypeReferenceImpl> getTypeReferencesOfIncludedFiles(@NotNull ErlangFile file) {
    List<ErlangTypeReferenceImpl> result = new ArrayList<>();
    for (ErlangFile header : ErlangPsiImplUtil.getIncludedFiles(file)) {
      for (ErlangTypeRef typeRef : PsiTreeUtil.findChildrenOfType(header, ErlangTypeRef.class)) {
        if (typeRef.getReference() instanceof ErlangTypeReferenceImpl reference) result.add(reference);
      }
    }
    return result;
  }
}
