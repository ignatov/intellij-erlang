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
import com.intellij.openapi.util.Condition;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Query;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangExportFunctionFix;
import org.intellij.erlang.quickfixes.ErlangRemoveFunctionFix;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * @author ignatov
 */
public class ErlangUnusedFunctionInspection extends ErlangBaseInspection {
  @Nls
  @NotNull
  @Override
  public String getDisplayName() {
    return "Unused function";
  }

  @NotNull
  @Override
  public String getShortName() {
    return "ErlangUnusedFunctionInspection";
  }

  @Override
  protected void checkFile(PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;
    final boolean isEunitImported = ErlangPsiImplUtil.isEunitImported((ErlangFile) file);
    file.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitFunction(@NotNull final ErlangFunction function) {
        String name = function.getName();
        if (isEunitImported && (StringUtil.endsWith(name, "_test") || StringUtil.endsWith(name, "_test_"))) return;
        final Ref<Object> usage = new Ref<Object>();

        function.getContainingFile().accept(
          new ErlangRecursiveVisitor() {
            @Override
            public void visitCompositeElement(@NotNull ErlangCompositeElement o) {
              if (!usage.isNull()) return;
              super.visitCompositeElement(o);
            }

            @Override
            public void visitExportFunction(@NotNull ErlangExportFunction o) {
              PsiReference reference = o.getReference();
              if (reference != null && function.equals(reference.resolve())) {
                usage.set(o);
              }
            }

            @Override
            public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
              PsiReference reference = o.getReference();
              if (reference != null && function.equals(reference.resolve()) && !ErlangPsiImplUtil.isRecursiveCall(o, function)) {
                usage.set(o);
              }
            }
          });

        if (usage.get() == null) {
          Query<PsiReference> search = ReferencesSearch.search(function, new LocalSearchScope(function.getContainingFile()));

          List<PsiReference> refs = ContainerUtil.filter(search.findAll(), new Condition<PsiReference>() { // filtered specs out
            @Override
            public boolean value(PsiReference psiReference) {
              PsiElement element = psiReference.getElement();
              return PsiTreeUtil.getParentOfType(element, ErlangSpecification.class) == null && !ErlangPsiImplUtil.isRecursiveCall(element, function);
            }
          });

          if (ContainerUtil.getFirstItem(refs) == null) {
            problemsHolder.registerProblem(function.getNameIdentifier(),
              "Unused function " + "'" + name + "/" + function.getArity() + "'",
              new ErlangRemoveFunctionFix(),
              new ErlangExportFunctionFix());
          }
        }
      }
    });
  }

}
