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
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangUnresolvedFunctionInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;
    file.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
        PsiReference reference = o.getReference();
        if (reference != null && reference.resolve() == null) {
          if (o.getQAtom().getMacros() != null) return;

          String name = o.getNameIdentifier().getText();
          int arity = o.getArgumentList().getExpressionList().size();
          if (ErlangBifTable.isBif("erlang", name, arity)) return;

          String signature = name + "/" + arity;

          PsiElement parent = o.getParent();
          if (parent instanceof ErlangGlobalFunctionCallExpression) {
            ErlangModuleRef moduleRef = ((ErlangGlobalFunctionCallExpression) parent).getModuleRef();
            if (moduleRef != null) {
              if (moduleRef.getQAtom().getMacros() != null) return;
              String moduleName = moduleRef.getText();
              if (ErlangBifTable.isBif(moduleName, name, arity)) return;
              signature = moduleName + ":" + signature;
            }
          }

          problemsHolder.registerProblem(o.getNameIdentifier(), "Unresolved function " + "'" + signature + "'");
        }
      }
    });
  }
}
