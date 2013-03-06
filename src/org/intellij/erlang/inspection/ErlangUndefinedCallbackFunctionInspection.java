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

import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.*;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author ignatov
 */
public class ErlangUndefinedCallbackFunctionInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(PsiFile file, ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;

    //noinspection unchecked
    ErlangCompositeElement warningHolder = PsiTreeUtil.getChildOfAnyType(file, ErlangAttribute.class, ErlangModule.class);
    if (warningHolder == null) return;

    HashMap<String, String> needed = new HashMap<String, String>();

    List<ErlangBehaviour> behaviours = ((ErlangFile) file).getBehaviours();
    for (ErlangBehaviour behaviour : behaviours) {
      ErlangModuleRef moduleRef = behaviour.getModuleRef();
      PsiReference reference = moduleRef != null ? moduleRef.getReference() : null;
      PsiElement resolve = reference != null ? reference.resolve() : null;

      if (resolve instanceof ErlangModule) {
        PsiFile containingFile = resolve.getContainingFile();
        if (containingFile instanceof ErlangFile) {
          Collection<String> fullNames = ((ErlangFile) containingFile).getAllCallbacksFullNames();
          for (String name : fullNames) {
            needed.put(name, ((ErlangModule) resolve).getName());
          }
        }
      }
    }

    for (Map.Entry<String, String> entry : needed.entrySet()) {
      String fullName = entry.getKey();
      List<String> split = StringUtil.split(fullName, "/");
      if (split.size() != 2) continue;
      ErlangFunction function = ((ErlangFile) file).getFunction(split.get(0), StringUtil.parseInt(split.get(1), -1));
      if (function == null) {
        problemsHolder.registerProblem(warningHolder, "Undefined callback function '" + fullName + "'" + " (behaviour '" + entry.getValue() + "')");
      }
    }
  }
}
