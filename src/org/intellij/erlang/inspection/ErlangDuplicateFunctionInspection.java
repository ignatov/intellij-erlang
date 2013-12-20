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
import com.intellij.openapi.util.Pair;
import com.intellij.psi.PsiFile;
import com.intellij.util.containers.MultiMap;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;

import java.util.Collection;
import java.util.List;

public class ErlangDuplicateFunctionInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(final PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;

    final MultiMap<Pair<String, Integer>, ErlangFunction> map = new MultiMap<Pair<String, Integer>, ErlangFunction>();
    List<ErlangFunction> funs = ((ErlangFile) file).getFunctions();
    for (ErlangFunction fun : funs) {
      map.putValue(Pair.create(fun.getName(), fun.getArity()), fun);
    }

    for (Pair<String, Integer> pair : map.keySet()) {
      Collection<ErlangFunction> erlangFunctions = map.get(pair);
      if (erlangFunctions.size() > 1) {
        for (ErlangFunction fun : erlangFunctions) {
          problemsHolder.registerProblem(fun.getNameIdentifier(), "Duplicate function " + "'" + fun.getName() + "/" + fun.getArity() + "'");
        }
      }
    }
  }
}

