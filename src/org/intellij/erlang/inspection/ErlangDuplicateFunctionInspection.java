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

import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.openapi.util.Pair;
import com.intellij.util.containers.MultiMap;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;

public class ErlangDuplicateFunctionInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder problemsHolder) {
    MultiMap<Pair<String, Integer>, ErlangFunction> map = new MultiMap<>();
    for (ErlangFunction fun : file.getFunctions()) {
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

