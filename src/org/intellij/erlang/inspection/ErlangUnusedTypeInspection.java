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
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangTypeDefinition;
import org.intellij.erlang.quickfixes.ErlangExportTypeFix;
import org.intellij.erlang.quickfixes.ErlangRemoveTypeFix;
import org.jetbrains.annotations.NotNull;

public class ErlangUnusedTypeInspection extends ErlangInspectionBase {
  @Override
  protected boolean canRunOn(@NotNull ErlangFile file) {
    return !file.getName().endsWith(ErlangFileType.HEADER.getDefaultExtension());
  }

  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull final ProblemsHolder problemsHolder) {
    // a type is used when a use of it is reachable from a spec, a callback, a record, -export_type or a used type
    for (ErlangTypeDefinition o : ErlangUnusedCode.unreachable(file, file.getTypes(), ErlangTypeDefinition.class, element -> false)) {
      problemsHolder.registerProblem(o.getNameIdentifier(),
        "Unused type " + "'" + o.getName() + "'",
        ProblemHighlightType.LIKE_UNUSED_SYMBOL,
        new ErlangRemoveTypeFix(),
        new ErlangExportTypeFix());
    }
  }
}
