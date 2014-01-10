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
import com.intellij.psi.PsiFile;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangInclude;
import org.intellij.erlang.psi.ErlangIncludeString;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;

import java.util.List;

public class ErlangUnresolvedIncludeInspection extends ErlangInspectionBase {
  public static String INCLUDE_LABEL = "include";

  @Override
  protected void checkFile(PsiFile file, ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;

    for (ErlangInclude erlangInclude : ((ErlangFile) file).getIncludes()) {
      ErlangIncludeString string = erlangInclude.getIncludeStringSafe();
      if (string == null) continue;
      List<ErlangFile> files = ErlangPsiImplUtil.getDirectlyIncludedFiles(erlangInclude, (ErlangFile) file);
      ErlangUnresolvedIncludeLibInspection.processInclude(problemsHolder, files, string, INCLUDE_LABEL);
    }
  }
}
