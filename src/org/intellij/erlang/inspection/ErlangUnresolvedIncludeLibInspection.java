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
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangIncludeLib;
import org.intellij.erlang.psi.ErlangIncludeString;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;

import java.util.List;

/**
 * @author savenko
 */
public class ErlangUnresolvedIncludeLibInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(PsiFile file, ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;

    for (ErlangIncludeLib erlangIncludeLib : ((ErlangFile) file).getIncludeLibs()) {
      ErlangIncludeString includeString = erlangIncludeLib.getIncludeString();
      if (includeString == null) continue;
      processInclude(problemsHolder, ErlangPsiImplUtil.filesFromIncludeLib(erlangIncludeLib), includeString, "include_lib");
    }
  }

  static void processInclude(ProblemsHolder problemsHolder, List<ErlangFile> files, ErlangIncludeString string, String what) {
    TextRange range = string.getTextLength() <= 2 ? TextRange.create(0, string.getTextLength()) : TextRange.create(1, string.getTextLength() - 1);
    if (files.size() == 0) {
      problemsHolder.registerProblem(string, range, "Unresolved " + what + ": file not found");
    }
    else if (files.size() > 1) {
      problemsHolder.registerProblem(string, range, "Unresolved " + what + ": ambiguous file reference");
    }
  }
}
