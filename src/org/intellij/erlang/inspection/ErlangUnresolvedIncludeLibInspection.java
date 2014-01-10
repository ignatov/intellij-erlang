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
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.util.Function;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangIncludeLib;
import org.intellij.erlang.psi.ErlangIncludeString;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangFindIncludeQuickFix;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class ErlangUnresolvedIncludeLibInspection extends ErlangInspectionBase {
  public static String INCLUDE_LIB_LABEL = "include_lib";
  private static final Logger LOG = Logger.getInstance(ErlangUnresolvedIncludeLibInspection.class);

  @Override
  protected void checkFile(PsiFile file, @NotNull ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;

    for (ErlangIncludeLib erlangIncludeLib : ((ErlangFile) file).getIncludeLibs()) {
      ErlangIncludeString includeString = erlangIncludeLib.getIncludeStringSafe();
      if (includeString == null) continue;
      processInclude(problemsHolder, ErlangPsiImplUtil.getDirectlyIncludedFiles(erlangIncludeLib, (ErlangFile) file), includeString, INCLUDE_LIB_LABEL);
    }
  }

  static void processInclude(@NotNull ProblemsHolder problemsHolder,
                             @NotNull List<ErlangFile> files,
                             @NotNull ErlangIncludeString string,
                             String what) {
    boolean empty = string.getTextLength() <= 2;
    TextRange range = empty ? TextRange.create(0, string.getTextLength()) : TextRange.create(1, string.getTextLength() - 1);
    if (files.size() == 0) {
      LOG.debug(what + ": " + string.getText() + " unresolved");
      if (empty) {
        problemsHolder.registerProblem(string, range, "Unresolved " + what + ": file not found", getFindIncludeQuickFix(what));
      }
      else {
        problemsHolder.registerProblem(string, "Unresolved " + what + ": file not found",
          ProblemHighlightType.LIKE_UNKNOWN_SYMBOL, range, getFindIncludeQuickFix(what));
      }
    }
    else if (files.size() > 1) {
      String resolvedFilesList = StringUtil.join(files, new Function<ErlangFile, String>() {
        @NotNull
        @Override
        public String fun(@NotNull ErlangFile erlangFile) {
          PsiFile originalFile = erlangFile.getOriginalFile();
          VirtualFile virtualFile = originalFile.getVirtualFile();
          return virtualFile == null ? "null" : virtualFile.getPath();
        }
      }, ", ");
      LOG.debug(what + ": " + string.getText() + " resolved to: " + resolvedFilesList);
      problemsHolder.registerProblem(string, range, "Unresolved " + what + ": ambiguous file reference");
    }
  }

  private static ErlangFindIncludeQuickFix getFindIncludeQuickFix(String what) {
    boolean setDirectHrlLink = true;
    if (what.equals(INCLUDE_LIB_LABEL)) {
      setDirectHrlLink = false;
    }
    return new ErlangFindIncludeQuickFix(setDirectHrlLink);
  }
}
