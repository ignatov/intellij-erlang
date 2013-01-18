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

package org.intellij.erlang.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.PsiFile;
import com.intellij.psi.impl.source.resolve.FileContextUtil;
import org.intellij.erlang.ErlangFileType;
import org.jetbrains.annotations.NotNull;

public class ErlangParserUtil extends GeneratedParserUtilBase {
  public static boolean isApplicationLanguage(PsiBuilder builder_, @SuppressWarnings("UnusedParameters") int level) {
    PsiFile file = builder_.getUserDataUnprotected(FileContextUtil.CONTAINING_FILE_KEY);
    assert file != null;
    return isApplicationConfigFileType(file);
  }

  public static boolean isApplicationConfigFileType(@NotNull PsiFile file) {
    FileType fileType = file.getViewProvider().getVirtualFile().getFileType();
    return fileType == ErlangFileType.APP || fileType == ErlangFileType.TERMS ||
      (ApplicationManager.getApplication().isUnitTestMode() && (fileType.getDefaultExtension().equals("app") || fileType.getDefaultExtension().equals("config")));
  }
}
