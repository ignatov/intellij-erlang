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
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.impl.source.resolve.FileContextUtil;
import org.intellij.erlang.ErlangFileType;

public class ErlangParserUtil extends GeneratedParserUtilBase {
  public static boolean isApplicationLanguage(PsiBuilder builder_, int level) {
    FileType fileType = getFileType(builder_);
    return fileType == ErlangFileType.APP || fileType == ErlangFileType.CONFIG;
  }

  private static FileType getFileType(PsiBuilder builder_) {
    PsiFile file = builder_.getUserDataUnprotected(FileContextUtil.CONTAINING_FILE_KEY);
    assert file != null;
    return file.getViewProvider().getVirtualFile().getFileType();
  }
}
