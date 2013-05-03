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

package org.intellij.erlang.documentation;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;

import java.util.regex.Pattern;

final class ErlangSdkModuleDocProvider extends ErlangSdkDocProviderBase {
  private static final Pattern PATTERN_MODULE_BEGIN = Pattern.compile("^  <h3>MODULE</h3>$");
  private static final Pattern PATTERN_MODULE_END = Pattern.compile("^  <h3>EXPORTS</h3>$");

  public ErlangSdkModuleDocProvider(@NotNull Project project, @NotNull VirtualFile virtualFile) {
    super(project, virtualFile);
  }

  @NotNull
  @Override
  protected String getInDocRef() {
    return "";
  }

  @Override
  public boolean isDocBegin(@NotNull String line) {
    return PATTERN_MODULE_BEGIN.matcher(line).matches();
  }

  @Override
  public boolean isDocEnd(@NotNull String line) {
    return PATTERN_MODULE_END.matcher(line).matches();
  }
}
