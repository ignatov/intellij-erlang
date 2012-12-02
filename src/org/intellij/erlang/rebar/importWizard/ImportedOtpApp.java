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

package org.intellij.erlang.rebar.importWizard;

import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

final class ImportedOtpApp {
  @NotNull
  private final String myName;
  @NotNull
  private final VirtualFile myRoot;
  @Nullable
  private VirtualFile myIdeaModuleFile;

  public ImportedOtpApp(@NotNull String name, @NotNull VirtualFile root) {
    myName = name;
    myRoot = root;
  }

  @NotNull
  public String getName() {
    return myName;
  }

  @NotNull
  public VirtualFile getRoot() {
    return myRoot;
  }

  public void setIdeaModuleFile(@Nullable VirtualFile ideaModuleFile) {
    myIdeaModuleFile = ideaModuleFile;
  }

  @Nullable
  public VirtualFile getIdeaModuleFile() {
    return myIdeaModuleFile;
  }

  public boolean hasIdeaModuleFile() {
    return myIdeaModuleFile != null;
  }

  @Override
  public String toString() {
    return myName + " (" + myRoot + ')';
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    ImportedOtpApp that = (ImportedOtpApp) o;

    if (!myName.equals(that.myName)) return false;
    if (!myRoot.equals(that.myRoot)) return false;

    return true;
  }

  @Override
  public int hashCode() {
    int result = myName.hashCode();
    result = 31 * result + myRoot.hashCode();
    return result;
  }

}
