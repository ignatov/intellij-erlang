/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.jps.builder;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.builders.BuildRootDescriptor;
import org.jetbrains.jps.builders.BuildTarget;

import java.io.File;
import java.io.FileFilter;

public class ErlangSourceRootDescriptor extends BuildRootDescriptor {
  private final File myRoot;
  private final String myModuleName;
  private final BuildTarget<? extends ErlangSourceRootDescriptor> myErlangTarget;
  private final boolean myTests;

  public ErlangSourceRootDescriptor(@NotNull File root,
                                    @NotNull BuildTarget<? extends ErlangSourceRootDescriptor> erlangTarget,
                                    @NotNull String moduleName,
                                    boolean isTests) {
    myRoot = root;
    myErlangTarget = erlangTarget;
    myModuleName = moduleName;
    myTests = isTests;
  }

  @Override
  public String getRootId() {
    return myRoot.getAbsolutePath();
  }

  @Override
  public File getRootFile() {
    return myRoot;
  }

  @Override
  public BuildTarget<?> getTarget() {
    return myErlangTarget;
  }

  @NotNull
  @Override
  public FileFilter createFileFilter() {
    return new FileFilter() {
      @Override
      public boolean accept(@NotNull File file) {
        String name = file.getName();
        return ErlangBuilderUtil.isSource(name) ||
               ErlangBuilderUtil.isHeader(name) ||
               ErlangBuilderUtil.isAppConfigFileName(name);
      }
    };
  }

  public boolean isTests() {
    return myTests;
  }

  @NotNull
  public String getModuleName() {
    return myModuleName;
  }
}
