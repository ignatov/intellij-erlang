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

import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.builders.BuildTarget;
import org.jetbrains.jps.builders.DirtyFilesHolder;
import org.jetbrains.jps.builders.FileProcessor;

import java.io.File;
import java.io.IOException;
import java.util.List;

public abstract class DirtyFilesProcessor<T, P extends BuildTarget<ErlangSourceRootDescriptor>> implements FileProcessor<ErlangSourceRootDescriptor, P> {
  private final List<T> myDirtyElements = ContainerUtil.newArrayList();

  public List<T> collectDirtyElements(@NotNull DirtyFilesHolder<ErlangSourceRootDescriptor, P> holder) throws IOException {
    holder.processDirtyFiles(this);
    return myDirtyElements;
  }

  @Override
  public boolean apply(P erlangTarget,
                       File file,
                       ErlangSourceRootDescriptor erlangSourceRootDescriptor) throws IOException {
    ContainerUtil.addIfNotNull(myDirtyElements, getDirtyElement(file));
    return true;
  }

  @Nullable
  protected abstract T getDirtyElement(@NotNull File file) throws IOException;
}
