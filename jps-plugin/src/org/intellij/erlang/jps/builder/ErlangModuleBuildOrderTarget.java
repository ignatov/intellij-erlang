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
import org.jetbrains.jps.builders.BuildRootIndex;
import org.jetbrains.jps.builders.BuildTarget;
import org.jetbrains.jps.builders.BuildTargetRegistry;
import org.jetbrains.jps.builders.TargetOutputIndex;
import org.jetbrains.jps.builders.storage.BuildDataPaths;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.indices.IgnoredFileIndex;
import org.jetbrains.jps.indices.ModuleExcludeIndex;
import org.jetbrains.jps.model.JpsModel;
import org.jetbrains.jps.model.JpsProject;
import org.jetbrains.jps.model.module.JpsModule;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class ErlangModuleBuildOrderTarget extends BuildTarget<ErlangSourceRootDescriptor> {
  private static final String NAME = "prepare files target";

  private final JpsProject myProject;

  public ErlangModuleBuildOrderTarget(@NotNull JpsProject project,
                                      @NotNull ErlangModuleBuildOrderTargetType targetType) {
    super(targetType);
    myProject = project;
  }

  @Override
  public @NotNull String getId() {
    return myProject.getName();
  }

  @Override
  public @NotNull Collection<BuildTarget<?>> computeDependencies(@NotNull BuildTargetRegistry targetRegistry,
                                                                 @NotNull TargetOutputIndex outputIndex) {
    return ContainerUtil.emptyList();
  }

  @NotNull
  @Override
  public List<ErlangSourceRootDescriptor> computeRootDescriptors(@NotNull JpsModel model,
                                                                 @NotNull ModuleExcludeIndex index,
                                                                 @NotNull IgnoredFileIndex ignoredFileIndex,
                                                                 @NotNull BuildDataPaths dataPaths) {
    if (model == null) {
      return ContainerUtil.emptyList();
    }
    List<ErlangSourceRootDescriptor> result = new ArrayList<>();
    for (JpsModule module : model.getProject().getModules()) {
      ErlangTargetBuilderUtil.addRootDescriptors(this, module, result);
    }
    return result;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof ErlangModuleBuildOrderTarget that)) return false;

    if (!myProject.equals(that.myProject)) return false;

    return true;
  }

  @Override
  public int hashCode() {
    return myProject.hashCode();
  }

  @Nullable
  @Override
  public ErlangSourceRootDescriptor findRootDescriptor(@NotNull String rootId, @NotNull BuildRootIndex rootIndex) {
    return ErlangTargetBuilderUtil.findRootDescriptor(rootId,
                                                      rootIndex,
                                                      getTargetType());
  }

  @NotNull
  @Override
  public String getPresentableName() {
    return NAME;
  }

  @NotNull
  @Override
  public Collection<File> getOutputRoots(@NotNull CompileContext context) {
    return ContainerUtil.emptyList();
  }

  @NotNull
  public JpsProject getProject() {
    return myProject;
  }
}
