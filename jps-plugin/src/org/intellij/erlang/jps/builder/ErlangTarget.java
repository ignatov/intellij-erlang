/*
 * Copyright 2012-2014 Sergey Ignatov
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
import org.intellij.erlang.jps.model.JpsErlangModuleType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.builders.*;
import org.jetbrains.jps.builders.storage.BuildDataPaths;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.indices.IgnoredFileIndex;
import org.jetbrains.jps.indices.ModuleExcludeIndex;
import org.jetbrains.jps.model.JpsModel;
import org.jetbrains.jps.model.java.JpsJavaClasspathKind;
import org.jetbrains.jps.model.java.JpsJavaExtensionService;
import org.jetbrains.jps.model.module.JpsModule;

import java.io.File;
import java.util.Collection;
import java.util.List;
import java.util.Set;

public class ErlangTarget extends ModuleBasedTarget<ErlangSourceRootDescriptor> {
  private ErlangModuleBuildOrder myBuildOrder;

  public ErlangTarget(@NotNull JpsModule module, ErlangTargetType targetType) {
    super(targetType, module);
  }

  @Override
  public String getId() {
    return myModule.getName();
  }

  @Override
  public Collection<BuildTarget<?>> computeDependencies(BuildTargetRegistry targetRegistry,
                                                        TargetOutputIndex outputIndex) {
    List<BuildTarget<?>> dependencies = ContainerUtil.newArrayList();
    Set<JpsModule> modules = getDependenciesModules();
    for (JpsModule module : modules) {
      if (module.getModuleType().equals(JpsErlangModuleType.INSTANCE)) {
        dependencies.addAll(targetRegistry.getModuleBasedTargets(module, BuildTargetRegistry.ModuleTargetSelector.ALL));
      }
    }
    dependencies.addAll(targetRegistry.getAllTargets(ErlangModuleBuildOrderTargetType.INSTANCE));
    return dependencies;
  }

  @NotNull
  private Set<JpsModule> getDependenciesModules() {
    return JpsJavaExtensionService.dependencies(myModule).includedIn(JpsJavaClasspathKind.compile(isTests())).getModules();
  }

  @NotNull
  @Override
  public List<ErlangSourceRootDescriptor> computeRootDescriptors(JpsModel model,
                                                                 ModuleExcludeIndex index,
                                                                 IgnoredFileIndex ignoredFileIndex,
                                                                 BuildDataPaths dataPaths) {
    List<ErlangSourceRootDescriptor> result = ContainerUtil.newArrayList();
    ErlangTargetBuilderUtil.addRootDescriptors(this, myModule, result);
    return result;
  }

  @Nullable
  @Override
  public ErlangSourceRootDescriptor findRootDescriptor(String rootId, BuildRootIndex rootIndex) {
    return ErlangTargetBuilderUtil.findRootDescriptor(rootId, rootIndex, (ErlangTargetType) getTargetType());
  }

  @NotNull
  @Override
  public String getPresentableName() {
    return "Erlang '" + myModule.getName() + "'";
  }

  @NotNull
  @Override
  public Collection<File> getOutputRoots(CompileContext context) {
    return ContainerUtil.newArrayList(JpsJavaExtensionService.getInstance().getOutputDirectory(myModule, false),
                                      JpsJavaExtensionService.getInstance().getOutputDirectory(myModule, true));
  }

  @Override
  public boolean isTests() {
    return false;
  }

  @Nullable
  public ErlangModuleBuildOrder getBuildOrder() {
    return myBuildOrder;
  }

  public void setBuildOrder(@NotNull ErlangModuleBuildOrder buildOrder) {
    myBuildOrder = buildOrder;
  }
}
