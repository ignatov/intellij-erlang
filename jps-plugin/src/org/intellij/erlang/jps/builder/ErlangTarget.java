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
import org.jetbrains.jps.model.java.JavaSourceRootProperties;
import org.jetbrains.jps.model.java.JavaSourceRootType;
import org.jetbrains.jps.model.java.JpsJavaClasspathKind;
import org.jetbrains.jps.model.java.JpsJavaExtensionService;
import org.jetbrains.jps.model.module.JpsModule;
import org.jetbrains.jps.model.module.JpsTypedModuleSourceRoot;

import java.io.File;
import java.util.*;

public class ErlangTarget extends ModuleBasedTarget<ErlangSourceRootDescriptor> {
  public ErlangTarget(@NotNull JpsModule module, ErlangTargetType targetType) {
    super(targetType, module);
  }

  @Override
  public String getId() {
    return myModule.getName();
  }

  @Override
  public Collection<BuildTarget<?>> computeDependencies(BuildTargetRegistry targetRegistry, TargetOutputIndex outputIndex) {
    return computeDependencies();
  }

  public Collection<BuildTarget<?>> computeDependencies() {
    List<BuildTarget<?>> dependencies = new ArrayList<BuildTarget<?>>();
    Set<JpsModule> modules = JpsJavaExtensionService.dependencies(myModule).includedIn(JpsJavaClasspathKind.compile(isTests())).getModules();
    for (JpsModule module : modules) {
      if (module.getModuleType().equals(JpsErlangModuleType.INSTANCE)) {
        dependencies.add(new ErlangTarget(module, getErlangTargetType()));
      }
    }
    if (isTests()) {
      dependencies.add(new ErlangTarget(myModule, ErlangTargetType.PRODUCTION));
    }
    return dependencies;
  }

  @NotNull
  @Override
  public List<ErlangSourceRootDescriptor> computeRootDescriptors(JpsModel model, ModuleExcludeIndex index, IgnoredFileIndex ignoredFileIndex, BuildDataPaths dataPaths) {
    List<ErlangSourceRootDescriptor> result = new ArrayList<ErlangSourceRootDescriptor>();
    JavaSourceRootType type = isTests() ? JavaSourceRootType.TEST_SOURCE : JavaSourceRootType.SOURCE;
    for (JpsTypedModuleSourceRoot<JavaSourceRootProperties> root : myModule.getSourceRoots(type)) {
      result.add(new ErlangSourceRootDescriptor(root.getFile(), this));
    }
    return result;
  }

  @Nullable
  @Override
  public ErlangSourceRootDescriptor findRootDescriptor(String rootId, BuildRootIndex rootIndex) {
    return ContainerUtil.getFirstItem(rootIndex.getRootDescriptors(new File(rootId), Collections.singletonList(getErlangTargetType()), null));
  }

  @NotNull
  @Override
  public String getPresentableName() {
    return "Erlang '" + myModule.getName() + "' " + (isTests() ? "tests" : "production");
  }

  @NotNull
  @Override
  public Collection<File> getOutputRoots(CompileContext context) {
    return ContainerUtil.createMaybeSingletonList(JpsJavaExtensionService.getInstance().getOutputDirectory(myModule, isTests()));
  }
  
  @Override
  public boolean isTests() {
    return getErlangTargetType().isTests();
  }

  public ErlangTargetType getErlangTargetType() {
    return (ErlangTargetType) getTargetType();
  }
}
