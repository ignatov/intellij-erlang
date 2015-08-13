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
import org.intellij.erlang.jps.model.ErlangIncludeSourceRootType;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.builders.BuildRootIndex;
import org.jetbrains.jps.builders.BuildTarget;
import org.jetbrains.jps.builders.BuildTargetType;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.incremental.ProjectBuildException;
import org.jetbrains.jps.incremental.messages.BuildMessage;
import org.jetbrains.jps.incremental.messages.CompilerMessage;
import org.jetbrains.jps.model.JpsDummyElement;
import org.jetbrains.jps.model.java.JavaSourceRootProperties;
import org.jetbrains.jps.model.java.JavaSourceRootType;
import org.jetbrains.jps.model.library.sdk.JpsSdk;
import org.jetbrains.jps.model.module.JpsModule;
import org.jetbrains.jps.model.module.JpsTypedModuleSourceRoot;

import java.io.File;
import java.util.Collections;
import java.util.List;

public class ErlangTargetBuilderUtil {
  private ErlangTargetBuilderUtil() {
  }

  @NotNull
  public static JpsSdk<JpsDummyElement> getSdk(@NotNull CompileContext context,
                                               @NotNull JpsModule module) throws ProjectBuildException {
    JpsSdk<JpsDummyElement> sdk = module.getSdk(JpsErlangSdkType.INSTANCE);
    if (sdk == null) {
      String errorMessage = "No SDK for module " + module.getName();
      context.processMessage(new CompilerMessage(ErlangBuilder.NAME, BuildMessage.Kind.ERROR, errorMessage));
      throw new ProjectBuildException(errorMessage);
    }
    return sdk;
  }

  public static void addRootDescriptors(BuildTarget<ErlangSourceRootDescriptor> target,
                                        JpsModule jpsModule,
                                        List<ErlangSourceRootDescriptor> result) {
    for (JpsTypedModuleSourceRoot<JavaSourceRootProperties> root : jpsModule.getSourceRoots(JavaSourceRootType.SOURCE)) {
      result.add(new ErlangSourceRootDescriptor(root.getFile(), target, jpsModule.getName(), false));
    }
    for (JpsTypedModuleSourceRoot<JavaSourceRootProperties> root : jpsModule.getSourceRoots(JavaSourceRootType.TEST_SOURCE)) {
      result.add(new ErlangSourceRootDescriptor(root.getFile(), target, jpsModule.getName(), true));
    }
    for (JpsTypedModuleSourceRoot<JpsDummyElement> root : jpsModule.getSourceRoots(ErlangIncludeSourceRootType.INSTANCE)) {
      result.add(new ErlangSourceRootDescriptor(root.getFile(), target, jpsModule.getName(), false));
    }
  }

  @Nullable
  public static ErlangSourceRootDescriptor findRootDescriptor(String rootId,
                                                              BuildRootIndex rootIndex,
                                                              BuildTargetType<? extends BuildTarget<ErlangSourceRootDescriptor>> targetType) {
    return ContainerUtil.getFirstItem(rootIndex.getRootDescriptors(new File(rootId), Collections.singletonList(targetType), null));
  }
}
