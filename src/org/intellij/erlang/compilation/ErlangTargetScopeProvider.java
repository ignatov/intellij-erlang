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

package org.intellij.erlang.compilation;

import com.intellij.compiler.impl.BuildTargetScopeProvider;
import com.intellij.openapi.compiler.CompileScope;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.project.Project;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.configuration.ErlangCompilerSettings;
import org.intellij.erlang.jps.builder.ErlangModuleBuildOrderTargetType;
import org.intellij.erlang.module.ErlangModuleType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.api.CmdlineProtoUtil;
import org.jetbrains.jps.api.CmdlineRemoteProto.Message.ControllerMessage.ParametersMessage.TargetTypeBuildScope;

import java.util.Collections;
import java.util.List;

public class ErlangTargetScopeProvider extends BuildTargetScopeProvider {
  @NotNull
  @Override
  public List<TargetTypeBuildScope> getBuildTargetScopes(@NotNull CompileScope baseScope,
                                                         @NotNull Project project,
                                                         boolean forceBuild) {
    if (ErlangCompilerSettings.getInstance(project).isUseRebarCompilerEnabled() || !hasErlangModules(baseScope)) {
      return ContainerUtil.emptyList();
    }
    return Collections.singletonList(CmdlineProtoUtil.createTargetsScope(ErlangModuleBuildOrderTargetType.TYPE_ID,
                                                                         Collections.singletonList(project.getName()),
                                                                         forceBuild));
  }

  private static boolean hasErlangModules(@NotNull CompileScope baseScope) {
    for (Module module : baseScope.getAffectedModules()) {
      if (ModuleType.get(module) instanceof ErlangModuleType) {
        return true;
      }
    }
    return false;
  }
}
