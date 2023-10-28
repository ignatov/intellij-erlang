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

package org.intellij.erlang.configuration;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.project.Project;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.jps.model.ErlangCompilerOptions;
import org.intellij.erlang.jps.model.JpsErlangCompilerOptionsSerializer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

@State(name = JpsErlangCompilerOptionsSerializer.COMPILER_OPTIONS_COMPONENT_NAME, storages = {@Storage(value = "compiler.xml")})
public class ErlangCompilerSettings implements PersistentStateComponent<ErlangCompilerOptions> {
  private ErlangCompilerOptions myCompilerOptions = new ErlangCompilerOptions();

  @Nullable
  @Override
  public ErlangCompilerOptions getState() {
    return myCompilerOptions;
  }

  @Override
  public void loadState(@NotNull ErlangCompilerOptions state) {
    myCompilerOptions = state;
  }

  public boolean isUseRebarCompilerEnabled() {
    return myCompilerOptions.myUseRebarCompiler;
  }

  public void setUseRebarCompilerEnabled(boolean useRebarCompiler) {
    myCompilerOptions.myUseRebarCompiler = useRebarCompiler;
  }

  public boolean isAddDebugInfoEnabled() {
    return myCompilerOptions.myAddDebugInfoEnabled;
  }

  public void setAddDebugInfoEnabled(boolean useDebugInfo) {
    myCompilerOptions.myAddDebugInfoEnabled = useDebugInfo;
  }

  @NotNull
  public List<String> getAdditionalErlcArguments() {
    return ContainerUtil.notNullize(myCompilerOptions.myAdditionalErlcArguments);
  }

  public void setAdditionalErlcArguments(@NotNull List<String> arguments) {
    myCompilerOptions.myAdditionalErlcArguments = new ArrayList<>(arguments);
  }

  @NotNull
  public static ErlangCompilerSettings getInstance(@NotNull Project project) {
    ErlangCompilerSettings persisted = project.getService(ErlangCompilerSettings.class);
    return persisted != null ? persisted : new ErlangCompilerSettings();
  }
}
