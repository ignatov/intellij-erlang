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

package org.intellij.erlang.debugger.remote;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.RunConfigurationWithSuppressedDefaultRunAction;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import org.intellij.erlang.debugger.remote.ui.ErlangRemoteDebugConfigurationEditorForm;
import org.intellij.erlang.runconfig.ErlangModuleBasedConfiguration;
import org.intellij.erlang.runconfig.ErlangRunConfigurationBase;
import org.intellij.erlang.runconfig.ErlangRunner;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangRemoteDebugRunConfiguration extends ErlangRunConfigurationBase<ErlangRemoteDebugRunningState> implements RunConfigurationWithSuppressedDefaultRunAction {
  private String myErlangNode;
  private String myCookie;

  public ErlangRemoteDebugRunConfiguration(Project project, String name) {
    super(name, new ErlangModuleBasedConfiguration(project), ErlangRemoteDebugRunConfigurationType.getInstance().getConfigurationFactories()[0]);
  }

  @Override
  public boolean isTestRunConfiguration() {
    return false;
  }

  @Override
  protected ErlangRemoteDebugRunningState newRunningState(ExecutionEnvironment env, Module module) {
    return new ErlangRemoteDebugRunningState(env, module);
  }

  @NotNull
  @Override
  public SettingsEditor<? extends RunConfiguration> getConfigurationEditor() {
    return new ErlangRemoteDebugConfigurationEditorForm();
  }

  @Nullable
  @Override
  public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment env) throws ExecutionException {
    return ErlangRunner.EMPTY_RUN_STATE;
  }

  public String getErlangNode() {
    return myErlangNode;
  }

  public void setErlangNode(String erlangNode) {
    myErlangNode = erlangNode;
  }

  public String getCookie() {
    return myCookie;
  }

  public void setCookie(String cookie) {
    myCookie = cookie;
  }
}
