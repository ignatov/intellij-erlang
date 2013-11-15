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

package org.intellij.erlang.application;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.ConfigurationType;
import com.intellij.execution.configurations.ModuleBasedConfiguration;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import org.intellij.erlang.runconfig.ErlangModuleBasedConfiguration;
import org.intellij.erlang.runconfig.ErlangRunConfigurationBase;
import org.intellij.erlang.application.ui.ErlangRunConfigurationEditorForm;
import org.intellij.erlang.runconfig.ErlangRunner;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangApplicationConfiguration extends ErlangRunConfigurationBase<ErlangApplicationRunningState> {
  private String myParams = "";
  private String myModuleAndFunction = "";
  private String myErlFlags = "";
  private boolean myStopErlang = true;

  public ErlangApplicationConfiguration(Project project, String name, ConfigurationType configurationType) {
    super(name, new ErlangModuleBasedConfiguration(project), configurationType.getConfigurationFactories()[0]);
  }

  @Override
  protected ModuleBasedConfiguration createInstance() {
    return new ErlangApplicationConfiguration(getProject(), getName(), ErlangApplicationRunConfigurationType.getInstance());
  }

  @NotNull
  @Override
  public SettingsEditor<? extends RunConfiguration> getConfigurationEditor() {
    return new ErlangRunConfigurationEditorForm();
  }

  @Override
  public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment env) throws ExecutionException {
    return ErlangRunner.EMPTY_RUN_STATE; // todo: CommandLineState
  }

  @Override
  protected ErlangApplicationRunningState newRunningState(ExecutionEnvironment env, Module module) {
    return new ErlangApplicationRunningState(env, module, this);
  }

  @Override
  public boolean isTestRunConfiguration() {
    return false;
  }

  public String getParams() {
    return myParams;
  }

  public void setParams(String params) {
    this.myParams = params;
  }

  public String getModuleAndFunction() {
    return myModuleAndFunction;
  }

  public void setModuleAndFunction(String moduleAndFunction) {
    myModuleAndFunction = moduleAndFunction;
  }

  public boolean stopErlang() {
    return myStopErlang;
  }

  public void setStopErlang(boolean stopErlang) {
    myStopErlang = stopErlang;
  }

  public String getErlFlags() {
    return myErlFlags;
  }

  public void setErlFlags(String erlFlags) {
    myErlFlags = erlFlags;
  }
}
