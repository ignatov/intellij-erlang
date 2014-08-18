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

package org.intellij.erlang.rebar.runner;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.*;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.RunConfigurationWithSuppressedDefaultRunAction;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.InvalidDataException;
import com.intellij.openapi.util.WriteExternalException;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.runconfig.ErlangModuleBasedConfiguration;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.Collection;

public abstract class RebarRunConfigurationBase extends ModuleBasedConfiguration<ErlangModuleBasedConfiguration> implements RunConfigurationWithSuppressedDefaultRunAction, RunConfigurationWithSuppressedDefaultDebugAction {
  @NotNull
  private String myCommand = "";
  private boolean mySkipDependencies = false;

  protected RebarRunConfigurationBase(@NotNull String name, @NotNull Project project, @NotNull ConfigurationFactory configurationFactory) {
    super(name, new ErlangModuleBasedConfiguration(project), configurationFactory);
  }

  @Override
  public Collection<Module> getValidModules() {
    Module[] modules = ModuleManager.getInstance(getProject()).getModules();
    return Arrays.asList(modules);
  }

  @NotNull
  @Override
  public SettingsEditor<? extends RunConfiguration> getConfigurationEditor() {
    return new RebarRunConfigurationEditorForm();
  }

  @NotNull
  public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment env) throws ExecutionException {
    return new RebarRunningState(env, this);
  }

  @Override
  public void checkConfiguration() throws RuntimeConfigurationException {
    // TODO parse rebar command line to check if it is valid
  }

  public void writeExternal(@NotNull Element element) throws WriteExternalException {
    super.writeExternal(element);
    writeModule(element);
    XmlSerializer.serializeInto(this, element);
  }

  public void readExternal(@NotNull Element element) throws InvalidDataException {
    super.readExternal(element);
    readModule(element);
    XmlSerializer.deserializeInto(this, element);
  }

  @NotNull
  public String getCommand() {
    return myCommand;
  }

  public void setCommand(@NotNull String command) {
    myCommand = command;
  }

  public boolean isSkipDependencies() {
    return mySkipDependencies;
  }

  public void setSkipDependencies(boolean skipDeps) {
    mySkipDependencies = skipDeps;
  }
}