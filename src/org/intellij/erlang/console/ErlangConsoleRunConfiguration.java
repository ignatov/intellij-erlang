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

package org.intellij.erlang.console;

import com.intellij.execution.Executor;
import com.intellij.execution.configurations.*;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.InvalidDataException;
import com.intellij.openapi.util.WriteExternalException;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.Collection;
import java.util.Objects;

public final class ErlangConsoleRunConfiguration extends ModuleBasedConfiguration<RunConfigurationModule, Element> {
  @NotNull private String myWorkingDirPath;
  private String myConsoleArgs;

  public ErlangConsoleRunConfiguration(@NotNull String name, @NotNull Project project) {
    super(name, new RunConfigurationModule(project), ErlangConsoleRunConfigurationFactory.getInstance());
    myWorkingDirPath = Objects.requireNonNull(project.getBasePath());
    myConsoleArgs = "";
  }

  @NotNull
  @Override
  public SettingsEditor<? extends RunConfiguration> getConfigurationEditor() {
    return new ErlangConsoleRunConfigurationForm(getProject(), getConfigurationModule().getModule());
  }

  @NotNull
  public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment environment) {
    return new ErlangConsoleCommandLineState(this, environment);
  }

  @Override
  public Collection<Module> getValidModules() {
    return Arrays.asList(ModuleManager.getInstance(getProject()).getModules());
  }

  @Override
  public void writeExternal(@NotNull Element element) throws WriteExternalException {
    super.writeExternal(element);
    XmlSerializer.serializeInto(this, element);
  }

  @Override
  public void checkConfiguration() throws RuntimeConfigurationException {
    Module selectedModule = getConfigurationModule().getModule();
    if (selectedModule == null) {
      Sdk projectSdk = ProjectRootManager.getInstance(getProject()).getProjectSdk();
      if (projectSdk == null || projectSdk.getSdkType() != ErlangSdkType.getInstance()) {
        throw new RuntimeConfigurationException("Neither Erlang module selected nor Erlang SDK is configured for the project");
      }
    }
    else {
      Sdk moduleSdk = ModuleRootManager.getInstance(selectedModule).getSdk();
      if (moduleSdk == null || moduleSdk.getSdkType() != ErlangSdkType.getInstance()) {
        throw new RuntimeConfigurationException("Erlang SDK is not configured for the selected module");
      }
    }
  }

  @Override
  public void readExternal(@NotNull Element element) throws InvalidDataException {
    super.readExternal(element);
    XmlSerializer.deserializeInto(this, element);
  }

  public void setWorkingDirPath(@NotNull String workingDirPath) {
    myWorkingDirPath = workingDirPath;
  }

  @NotNull
  public String getWorkingDirPath() {
    return myWorkingDirPath;
  }

  public void setConsoleArgs(@NotNull String consoleArgs) {
    myConsoleArgs = consoleArgs;
  }

  @NotNull
  public String getConsoleArgs() {
    return myConsoleArgs;
  }
}
