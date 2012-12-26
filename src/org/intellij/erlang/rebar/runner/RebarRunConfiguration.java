/*
 * Copyright 2012 Sergey Ignatov
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
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.configurations.RuntimeConfiguration;
import com.intellij.execution.configurations.RuntimeConfigurationException;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.RunConfigurationWithSuppressedDefaultRunAction;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.InvalidDataException;
import com.intellij.openapi.util.WriteExternalException;
import com.intellij.util.xmlb.XmlSerializer;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;

public final class RebarRunConfiguration extends RuntimeConfiguration implements RunConfigurationWithSuppressedDefaultRunAction {
  @NotNull private String myCommand = "";
  private boolean myUseTestConsole = false;
  private boolean mySkipDependencies = false;

  public RebarRunConfiguration(@NotNull String name, @NotNull Project project) {
    super(name, project, RebarRunConfigurationFactory.getInstance());
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

  public void writeExternal(@NotNull final Element element) throws WriteExternalException {
    super.writeExternal(element);
    XmlSerializer.serializeInto(this, element);
  }

  public void readExternal(@NotNull final Element element) throws InvalidDataException {
    super.readExternal(element);
    XmlSerializer.deserializeInto(this, element);
  }

  @NotNull
  public String getCommand() {
    return myCommand;
  }

  public void setCommand(@NotNull String command) {
    myCommand = command;
  }

  public boolean isUseTestConsole() {
    return myUseTestConsole;
  }

  public void setUseTestConsole(boolean useTestConsole) {
    myUseTestConsole = useTestConsole;
  }

  public boolean isSkipDependencies() {
    return mySkipDependencies;
  }

  public void setSkipDependencies(boolean skipDeps) {
    mySkipDependencies = skipDeps;
  }
}
