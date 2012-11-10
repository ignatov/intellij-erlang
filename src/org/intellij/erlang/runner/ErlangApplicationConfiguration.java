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

package org.intellij.erlang.runner;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.ConfigurationType;
import com.intellij.execution.configurations.ModuleBasedConfiguration;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.RunConfigurationWithSuppressedDefaultRunAction;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.InvalidDataException;
import com.intellij.openapi.util.WriteExternalException;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.runner.ui.ErlangRunConfigurationEditorForm;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.Collection;

/**
 * @author ignatov
 */
public class ErlangApplicationConfiguration extends ModuleBasedConfiguration<ErlangApplicationModuleBasedConfiguration>
  implements RunConfigurationWithSuppressedDefaultRunAction {
  private String myParams = "";
  private String myModuleAndFunction = "";

  public ErlangApplicationConfiguration(Project project, String name, ConfigurationType configurationType) {
    super(name, new ErlangApplicationModuleBasedConfiguration(project), configurationType.getConfigurationFactories()[0]);
  }

  @Override
  public Collection<Module> getValidModules() {
    Module[] modules = ModuleManager.getInstance(getProject()).getModules();
    return Arrays.asList(modules);
  }

  @Override
  protected ModuleBasedConfiguration createInstance() {
    return new ErlangApplicationConfiguration(getProject(), getName(), ErlangRunConfigurationType.getInstance());
  }

  public SettingsEditor<? extends RunConfiguration> getConfigurationEditor() {
    return new ErlangRunConfigurationEditorForm();
  }

  public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment env) throws ExecutionException {
    return ErlangRunner.EMPTY_RUN_STATE; // todo: CommandLineState
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

  public void writeExternal(final Element element) throws WriteExternalException {
    super.writeExternal(element);
    writeModule(element);
    XmlSerializer.serializeInto(this, element);
  }

  public void readExternal(final Element element) throws InvalidDataException {
    super.readExternal(element);
    readModule(element);
    XmlSerializer.deserializeInto(this, element);
  }
}
