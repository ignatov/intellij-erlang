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

package org.intellij.erlang.runconfig;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.ModuleBasedConfiguration;
import com.intellij.execution.configurations.RuntimeConfigurationException;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.RunConfigurationWithSuppressedDefaultRunAction;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.util.InvalidDataException;
import com.intellij.openapi.util.WriteExternalException;
import com.intellij.util.xmlb.XmlSerializer;
import org.jdom.Element;

import java.util.Arrays;
import java.util.Collection;

public abstract class ErlangRunConfigurationBase<RunningState extends ErlangRunningState> extends ModuleBasedConfiguration<ErlangModuleBasedConfiguration>
  implements RunConfigurationWithSuppressedDefaultRunAction {
  public ErlangRunConfigurationBase(String name, ErlangModuleBasedConfiguration configurationModule, ConfigurationFactory factory) {
    super(name, configurationModule, factory);
  }

  @Override
  public Collection<Module> getValidModules() {
    Module[] modules = ModuleManager.getInstance(getProject()).getModules();
    return Arrays.asList(modules);
  }

  @Override
  public void writeExternal(final Element element) throws WriteExternalException {
    super.writeExternal(element);
    writeModule(element);
    XmlSerializer.serializeInto(this, element);
  }

  @Override
  public void checkConfiguration() throws RuntimeConfigurationException {
    ErlangModuleBasedConfiguration configurationModule = getConfigurationModule();
    configurationModule.checkForWarning();
  }

  @Override
  public void readExternal(final Element element) throws InvalidDataException {
    super.readExternal(element);
    readModule(element);
    XmlSerializer.deserializeInto(this, element);
  }

  public final RunningState createRunningState(ExecutionEnvironment env) throws ExecutionException {
    ErlangModuleBasedConfiguration configuration = getConfigurationModule();
    Module module = configuration.getModule();
    if (module == null) {
      throw new ExecutionException("No Erlang configuration for run configuration: " + getName());
    }
    return newRunningState(env, module);
  }

  public abstract boolean isTestRunConfiguration();

  protected abstract RunningState newRunningState(ExecutionEnvironment env, Module module);
}
