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

package org.intellij.erlang.eunit;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.ModuleBasedConfiguration;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.InvalidDataException;
import com.intellij.openapi.util.WriteExternalException;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.eunit.ui.ErlangUnitRunConfigurationEditorForm;
import org.intellij.erlang.runconfig.ErlangModuleBasedConfiguration;
import org.intellij.erlang.runconfig.ErlangRunConfigurationBase;
import org.intellij.erlang.runconfig.ErlangRunner;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * @author ignatov
 */
public class ErlangUnitRunConfiguration extends ErlangRunConfigurationBase<ErlangUnitRunningState> {
  private final ErlangUnitConfigData myConfigData = new ErlangUnitConfigData();

  public ErlangUnitRunConfiguration(Project project, String name, ErlangUnitRunConfigurationType configurationType) {
    super(name, new ErlangModuleBasedConfiguration(project), configurationType.getConfigurationFactories()[0]);
  }

  @Override
  protected ModuleBasedConfiguration createInstance() {
    return new ErlangUnitRunConfiguration(getProject(), getName(), ErlangUnitRunConfigurationType.getInstance());
  }

  @Override
  public SettingsEditor<? extends RunConfiguration> getConfigurationEditor() {
    return new ErlangUnitRunConfigurationEditorForm();
  }

  @Override
  public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment env) throws ExecutionException {
    return ErlangRunner.EMPTY_RUN_STATE; // todo: CommandLineState
  }

  @Override
  protected ErlangUnitRunningState newRunningState(ExecutionEnvironment env, Module module) {
    return new ErlangUnitRunningState(env, module, this);
  }

  public ErlangUnitConfigData getConfigData() {
    return myConfigData;
  }

  @Override
  public void writeExternal(Element element) throws WriteExternalException {
    super.writeExternal(element);
    XmlSerializer.serializeInto(myConfigData, element);
  }

  @Override
  public void readExternal(Element element) throws InvalidDataException {
    super.readExternal(element);
    XmlSerializer.deserializeInto(myConfigData, element);
  }

  public enum ErlangUnitRunConfigurationKind {
    FUNCTION, MODULE
  }

  public static final class ErlangUnitConfigData {
    private ErlangUnitRunConfigurationKind myKind = ErlangUnitRunConfigurationKind.MODULE;
    private Set<String> myModuleNames = new LinkedHashSet<String>();
    private Set<String> myFunctionNames = new LinkedHashSet<String>();

    public ErlangUnitRunConfigurationKind getKind() {
      return myKind;
    }

    public void setKind(ErlangUnitRunConfigurationKind kind) {
      myKind = kind;
    }

    public Set<String> getModuleNames() {
      return myModuleNames;
    }

    public void setModuleNames(Set<String> moduleNames) {
      myModuleNames = moduleNames;
    }

    public Set<String> getFunctionNames() {
      return myFunctionNames;
    }

    public void setFunctionNames(Set<String> functionNames) {
      myFunctionNames = functionNames;
    }
  }
}
