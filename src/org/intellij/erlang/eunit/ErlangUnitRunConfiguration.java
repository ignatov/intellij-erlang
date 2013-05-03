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

import com.intellij.execution.configurations.ModuleBasedConfiguration;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import org.intellij.erlang.eunit.ui.ErlangUnitRunConfigurationEditorForm;
import org.intellij.erlang.runner.ErlangApplicationConfiguration;

/**
 * @author ignatov
 */
public class ErlangUnitRunConfiguration extends ErlangApplicationConfiguration {
  public ErlangUnitRunConfiguration(Project project, String name, ErlangUnitRunConfigurationType configurationType) {
    super(project, name, configurationType);
  }

  @Override
  public SettingsEditor<? extends RunConfiguration> getConfigurationEditor() {
    return new ErlangUnitRunConfigurationEditorForm();
  }

  @Override
  protected ModuleBasedConfiguration createInstance() {
    return new ErlangUnitRunConfiguration(getProject(), getName(), ErlangUnitRunConfigurationType.getInstance());
  }

  @Override
  public boolean isGeneratedName() {
    return "Unnamed".equals(getName());
  }

  @Override
  public String suggestedName() {
    return "Unnamed";
  }
}
