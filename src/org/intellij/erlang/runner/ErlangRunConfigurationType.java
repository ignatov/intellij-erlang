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

import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.ConfigurationType;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.openapi.project.Project;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangIcons;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class ErlangRunConfigurationType implements ConfigurationType {
  public static final String ERLANG_APPLICATION = "Erlang application";
  private final ErlangFactory configurationFactory;

  public ErlangRunConfigurationType() {
    configurationFactory = new ErlangFactory(this);
  }

  public static ErlangRunConfigurationType getInstance() {
    return ContainerUtil.findInstance(Extensions.getExtensions(CONFIGURATION_TYPE_EP), ErlangRunConfigurationType.class);
  }

  public String getDisplayName() {
    return ERLANG_APPLICATION;
  }

  public String getConfigurationTypeDescription() {
    return ERLANG_APPLICATION;
  }

  public Icon getIcon() {
    return ErlangIcons.FILE;
  }

  @NotNull
  public String getId() {
    return "ErlangApplicationRunConfiguration";
  }

  public ConfigurationFactory[] getConfigurationFactories() {
    return new ConfigurationFactory[]{configurationFactory};
  }

  public static class ErlangFactory extends ConfigurationFactory {

    public ErlangFactory(ConfigurationType type) {
      super(type);
    }

    public RunConfiguration createTemplateConfiguration(Project project) {
      final String name = ERLANG_APPLICATION;
      return new ErlangApplicationConfiguration(name, project, getInstance());
    }
  }
}
