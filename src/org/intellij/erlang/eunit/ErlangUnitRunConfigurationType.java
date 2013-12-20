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

import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.ConfigurationTypeBase;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.openapi.project.Project;
import org.intellij.erlang.ErlangIcons;

public class ErlangUnitRunConfigurationType extends ConfigurationTypeBase {
  public static final String PROTOCOL = "eunit";

  protected ErlangUnitRunConfigurationType() {
    super("ErlangUnitRunConfigurationType",
          "Erlang Eunit",
          "Eunit run configuration",
      ErlangIcons.EUNIT);
    addFactory(new ErlangUnitConfigurationFactory(this));
  }

  public static ErlangUnitRunConfigurationType getInstance() {
    return Extensions.findExtension(CONFIGURATION_TYPE_EP, ErlangUnitRunConfigurationType.class);
  }

  public static class ErlangUnitConfigurationFactory extends ConfigurationFactory {
    protected ErlangUnitConfigurationFactory(ErlangUnitRunConfigurationType type) {
      super(type);
    }

    @Override
    public RunConfiguration createTemplateConfiguration(Project project) {
      return new ErlangUnitRunConfiguration(project, "Erlang", getInstance());
    }
  }
}
