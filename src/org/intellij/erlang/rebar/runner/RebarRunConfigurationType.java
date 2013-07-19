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

package org.intellij.erlang.rebar.runner;

import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.ConfigurationTypeBase;
import com.intellij.openapi.extensions.Extensions;
import org.intellij.erlang.ErlangIcons;
import org.jetbrains.annotations.NotNull;

public final class RebarRunConfigurationType extends ConfigurationTypeBase {
  RebarRunConfigurationType() {
    super("RebarRunConfigurationType", "Erlang Rebar", "Runs a Rebar command", ErlangIcons.REBAR);
  }

  public static RebarRunConfigurationType getInstance() {
    return Extensions.findExtension(CONFIGURATION_TYPE_EP, RebarRunConfigurationType.class);
  }

  @NotNull
  @Override
  public ConfigurationFactory[] getConfigurationFactories() {
    return new ConfigurationFactory[]{RebarRunConfigurationFactory.getInstance()};
  }
}
