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

package org.intellij.erlang.console;

import com.intellij.compiler.options.CompileStepBeforeRun;
import com.intellij.execution.BeforeRunTask;
import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import org.jetbrains.annotations.NotNull;

final class ErlangConsoleRunConfigurationFactory extends ConfigurationFactory {
  private static final ErlangConsoleRunConfigurationFactory ourInstance = new ErlangConsoleRunConfigurationFactory();

  private ErlangConsoleRunConfigurationFactory() {
    super(ErlangConsoleRunConfigurationType.getInstance());
  }

  @Override
  public void configureBeforeRunTaskDefaults(@NotNull Key<? extends BeforeRunTask> providerID,
                                             @NotNull BeforeRunTask task) {
    if (providerID == CompileStepBeforeRun.ID) {
      task.setEnabled(false);
    }
  }

  @NotNull
  public static ErlangConsoleRunConfigurationFactory getInstance() {
    return ourInstance;
  }

  @Override
  public RunConfiguration createTemplateConfiguration(@NotNull Project project) {
    return new ErlangConsoleRunConfiguration("Erlang Shell", project);
  }
}
