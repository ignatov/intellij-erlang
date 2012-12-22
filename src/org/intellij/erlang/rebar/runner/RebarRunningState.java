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
import com.intellij.execution.configurations.CommandLineState;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.filters.TextConsoleBuilder;
import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.jetbrains.annotations.NotNull;

final class RebarRunningState extends CommandLineState {
  private final RebarRunConfiguration myConfig;

  public RebarRunningState(@NotNull final ExecutionEnvironment env, @NotNull final RebarRunConfiguration config) {
    super(env);
    myConfig = config;
    TextConsoleBuilder builder = TextConsoleBuilderFactory.getInstance().createBuilder(myConfig.getProject());
    builder.addFilter(new FileReferenceFilter(myConfig.getProject(),
      FileReferenceFilter.PATH_MACROS + ":" + FileReferenceFilter.LINE_MACROS));
    builder.addFilter(new FileReferenceFilter(myConfig.getProject(),
      "\\(" + FileReferenceFilter.PATH_MACROS + ", line " + FileReferenceFilter.LINE_MACROS + "\\)"));
    builder.addFilter(new FileReferenceFilter(myConfig.getProject(),
      "\\[\\{file,\"" + FileReferenceFilter.PATH_MACROS + "\"\\},\\{line," + FileReferenceFilter.LINE_MACROS + "\\}\\]"));
    setConsoleBuilder(builder);
  }

  @NotNull
  @Override
  protected ProcessHandler startProcess() throws ExecutionException {
    final GeneralCommandLine commandLine = new GeneralCommandLine();
    final RebarSettings rebarSettings = RebarSettings.getInstance(myConfig.getProject());
    commandLine.setWorkDirectory(myConfig.getProject().getBasePath());
    commandLine.setExePath(rebarSettings.getRebarPath());
    commandLine.addParameters(myConfig.getCommand().split("\\s+"));
    return new OSProcessHandler(commandLine.createProcess(), commandLine.getCommandLineString());
  }
}
