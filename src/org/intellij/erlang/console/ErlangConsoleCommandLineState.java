/*
 * Copyright 2012-2014 Sergey Ignatov
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

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.CommandLineState;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.filters.TextConsoleBuilder;
import com.intellij.execution.filters.TextConsoleBuilderImpl;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.process.ProcessTerminatedListener;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.EnvironmentUtil;
import com.intellij.util.execution.ParametersListUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.VisibleForTesting;

import java.nio.charset.StandardCharsets;
import java.util.Map;

public class ErlangConsoleCommandLineState extends CommandLineState {
  @NotNull private final ErlangConsoleRunConfiguration myConfig;

  public ErlangConsoleCommandLineState(@NotNull ErlangConsoleRunConfiguration config,
                                       @NotNull ExecutionEnvironment env) {
    super(env);
    myConfig = config;
    TextConsoleBuilder consoleBuilder = new TextConsoleBuilderImpl(myConfig.getProject()) {
      @Override
      public @NotNull ConsoleView getConsole() {
        ErlangConsoleView consoleView = new ErlangConsoleView(myConfig.getProject());
        ErlangConsoleUtil.attachFilters(myConfig.getProject(), consoleView);
        return consoleView;
      }
    };
    setConsoleBuilder(consoleBuilder);
  }

  @NotNull
  @Override
  protected ProcessHandler startProcess() throws ExecutionException {
    GeneralCommandLine commandLine = createCommandLine(myConfig);
    OSProcessHandler handler = new OSProcessHandler(commandLine.createProcess(), commandLine.getCommandLineString(),
                                                  commandLine.getCharset());
    ProcessTerminatedListener.attach(handler);
    return handler;
  }

  @VisibleForTesting
  @NotNull
  static GeneralCommandLine createCommandLine(@NotNull ErlangConsoleRunConfiguration config) throws ExecutionException {
    Project project = config.getProject();
    Module module = config.getConfigurationModule().getModule();
    GeneralCommandLine commandLine = new GeneralCommandLine();
    commandLine.setExePath(ErlangConsoleUtil.getErlPath(project, module));
    String consoleArgs = config.getConsoleArgs();
    commandLine.addParameters(ParametersListUtil.parse(consoleArgs));
    commandLine.addParameters(ErlangConsoleUtil.getCodePath(project, module, false));
    commandLine.setWorkDirectory(ErlangConsoleUtil.getWorkingDirPath(project, config.getWorkingDirPath()));
    commandLine.setCharset(StandardCharsets.UTF_8);
    setUtf8Locale(commandLine);
    return commandLine;
  }

  /**
   * erl reads a piped standard input as latin1 unless the locale of its character type is UTF-8,
   * and the console writes the input as UTF-8 (#692).
   */
  @VisibleForTesting
  static void setUtf8Locale(@NotNull GeneralCommandLine commandLine) {
    Map<String, String> inherited = commandLine.getEffectiveEnvironment();
    if (isUtf8Locale(getLocaleCharacterType(inherited))) return;
    Map<String, String> environment = commandLine.getEnvironment();
    String locale = EnvironmentUtil.setLocaleEnv(environment, StandardCharsets.UTF_8);
    // LC_ALL overrides LC_CTYPE.
    if (StringUtil.isNotEmpty(inherited.get("LC_ALL"))) environment.put("LC_ALL", locale);
  }

  @VisibleForTesting
  @Nullable
  static String getLocaleCharacterType(@NotNull Map<String, String> environment) {
    for (String name : new String[]{"LC_ALL", "LC_CTYPE", "LANG"}) {
      String value = environment.get(name);
      if (StringUtil.isNotEmpty(value)) return value;
    }
    return null;
  }

  @VisibleForTesting
  static boolean isUtf8Locale(@Nullable String locale) {
    return locale != null && StringUtil.toUpperCase(locale).replace("-", "").contains("UTF8");
  }
}
