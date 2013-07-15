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

import com.intellij.execution.DefaultExecutionResult;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.ExecutionResult;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ProgramRunner;
import com.intellij.execution.testframework.TestFrameworkRunningModel;
import com.intellij.execution.testframework.autotest.ToggleAutoTestAction;
import com.intellij.execution.testframework.sm.SMTestRunnerConnectionUtil;
import com.intellij.execution.testframework.sm.runner.ui.SMTRunnerConsoleView;
import com.intellij.execution.testframework.ui.BaseTestsOutputConsoleView;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.util.Getter;
import com.intellij.openapi.util.text.StringUtil;
import org.intellij.erlang.console.ErlangConsoleUtil;
import org.intellij.erlang.runconfig.ErlangRunningState;
import org.jetbrains.annotations.NotNull;

import java.util.*;

/**
 * @author ignatov
 */
public class ErlangUnitRunningState extends ErlangRunningState {
  private ErlangUnitRunConfiguration myConfiguration;

  public ErlangUnitRunningState(ExecutionEnvironment env, Module module, ErlangUnitRunConfiguration configuration) {
    super(env, module);
    myConfiguration = configuration;
  }

  @Override
  protected void setUpCommandLineParameters(GeneralCommandLine commandLine) {
    commandLine.addParameter("-run");
    commandLine.addParameter("-eval");
    commandLine.addParameter("eunit:test([" + getTestObjectsString() + "], [verbose]).");
    commandLine.addParameters("-s", "init", "stop", "-noshell");
  }

  @Override
  @NotNull
  public ExecutionResult execute(@NotNull Executor executor, @NotNull ProgramRunner runner) throws ExecutionException {
    ProcessHandler processHandler = startProcess();
    setConsoleBuilder(getConsoleBuilder());

    final ConsoleView consoleView = createConsoleView(executor);
    ErlangConsoleUtil.attachFilters(myConfiguration.getProject(), consoleView);
    consoleView.attachToProcess(processHandler);

    DefaultExecutionResult executionResult = new DefaultExecutionResult(consoleView, processHandler);

    ErlangUnitRerunFailedTestsAction rerunFailedAction = new ErlangUnitRerunFailedTestsAction(consoleView);
    rerunFailedAction.init(((BaseTestsOutputConsoleView)consoleView).getProperties(), getEnvironment());
    rerunFailedAction.setModelProvider(new Getter<TestFrameworkRunningModel>() {
      @Override
      public TestFrameworkRunningModel get() {
        return ((SMTRunnerConsoleView)consoleView).getResultsViewer();
      }
    });

    executionResult.setRestartActions(rerunFailedAction, new ToggleAutoTestAction());
    return executionResult;
  }

  @NotNull
  private ConsoleView createConsoleView(Executor executor) throws ExecutionException {
    final ErlangUnitRunConfiguration runConfiguration = (ErlangUnitRunConfiguration) getRunnerSettings().getRunProfile();
    return SMTestRunnerConnectionUtil.createConsoleWithCustomLocator(
      "Erlang",
      new ErlangUnitConsoleProperties(runConfiguration, executor, true),
      getRunnerSettings(),
      getConfigurationSettings(),
      new ErlangTestLocationProvider()
    );
  }

  private String getTestObjectsString() {
    ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind kind = myConfiguration.getConfigData().getKind();

    if (kind == ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE) {
      return StringUtil.join(myConfiguration.getConfigData().getModuleNames(), ", ");
    }

    if (kind == ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION) {
      StringBuilder result = new StringBuilder();

      Map<String, List<String>> modules = groupByModule(myConfiguration.getConfigData().getFunctionNames());

      for (Map.Entry<String, List<String>> e : modules.entrySet()) {
        String moduleName = e.getKey();

        result.append("{\"module \'");
        result.append(moduleName);
        result.append("\'\", [");
        for (String function : e.getValue()) {
          result.append("fun ");
          result.append(moduleName);
          result.append(':');
          result.append(function);
          result.append("/0, ");
        }
        result.setLength(result.length() - 2);
        result.append("]}, ");
      }
      if (result.length() != 0) {
        result.setLength(result.length() - 2);
      }

      return result.toString();
    }

    return "UNKNOWN RUN CONFIG KIND";
  }

  private static Map<String, List<String>> groupByModule(Collection<String> qualifiedFunctionNames) {
    Map<String, List<String>> result = new HashMap<String, List<String>>(qualifiedFunctionNames.size());
    for (String qualifiedFunctionName : qualifiedFunctionNames) {
      String[] moduleAndFunction = qualifiedFunctionName.split(":");
      String module = moduleAndFunction[0];
      String function = moduleAndFunction[1];

      if (moduleAndFunction.length != 2) continue;

      List<String> functions = result.get(module);
      if (functions == null) {
        functions = new ArrayList<String>();
        result.put(module, functions);
      }
      functions.add(function);
    }
    return result;
  }
}
