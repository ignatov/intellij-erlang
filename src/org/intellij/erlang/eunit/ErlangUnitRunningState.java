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
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.console.ErlangConsoleUtil;
import org.intellij.erlang.runconfig.ErlangRunningState;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.util.*;

/**
 * @author ignatov
 */
public class ErlangUnitRunningState extends ErlangRunningState {
  private static int DEBUG_TEST_TIMEOUT = Integer.MAX_VALUE;
  private ErlangUnitRunConfiguration myConfiguration;

  public ErlangUnitRunningState(ExecutionEnvironment env, Module module, ErlangUnitRunConfiguration configuration) {
    super(env, module);
    myConfiguration = configuration;
  }

  @Override
  public List<String> getCodePath() throws ExecutionException {
    try {
      File reporterDir = createReporterModuleDirectory();
      List<String> reporterModuleCodePath = Arrays.asList("-pa", reporterDir.getPath());
      return ContainerUtil.concat(reporterModuleCodePath, super.getCodePath());
    } catch (IOException e) {
      throw new ExecutionException("Failed to setup eunit reports environment", e);
    }
  }

  @Override
  protected boolean useTestCodePath() {
    return true;
  }

  @Override
  protected boolean isNoShellMode() {
    return true;
  }

  @Override
  protected boolean isStopErlang() {
    return true;
  }

  @Override
  public ErlangEntryPoint getEntryPoint() throws ExecutionException {
    return getEntryPointInternal(false);
  }

  @Override
  public ErlangEntryPoint getDebugEntryPoint() throws ExecutionException {
    return getEntryPointInternal(true);
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

    ErlangUnitRerunFailedTestsAction rerunAction = new ErlangUnitRerunFailedTestsAction(consoleView);
    rerunAction.init(((BaseTestsOutputConsoleView) consoleView).getProperties(), getEnvironment());
    rerunAction.setModelProvider(new Getter<TestFrameworkRunningModel>() {
      @Override
      public TestFrameworkRunningModel get() {
        return ((SMTRunnerConsoleView) consoleView).getResultsViewer();
      }
    });

    executionResult.setRestartActions(rerunAction, new ToggleAutoTestAction());
    return executionResult;
  }

  @Override
  @NotNull
  public ConsoleView createConsoleView(Executor executor) throws ExecutionException {
    final ErlangUnitRunConfiguration runConfiguration = (ErlangUnitRunConfiguration) getRunnerSettings().getRunProfile();
    return SMTestRunnerConnectionUtil.createConsoleWithCustomLocator(
      "Erlang",
      new ErlangUnitConsoleProperties(runConfiguration, executor),
      getRunnerSettings(),
      getConfigurationSettings(),
      new ErlangTestLocationProvider()
    );
  }

  private ErlangEntryPoint getEntryPointInternal(boolean debug) throws ExecutionException {
    List<String> args = Arrays.asList("[" + getTestObjectsString(debug) + "]", "[{report, {" + ErlangEunitReporterModule.MODULE_NAME + ",[]}}, {no_tty, true}]");
    return new ErlangEntryPoint("eunit", "test", args);
  }

  @NotNull
  private String getTestObjectsString(boolean debug) throws ExecutionException {
    ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind kind = myConfiguration.getConfigData().getKind();
    String tests;
    if (kind == ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE) {
      tests = StringUtil.join(myConfiguration.getConfigData().getModuleNames(), ", ");
    }
    else if (kind == ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION) {
      StringBuilder result = new StringBuilder();
      Map<String, List<String>> modules = groupByModule(myConfiguration.getConfigData().getFunctionNames());
      for (Map.Entry<String, List<String>> e : modules.entrySet()) {
        String moduleName = e.getKey();

        result.append("{\"module \'").append(moduleName).append("\'\", [");
        for (String function : e.getValue()) {
          result.append("fun ").append(moduleName).append(':').append(function).append("/0, ");
        }
        result.setLength(result.length() - 2);
        result.append("]}, ");
      }
      if (result.length() != 0) {
        result.setLength(result.length() - 2);
      }
      tests = result.toString();
    }
    else {
      throw new ExecutionException("Unknown run config kind");
    }
    return debug ? "{timeout, " + DEBUG_TEST_TIMEOUT + ", [" + tests + "]}" : tests;
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

  private static File createReporterModuleDirectory() throws IOException {
    File tempDirectory = FileUtil.createTempDirectory(ErlangEunitReporterModule.MODULE_NAME, null);
    ErlangEunitReporterModule.putReporterModuleTo(tempDirectory);
    return tempDirectory;
  }
}