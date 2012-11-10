package org.intellij.erlang.eunit;

import com.intellij.execution.DefaultExecutionResult;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.ExecutionResult;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ProgramRunner;
import com.intellij.execution.testframework.autotest.ToggleAutoTestAction;
import com.intellij.execution.testframework.sm.SMTestRunnerConnectionUtil;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.openapi.module.Module;
import org.intellij.erlang.runner.ErlangApplicationConfiguration;
import org.intellij.erlang.runner.ErlangRunningState;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangUnitRunningState extends ErlangRunningState {
  public ErlangUnitRunningState(ExecutionEnvironment env, Module module, ErlangApplicationConfiguration configuration) {
    super(env, module, configuration);
  }

  @Override
  protected void setUpParameters(GeneralCommandLine commandLine) {
    commandLine.addParameter("-run");
    commandLine.addParameter("-eval");
    commandLine.addParameter("eunit:test([" + myConfiguration.getModuleAndFunction() + "], [verbose]).");
    commandLine.addParameters("-s", "init", "stop", "-noshell");
  }

  @Override
  @NotNull
  public ExecutionResult execute(@NotNull Executor executor, @NotNull ProgramRunner runner) throws ExecutionException {
    ProcessHandler processHandler = startProcess();
    setConsoleBuilder(getConsoleBuilder());

    ConsoleView consoleView = createConsole1(executor);
    if (consoleView != null) {
      consoleView.attachToProcess(processHandler);
    }

    DefaultExecutionResult executionResult = new DefaultExecutionResult(consoleView, processHandler);
    executionResult.setRestartActions(new ToggleAutoTestAction());
    return executionResult;
  }

  private ConsoleView createConsole1(Executor executor) throws ExecutionException {
    final ErlangUnitRunConfiguration runConfiguration = (ErlangUnitRunConfiguration) getRunnerSettings().getRunProfile();
    return SMTestRunnerConnectionUtil.createConsoleWithCustomLocator(
      "Erlang",
      new ErlangUnitConsoleProperties(runConfiguration, executor),
      getRunnerSettings(),
      getConfigurationSettings(),
      new ErlangTestLocationProvider()
    );
  }
}
