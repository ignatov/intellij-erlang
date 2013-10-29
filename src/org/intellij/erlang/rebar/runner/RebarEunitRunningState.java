package org.intellij.erlang.rebar.runner;

import com.intellij.execution.DefaultExecutionResult;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.ExecutionResult;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.CommandLineState;
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
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Getter;
import com.intellij.openapi.util.io.FileUtil;
import org.intellij.erlang.console.ErlangConsoleUtil;
import org.intellij.erlang.console.FileReferenceFilter;
import org.intellij.erlang.eunit.ErlangEunitReporterModule;
import org.intellij.erlang.eunit.ErlangTestLocationProvider;
import org.intellij.erlang.eunit.ErlangUnitConsoleProperties;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;

/**
 * @author savenko
 */
public class RebarEunitRunningState extends CommandLineState {
  private static final String CONFIG_FILE_NAME = "rebar.config";
  private static final String REBAR_CONFIG = "\n{eunit_opts,[{report,{" + ErlangEunitReporterModule.MODULE_NAME + ", []}}, {no_tty, true}]}.";

  private final RebarEunitRunConfiguration myConfiguration;

  public RebarEunitRunningState(@NotNull final ExecutionEnvironment env, @NotNull RebarEunitRunConfiguration configuration) {
    super(env);
    myConfiguration = configuration;
  }

  @NotNull
  @Override
  public ExecutionResult execute(@NotNull Executor executor, @NotNull ProgramRunner runner) throws ExecutionException {
    ProcessHandler processHandler = startProcess();
    setConsoleBuilder(getConsoleBuilder());

    final ConsoleView consoleView = createConsoleView(executor);
    ErlangConsoleUtil.attachFilters(myConfiguration.getProject(), consoleView);
    consoleView.attachToProcess(processHandler);

    RebarEunitRerunFailedTestsAction rerunAction = new RebarEunitRerunFailedTestsAction(consoleView);
    rerunAction.init(((BaseTestsOutputConsoleView) consoleView).getProperties(), getEnvironment());
    rerunAction.setModelProvider(new Getter<TestFrameworkRunningModel>() {
      @Override
      public TestFrameworkRunningModel get() {
        return ((SMTRunnerConsoleView) consoleView).getResultsViewer();
      }
    });

    DefaultExecutionResult executionResult = new DefaultExecutionResult(consoleView, processHandler);
    executionResult.setRestartActions(rerunAction, new ToggleAutoTestAction(getEnvironment()));
    return executionResult;
  }

  @NotNull
  private ConsoleView createConsoleView(Executor executor) throws ExecutionException {
    ErlangUnitConsoleProperties consoleProperties = new ErlangUnitConsoleProperties(myConfiguration, executor);
    consoleProperties.addStackTraceFilter(new FileReferenceFilter(myConfiguration.getProject(), ErlangConsoleUtil.COMPILATION_ERROR_PATH));
    consoleProperties.addStackTraceFilter(new FileReferenceFilter(myConfiguration.getProject(), ErlangConsoleUtil.EUNIT_ERROR_PATH));
    consoleProperties.addStackTraceFilter(new FileReferenceFilter(myConfiguration.getProject(), ErlangConsoleUtil.EUNIT_FAILURE_PATH));
    return SMTestRunnerConnectionUtil.createConsoleWithCustomLocator("Rebar", consoleProperties, getEnvironment(), new ErlangTestLocationProvider());
  }

  @NotNull
  @Override
  protected ProcessHandler startProcess() throws ExecutionException {
    GeneralCommandLine commandLine = RebarRunningStateUtil.getRebarCommandLine(myConfiguration);
    File reportsRoot = createEunitReportsEnvironment();

    addEnvParams(commandLine, reportsRoot);
    addConfigFileArgument(commandLine, reportsRoot);

    return RebarRunningStateUtil.runRebar(myConfiguration.getProject(), commandLine);
  }

  private static void addConfigFileArgument(GeneralCommandLine commandLine, File reportsRoot) {
    commandLine.addParameters("-C", new File(reportsRoot, CONFIG_FILE_NAME).getPath());
  }

  private static void addEnvParams(GeneralCommandLine commandLine, File reportsRoot) {
    commandLine.getEnvironment().put("ERL_FLAGS", "-pa " + reportsRoot.getPath());
  }

  private File createEunitReportsEnvironment() throws ExecutionException {
    try {
      Project project = myConfiguration.getProject();
      String projectRoot = project.getBasePath();
      File tempDirectory = FileUtil.createTempDirectory(ErlangEunitReporterModule.MODULE_NAME, null);
      File configFile = new File(tempDirectory, CONFIG_FILE_NAME);

      writeAugmentedConfig(new File(projectRoot, CONFIG_FILE_NAME), configFile);
      ErlangEunitReporterModule.putReporterModuleTo(tempDirectory);

      return tempDirectory;
    } catch (IOException e) {
      throw new ExecutionException("Failed to setup eunit reports environment", e);
    }
  }

  private static void writeAugmentedConfig(File oldConfig, File newConfig) throws IOException {
    FileUtil.copy(oldConfig, newConfig);
    //TODO modify config (the config option we add at the end of file overrides by previously defined option from user's rebar.config)
    FileUtil.appendToFile(newConfig, REBAR_CONFIG);
  }
}
