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
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Getter;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.impl.file.PsiDirectoryFactory;
import com.intellij.util.PathUtil;
import com.intellij.util.Processor;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.console.ErlangConsoleUtil;
import org.intellij.erlang.console.FileReferenceFilter;
import org.intellij.erlang.eunit.ErlangEunitReporterModule;
import org.intellij.erlang.eunit.ErlangTestLocationProvider;
import org.intellij.erlang.eunit.ErlangUnitConsoleProperties;
import org.intellij.erlang.psi.ErlangExpression;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangListExpression;
import org.intellij.erlang.psi.ErlangTupleExpression;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.rebar.util.ErlangTermFileUtil;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class RebarEunitRunningState extends CommandLineState {
  private static final String CONFIG_FILE_NAME = "rebar.config";
  private static final String EUNIT_NO_TTY_OPTION = "{no_tty, true}";
  private static final String EUNIT_TEAMCITY_REPORTER = "{report,{" + ErlangEunitReporterModule.MODULE_NAME + ", []}}";
  private static final String EUNIT_OPTS = "\n{eunit_opts,[" + EUNIT_TEAMCITY_REPORTER + "," + EUNIT_NO_TTY_OPTION + "]}.";

  private final RebarEunitRunConfiguration myConfiguration;

  public RebarEunitRunningState(@NotNull ExecutionEnvironment env, @NotNull RebarEunitRunConfiguration configuration) {
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
    rerunAction.init(((BaseTestsOutputConsoleView) consoleView).getProperties());
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
  private ConsoleView createConsoleView(Executor executor) {
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
    commandLine.getEnvironment().put("ERL_FLAGS", "-pa " + PathUtil.toSystemIndependentName(reportsRoot.getPath()));
  }

  private File createEunitReportsEnvironment() throws ExecutionException {
    try {
      String workingDirectory = RebarRunningStateUtil.getWorkingDirectory(myConfiguration);
      File tempDirectory = FileUtil.createTempDirectory(ErlangEunitReporterModule.MODULE_NAME, null);
      File configFile = new File(tempDirectory, CONFIG_FILE_NAME);

      writeModifiedConfig(new File(workingDirectory, CONFIG_FILE_NAME), configFile);
      ErlangEunitReporterModule.putReporterModuleTo(tempDirectory);

      return tempDirectory;
    } catch (IOException e) {
      throw new ExecutionException("Failed to setup eunit reports environment", e);
    }
  }

  private void writeModifiedConfig(File oldConfig, final File newConfig) throws IOException {
    Project project = myConfiguration.getProject();
    final PsiFile configPsi = createModifiedConfigPsi(oldConfig);
    VirtualFile outputDirectory = LocalFileSystem.getInstance().refreshAndFindFileByIoFile(newConfig.getParentFile());
    final PsiDirectory psiDirectory = outputDirectory != null ? PsiDirectoryFactory.getInstance(project).createDirectory(outputDirectory) : null;
    if (psiDirectory == null) {
      throw new IOException("Failed to save modified rebar.config");
    }
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        configPsi.setName(newConfig.getName());
        psiDirectory.add(configPsi);
      }
    });
  }

  private PsiFile createModifiedConfigPsi(File oldConfig) throws IOException {
    String oldConfigText = oldConfig.exists() ? new String(FileUtil.loadFileText(oldConfig)) : "";
    ErlangFile configPsi = (ErlangFile) PsiFileFactory.getInstance(myConfiguration.getProject())
      .createFileFromText(CONFIG_FILE_NAME, ErlangFileType.TERMS, oldConfigText);
    List<ErlangTupleExpression> eunitOptsSections = ErlangTermFileUtil.getConfigSections(configPsi, "eunit_opts");
    if (eunitOptsSections.isEmpty()) {
      ErlangExpression form = ErlangTermFileUtil.createForm(EUNIT_OPTS);
      assert form != null;
      configPsi.add(form);
    } else {
      removeReportOptions(eunitOptsSections);
      addEunitTeamcityReportOptions(eunitOptsSections.get(0));
    }
    return configPsi;
  }

  private static void removeReportOptions(List<ErlangTupleExpression> eunitOptsSections) {
    Processor<ErlangTupleExpression> deletingProcessor = new Processor<ErlangTupleExpression>() {
      @Override
      public boolean process(ErlangTupleExpression erlangTupleExpression) {
        ErlangTermFileUtil.deleteListExpressionItem(erlangTupleExpression);
        return true;
      }
    };
    for (ErlangTupleExpression eunitOptsSection : eunitOptsSections) {
      ErlangExpression eunitOptsList = eunitOptsSection.getExpressionList().get(1);
      ContainerUtil.process(ErlangTermFileUtil.getConfigSections(eunitOptsList, "report"), deletingProcessor);
      ContainerUtil.process(ErlangTermFileUtil.getConfigSections(eunitOptsList, "no_tty"), deletingProcessor);
    }
  }

  private void addEunitTeamcityReportOptions(ErlangTupleExpression eunitOptsTuple) throws IOException {
    ErlangExpression eunitOptsList = eunitOptsTuple.getExpressionList().get(1);
    if (!(eunitOptsList instanceof ErlangListExpression)) {
      throw new IOException("Invalid rebar.config file");
    }
    ErlangTermFileUtil.addListExpressionItem((ErlangListExpression) eunitOptsList, createExpression(EUNIT_TEAMCITY_REPORTER));
    ErlangTermFileUtil.addListExpressionItem((ErlangListExpression) eunitOptsList, createExpression(EUNIT_NO_TTY_OPTION));
  }

  private ErlangExpression createExpression(String expressionText) {
    return ErlangElementFactory.createExpressionFromText(myConfiguration.getProject(), expressionText);
  }
}
