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
import com.intellij.execution.testframework.autotest.ToggleAutoTestAction;
import com.intellij.execution.testframework.sm.SMTestRunnerConnectionUtil;
import com.intellij.execution.testframework.sm.runner.SMTRunnerConsoleProperties;
import com.intellij.execution.testframework.sm.runner.ui.SMTRunnerConsoleView;
import com.intellij.execution.testframework.ui.BaseTestsOutputConsoleView;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.util.PathUtil;
import com.intellij.util.Processor;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.console.ErlangConsoleUtil;
import org.intellij.erlang.console.FileReferenceFilter;
import org.intellij.erlang.eunit.ErlangEunitReporterModule;
import org.intellij.erlang.eunit.ErlangTestConsoleProperties;
import org.intellij.erlang.psi.ErlangExpression;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangListExpression;
import org.intellij.erlang.psi.ErlangTupleExpression;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.rebar.util.ErlangTermFileUtil;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.VisibleForTesting;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class RebarEunitRunningState extends CommandLineState {
  private static final String CONFIG_FILE_NAME = "rebar.config";
  private static final String CONFIG_SCRIPT_FILE_NAME = CONFIG_FILE_NAME + ".script";
  private static final String ERL_FLAGS = "ERL_FLAGS";
  private static final String EUNIT_NO_TTY_OPTION = "{no_tty, true}";
  private static final String EUNIT_TEAMCITY_REPORTER = "{report,{" + ErlangEunitReporterModule.MODULE_NAME + ", []}}";
  private static final String EUNIT_OPTS = "{eunit_opts,[" + EUNIT_TEAMCITY_REPORTER + "," + EUNIT_NO_TTY_OPTION + "]}.";

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
    rerunAction.setModelProvider(((SMTRunnerConsoleView) consoleView)::getResultsViewer);

    DefaultExecutionResult executionResult = new DefaultExecutionResult(consoleView, processHandler);
    executionResult.setRestartActions(rerunAction, new ToggleAutoTestAction());
    return executionResult;
  }

  @NotNull
  private ConsoleView createConsoleView(Executor executor) {
    SMTRunnerConsoleProperties consoleProperties = new ErlangTestConsoleProperties(myConfiguration, executor);
    consoleProperties.addStackTraceFilter(new FileReferenceFilter(myConfiguration.getProject(), ErlangConsoleUtil.COMPILATION_ERROR_PATH));
    consoleProperties.addStackTraceFilter(new FileReferenceFilter(myConfiguration.getProject(), ErlangConsoleUtil.EUNIT_ERROR_PATH));
    consoleProperties.addStackTraceFilter(new FileReferenceFilter(myConfiguration.getProject(), ErlangConsoleUtil.EUNIT_FAILURE_PATH));
    return SMTestRunnerConnectionUtil.createConsole("Rebar", consoleProperties);
  }

  @NotNull
  @Override
  protected ProcessHandler startProcess() throws ExecutionException {
    GeneralCommandLine commandLine = getCommandLine(myConfiguration, createEunitReportsEnvironment(
      myConfiguration.getProject(), RebarRunningStateUtil.getWorkingDirectory(myConfiguration)));
    return RebarRunningStateUtil.runRebar(myConfiguration.getProject(), commandLine);
  }

  @VisibleForTesting
  @NotNull
  static GeneralCommandLine getCommandLine(@NotNull RebarEunitRunConfiguration configuration, @NotNull File reportsRoot) {
    GeneralCommandLine commandLine = RebarRunningStateUtil.getRebarCommandLine(configuration);

    // Put the reporter on the code path before the user's ERL_FLAGS, so that a -extra there does not make it a plain argument.
    String reportsPath = quoteErlFlagsArgument(PathUtil.toSystemIndependentName(reportsRoot.getPath()));
    String userErlFlags = commandLine.getEffectiveEnvironment().get(ERL_FLAGS);
    commandLine.getEnvironment().put(ERL_FLAGS, "-pa " + reportsPath + (StringUtil.isEmptyOrSpaces(userErlFlags) ? "" : " " + userErlFlags));

    boolean rebar3 = RebarSettings.getInstance(configuration.getProject()).isRebar3();
    if (rebar3) {
      commandLine.getEnvironment().put("REBAR_CONFIG", new File(reportsRoot, CONFIG_FILE_NAME).getPath());
    }
    else {
      commandLine.addParameters("-C", new File(reportsRoot, CONFIG_FILE_NAME).getPath());
    }
    return commandLine;
  }

  // erlexec splits ERL_FLAGS on whitespace and takes the text between double quotes literally (build_args_from_string
  // in erts/etc/common/erlexec.c). A double quote is written as \" outside the quotes.
  @NotNull
  private static String quoteErlFlagsArgument(@NotNull String argument) {
    return "\"" + argument.replace("\"", "\"\\\"\"") + "\"";
  }

  // rebar evaluates the rebar.config.script next to the configuration it is given, so the project's script is called
  // from a script in the temporary directory, which then adds the reporter options. rebar runs in the working directory.
  @VisibleForTesting
  @NotNull
  public static File createEunitReportsEnvironment(@NotNull Project project, @NotNull String workingDirectory) throws ExecutionException {
    try {
      File tempDirectory = FileUtil.createTempDirectory(ErlangEunitReporterModule.MODULE_NAME, null);
      File configFile = new File(tempDirectory, CONFIG_FILE_NAME);
      File projectConfig = new File(workingDirectory, CONFIG_FILE_NAME);
      File projectScript = new File(workingDirectory, CONFIG_SCRIPT_FILE_NAME);

      if (projectScript.isFile()) {
        if (projectConfig.isFile()) {
          FileUtil.copy(projectConfig, configFile);
        }
        FileUtil.writeToFile(new File(tempDirectory, CONFIG_SCRIPT_FILE_NAME), createConfigScript());
      }
      else {
        writeModifiedConfig(project, projectConfig, configFile);
      }
      ErlangEunitReporterModule.putReporterModuleTo(tempDirectory, ErlangSdkType.getProjectSdkRelease(project));

      return tempDirectory;
    } catch (IOException e) {
      throw new ExecutionException("Failed to setup eunit reports environment", e);
    }
  }

  // The project's script is named relative to the working directory, as plain rebar names it: an absolute name with
  // non-ASCII characters is not found when Erlang uses Latin-1 file names. rebar3 uses the last eunit_opts entry and
  // rebar 2 the first one, so the reporter options go into every entry.
  @NotNull
  private static String createConfigScript() {
    return "ProjectScript = \"" + CONFIG_SCRIPT_FILE_NAME + "\",\n" +
           "ProjectBindings = erl_eval:add_binding('CONFIG', CONFIG, erl_eval:add_binding('SCRIPT', ProjectScript, erl_eval:new_bindings())),\n" +
           "ProjectTerms = case file:script(ProjectScript, ProjectBindings) of\n" +
           "  {ok, Terms} when is_list(Terms) -> Terms;\n" +
           "  {ok, Term} -> [Term];\n" +
           "  {error, Reason} -> erlang:error({filename:absname(ProjectScript), Reason})\n" +
           "end,\n" +
           "ReporterOptions = [" + EUNIT_TEAMCITY_REPORTER + ", " + EUNIT_NO_TTY_OPTION + "],\n" +
           "IsReportOption = fun({report, _}) -> true; ({no_tty, _}) -> true; (_) -> false end,\n" +
           "case lists:keymember(eunit_opts, 1, ProjectTerms) of\n" +
           "  true ->\n" +
           "    [case Entry of\n" +
           "       {eunit_opts, EntryOptions} when is_list(EntryOptions) ->\n" +
           "         {eunit_opts, [Option || Option <- EntryOptions, not IsReportOption(Option)] ++ ReporterOptions};\n" +
           "       _ -> Entry\n" +
           "     end || Entry <- ProjectTerms];\n" +
           "  false -> ProjectTerms ++ [{eunit_opts, ReporterOptions}]\n" +
           "end.\n";
  }

  @VisibleForTesting
  static void writeModifiedConfig(@NotNull Project project, @NotNull File oldConfig, @NotNull File newConfig) throws IOException {
    PsiFile configPsi = createModifiedConfigPsi(project, oldConfig);
    // Write the text as is, with the \n line separators of PSI. Adding the file to a PsiDirectory would save it with
    // the line separator of the code style, which is the one of the OS (\r\n on Windows) for a new file.
    String text = ReadAction.computeBlocking(configPsi::getText);
    FileUtil.writeToFile(newConfig, text);
  }

  private static PsiFile createModifiedConfigPsi(@NotNull Project project, @NotNull File oldConfig) throws IOException {
    String oldConfigText = oldConfig.exists() ? StringUtil.convertLineSeparators(new String(FileUtil.loadFileText(oldConfig))) : "";
    ErlangFile configPsi = (ErlangFile) PsiFileFactory.getInstance(project)
      .createFileFromText(CONFIG_FILE_NAME, ErlangFileType.TERMS, oldConfigText);
    List<ErlangTupleExpression> eunitOptsSections = ErlangTermFileUtil.getConfigSections(configPsi, "eunit_opts");
    if (eunitOptsSections.isEmpty()) {
      configPsi.add(ErlangElementFactory.createWhitespaceFromText(project, "\n"));
      configPsi.add(ErlangTermFileUtil.createForm(EUNIT_OPTS));
    } else {
      removeReportOptions(eunitOptsSections);
      addEunitTeamcityReportOptions(project, eunitOptsSections.getFirst());
    }
    return configPsi;
  }

  private static void removeReportOptions(List<ErlangTupleExpression> eunitOptsSections) {
    Processor<ErlangTupleExpression> deletingProcessor = erlangTupleExpression -> {
      ErlangTermFileUtil.deleteListExpressionItem(erlangTupleExpression);
      return true;
    };
    for (ErlangTupleExpression eunitOptsSection : eunitOptsSections) {
      ErlangExpression eunitOptsList = eunitOptsSection.getExpressionList().get(1);
      ContainerUtil.process(ErlangTermFileUtil.getConfigSections(eunitOptsList, "report"), deletingProcessor);
      ContainerUtil.process(ErlangTermFileUtil.getConfigSections(eunitOptsList, "no_tty"), deletingProcessor);
    }
  }

  private static void addEunitTeamcityReportOptions(@NotNull Project project, ErlangTupleExpression eunitOptsTuple) throws IOException {
    ErlangExpression eunitOptsList = eunitOptsTuple.getExpressionList().get(1);
    if (!(eunitOptsList instanceof ErlangListExpression)) {
      throw new IOException("Invalid rebar.config file");
    }
    ErlangTermFileUtil.addListExpressionItem((ErlangListExpression) eunitOptsList, createExpression(project, EUNIT_TEAMCITY_REPORTER));
    ErlangTermFileUtil.addListExpressionItem((ErlangListExpression) eunitOptsList, createExpression(project, EUNIT_NO_TTY_OPTION));
  }

  private static ErlangExpression createExpression(@NotNull Project project, String expressionText) {
    return ErlangElementFactory.createExpressionFromText(project, expressionText);
  }
}
