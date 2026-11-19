/*
 * Copyright 2012-2026 Sergey Ignatov
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

package org.intellij.erlang.build;

import com.intellij.execution.ExecutionResult;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ExecutionEnvironmentBuilder;
import com.intellij.execution.runners.ProgramRunner;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.JUnit38AssumeSupportRunner;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.intellij.erlang.jps.rebar.RebarBuilder;
import org.intellij.erlang.rebar.runner.RebarEunitRunConfiguration;
import org.intellij.erlang.rebar.runner.RebarEunitRunConfigurationFactory;
import org.intellij.erlang.rebar.runner.RebarEunitRunningState;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.junit.Assume;
import org.junit.runner.RunWith;

import java.io.File;

/**
 * Runs a Rebar Eunit run configuration with a real rebar3.
 * The tests need the rebar3 escript in the rebar.path system property and are skipped without it.
 */
@RunWith(JUnit38AssumeSupportRunner.class)
public class ErlangRebarEunitRunTest extends ErlangCompilationTestBase {
  private static final long TIMEOUT_MS = 120_000;

  // #1333: rebar3 eunit in a terminal applies rebar.config.script, which defines FROM_SCRIPT, so p:v() returns script.
  public void testRebarConfigScriptIsApplied() throws Exception {
    File projectDirectory = setUpRebarProject("eunitConfigScript");
    String output = runRebarEunit();

    File ebin = new File(projectDirectory, "_build/test/lib/p/ebin");
    assertTrue("rebar3 did not compile p:\n" + output, new File(ebin, "p.beam").isFile());
    assertEquals("p was compiled without the options of rebar.config.script; rebar3 output:\n" + output,
                 "script", evaluate(ebin, "io:put_chars(atom_to_list(p:v()))"));
  }

  // #1333: a project with only rebar.config.script, which reads a file next to itself and sets eunit_opts.
  public void testRebarConfigScriptWithoutRebarConfigIsApplied() throws Exception {
    File projectDirectory = setUpRebarProject("eunitConfigScriptOnly");
    String output = runRebarEunit();

    File ebin = new File(projectDirectory, "_build/test/lib/p/ebin");
    assertTrue("rebar3 did not compile p:\n" + output, new File(ebin, "p.beam").isFile());
    assertEquals("p was compiled without the options of rebar.config.script; rebar3 output:\n" + output,
                 "script", evaluate(ebin, "io:put_chars(atom_to_list(p:v()))"));
  }

  // #1333: the reporter options replace the report options that rebar.config.script sets in eunit_opts.
  public void testReporterOptionsAreAddedToEunitOptsOfRebarConfigScript() throws Exception {
    File projectDirectory = setUpRebarProject("eunitConfigScriptOnly");
    File reportsRoot = RebarEunitRunningState.createEunitReportsEnvironment(myProject, projectDirectory.getPath());
    String reportsPath = FileUtil.toSystemIndependentName(reportsRoot.getPath());

    // Evaluate the configuration as rebar does for the file named by REBAR_CONFIG (rebar_config:consult_file/1).
    String eunitOpts = evaluate(reportsRoot,
      "Config = filename:join(\"" + reportsPath + "\", \"rebar.config\"), " +
      "Script = Config ++ \".script\", " +
      "Terms = case file:consult(Config) of {ok, T} -> T; {error, enoent} -> [] end, " +
      "Bindings = erl_eval:add_binding('CONFIG', Terms, erl_eval:add_binding('SCRIPT', Script, erl_eval:new_bindings())), " +
      "{ok, Result} = file:script(Script, Bindings), " +
      "io:format(\"~w~n~w\", [proplists:get_value(erl_opts, Result), proplists:get_value(eunit_opts, Result)])");
    assertEquals("[debug_info,{d,'FROM_SCRIPT'}]\n[verbose,{report,{eunit_teamcity,[]}},{no_tty,true}]", eunitOpts);
  }

  private File setUpRebarProject(String testDataDirectory) throws Exception {
    String rebarPath = RebarBuilder.getTestsRebarPath();
    Assume.assumeTrue("Set the rebar.path system property to the rebar3 escript to run this test", rebarPath != null);
    assertTrue("rebar.path is set to " + rebarPath + ", which is not a file", new File(rebarPath).isFile());
    RebarSettings.getInstance(myProject).setRebarPath(rebarPath);

    VirtualFile[] contentRoots = ModuleRootManager.getInstance(myModule).getContentRoots();
    assertSize(1, contentRoots);
    File projectDirectory = new File(contentRoots[0].getPath());
    FileUtil.copyDir(new File("testData/rebar/" + testDataDirectory), projectDirectory);
    return projectDirectory;
  }

  private String runRebarEunit() throws Exception {
    RebarEunitRunConfiguration configuration =
      (RebarEunitRunConfiguration) RebarEunitRunConfigurationFactory.getInstance().createTemplateConfiguration(myProject);
    configuration.setModule(myModule);
    configuration.setCommand("eunit");

    Executor executor = DefaultRunExecutor.getRunExecutorInstance();
    ProgramRunner<?> runner = ProgramRunner.findRunnerById("RebarRunner");
    assertNotNull(runner);
    ExecutionEnvironment environment = ExecutionEnvironmentBuilder.create(myProject, executor, configuration).runner(runner).build();
    RunProfileState state = configuration.getState(executor, environment);
    ExecutionResult result = state.execute(executor, runner);
    assertNotNull(result);
    if (result.getExecutionConsole() != null) {
      Disposer.register(getTestRootDisposable(), result.getExecutionConsole());
    }

    ProcessHandler processHandler = result.getProcessHandler();
    StringBuilder output = new StringBuilder();
    processHandler.addProcessListener(new ProcessAdapter() {
      @Override
      public void onTextAvailable(ProcessEvent event, Key outputType) {
        synchronized (output) {
          output.append(event.getText());
        }
      }
    });
    processHandler.startNotify();
    assertTrue("rebar3 eunit did not finish in time", processHandler.waitFor(TIMEOUT_MS));
    synchronized (output) {
      return output.toString();
    }
  }

  private static String evaluate(File codePath, String expression) throws Exception {
    String erl = JpsErlangSdkType.getByteCodeInterpreterExecutable(JpsErlangSdkType.getTestsSdkPath()).getAbsolutePath();
    GeneralCommandLine commandLine = new GeneralCommandLine(erl, "-noshell", "-pa", codePath.getPath(),
                                                            "-eval", expression + ", halt().");
    ProcessOutput output = new CapturingProcessHandler(commandLine).runProcess((int)TIMEOUT_MS);
    assertEquals(output.getStderr(), 0, output.getExitCode());
    return output.getStdout();
  }
}
