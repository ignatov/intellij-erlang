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

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.executors.DefaultDebugExecutor;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ExecutionEnvironmentBuilder;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.concurrency.AppExecutorUtil;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;
import org.intellij.erlang.debugger.node.ErlangProcessSnapshot;
import org.intellij.erlang.eunit.ErlangUnitRunConfiguration;
import org.intellij.erlang.eunit.ErlangUnitRunConfigurationType;
import org.intellij.erlang.eunit.ErlangUnitRunningState;
import org.intellij.erlang.runconfig.ErlangRunningState;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;

/**
 * Debugs an EUnit run configuration with the plugin's debugger node and a real Erlang SDK, as ErlangXDebugProcess
 * does, and stays at a breakpoint as a user does.
 */
public class ErlangEunitDebugTest extends ErlangCompilationTestBase {
  // EUnit's own timeout of a test is 5 seconds.
  private static final long STOP_AT_BREAKPOINT_MS = 6_000;

  // A test of a module with several tests was killed after 5 seconds at a breakpoint, and EUnit cancelled the
  // module: "Test suite was cancelled. Reason: {blame,[1,1]}" and only the first test reported (#1485).
  public void testTestStaysAtBreakpointLongerThanEunitTimeout() throws Exception {
    VirtualFile source = addSourceFile(myModule, "pricing.erl", """
      -module(pricing).
      -export([sum/1]).
      sum(Items) ->
        lists:sum(Items).
      """);
    VirtualFile tests = addTestFile(myModule, "pricing_tests.erl", """
      -module(pricing_tests).
      -include_lib("eunit/include/eunit.hrl").
      sum_test() -> ?assertEqual(3, pricing:sum([1, 2])).
      other_test() -> ?assertEqual(2, 1 + 1).
      """);
    compileAndAssertOutput(true);

    ErlangUnitRunConfiguration configuration =
      new ErlangUnitRunConfiguration(myProject, "pricing_tests", ErlangUnitRunConfigurationType.getInstance());
    configuration.setModule(myModule);
    configuration.getConfigData().setKind(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE);
    configuration.getConfigData().setModuleNames(Set.of("pricing_tests"));
    Executor executor = DefaultDebugExecutor.getDebugExecutorInstance();
    ExecutionEnvironment environment = ExecutionEnvironmentBuilder.create(executor, configuration).build();
    ErlangUnitRunningState state = configuration.getState(executor, environment);
    assertNotNull(state);

    // What ErlangXDebugProcess sends, with the breakpoint before the run so that the test does not depend on timing.
    List<OtpErlangPid> stops = new CopyOnWriteArrayList<>();
    List<String> errors = new CopyOnWriteArrayList<>();
    ErlangDebuggerNode[] node = new ErlangDebuggerNode[1];
    node[0] = new ErlangDebuggerNode(new ResumingListener(stops, errors, () -> node[0].resume()));
    ErlangDebuggerNode debuggerNode = node[0];
    debuggerNode.interpretModules(List.of(source.getPath(), tests.getPath()));
    // the line of lists:sum/1, 0-based as in XSourcePosition
    debuggerNode.setBreakpoint("pricing", 3);
    ErlangRunningState.ErlangEntryPoint entryPoint = state.getDebugEntryPoint();
    debuggerNode.runDebugger(entryPoint.getModuleName(), entryPoint.getFunctionName(), entryPoint.getArgsList());

    String beams = ErlangSdkType.getRelease(myModule).getBundledBeamDirectory();
    GeneralCommandLine commandLine = new GeneralCommandLine();
    state.setExePath(commandLine);
    commandLine.addParameters("-pa", new File("resources/debugger/beams/" + beams).getAbsolutePath());
    commandLine.addParameters(state.getCodePath());
    commandLine.addParameters("-run", "debugnode", "main", String.valueOf(debuggerNode.getLocalDebuggerPort()));
    state.setNoShellMode(commandLine);
    state.setStopErlang(commandLine);

    OSProcessHandler processHandler = new OSProcessHandler(commandLine);
    StringBuilder output = new StringBuilder();
    processHandler.addProcessListener(new ProcessAdapter() {
      @Override
      public void onTextAvailable(@NotNull ProcessEvent event, @NotNull Key outputType) {
        synchronized (output) {
          output.append(event.getText());
        }
      }
    });
    processHandler.startNotify();
    try {
      assertTrue("the debugged EUnit run did not finish in time", processHandler.waitFor(TimeUnit.SECONDS.toMillis(120)));
    }
    finally {
      processHandler.destroyProcess();
      debuggerNode.stop();
    }

    String text;
    synchronized (output) {
      text = output.toString();
    }
    assertEmpty(text, errors);
    assertEquals(text, 1, stops.size());
    assertFalse(text, text.contains("cancelled"));
    assertTrue(text, text.contains("##teamcity[testFinished name='sum_test'"));
    assertTrue(text, text.contains("##teamcity[testFinished name='other_test'"));
    assertFalse(text, text.contains("##teamcity[testFailed"));
  }

  private static class ResumingListener implements ErlangDebuggerEventListener {
    private final List<OtpErlangPid> myStops;
    private final List<String> myErrors;
    private final Runnable myResume;

    ResumingListener(@NotNull List<OtpErlangPid> stops, @NotNull List<String> errors, @NotNull Runnable resume) {
      myStops = stops;
      myErrors = errors;
      myResume = resume;
    }

    @Override
    public void breakpointReached(OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots) {
      myStops.add(pid);
      AppExecutorUtil.getAppScheduledExecutorService().schedule(myResume, STOP_AT_BREAKPOINT_MS, TimeUnit.MILLISECONDS);
    }

    @Override
    public void debuggerStarted() {
    }

    @Override
    public void failedToInterpretModules(String nodeName, List<String> modules) {
      myErrors.add("failed to interpret " + modules);
    }

    @Override
    public void failedToDebugRemoteNode(String nodeName, String error) {
    }

    @Override
    public void unknownMessage(String messageText) {
    }

    @Override
    public void failedToReceiveMessage(String error) {
    }

    @Override
    public void failedToSetBreakpoint(String module, int line, String errorMessage) {
      myErrors.add("failed to set a breakpoint at " + module + ":" + line + ": " + errorMessage);
    }

    @Override
    public void breakpointIsSet(String module, int line) {
    }

    @Override
    public void debuggerStopped() {
    }

    @Override
    public void handleEvaluationResponse(OtpErlangObject response) {
    }

    @Override
    public void evaluationFailed(String errorMessage) {
    }
  }
}
