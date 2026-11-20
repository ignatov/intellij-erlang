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
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;
import org.intellij.erlang.debugger.node.ErlangProcessSnapshot;
import org.intellij.erlang.jps.model.JpsErlangSdkType;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * Debugs an entry function that fails with an uncaught error, with the command line and the messages that
 * ErlangXDebugProcess uses for an Erlang Application run configuration. Needs the Erlang SDK from the erlang.sdk.path
 * system property, so it runs in testCompilation.
 */
public class ErlangDebugUncaughtErrorTest extends BasePlatformTestCase {
  private final List<Process> myProcesses = new ArrayList<>();
  private File myTempDir;
  private String mySdkPath;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    mySdkPath = JpsErlangSdkType.getTestsSdkPath();
    myTempDir = FileUtil.createTempDirectory("debug_uncaught_error", null);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      for (Process process : myProcesses) {
        process.destroyForcibly();
      }
    }
    finally {
      super.tearDown();
    }
  }

  // #1387: Run prints the reason and the stack trace, so Debug must print them too.
  public void testUncaughtErrorOfEntryFunctionIsPrinted() throws Exception {
    File source = new File(myTempDir, "main.erl");
    FileUtil.copy(new File("testData/debugger/uncaughtError/main.erl"), source);
    String erlc = JpsErlangSdkType.getByteCodeCompilerExecutable(mySdkPath).getPath();
    assertEquals("", run(Arrays.asList(erlc, "+debug_info", "-o", myTempDir.getPath(), source.getPath())));
    String release = run(Arrays.asList(erl(), "-noshell", "-eval", "io:put_chars(erlang:system_info(otp_release)), halt()."));
    String beams = Integer.parseInt(release) >= 27 ? "otp_27_plus" : "otp_pre_27";

    // What ErlangXDebugProcess.runDebugTarget does for an Erlang Application run configuration of main:main().
    ErlangDebuggerNode debuggerNode = new ErlangDebuggerNode(new NoOpListener());
    try {
      debuggerNode.interpretModules(List.of(source.getPath()));
      debuggerNode.runDebugger("main", "main", List.of());
      Process target = start(Arrays.asList(erl(), "-pa", new File("resources/debugger/beams/" + beams).getAbsolutePath(),
                                           "-pa", myTempDir.getPath(),
                                           "-run", "debugnode", "main", String.valueOf(debuggerNode.getLocalDebuggerPort()),
                                           "-noshell", "-s", "init", "stop"));
      String output = new String(target.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
      assertTrue("the debug target did not stop", target.waitFor(60, TimeUnit.SECONDS));

      assertTrue("the debug console must show the reason of the failure, it shows:\n" + output, output.contains("not_implemented"));
      assertTrue("the debug console must show where it failed, it shows:\n" + output, output.contains("{main,foo,0"));
    }
    finally {
      debuggerNode.stop();
    }
  }

  private String erl() {
    return JpsErlangSdkType.getByteCodeInterpreterExecutable(mySdkPath).getPath();
  }

  private Process start(List<String> command) throws Exception {
    Process process = new ProcessBuilder(command).directory(myTempDir).redirectErrorStream(true).start();
    myProcesses.add(process);
    return process;
  }

  private String run(List<String> command) throws Exception {
    Process process = start(command);
    String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
    assertTrue(process.waitFor(60, TimeUnit.SECONDS));
    return output.trim();
  }

  private static class NoOpListener implements ErlangDebuggerEventListener {
    @Override
    public void debuggerStarted() {
    }

    @Override
    public void failedToInterpretModules(String nodeName, List<String> modules) {
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
    }

    @Override
    public void breakpointIsSet(String module, int line) {
    }

    @Override
    public void breakpointReached(OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots) {
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
