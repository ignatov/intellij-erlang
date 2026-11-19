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
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;
import org.intellij.erlang.debugger.node.ErlangProcessSnapshot;
import org.intellij.erlang.jps.model.JpsErlangSdkType;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * Debugs a real Erlang node with the plugin's debugger node, as a remote debug run configuration does, and stops
 * the session in the order the platform uses. Needs the Erlang SDK from the erlang.sdk.path system property, so it
 * runs in testCompilation.
 */
public class ErlangRemoteDebugStopTest extends BasePlatformTestCase {
  private static final String MODULE = "remote_stop_target";
  private static final String COOKIE = "remote_debug_stop_test";

  private final List<Process> myProcesses = new ArrayList<>();
  private final List<String> myDebuggerErrors = new ArrayList<>();
  private File myTempDir;
  private String mySdkPath;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    mySdkPath = JpsErlangSdkType.getTestsSdkPath();
    myTempDir = FileUtil.createTempDirectory("remote_debug_stop", null);
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

  public void testStoppingSessionLeavesModulesOnRemoteNodeCallable() throws Exception {
    File source = compileModule();

    long id = System.currentTimeMillis();
    String[] targetInfo = startNode("target_" + id, "io:format('~s ~s~n', [node(), erlang:system_info(otp_release)]).");
    String targetNode = targetInfo[0];
    String beams = Integer.parseInt(targetInfo[1]) >= 27 ? "otp_27_plus" : "otp_pre_27";
    String expectedCompiled = "{" + MODULE + ",current,0}";
    assertEquals(expectedCompiled, callOnNode(targetNode));

    // What ErlangXDebugProcess does for a remote debug run configuration.
    ErlangDebuggerNode debuggerNode = new ErlangDebuggerNode(new RecordingListener());
    debuggerNode.interpretModules(List.of(source.getPath()));
    debuggerNode.debugRemoteNode(targetNode, COOKIE);
    Process debugger = start(Arrays.asList(erl(), "-pa", new File("resources/debugger/beams/" + beams).getAbsolutePath(),
                                           "-run", "debugnode", "main", String.valueOf(debuggerNode.getLocalDebuggerPort()),
                                           "-sname", "debugger_node_" + id, "-noshell"));
    OSProcessHandler debuggerHandler = new OSProcessHandler(debugger, "debugnode");
    debuggerHandler.startNotify();

    assertInterpreted(targetNode);

    // What XDebugSessionImpl.stop() does: destroy the process of the session, then stop the XDebugProcess once it is terminated.
    debuggerHandler.destroyProcess();
    assertTrue("the debugger node did not stop", debuggerHandler.waitFor(TimeUnit.SECONDS.toMillis(30)));
    debuggerNode.stop();

    assertEquals("the target node must run its modules as before the debug session", expectedCompiled, callOnNode(targetNode));
  }

  // int:ni/1 interprets the modules on every node the debugger node is connected to, and once it is connected to the
  // target it is connected to the target's cluster too.
  public void testStoppingSessionLeavesModulesOnConnectedNodeCallable() throws Exception {
    File source = compileModule();

    long id = System.currentTimeMillis();
    String[] targetInfo = startNode("target_" + id, "io:format('~s ~s~n', [node(), erlang:system_info(otp_release)]).");
    String targetNode = targetInfo[0];
    String beams = Integer.parseInt(targetInfo[1]) >= 27 ? "otp_27_plus" : "otp_pre_27";
    String peerNode = startNode("peer_" + id, "pong = net_adm:ping('" + targetNode + "'), io:format('~s~n', [node()]).")[0];
    String host = targetNode.substring(targetNode.indexOf('@'));
    String debuggerNodeName = "debugger_node_" + id + host;
    String expectedCompiled = "{" + MODULE + ",current,0}";
    assertEquals(expectedCompiled, callOnNode(targetNode));
    assertEquals(expectedCompiled, callOnNode(peerNode));

    ErlangDebuggerNode debuggerNode = new ErlangDebuggerNode(new RecordingListener());
    Process debugger = start(Arrays.asList(erl(), "-pa", new File("resources/debugger/beams/" + beams).getAbsolutePath(),
                                           "-run", "debugnode", "main", String.valueOf(debuggerNode.getLocalDebuggerPort()),
                                           "-sname", "debugger_node_" + id, "-setcookie", COOKIE, "-noshell"));
    OSProcessHandler debuggerHandler = new OSProcessHandler(debugger, "debugnode");
    debuggerHandler.startNotify();
    // Connect the peer to the debugger node before the modules are interpreted, as the debugger node's connection to
    // the target would do, but without depending on when global connects them.
    String connected = call(peerNode, "net_kernel, connect_node, ['" + debuggerNodeName + "']");
    for (int i = 0; i < 60 && !connected.equals("true"); i++) {
      TimeUnit.MILLISECONDS.sleep(500);
      connected = call(peerNode, "net_kernel, connect_node, ['" + debuggerNodeName + "']");
    }
    assertEquals("the peer node did not connect to the debugger node", "true", connected);
    debuggerNode.interpretModules(List.of(source.getPath()));
    debuggerNode.debugRemoteNode(targetNode, COOKIE);

    assertInterpreted(targetNode);
    assertInterpreted(peerNode);

    debuggerHandler.destroyProcess();
    assertTrue("the debugger node did not stop", debuggerHandler.waitFor(TimeUnit.SECONDS.toMillis(30)));
    debuggerNode.stop();

    assertEquals("the target node must run its modules as before the debug session", expectedCompiled, callOnNode(targetNode));
    assertEquals("the peer node must run its modules as before the debug session", expectedCompiled, callOnNode(peerNode));
  }

  private File compileModule() throws Exception {
    File source = new File(myTempDir, MODULE + ".erl");
    FileUtil.copy(new File("testData/debugger/remoteStop/" + MODULE + ".erl"), source);
    String erlc = JpsErlangSdkType.getByteCodeCompilerExecutable(mySdkPath).getPath();
    assertEquals("", run(Arrays.asList(erlc, "+debug_info", "-o", myTempDir.getPath(), source.getPath())));
    return source;
  }

  // The -eval expressions use no double quotes: on Windows ProcessBuilder passes them to erl unescaped.
  private String[] startNode(String name, String eval) throws IOException {
    Process node = start(Arrays.asList(erl(), "-noshell", "-sname", name, "-setcookie", COOKIE, "-pa", myTempDir.getPath(),
                                       "-eval", eval));
    return new BufferedReader(new InputStreamReader(node.getInputStream(), StandardCharsets.UTF_8)).readLine().split(" ");
  }

  private void assertInterpreted(String node) throws Exception {
    String interpreted = callOnNode(node);
    for (int i = 0; i < 60 && !interpreted.startsWith("{dbg_debugged,"); i++) {
      TimeUnit.MILLISECONDS.sleep(500);
      interpreted = callOnNode(node);
    }
    assertEmpty(myDebuggerErrors);
    assertTrue("the module is not interpreted on " + node + ": " + interpreted, interpreted.startsWith("{dbg_debugged,"));
  }

  private String erl() {
    return JpsErlangSdkType.getByteCodeInterpreterExecutable(mySdkPath).getPath();
  }

  private String callOnNode(String node) throws Exception {
    return call(node, MODULE + ", current, []");
  }

  private String call(String node, String mfa) throws Exception {
    return run(Arrays.asList(erl(), "-noshell", "-sname", "probe_" + System.nanoTime(), "-setcookie", COOKIE, "-eval",
                             "io:format('~0p', [rpc:call('" + node + "', " + mfa + ")]), halt()."));
  }

  private Process start(List<String> command) throws IOException {
    Process process = new ProcessBuilder(command).redirectErrorStream(true).start();
    myProcesses.add(process);
    return process;
  }

  private String run(List<String> command) throws Exception {
    Process process = start(command);
    String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
    assertTrue(process.waitFor(60, TimeUnit.SECONDS));
    return output.trim();
  }

  private class RecordingListener implements ErlangDebuggerEventListener {
    @Override
    public void debuggerStarted() {
    }

    @Override
    public void failedToInterpretModules(String nodeName, List<String> modules) {
      myDebuggerErrors.add("failed to interpret " + modules + " on " + nodeName);
    }

    @Override
    public void failedToDebugRemoteNode(String nodeName, String error) {
      myDebuggerErrors.add("failed to debug " + nodeName + ": " + error);
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
