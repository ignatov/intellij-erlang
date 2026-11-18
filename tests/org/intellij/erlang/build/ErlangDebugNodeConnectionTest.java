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
import java.net.InetAddress;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Runs the debugger's helper node (resources/debugger/src/debugnode.erl) with the real Erlang from erlang.sdk.path
 * and checks that it connects back to the IDE (#939).
 */
public class ErlangDebugNodeConnectionTest extends BasePlatformTestCase {
  private static final int TIMEOUT_SECONDS = 20;
  // TEST-NET-1 (RFC 5737): never routed, so a connection to it times out.
  private static final String UNREACHABLE_ADDRESS = "{192,0,2,1}";

  // The machine's own host name resolves to an address at which the IDE cannot be reached, as in #939 and #573.
  // The IDE listens on the wildcard address of the same machine, so the debug node must still reach it.
  public void testDebugNodeConnectsWhenHostNameResolvesToUnreachableAddress() throws Exception {
    String sdkHome = JpsErlangSdkType.getTestsSdkPath();
    String erl = JpsErlangSdkType.getByteCodeInterpreterExecutable(sdkHome).getPath();
    File dir = FileUtil.createTempDirectory("debugnode_939", null);
    compileDebugNode(sdkHome, dir);

    String hostName = run(new ProcessBuilder(erl, "-noshell", "-eval",
                                             "{ok, H} = inet:gethostname(), io:put_chars(H), halt().")).trim();
    File inetrc = new File(dir, "inetrc");
    FileUtil.writeToFile(inetrc, "{lookup, [file]}.\n{host, " + UNREACHABLE_ADDRESS + ", [\"" + hostName + "\"]}.\n");

    CompletableFuture<Void> started = new CompletableFuture<>();
    ErlangDebuggerNode node = new ErlangDebuggerNode(new StartListener(started));
    int port = node.getLocalDebuggerPort();
    ProcessBuilder debugNode = new ProcessBuilder(erl, "-pa", dir.getPath(), "-run", "debugnode", "main",
                                                  String.valueOf(port), "-noshell");
    File output = new File(dir, "debugnode.out");
    debugNode.redirectErrorStream(true).redirectOutput(output);
    debugNode.environment().put("ERL_INETRC", inetrc.getPath());
    Process process = debugNode.start();
    try {
      started.get(TIMEOUT_SECONDS, TimeUnit.SECONDS);
    }
    catch (TimeoutException e) {
      process.destroyForcibly().waitFor(5, TimeUnit.SECONDS);
      fail("debugnode did not connect to the IDE on port " + port + " within " + TIMEOUT_SECONDS + " s; host name '" +
           hostName + "' resolves to " + UNREACHABLE_ADDRESS + ". debugnode output: '" +
           FileUtil.loadFile(output) + "'");
    }
    finally {
      process.destroyForcibly();
      node.stop();
      if (!started.isDone()) {
        // Unblocks the accept() of the debugger node's server thread.
        new Socket(InetAddress.getLoopbackAddress(), port).close();
      }
    }
  }

  private static void compileDebugNode(String sdkHome, File outDir) throws Exception {
    String erlc = JpsErlangSdkType.getByteCodeCompilerExecutable(sdkHome).getPath();
    String source = new File("resources/debugger/src/debugnode.erl").getAbsolutePath();
    run(new ProcessBuilder(erlc, "-o", outDir.getPath(), source));
    assertTrue(new File(outDir, "debugnode.beam").isFile());
  }

  private static String run(ProcessBuilder builder) throws Exception {
    Process process = builder.redirectErrorStream(true).start();
    String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
    assertTrue(process.waitFor(60, TimeUnit.SECONDS));
    assertEquals(output, 0, process.exitValue());
    return output;
  }

  private static class StartListener implements ErlangDebuggerEventListener {
    private final CompletableFuture<Void> myStarted;

    StartListener(CompletableFuture<Void> started) {
      myStarted = started;
    }

    @Override public void debuggerStarted() { myStarted.complete(null); }
    @Override public void failedToInterpretModules(String nodeName, List<String> modules) { }
    @Override public void failedToDebugRemoteNode(String nodeName, String error) { }
    @Override public void unknownMessage(String messageText) { }
    @Override public void failedToSetBreakpoint(String module, int line, String errorMessage) { }
    @Override public void breakpointIsSet(String module, int line) { }
    @Override public void breakpointReached(OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots) { }
    @Override public void debuggerStopped() { }
    @Override public void handleEvaluationResponse(OtpErlangObject response) { }
  }
}
