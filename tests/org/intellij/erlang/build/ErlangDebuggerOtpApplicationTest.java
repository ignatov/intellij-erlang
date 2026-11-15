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
import com.intellij.util.ResourceUtil;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;
import org.intellij.erlang.debugger.node.ErlangProcessSnapshot;
import org.intellij.erlang.jps.model.JpsErlangSdkType;

import java.io.File;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * Runs the debugger node of the plugin on a real Erlang node, the way ErlangXDebugProcess launches it for an
 * Erlang Application run configuration with the entry point "application start" and the argument ti_app (#580).
 * It needs the Erlang SDK in the erlang.sdk.path system property, so it lives with the compilation tests.
 */
public class ErlangDebuggerOtpApplicationTest extends BasePlatformTestCase {
  private static final int TIMEOUT_SECONDS = 30;
  private static final int STAYS_UP_SECONDS = 5;
  // zero-based, as ErlangDebuggerNode takes and reports them: ti_server.erl lines 11 and 20
  private static final int INIT_LINE = 10;
  private static final int HANDLE_INFO_TIMEOUT_LINE = 19;
  private static final String[] DEBUGGER_BEAMS =
    {"debugnode.beam", "remote_debugger.beam", "remote_debugger_listener.beam", "remote_debugger_notifier.beam"};

  public void testBreakpointInHandleInfoAfterApplicationStartReturns() throws Exception {
    String sdkHome = JpsErlangSdkType.getTestsSdkPath();
    File appDir = FileUtil.createTempDirectory("erlang_debugger_app_", null);
    File beamsDir = FileUtil.createTempDirectory("erlang_debugger_beams_", null);
    FileUtil.copyDir(new File("testData/debugger/otpApplication"), appDir);
    List<String> sources = new ArrayList<>();
    for (String module : new String[]{"ti_app", "ti_sup", "ti_server"}) {
      sources.add(new File(appDir, module + ".erl").getPath());
    }
    List<String> erlc = new ArrayList<>();
    erlc.add(JpsErlangSdkType.getByteCodeCompilerExecutable(sdkHome).getPath());
    erlc.add("+debug_info");
    erlc.add("-o");
    erlc.add(appDir.getPath());
    erlc.addAll(sources);
    assertEquals("erlc failed", 0, new ProcessBuilder(erlc).inheritIO().start().waitFor());
    copyDebuggerBeams(sdkHome, beamsDir);

    BlockingQueue<String> events = new LinkedBlockingQueue<>();
    ErlangDebuggerNode node = new ErlangDebuggerNode(new RecordingListener(events));
    // the same commands, in the same order, as ErlangXDebugProcess sends for a local Erlang Application
    node.interpretModules(sources);
    node.setBreakpoint("ti_server", INIT_LINE);
    node.setBreakpoint("ti_server", HANDLE_INFO_TIMEOUT_LINE);
    node.runDebugger("application", "start", List.of("ti_app"));

    File output = new File(appDir, "erl.out");
    Process erl = new ProcessBuilder(JpsErlangSdkType.getByteCodeInterpreterExecutable(sdkHome).getPath(),
                                     "-pa", beamsDir.getPath(),
                                     "-pa", appDir.getPath(),
                                     "-run", "debugnode", "main", String.valueOf(node.getLocalDebuggerPort()),
                                     "-noshell",
                                     "-s", "init", "stop")
      .redirectErrorStream(true).redirectOutput(output).start();
    try {
      assertEquals("breakpoint ti_server:" + INIT_LINE, nextEvent(events, erl, output));
      node.resume();
      assertEquals("breakpoint ti_server:" + HANDLE_INFO_TIMEOUT_LINE, nextEvent(events, erl, output));
      // ti_server is suspended at the breakpoint, so the debugger must stay up until the user resumes it
      String unexpected = events.poll(STAYS_UP_SECONDS, TimeUnit.SECONDS);
      assertNull(unexpected == null ? null : withNodeOutput(unexpected, erl, output), unexpected);
      node.resume();
    }
    finally {
      node.stop();
      erl.destroyForcibly().waitFor(TIMEOUT_SECONDS, TimeUnit.SECONDS);
    }
  }

  private static String nextEvent(BlockingQueue<String> events, Process erl, File output) throws Exception {
    String event = events.poll(TIMEOUT_SECONDS, TimeUnit.SECONDS);
    return event != null && event.startsWith("breakpoint") ? event : withNodeOutput(event, erl, output);
  }

  private static String withNodeOutput(String event, Process erl, File output) throws Exception {
    erl.waitFor(TIMEOUT_SECONDS, TimeUnit.SECONDS);
    String text = output.exists() ? Files.readString(output.toPath(), StandardCharsets.UTF_8) : "";
    return event + "\nErlang node output:\n" + text;
  }

  private static void copyDebuggerBeams(String sdkHome, File directory) throws Exception {
    Process release = new ProcessBuilder(JpsErlangSdkType.getByteCodeInterpreterExecutable(sdkHome).getPath(), "-noshell",
                                         "-eval", "io:put_chars(erlang:system_info(otp_release)), halt().").start();
    String otpRelease = new String(release.getInputStream().readAllBytes(), StandardCharsets.UTF_8).trim();
    String beamSubdir = Integer.parseInt(otpRelease) >= 27 ? "otp_27_plus" : "otp_pre_27";
    for (String beam : DEBUGGER_BEAMS) {
      try (InputStream in = ResourceUtil.getResourceAsStream(ErlangDebuggerNode.class.getClassLoader(),
                                                             "/debugger/beams/" + beamSubdir, beam)) {
        assertNotNull("Failed to locate debugger module: " + beam, in);
        Files.write(new File(directory, beam).toPath(), in.readAllBytes());
      }
    }
  }

  private static class RecordingListener implements ErlangDebuggerEventListener {
    private final BlockingQueue<String> myEvents;

    RecordingListener(BlockingQueue<String> events) {
      myEvents = events;
    }

    @Override
    public void debuggerStarted() {
    }

    @Override
    public void failedToInterpretModules(String nodeName, List<String> modules) {
      myEvents.add("failed to interpret " + modules);
    }

    @Override
    public void failedToDebugRemoteNode(String nodeName, String error) {
      myEvents.add("failed to debug remote node " + error);
    }

    @Override
    public void unknownMessage(String messageText) {
      myEvents.add("unknown message " + messageText);
    }

    @Override
    public void failedToSetBreakpoint(String module, int line, String errorMessage) {
      myEvents.add("failed to set breakpoint " + module + ":" + line + " " + errorMessage);
    }

    @Override
    public void breakpointIsSet(String module, int line) {
    }

    @Override
    public void breakpointReached(OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots) {
      for (ErlangProcessSnapshot snapshot : snapshots) {
        if (snapshot.getPid().equals(pid)) {
          myEvents.add("breakpoint " + snapshot.getBreakModule() + ":" + snapshot.getBreakLine());
          return;
        }
      }
      myEvents.add("breakpoint in an unknown process " + pid);
    }

    @Override
    public void debuggerStopped() {
      myEvents.add("debugger stopped");
    }

    @Override
    public void handleEvaluationResponse(OtpErlangObject response) {
    }
  }
}
