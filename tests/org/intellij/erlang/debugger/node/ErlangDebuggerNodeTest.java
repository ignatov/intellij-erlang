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

package org.intellij.erlang.debugger.node;

import com.ericsson.otp.erlang.*;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

public class ErlangDebuggerNodeTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final OtpErlangPid PID_A = new OtpErlangPid("debuggee@localhost", 101, 0, 1);
  private static final OtpErlangPid PID_B = new OtpErlangPid("debuggee@localhost", 102, 0, 1);

  private final BlockingQueue<List<ErlangProcessSnapshot>> myBreakpointsReached = new LinkedBlockingQueue<>();
  private ErlangDebuggerNode myNode;
  private Socket myPeer;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myNode = new ErlangDebuggerNode(new RecordingListener());
    myPeer = new Socket(InetAddress.getLoopbackAddress(), myNode.getLocalDebuggerPort());
    myPeer.setSoTimeout(10_000);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      myNode.stop();
      myPeer.close();
    }
    finally {
      super.tearDown();
    }
  }

  // Two processes run the same function and reach the same breakpoint: A first, then B (the second
  // snapshot lists both at the break), as remote_debugger_notifier announces them. Resume must not
  // leave a process that was announced at a breakpoint suspended there.
  public void testResumeReachesEveryProcessSuspendedAtABreakpoint() throws Exception {
    announceTwoProcessesAtBreakpoint();

    myNode.resume();

    List<OtpErlangTuple> commands = readCommandsUpToSentinel();
    assertContainsElements(commands, pidCommand("continue", PID_A), pidCommand("continue", PID_B));
  }

  // Evaluating in a frame of A's stack must evaluate in process A, not in the process that broke last.
  public void testEvaluateInFrameOfFirstProcessTargetsThatProcess() throws Exception {
    List<ErlangProcessSnapshot> snapshots = announceTwoProcessesAtBreakpoint();
    ErlangProcessSnapshot snapshotOfA = snapshots.stream().filter(s -> s.getPid().equals(PID_A)).findFirst().orElseThrow();

    myNode.evaluate("X.", snapshotOfA.getStack().get(0));

    List<OtpErlangTuple> commands = readCommandsUpToSentinel();
    assertEquals(1, commands.size());
    OtpErlangTuple evaluate = commands.get(0);
    assertEquals(new OtpErlangAtom("evaluate"), evaluate.elementAt(0));
    assertEquals(PID_A, evaluate.elementAt(1));
  }

  private List<ErlangProcessSnapshot> announceTwoProcessesAtBreakpoint() throws Exception {
    writeFrame(breakpointReached(PID_A, snapshot(PID_A)));
    assertNotNull(myBreakpointsReached.poll(10, TimeUnit.SECONDS));
    writeFrame(breakpointReached(PID_B, snapshot(PID_A), snapshot(PID_B)));
    List<ErlangProcessSnapshot> snapshots = myBreakpointsReached.poll(10, TimeUnit.SECONDS);
    assertNotNull(snapshots);
    return snapshots;
  }

  private List<OtpErlangTuple> readCommandsUpToSentinel() throws Exception {
    myNode.removeBreakpoint("sentinel", 0);
    List<OtpErlangTuple> commands = new ArrayList<>();
    DataInputStream in = new DataInputStream(myPeer.getInputStream());
    while (true) {
      byte[] bytes = new byte[in.readInt()];
      in.readFully(bytes);
      OtpErlangTuple command = (OtpErlangTuple) new OtpInputStream(bytes).read_any();
      if (command.elementAt(0).equals(new OtpErlangAtom("remove_breakpoint")) &&
          command.elementAt(1).equals(new OtpErlangAtom("sentinel"))) {
        return commands;
      }
      commands.add(command);
    }
  }

  private void writeFrame(OtpErlangObject term) throws IOException {
    byte[] bytes = new OtpOutputStream(term).toByteArray();
    OutputStream out = myPeer.getOutputStream();
    out.write(ByteBuffer.allocate(4).putInt(1 + bytes.length).array());
    out.write(OtpExternal.versionTag);
    out.write(bytes);
    out.flush();
  }

  private static OtpErlangTuple pidCommand(String name, OtpErlangPid pid) {
    return new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom(name), pid});
  }

  // {breakpoint_reached, ActivePid, [{Pid, Init, Status, Info, Stack}]}
  private static OtpErlangTuple breakpointReached(OtpErlangPid activePid, OtpErlangTuple... snapshots) {
    return new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom("breakpoint_reached"), activePid, new OtpErlangList(snapshots)});
  }

  private static OtpErlangTuple snapshot(OtpErlangPid pid) {
    OtpErlangTuple mfa = new OtpErlangTuple(new OtpErlangObject[]{
      new OtpErlangAtom("worker"), new OtpErlangAtom("loop"), new OtpErlangList()});
    OtpErlangTuple frame = new OtpErlangTuple(new OtpErlangObject[]{
      new OtpErlangInt(1), mfa,
      new OtpErlangList(new OtpErlangObject[]{new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom("X"), pid})})});
    return new OtpErlangTuple(new OtpErlangObject[]{
      pid, mfa, new OtpErlangAtom("break"),
      new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom("worker"), new OtpErlangInt(5)}),
      new OtpErlangList(frame)});
  }

  private class RecordingListener implements ErlangDebuggerEventListener {
    @Override public void debuggerStarted() { }
    @Override public void failedToInterpretModules(String nodeName, List<String> modules) { }
    @Override public void failedToDebugRemoteNode(String nodeName, String error) { }
    @Override public void unknownMessage(String messageText) { }
    @Override public void failedToSetBreakpoint(String module, int line, String errorMessage) { }
    @Override public void breakpointIsSet(String module, int line) { }
    @Override public void debuggerStopped() { }
    @Override public void handleEvaluationResponse(OtpErlangObject response) { }

    @Override
    public void breakpointReached(OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots) {
      myBreakpointsReached.add(snapshots);
    }
  }
}
