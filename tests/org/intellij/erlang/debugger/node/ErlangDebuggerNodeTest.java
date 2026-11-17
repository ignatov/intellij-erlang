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
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import org.jetbrains.annotations.NotNull;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public class ErlangDebuggerNodeTest extends BasePlatformTestCase {
  private static final OtpErlangPid PID = new OtpErlangPid("nonode@nohost", 91, 0, 0);
  private static final String SENTINEL_MODULE = "sentinel";

  private final CountDownLatch myBreakpointReached = new CountDownLatch(1);
  private final CountDownLatch myStopped = new CountDownLatch(1);
  private ErlangDebuggerNode myNode;
  private Socket myPeer;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myNode = new ErlangDebuggerNode(new Listener());
    myPeer = new Socket(InetAddress.getLoopbackAddress(), myNode.getLocalDebuggerPort());
    myPeer.setSoTimeout(10_000);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      myNode.stop();
      myPeer.close();
      myStopped.await(10, TimeUnit.SECONDS);
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  public void testEvaluateIsSentForASuspendedProcess() throws Exception {
    reachBreakpoint();
    myNode.evaluate("A", traceElement());
    List<OtpErlangTuple> commands = commandsUpToSentinel();
    assertEquals(1, evaluateCommandsFor(PID, commands));
  }

  public void testNoEvaluateIsSentForAResumedProcess() throws Exception {
    reachBreakpoint();
    myNode.resume();
    myNode.evaluate("A", traceElement());
    List<OtpErlangTuple> commands = commandsUpToSentinel();
    // the interpreter's meta process exits with function_clause on {eval, ...} unless the process is at a break
    assertEquals("evaluate sent for a resumed process: " + commands, 0, evaluateCommandsFor(PID, commands));
  }

  private void reachBreakpoint() throws Exception {
    OtpErlangTuple init = tuple(new OtpErlangAtom("tgt"), new OtpErlangAtom("main"), new OtpErlangList());
    OtpErlangTuple frame = tuple(new OtpErlangLong(2), init,
                                 new OtpErlangList(tuple(new OtpErlangAtom("A"), new OtpErlangLong(1))));
    OtpErlangTuple snapshot = tuple(PID, init, new OtpErlangAtom("break"),
                                    tuple(new OtpErlangAtom("tgt"), new OtpErlangLong(5)), new OtpErlangList(frame));
    send(tuple(new OtpErlangAtom("breakpoint_reached"), PID, new OtpErlangList(snapshot)));
    assertTrue("breakpoint_reached was not processed", myBreakpointReached.await(10, TimeUnit.SECONDS));
  }

  @NotNull
  private static ErlangTraceElement traceElement() {
    return new ErlangTraceElement(new OtpErlangLong(2), "tgt", "main", new OtpErlangList(), Collections.emptyList());
  }

  @NotNull
  private List<OtpErlangTuple> commandsUpToSentinel() throws Exception {
    myNode.setBreakpoint(SENTINEL_MODULE, 0);
    List<OtpErlangTuple> commands = new ArrayList<>();
    while (true) {
      OtpErlangTuple command = (OtpErlangTuple) receive();
      if ("set_breakpoint".equals(((OtpErlangAtom) command.elementAt(0)).atomValue()) &&
          SENTINEL_MODULE.equals(((OtpErlangAtom) command.elementAt(1)).atomValue())) {
        return commands;
      }
      commands.add(command);
    }
  }

  private static int evaluateCommandsFor(@NotNull OtpErlangPid pid, @NotNull List<OtpErlangTuple> commands) {
    int count = 0;
    for (OtpErlangTuple command : commands) {
      if ("evaluate".equals(((OtpErlangAtom) command.elementAt(0)).atomValue()) && pid.equals(command.elementAt(1))) {
        count++;
      }
    }
    return count;
  }

  private void send(@NotNull OtpErlangObject message) throws IOException {
    byte[] bytes = new OtpOutputStream(message).toByteArray();
    OutputStream out = myPeer.getOutputStream();
    out.write(ByteBuffer.allocate(4).putInt(1 + bytes.length).array());
    out.write(OtpExternal.versionTag);
    out.write(bytes);
    out.flush();
  }

  @NotNull
  private OtpErlangObject receive() throws Exception {
    DataInputStream in = new DataInputStream(myPeer.getInputStream());
    byte[] bytes = new byte[in.readInt()];
    in.readFully(bytes);
    return new OtpInputStream(bytes).read_any();
  }

  @NotNull
  private static OtpErlangTuple tuple(OtpErlangObject... elements) {
    return new OtpErlangTuple(elements);
  }

  private class Listener implements ErlangDebuggerEventListener {
    @Override public void debuggerStarted() { }
    @Override public void failedToInterpretModules(String nodeName, List<String> modules) { }
    @Override public void failedToDebugRemoteNode(String nodeName, String error) { }
    @Override public void unknownMessage(String messageText) { }
    @Override public void failedToSetBreakpoint(String module, int line, String errorMessage) { }
    @Override public void breakpointIsSet(String module, int line) { }
    @Override public void breakpointReached(OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots) { myBreakpointReached.countDown(); }
    @Override public void debuggerStopped() { myStopped.countDown(); }
    @Override public void handleEvaluationResponse(OtpErlangObject response) { }
  }
}
