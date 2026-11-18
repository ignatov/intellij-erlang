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
import java.io.DataOutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

public class ErlangDebuggerNodeTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final int TIMEOUT_SECONDS = 10;

  // The stack as remote_debugger_notifier sends it: int:meta(Meta, backtrace, all) gives integer call levels,
  // top frame first (observed with OTP 27: [{3,{t,f,[1]}},{2,{t,main,[]}}]).
  public void testEvaluateSendsTheStackPointerOfTheSelectedFrame() throws Exception {
    CompletableFuture<List<ErlangProcessSnapshot>> snapshotsFuture = new CompletableFuture<>();
    ErlangDebuggerNode node = new ErlangDebuggerNode(new RecordingListener(snapshotsFuture));
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      OtpErlangPid pid = new OtpErlangPid("nonode@nohost", 85, 0, 0);
      OtpErlangTuple fFrame = tuple(new OtpErlangLong(3), tuple(atom("t"), atom("f"), list(new OtpErlangLong(1))),
                                    list(tuple(atom("A"), new OtpErlangLong(1))));
      OtpErlangTuple mainFrame = tuple(new OtpErlangLong(2), tuple(atom("t"), atom("main"), list()),
                                       list(tuple(atom("X"), new OtpErlangLong(1))));
      OtpErlangTuple snapshot = tuple(pid, tuple(atom("t"), atom("main"), list()), atom("break"),
                                      tuple(atom("t"), new OtpErlangLong(9)), list(fFrame, mainFrame));
      send(peer, tuple(atom("breakpoint_reached"), pid, list(snapshot)));

      List<ErlangProcessSnapshot> snapshots = snapshotsFuture.get(TIMEOUT_SECONDS, TimeUnit.SECONDS);
      ErlangTraceElement mainTraceElement = snapshots.get(0).getStack().get(1);
      assertEquals("main", mainTraceElement.getFunction());

      node.evaluate("X", mainTraceElement);
      OtpErlangTuple evaluate = (OtpErlangTuple)receive(peer);
      assertEquals(atom("evaluate"), evaluate.elementAt(0));
      assertEquals(pid, evaluate.elementAt(1));
      assertEquals(new OtpErlangLong(2), evaluate.elementAt(3));
    }
    finally {
      node.stop();
    }
  }

  // #1305: a message that stalls after its length and part of its body must end the session (readBytes promises a
  // SocketException once the retries on timeout are used up), not be dropped so that its rest is read as a new message.
  public void testStalledFrameEndsTheSession() throws Exception {
    CompletableFuture<Void> stoppedFuture = new CompletableFuture<>();
    CompletableFuture<OtpErlangObject> evaluationFuture = new CompletableFuture<>();
    ErlangDebuggerNode node = new ErlangDebuggerNode(new StallListener(stoppedFuture, evaluationFuture));
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      byte[] inner = new OtpOutputStream(tuple(atom("evaluate_response"), atom("desync"))).toByteArray();
      int innerSize = 1 + inner.length;
      DataOutputStream out = new DataOutputStream(peer.getOutputStream());
      out.writeInt(4 + 4 + innerSize);
      out.write(new byte[4]);
      out.flush();

      peer.setSoTimeout(5000);
      int read;
      try {
        read = peer.getInputStream().read();
      }
      catch (SocketTimeoutException e) {
        out.writeInt(innerSize);
        out.write(OtpExternal.versionTag);
        out.write(inner);
        out.flush();
        OtpErlangObject response = evaluationFuture.get(TIMEOUT_SECONDS, TimeUnit.SECONDS);
        fail("The node kept the session open after a stalled message and parsed the rest of its body as a new message: " +
             "handleEvaluationResponse(" + response + ")");
        return;
      }
      assertEquals("The node should close the connection", -1, read);
      stoppedFuture.get(TIMEOUT_SECONDS, TimeUnit.SECONDS);
    }
    finally {
      node.stop();
    }
  }

  private static void send(Socket socket, OtpErlangObject message) throws Exception {
    byte[] bytes = new OtpOutputStream(message).toByteArray();
    DataOutputStream out = new DataOutputStream(socket.getOutputStream());
    out.writeInt(1 + bytes.length);
    out.write(OtpExternal.versionTag);
    out.write(bytes);
    out.flush();
  }

  private static OtpErlangObject receive(Socket socket) throws Exception {
    DataInputStream in = new DataInputStream(socket.getInputStream());
    byte[] bytes = new byte[in.readInt()];
    in.readFully(bytes);
    return new OtpInputStream(bytes).read_any();
  }

  private static OtpErlangAtom atom(String name) {
    return new OtpErlangAtom(name);
  }

  private static OtpErlangTuple tuple(OtpErlangObject... elements) {
    return new OtpErlangTuple(elements);
  }

  private static OtpErlangList list(OtpErlangObject... elements) {
    return new OtpErlangList(elements);
  }

  private static class RecordingListener implements ErlangDebuggerEventListener {
    private final CompletableFuture<List<ErlangProcessSnapshot>> mySnapshots;

    RecordingListener(CompletableFuture<List<ErlangProcessSnapshot>> snapshots) {
      mySnapshots = snapshots;
    }

    @Override public void debuggerStarted() { }
    @Override public void failedToInterpretModules(String nodeName, List<String> modules) { }
    @Override public void failedToDebugRemoteNode(String nodeName, String error) { }
    @Override public void unknownMessage(String messageText) { mySnapshots.completeExceptionally(new AssertionError(messageText)); }
    @Override public void failedToSetBreakpoint(String module, int line, String errorMessage) { }
    @Override public void breakpointIsSet(String module, int line) { }
    @Override public void breakpointReached(OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots) { mySnapshots.complete(snapshots); }
    @Override public void debuggerStopped() { }
    @Override public void handleEvaluationResponse(OtpErlangObject response) { }
  }

  private static class StallListener implements ErlangDebuggerEventListener {
    private final CompletableFuture<Void> myStopped;
    private final CompletableFuture<OtpErlangObject> myEvaluationResponse;

    StallListener(CompletableFuture<Void> stopped, CompletableFuture<OtpErlangObject> evaluationResponse) {
      myStopped = stopped;
      myEvaluationResponse = evaluationResponse;
    }

    @Override public void debuggerStarted() { }
    @Override public void failedToInterpretModules(String nodeName, List<String> modules) { }
    @Override public void failedToDebugRemoteNode(String nodeName, String error) { }
    @Override public void unknownMessage(String messageText) { }
    @Override public void failedToSetBreakpoint(String module, int line, String errorMessage) { }
    @Override public void breakpointIsSet(String module, int line) { }
    @Override public void breakpointReached(OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots) { }
    @Override public void debuggerStopped() { myStopped.complete(null); }
    @Override public void handleEvaluationResponse(OtpErlangObject response) { myEvaluationResponse.complete(response); }
  }
}
