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

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;

public class ErlangDebuggerNodeTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final int TIMEOUT_SECONDS = 10;

  // The stack as remote_debugger_notifier sends it: int:meta(Meta, backtrace, all) gives integer call levels,
  // top frame first (observed with OTP 27: [{3,{t,f,[1]}},{2,{t,main,[]}}]).
  public void testEvaluateSendsTheStackPointerOfTheSelectedFrame() throws Exception {
    CompletableFuture<List<ErlangProcessSnapshot>> snapshotsFuture = new CompletableFuture<>();
    ErlangDebuggerNode node = new ErlangDebuggerNode(new RecordingListener(snapshotsFuture, new CompletableFuture<>(), true));
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

  // #1309: a frame binds T = lists:foldl(fun(I, A) -> {I, A} end, nil, lists:seq(1, 20000)), nested 20000 levels deep.
  // The session must survive such a snapshot and go on processing the next message.
  public void testDeeplyNestedTermDoesNotKillTheServerThread() throws Exception {
    CompletableFuture<List<ErlangProcessSnapshot>> snapshotsFuture = new CompletableFuture<>();
    CompletableFuture<Void> stoppedFuture = new CompletableFuture<>();
    ErlangDebuggerNode node = new ErlangDebuggerNode(new RecordingListener(snapshotsFuture, stoppedFuture, false));
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      OtpErlangPid pid = new OtpErlangPid("nonode@nohost", 85, 0, 0);
      OtpErlangAtom placeholder = atom("deep_term_placeholder");
      OtpErlangTuple frame = tuple(new OtpErlangLong(2), tuple(atom("t"), atom("main"), list()),
                                   list(tuple(atom("T"), placeholder)));
      OtpErlangTuple deepSnapshot = tuple(pid, tuple(atom("t"), atom("main"), list()), atom("break"),
                                          tuple(atom("t"), new OtpErlangLong(9)), list(frame));
      byte[] message = new OtpOutputStream(tuple(atom("breakpoint_reached"), pid, list(deepSnapshot))).toByteArray();
      sendBytes(peer, replace(message, new OtpOutputStream(placeholder).toByteArray(), nestedTuples(20000)));

      OtpErlangTuple snapshot = tuple(pid, tuple(atom("t"), atom("main"), list()), atom("break"),
                                      tuple(atom("t"), new OtpErlangLong(9)), list());
      send(peer, tuple(atom("breakpoint_reached"), pid, list(snapshot)));

      List<ErlangProcessSnapshot> snapshots = snapshotsFuture.get(TIMEOUT_SECONDS, TimeUnit.SECONDS);
      assertEquals(1, snapshots.size());
      assertFalse("the debugger session was stopped", stoppedFuture.isDone());
    }
    finally {
      node.stop();
    }
  }

  // A term that decodes can still be too deep for OtpErlangObject.toString(), which recurses with larger frames.
  // The depths at which decoding or toString() overflow shift with the JIT state, so try a range of them.
  public void testNestedTermsOfManyDepthsDoNotKillTheServerThread() throws Exception {
    OtpErlangPid deepPid = new OtpErlangPid("nonode@nohost", 85, 0, 0);
    OtpErlangPid lastPid = new OtpErlangPid("nonode@nohost", 86, 0, 0);
    CompletableFuture<List<ErlangProcessSnapshot>> snapshotsFuture = new CompletableFuture<>();
    CompletableFuture<Void> stoppedFuture = new CompletableFuture<>();
    ErlangDebuggerNode node = new ErlangDebuggerNode(new RecordingListener(snapshotsFuture, stoppedFuture, false) {
      @Override
      public void breakpointReached(OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots) {
        if (lastPid.equals(pid)) super.breakpointReached(pid, snapshots);
      }
    });
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      for (int round = 0; round < 5; round++) {
        for (int depth = 3000 + round * 50; depth <= 15000; depth += 250) {
          sendBytes(peer, breakpointReachedWithBinding(deepPid, nestedTuples(depth)));
          sendBytes(peer, breakpointReachedWithBinding(deepPid, nestedLists(depth)));
          sendBytes(peer, breakpointReachedWithBinding(deepPid, nestedMaps(depth)));
        }
      }
      send(peer, tuple(atom("breakpoint_reached"), lastPid, list(snapshot(lastPid, list()))));

      List<ErlangProcessSnapshot> snapshots = snapshotsFuture.get(TIMEOUT_SECONDS * 6, TimeUnit.SECONDS);
      assertEquals(lastPid, snapshots.get(0).getPid());
      assertFalse("the debugger session was stopped", stoppedFuture.isDone());
    }
    finally {
      node.stop();
    }
  }

  // An evaluate_response too deep to decode must end the evaluation, not leave it pending.
  public void testTooDeeplyNestedEvaluateResponseFailsTheEvaluation() throws Exception {
    CompletableFuture<String> evaluationErrorFuture = new CompletableFuture<>();
    CompletableFuture<Void> stoppedFuture = new CompletableFuture<>();
    ErlangDebuggerNode node = new ErlangDebuggerNode(new RecordingListener(new CompletableFuture<>(), stoppedFuture, false) {
      @Override
      public void evaluationFailed(String errorMessage) {
        evaluationErrorFuture.complete(errorMessage);
      }
    });
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      OtpErlangAtom placeholder = atom("deep_term_placeholder");
      byte[] message = new OtpOutputStream(tuple(atom("evaluate_response"), placeholder)).toByteArray();
      sendBytes(peer, replace(message, new OtpOutputStream(placeholder).toByteArray(), nestedTuples(20000)));

      assertEquals("The result is nested too deeply to show", evaluationErrorFuture.get(TIMEOUT_SECONDS, TimeUnit.SECONDS));
      assertFalse("the debugger session was stopped", stoppedFuture.isDone());
    }
    finally {
      node.stop();
    }
  }

  // With debug logging on (as under the test framework), a decoded term too deep for toString() in the
  // "Message received" line must still be delivered: only a term that cannot be decoded may be dropped.
  // The depths at which decoding or toString() overflow shift with the JIT state, so try a range of them.
  public void testDecodedTermTooDeepToLogIsDelivered() throws Exception {
    OtpErlangPid deepPid = new OtpErlangPid("nonode@nohost", 85, 0, 0);
    OtpErlangPid lastPid = new OtpErlangPid("nonode@nohost", 86, 0, 0);
    List<String> unknownMessages = new CopyOnWriteArrayList<>();
    CompletableFuture<List<ErlangProcessSnapshot>> snapshotsFuture = new CompletableFuture<>();
    CompletableFuture<Void> stoppedFuture = new CompletableFuture<>();
    ErlangDebuggerNode node = new ErlangDebuggerNode(new RecordingListener(snapshotsFuture, stoppedFuture, false) {
      @Override
      public void unknownMessage(String messageText) {
        unknownMessages.add(messageText);
      }

      @Override
      public void breakpointReached(OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots) {
        if (lastPid.equals(pid)) super.breakpointReached(pid, snapshots);
      }
    });
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      for (int round = 0; round < 3; round++) {
        for (int depth = 3000 + round * 50; depth <= 12000; depth += 250) {
          sendBytes(peer, breakpointReachedWithBinding(deepPid, nestedTuples(depth)));
          sendBytes(peer, breakpointReachedWithBinding(deepPid, nestedLists(depth)));
        }
      }
      send(peer, tuple(atom("breakpoint_reached"), lastPid, list(snapshot(lastPid, list()))));

      snapshotsFuture.get(TIMEOUT_SECONDS * 6, TimeUnit.SECONDS);
      assertFalse("the debugger session was stopped", stoppedFuture.isDone());
      for (String message : unknownMessages) {
        assertTrue("a message was dropped although it was decoded: " + message, message.endsWith("too deeply to decode"));
      }
    }
    finally {
      node.stop();
    }
  }

  // A watch or a value hint renders the evaluation result on the debugger server thread. An overflow there must
  // fail the evaluation and keep the session; the listener throws it to stand in for a value too deep to render.
  public void testOverflowWhileProcessingAnEvaluateResponseFailsTheEvaluation() throws Exception {
    CompletableFuture<String> evaluationErrorFuture = new CompletableFuture<>();
    CompletableFuture<Void> stoppedFuture = new CompletableFuture<>();
    ErlangDebuggerNode node = new ErlangDebuggerNode(new RecordingListener(new CompletableFuture<>(), stoppedFuture, false) {
      @Override
      public void handleEvaluationResponse(OtpErlangObject response) {
        throw new StackOverflowError();
      }

      @Override
      public void evaluationFailed(String errorMessage) {
        evaluationErrorFuture.complete(errorMessage);
      }
    });
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      send(peer, tuple(atom("evaluate_response"), tuple(new OtpErlangLong(1), atom("nil"))));

      assertEquals("The result is nested too deeply to show", evaluationErrorFuture.get(TIMEOUT_SECONDS, TimeUnit.SECONDS));
      assertFalse("the debugger session was stopped", stoppedFuture.isDone());
    }
    finally {
      node.stop();
    }
  }

  private static byte[] breakpointReachedWithBinding(OtpErlangPid pid, byte[] value) {
    OtpErlangAtom placeholder = atom("deep_term_placeholder");
    OtpErlangTuple frame = tuple(new OtpErlangLong(2), tuple(atom("t"), atom("main"), list()),
                                 list(tuple(atom("T"), placeholder)));
    byte[] message = new OtpOutputStream(tuple(atom("breakpoint_reached"), pid, list(snapshot(pid, list(frame))))).toByteArray();
    return replace(message, new OtpOutputStream(placeholder).toByteArray(), value);
  }

  private static OtpErlangTuple snapshot(OtpErlangPid pid, OtpErlangList stack) {
    return tuple(pid, tuple(atom("t"), atom("main"), list()), atom("break"), tuple(atom("t"), new OtpErlangLong(9)), stack);
  }

  // {1, {1, ... {1, nil} ...}} in the external term format, without the version tag.
  private static byte[] nestedTuples(int depth) {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    for (int i = 0; i < depth; i++) {
      out.write(OtpExternal.smallTupleTag);
      out.write(2);
      out.write(OtpExternal.smallIntTag);
      out.write(1);
    }
    out.write(OtpExternal.nilTag);
    return out.toByteArray();
  }

  // [[ ... [] ... ]] in the external term format, without the version tag.
  private static byte[] nestedLists(int depth) {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    for (int i = 0; i < depth; i++) {
      out.write(OtpExternal.listTag);
      out.writeBytes(new byte[]{0, 0, 0, 1});
    }
    for (int i = 0; i <= depth; i++) {
      out.write(OtpExternal.nilTag);
    }
    return out.toByteArray();
  }

  // #{1 => #{1 => ... #{1 => nil} ...}} in the external term format, without the version tag.
  private static byte[] nestedMaps(int depth) {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    for (int i = 0; i < depth; i++) {
      out.write(OtpExternal.mapTag);
      out.writeBytes(new byte[]{0, 0, 0, 1});
      out.write(OtpExternal.smallIntTag);
      out.write(1);
    }
    out.write(OtpExternal.nilTag);
    return out.toByteArray();
  }

  private static byte[] replace(byte[] bytes, byte[] target, byte[] replacement) {
    for (int i = 0; i + target.length <= bytes.length; i++) {
      if (Arrays.equals(Arrays.copyOfRange(bytes, i, i + target.length), target)) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        out.write(bytes, 0, i);
        out.write(replacement, 0, replacement.length);
        out.write(bytes, i + target.length, bytes.length - i - target.length);
        return out.toByteArray();
      }
    }
    throw new AssertionError("placeholder not found");
  }

  private static void send(Socket socket, OtpErlangObject message) throws Exception {
    sendBytes(socket, new OtpOutputStream(message).toByteArray());
  }

  private static void sendBytes(Socket socket, byte[] bytes) throws Exception {
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
    private final CompletableFuture<Void> myStopped;
    private final boolean myFailOnUnknownMessage;

    RecordingListener(CompletableFuture<List<ErlangProcessSnapshot>> snapshots, CompletableFuture<Void> stopped,
                      boolean failOnUnknownMessage) {
      mySnapshots = snapshots;
      myStopped = stopped;
      myFailOnUnknownMessage = failOnUnknownMessage;
    }

    @Override public void debuggerStarted() { }
    @Override public void failedToInterpretModules(String nodeName, List<String> modules) { }
    @Override public void failedToDebugRemoteNode(String nodeName, String error) { }
    @Override public void unknownMessage(String messageText) {
      if (myFailOnUnknownMessage) mySnapshots.completeExceptionally(new AssertionError(messageText));
    }
    @Override public void failedToSetBreakpoint(String module, int line, String errorMessage) { }
    @Override public void breakpointIsSet(String module, int line) { }
    @Override public void breakpointReached(OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots) { mySnapshots.complete(snapshots); }
    @Override public void debuggerStopped() { myStopped.complete(null); }
    @Override public void handleEvaluationResponse(OtpErlangObject response) { }
    @Override public void evaluationFailed(String errorMessage) { }
  }
}
