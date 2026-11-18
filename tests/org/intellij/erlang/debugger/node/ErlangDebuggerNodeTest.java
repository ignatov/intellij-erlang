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
import com.intellij.testFramework.LoggedErrorProcessor;
import com.intellij.util.ThrowableRunnable;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
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

  // #1289: a peer that declares a frame of 0x7FFFFFFF bytes must not make the node allocate it. HotSpot always rejects
  // new byte[Integer.MAX_VALUE] with "OutOfMemoryError: Requested array size exceeds VM limit".
  public void testHugeFrameLengthDoesNotAllocate() throws Throwable {
    List<Throwable> logged = Collections.synchronizedList(new ArrayList<>());
    CountDownLatch errorLogged = new CountDownLatch(1);
    LoggedErrorProcessor processor = new LoggedErrorProcessor() {
      @Override
      public @NotNull Set<Action> processError(@NotNull String category, @NotNull String message, String @NotNull [] details,
                                               @Nullable Throwable t) {
        logged.add(t != null ? t : new Throwable(message));
        errorLogged.countDown();
        return EnumSet.noneOf(Action.class);
      }
    };
    LoggedErrorProcessor.executeWith(processor, (ThrowableRunnable<Throwable>)() -> {
      CountDownLatch stopped = new CountDownLatch(1);
      ErlangDebuggerNode node = new ErlangDebuggerNode(new RecordingListener(new CompletableFuture<>()) {
        @Override public void debuggerStopped() { stopped.countDown(); }
      });
      try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
        DataOutputStream out = new DataOutputStream(peer.getOutputStream());
        out.writeInt(0x7FFFFFFF);
        out.flush();
        stopped.await(TIMEOUT_SECONDS, TimeUnit.SECONDS);
        // The pooled thread logs an escaping Error after debuggerStopped() has run in the finally block.
        errorLogged.await(2, TimeUnit.SECONDS);
      }
      finally {
        node.stop();
      }
    });
    for (Throwable t : logged) {
      for (Throwable c = t; c != null; c = c.getCause()) {
        if (c instanceof OutOfMemoryError) throw new AssertionError("The node allocated the declared frame length: " + c, t);
      }
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
}
