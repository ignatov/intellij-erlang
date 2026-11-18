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

package org.intellij.erlang.debugger.xdebug;

import com.ericsson.otp.erlang.*;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.xdebugger.XDebugSession;
import com.intellij.xdebugger.evaluation.XDebuggerEvaluator;
import com.intellij.xdebugger.frame.XValue;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;
import org.intellij.erlang.debugger.node.ErlangProcessSnapshot;
import org.intellij.erlang.debugger.node.ErlangTraceElement;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public class ErlangXDebugProcessTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final int TIMEOUT_SECONDS = 10;

  // Two watches (A = 1, B = b) are evaluated at the same suspend before either answer arrives.
  // The debugger node answers in request order, so each callback must get the value of its own expression.
  public void testTwoPendingEvaluationsCompleteTheirOwnCallbacks() throws Exception {
    CountDownLatch responses = new CountDownLatch(2);
    ErlangXDebugProcess[] processHolder = new ErlangXDebugProcess[1];
    ErlangDebuggerNode node = new ErlangDebuggerNode(new ResponseForwardingListener(processHolder, responses));
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      ErlangXDebugProcess process = createProcessWithoutTarget(node);
      processHolder[0] = process;

      OtpErlangPid pid = new OtpErlangPid("nonode@nohost", 85, 0, 0);
      node.processSuspended(pid);
      ErlangTraceElement frame = new ErlangTraceElement(new OtpErlangLong(1), "t", "main", new OtpErlangList(),
                                                        Collections.emptyList());

      RecordingCallback watchA = new RecordingCallback();
      RecordingCallback watchB = new RecordingCallback();
      process.evaluateExpression("A", watchA, frame);
      process.evaluateExpression("B", watchB, frame);

      assertEquals(atom("evaluate"), ((OtpErlangTuple)receive(peer)).elementAt(0));
      send(peer, new OtpErlangTuple(new OtpErlangObject[]{atom("evaluate_response"), new OtpErlangLong(1)}));
      assertEquals(atom("evaluate"), ((OtpErlangTuple)receive(peer)).elementAt(0));
      send(peer, new OtpErlangTuple(new OtpErlangObject[]{atom("evaluate_response"), atom("b")}));
      assertTrue("the node did not deliver both answers", responses.await(TIMEOUT_SECONDS, TimeUnit.SECONDS));

      assertEquals("watch A (watch B got " + watchB.myValues + ")", List.of(new OtpErlangLong(1)), watchA.myValues);
      assertEquals("watch B", List.of(atom("b")), watchB.myValues);
    }
    finally {
      node.stop();
    }
  }

  // The meta process of the debugged pid loses an answer when a second evaluation reaches it while it is
  // evaluating the first one (dbg_icmd:handle_user_msg/4 raises function_clause), so only one may be outstanding.
  public void testNextEvaluationIsSentOnlyAfterTheAnswerToThePreviousOne() throws Exception {
    CountDownLatch firstResponse = new CountDownLatch(1);
    ErlangXDebugProcess[] processHolder = new ErlangXDebugProcess[1];
    ErlangDebuggerNode node = new ErlangDebuggerNode(new ResponseForwardingListener(processHolder, firstResponse));
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      ErlangXDebugProcess process = createProcessWithoutTarget(node);
      processHolder[0] = process;

      node.processSuspended(new OtpErlangPid("nonode@nohost", 85, 0, 0));
      ErlangTraceElement frame = new ErlangTraceElement(new OtpErlangLong(1), "t", "main", new OtpErlangList(),
                                                        Collections.emptyList());
      RecordingCallback watchA = new RecordingCallback();
      RecordingCallback watchB = new RecordingCallback();
      process.evaluateExpression("A + 1", watchA, frame);
      process.evaluateExpression("B", watchB, frame);

      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      assertEquals(new OtpErlangList("A + 1"), ((OtpErlangTuple)receive(peer)).elementAt(2));
      peer.setSoTimeout(500);
      try {
        fail("a second evaluation was sent before the first was answered: " + receive(peer));
      }
      catch (SocketTimeoutException expected) {
      }

      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      send(peer, new OtpErlangTuple(new OtpErlangObject[]{atom("evaluate_response"), new OtpErlangLong(2)}));
      assertTrue("the node did not deliver the answer", firstResponse.await(TIMEOUT_SECONDS, TimeUnit.SECONDS));
      assertEquals(new OtpErlangList("B"), ((OtpErlangTuple)receive(peer)).elementAt(2));
      assertEquals(List.of(new OtpErlangLong(2)), watchA.myValues);
      assertEquals(List.of(), watchB.myValues);
    }
    finally {
      node.stop();
    }
  }

  // Resuming after the first of three answers let the third evaluation reach a running process, which killed it.
  public void testSessionResumesOnlyAfterTheLastQueuedAnswer() throws Exception {
    CountDownLatch responses = new CountDownLatch(3);
    ErlangXDebugProcess[] processHolder = new ErlangXDebugProcess[1];
    ErlangDebuggerNode node = new ErlangDebuggerNode(new ResponseForwardingListener(processHolder, responses));
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      List<String> sessionCalls = new CopyOnWriteArrayList<>();
      ErlangXDebugProcess process = createProcessWithoutTarget(node, sessionCalls);
      processHolder[0] = process;

      node.processSuspended(new OtpErlangPid("nonode@nohost", 85, 0, 0));
      ErlangTraceElement frame = new ErlangTraceElement(new OtpErlangLong(1), "t", "main", new OtpErlangList(),
                                                        Collections.emptyList());
      process.evaluateExpression("A", new RecordingCallback(), frame);
      process.evaluateExpression("B", new RecordingCallback(), frame);
      process.evaluateExpression("L", new RecordingCallback(), frame);
      for (int i = 0; i < 3; i++) {
        receive(peer);
        send(peer, new OtpErlangTuple(new OtpErlangObject[]{atom("evaluate_response"), new OtpErlangLong(i)}));
      }
      assertTrue("the node did not deliver all answers", responses.await(TIMEOUT_SECONDS, TimeUnit.SECONDS));

      assertEquals(1, ContainerUtil.count(sessionCalls, "resume"::equals));
    }
    finally {
      node.stop();
    }
  }

  // An evaluation for a process that has left its breakpoint gets no answer; it must not block later evaluations.
  public void testEvaluationOfProcessNoLongerAtBreakDoesNotBlockLaterOnes() throws Exception {
    CountDownLatch responses = new CountDownLatch(1);
    ErlangXDebugProcess[] processHolder = new ErlangXDebugProcess[1];
    ErlangDebuggerNode node = new ErlangDebuggerNode(new ResponseForwardingListener(processHolder, responses));
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      ErlangXDebugProcess process = createProcessWithoutTarget(node);
      processHolder[0] = process;
      ErlangTraceElement frame = new ErlangTraceElement(new OtpErlangLong(1), "t", "main", new OtpErlangList(),
                                                        Collections.emptyList());

      OtpErlangPid child = new OtpErlangPid("nonode@nohost", 86, 0, 0);
      node.processSuspended(child);
      RecordingCallback watchAtChild = new RecordingCallback();
      process.evaluateExpression("A", watchAtChild, frame);
      assertEquals(child, ((OtpErlangTuple)receive(peer)).elementAt(1));

      OtpErlangPid main = new OtpErlangPid("nonode@nohost", 85, 0, 0);
      node.processSuspended(main);
      process.breakpointReached(main, List.of(new ErlangProcessSnapshot(child, frame, "exit", null, 0, null, List.of()),
                                              new ErlangProcessSnapshot(main, frame, "break", null, 0, null, List.of())));
      assertEquals(List.of(new OtpErlangString("error: The process is no longer at a breakpoint")), watchAtChild.myValues);

      RecordingCallback watchAtMain = new RecordingCallback();
      process.evaluateExpression("A", watchAtMain, frame);
      assertEquals(main, ((OtpErlangTuple)receive(peer)).elementAt(1));
      send(peer, new OtpErlangTuple(new OtpErlangObject[]{atom("evaluate_response"), new OtpErlangLong(10)}));
      assertTrue("the node did not deliver the answer", responses.await(TIMEOUT_SECONDS, TimeUnit.SECONDS));
      assertEquals(List.of(new OtpErlangLong(10)), watchAtMain.myValues);
      assertEquals(1, watchAtChild.myValues.size());
    }
    finally {
      node.stop();
    }
  }

  // While the interpreter evaluates an expression it reports the process as running; its answer is still on the way.
  public void testEvaluationOfProcessRunningTheEvaluationIsKeptAtAnotherBreakpoint() throws Exception {
    CountDownLatch responses = new CountDownLatch(1);
    ErlangXDebugProcess[] processHolder = new ErlangXDebugProcess[1];
    ErlangDebuggerNode node = new ErlangDebuggerNode(new ResponseForwardingListener(processHolder, responses));
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      ErlangXDebugProcess process = createProcessWithoutTarget(node);
      processHolder[0] = process;
      ErlangTraceElement frame = new ErlangTraceElement(new OtpErlangLong(1), "t", "main", new OtpErlangList(),
                                                        Collections.emptyList());

      OtpErlangPid main = new OtpErlangPid("nonode@nohost", 85, 0, 0);
      node.processSuspended(main);
      RecordingCallback watchAtMain = new RecordingCallback();
      process.evaluateExpression("t3:slow(A)", watchAtMain, frame);
      assertEquals(main, ((OtpErlangTuple)receive(peer)).elementAt(1));

      OtpErlangPid child = new OtpErlangPid("nonode@nohost", 86, 0, 0);
      node.processSuspended(child);
      process.breakpointReached(child, List.of(new ErlangProcessSnapshot(main, frame, "running", null, 0, null, List.of()),
                                               new ErlangProcessSnapshot(child, frame, "break", null, 0, null, List.of())));
      RecordingCallback watchAtChild = new RecordingCallback();
      process.evaluateExpression("t3:slow(A)", watchAtChild, frame);

      send(peer, new OtpErlangTuple(new OtpErlangObject[]{atom("evaluate_response"), new OtpErlangLong(10)}));
      assertTrue("the node did not deliver the answer", responses.await(TIMEOUT_SECONDS, TimeUnit.SECONDS));
      assertEquals(List.of(new OtpErlangLong(10)), watchAtMain.myValues);
      assertEquals(List.of(), watchAtChild.myValues);
      assertEquals(child, ((OtpErlangTuple)receive(peer)).elementAt(1));
    }
    finally {
      node.stop();
    }
  }

  // An evaluation that never gets an answer (the evaluated process is gone) must not wait for ever.
  public void testPendingEvaluationFailsWhenSessionStops() throws Exception {
    ErlangXDebugProcess[] processHolder = new ErlangXDebugProcess[1];
    ErlangDebuggerNode node = new ErlangDebuggerNode(new ResponseForwardingListener(processHolder, new CountDownLatch(0)));
    try (Socket ignored = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      ErlangXDebugProcess process = createProcessWithoutTarget(node);
      processHolder[0] = process;

      node.processSuspended(new OtpErlangPid("nonode@nohost", 85, 0, 0));
      ErlangTraceElement frame = new ErlangTraceElement(new OtpErlangLong(1), "t", "main", new OtpErlangList(),
                                                        Collections.emptyList());
      RecordingCallback watch = new RecordingCallback();
      process.evaluateExpression("A", watch, frame);
      process.stop();

      assertEquals(List.of(new OtpErlangString("error: The debug session has stopped")), watch.myValues);
      process.handleEvaluationResponse(new OtpErlangLong(1));
      assertEquals(1, watch.myValues.size());
    }
    finally {
      node.stop();
    }
  }

  @NotNull
  private ErlangXDebugProcess createProcessWithoutTarget(@NotNull ErlangDebuggerNode node) {
    return createProcessWithoutTarget(node, new CopyOnWriteArrayList<>());
  }

  @NotNull
  private ErlangXDebugProcess createProcessWithoutTarget(@NotNull ErlangDebuggerNode node, @NotNull List<String> sessionCalls) {
    XDebugSession session = (XDebugSession)Proxy.newProxyInstance(
      ErlangXDebugProcessTest.class.getClassLoader(), new Class[]{XDebugSession.class}, (proxy, method, args) -> {
        sessionCalls.add(method.getName());
        return null;
      });
    return new ErlangXDebugProcess(session, node, new ErlangDebugLocationResolver(getProject(), null, false));
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

  private static class RecordingCallback implements XDebuggerEvaluator.XEvaluationCallback {
    private final List<OtpErlangObject> myValues = new CopyOnWriteArrayList<>();

    @Override
    public void evaluated(@NotNull XValue result) {
      try {
        Method getValue = Class.forName("org.intellij.erlang.debugger.xdebug.xvalue.ErlangXValueBase")
          .getDeclaredMethod("getValue");
        getValue.setAccessible(true);
        myValues.add((OtpErlangObject)getValue.invoke(result));
      }
      catch (ReflectiveOperationException e) {
        throw new RuntimeException(e);
      }
    }

    @Override
    public void errorOccurred(@NotNull String errorMessage) {
      myValues.add(new OtpErlangString("error: " + errorMessage));
    }
  }

  private static class ResponseForwardingListener implements ErlangDebuggerEventListener {
    private final ErlangXDebugProcess[] myProcess;
    private final CountDownLatch myResponses;

    ResponseForwardingListener(ErlangXDebugProcess[] process, CountDownLatch responses) {
      myProcess = process;
      myResponses = responses;
    }

    @Override
    public void handleEvaluationResponse(OtpErlangObject response) {
      try {
        myProcess[0].handleEvaluationResponse(response);
      }
      finally {
        myResponses.countDown();
      }
    }

    @Override public void debuggerStarted() { }
    @Override public void failedToInterpretModules(String nodeName, List<String> modules) { }
    @Override public void failedToDebugRemoteNode(String nodeName, String error) { }
    @Override public void unknownMessage(String messageText) { }
    @Override public void failedToSetBreakpoint(String module, int line, String errorMessage) { }
    @Override public void breakpointIsSet(String module, int line) { }
    @Override public void breakpointReached(OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots) { }
    @Override public void debuggerStopped() { }
  }
}
