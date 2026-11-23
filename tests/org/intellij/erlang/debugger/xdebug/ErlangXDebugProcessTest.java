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
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.xdebugger.XDebugSession;
import com.intellij.xdebugger.XDebuggerUtil;
import com.intellij.xdebugger.XSourcePosition;
import com.intellij.xdebugger.breakpoints.XLineBreakpoint;
import com.intellij.xdebugger.evaluation.XDebuggerEvaluator;
import com.intellij.xdebugger.frame.XValue;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;
import org.intellij.erlang.debugger.node.ErlangDebuggerNodeTest;
import org.intellij.erlang.debugger.node.ErlangProcessSnapshot;
import org.intellij.erlang.debugger.node.ErlangTraceElement;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.io.ByteArrayOutputStream;
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

  // While its evaluation waits in a receive of interpreted code the interpreter reports the process as waiting.
  public void testEvaluationOfProcessWaitingInTheEvaluationIsKeptAtAnotherBreakpoint() throws Exception {
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
      process.evaluateExpression("t6:wait(A)", watchAtMain, frame);
      assertEquals(main, ((OtpErlangTuple)receive(peer)).elementAt(1));

      OtpErlangPid child = new OtpErlangPid("nonode@nohost", 86, 0, 0);
      node.processSuspended(child);
      process.breakpointReached(child, List.of(new ErlangProcessSnapshot(main, frame, "waiting", null, 0, null, List.of()),
                                               new ErlangProcessSnapshot(child, frame, "break", null, 0, null, List.of())));
      RecordingCallback watchAtChild = new RecordingCallback();
      process.evaluateExpression("t6:wait(A)", watchAtChild, frame);

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

  // An evaluation queued for a process that the user resumes would reach the running process and kill it.
  public void testQueuedEvaluationOfResumedProcessIsNotSent() throws Exception {
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
      process.resume(null);
      assertEquals(List.of(new OtpErlangString("error: The process has been resumed")), watchAtChild.myValues);
      OtpErlangTuple resume = (OtpErlangTuple)receive(peer);
      assertEquals(atom("continue"), resume.elementAt(0));
      assertEquals(child, resume.elementAt(1));

      send(peer, new OtpErlangTuple(new OtpErlangObject[]{atom("evaluate_response"), new OtpErlangLong(10)}));
      assertTrue("the node did not deliver the answer", responses.await(TIMEOUT_SECONDS, TimeUnit.SECONDS));
      assertEquals(List.of(new OtpErlangLong(10)), watchAtMain.myValues);
      peer.setSoTimeout(500);
      try {
        fail("an evaluation was sent to the resumed process: " + receive(peer));
      }
      catch (SocketTimeoutException expected) {
      }
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

  // #1309: an answer too deeply nested to decode fails its own evaluation; the next queued one is still sent and answered,
  // and the session resumes once, after the last answer.
  public void testTooDeeplyNestedAnswerFailsOnlyItsOwnEvaluation() throws Exception {
    CountDownLatch responses = new CountDownLatch(2);
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
      RecordingCallback watchA = new RecordingCallback();
      RecordingCallback watchB = new RecordingCallback();
      process.evaluateExpression("T", watchA, frame);
      process.evaluateExpression("B", watchB, frame);

      assertEquals(new OtpErlangList("T"), ((OtpErlangTuple)receive(peer)).elementAt(2));
      sendBytes(peer, tooDeeplyNestedEvaluateResponse(ErlangDebuggerNodeTest.TOO_DEEP_TO_DECODE));
      assertEquals(new OtpErlangList("B"), ((OtpErlangTuple)receive(peer)).elementAt(2));
      send(peer, new OtpErlangTuple(new OtpErlangObject[]{atom("evaluate_response"), atom("b")}));
      assertTrue("the node did not deliver both answers", responses.await(TIMEOUT_SECONDS, TimeUnit.SECONDS));

      assertEquals(List.of(new OtpErlangString("error: The result is nested too deeply to show")), watchA.myValues);
      assertEquals(List.of(atom("b")), watchB.myValues);
      assertEquals(1, ContainerUtil.count(sessionCalls, "resume"::equals));
    }
    finally {
      node.stop();
    }
  }

  // #1309: an answer that decodes but overflows the stack while its watch renders it fails only its own evaluation.
  // handleEvaluationResponse has already taken it off the queue, so the next queued evaluation must keep its answer.
  public void testOverflowWhileDeliveringAnAnswerFailsOnlyItsOwnEvaluation() throws Exception {
    CountDownLatch responses = new CountDownLatch(2);
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
      RecordingCallback watchA = new OverflowingCallback();
      RecordingCallback watchB = new RecordingCallback();
      process.evaluateExpression("T", watchA, frame);
      process.evaluateExpression("B", watchB, frame);

      assertEquals(new OtpErlangList("T"), ((OtpErlangTuple)receive(peer)).elementAt(2));
      send(peer, new OtpErlangTuple(new OtpErlangObject[]{atom("evaluate_response"), atom("t")}));
      assertEquals(new OtpErlangList("B"), ((OtpErlangTuple)receive(peer)).elementAt(2));
      send(peer, new OtpErlangTuple(new OtpErlangObject[]{atom("evaluate_response"), atom("b")}));
      assertTrue("the node did not deliver both answers", responses.await(TIMEOUT_SECONDS, TimeUnit.SECONDS));

      assertEquals(List.of(new OtpErlangString("error: The result is nested too deeply to show")), watchA.myValues);
      assertEquals(List.of(atom("b")), watchB.myValues);
      assertEquals(List.of("pause", "pause", "resume"), sessionCalls);
    }
    finally {
      node.stop();
    }
  }

  // The same with a single evaluation: it fails, and the session paused for it resumes.
  public void testOverflowWhileDeliveringTheOnlyAnswerResumesTheSession() throws Exception {
    CountDownLatch responses = new CountDownLatch(1);
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
      RecordingCallback watch = new OverflowingCallback();
      process.evaluateExpression("T", watch, frame);

      receive(peer);
      send(peer, new OtpErlangTuple(new OtpErlangObject[]{atom("evaluate_response"), atom("t")}));
      assertTrue("the node did not deliver the answer", responses.await(TIMEOUT_SECONDS, TimeUnit.SECONDS));

      assertEquals(List.of(new OtpErlangString("error: The result is nested too deeply to show")), watch.myValues);
      assertEquals(List.of("pause", "resume"), sessionCalls);
    }
    finally {
      node.stop();
    }
  }

  // #1541: the platform marks the session as resumed before it calls runToPosition, so the suspended process
  // must be told to run on (to the line of the caret); otherwise the session shows a running process that never moves.
  public void testRunToPositionSendsACommandToTheDebuggerNode() throws Exception {
    ErlangDebuggerNode node = new ErlangDebuggerNode(new ResponseForwardingListener(new ErlangXDebugProcess[1], new CountDownLatch(0)));
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      ErlangXDebugProcess process = createProcessWithoutTarget(node);
      VirtualFile file = myFixture.configureByText("pricing.erl", """
        -module(pricing).
        -export([cheapest/1]).

        cheapest([H | T]) ->
          lists:foldl(fun(I, Best) when I < Best -> I; (_, Best) -> Best end, H, T).
        """).getVirtualFile();
      node.processSuspended(new OtpErlangPid("nonode@nohost", 85, 0, 0));

      XSourcePosition position = XDebuggerUtil.getInstance().createPosition(file, 4);
      assertNotNull(position);
      process.runToPosition(position, null);

      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      try {
        assertNotNull(receive(peer));
      }
      catch (SocketTimeoutException e) {
        fail("runToPosition sent no command to the debugger node in " + TIMEOUT_SECONDS + " s");
      }
    }
    finally {
      node.stop();
    }
  }

  // #1541: Run to Cursor sets a temporary breakpoint on the line of the caret and continues the suspended process.
  // When the process stops there, the temporary breakpoint is removed and the session shows the position.
  public void testRunToPositionStopsAtATemporaryBreakpointAndRemovesIt() throws Exception {
    ErlangDebuggerNode node = new ErlangDebuggerNode(new ResponseForwardingListener(new ErlangXDebugProcess[1], new CountDownLatch(0)));
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      List<String> sessionCalls = new CopyOnWriteArrayList<>();
      ErlangXDebugProcess process = createProcessWithoutTarget(node, sessionCalls);
      VirtualFile file = configurePricing();
      OtpErlangPid pid = new OtpErlangPid("nonode@nohost", 85, 0, 0);
      node.processSuspended(pid);

      process.runToPosition(XDebuggerUtil.getInstance().createPosition(file, 4), null);
      assertEquals(breakpointCommand("set_breakpoint", "pricing", 5), receive(peer));
      assertEquals(new OtpErlangTuple(new OtpErlangObject[]{atom("continue"), pid}), receive(peer));

      process.breakpointReached(pid, List.of(snapshotAt(pid, "pricing", 4)));
      assertEquals(breakpointCommand("remove_breakpoint", "pricing", 5), receive(peer));
      assertTrue(sessionCalls.toString(), sessionCalls.contains("positionReached"));
    }
    finally {
      node.stop();
    }
  }

  // #1541: a user breakpoint on the line of the caret must survive Run to Cursor, and its line gets no second breakpoint.
  public void testRunToPositionKeepsAUserBreakpointOnTheSameLine() throws Exception {
    ErlangDebuggerNode node = new ErlangDebuggerNode(new ResponseForwardingListener(new ErlangXDebugProcess[1], new CountDownLatch(0)));
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      List<String> sessionCalls = new CopyOnWriteArrayList<>();
      ErlangXDebugProcess process = createProcessWithoutTarget(node, sessionCalls);
      VirtualFile file = configurePricing();
      XSourcePosition position = XDebuggerUtil.getInstance().createPosition(file, 4);
      process.addBreakpoint(lineBreakpointAt(position));
      assertEquals(breakpointCommand("set_breakpoint", "pricing", 5), receive(peer));
      OtpErlangPid pid = new OtpErlangPid("nonode@nohost", 85, 0, 0);
      node.processSuspended(pid);

      process.runToPosition(position, null);
      assertEquals(new OtpErlangTuple(new OtpErlangObject[]{atom("continue"), pid}), receive(peer));
      process.breakpointReached(pid, List.of(snapshotAt(pid, "pricing", 4)));
      assertTrue(sessionCalls.toString(), sessionCalls.contains("breakpointReached"));
      assertNoCommand(peer);
    }
    finally {
      node.stop();
    }
  }

  // #1541: a user breakpoint added on the line of the caret while the process runs to it, then removed, keeps the
  // temporary breakpoint until the process stops; then it goes.
  public void testRemovingAUserBreakpointOnTheLineOfRunToPositionKeepsTheTemporaryOne() throws Exception {
    ErlangDebuggerNode node = new ErlangDebuggerNode(new ResponseForwardingListener(new ErlangXDebugProcess[1], new CountDownLatch(0)));
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      ErlangXDebugProcess process = createProcessWithoutTarget(node);
      VirtualFile file = configurePricing();
      XSourcePosition position = XDebuggerUtil.getInstance().createPosition(file, 4);
      OtpErlangPid pid = new OtpErlangPid("nonode@nohost", 85, 0, 0);
      node.processSuspended(pid);

      process.runToPosition(position, null);
      receive(peer);
      receive(peer);
      XLineBreakpoint<ErlangLineBreakpointProperties> breakpoint = lineBreakpointAt(position);
      process.addBreakpoint(breakpoint);
      assertEquals(breakpointCommand("set_breakpoint", "pricing", 5), receive(peer));
      process.removeBreakpoint(breakpoint);
      assertNoCommand(peer);

      process.breakpointReached(pid, List.of(snapshotAt(pid, "pricing", 4)));
      assertEquals(breakpointCommand("remove_breakpoint", "pricing", 5), receive(peer));
    }
    finally {
      node.stop();
    }
  }

  // #1541: when the temporary breakpoint cannot be set the process runs on as after Resume Program; the user is told,
  // and nothing is removed at the next suspend.
  public void testFailedRunToPositionBreakpointIsReportedAndNotRemoved() throws Exception {
    ErlangDebuggerNode node = new ErlangDebuggerNode(new ResponseForwardingListener(new ErlangXDebugProcess[1], new CountDownLatch(0)));
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      List<String> sessionCalls = new CopyOnWriteArrayList<>();
      ErlangXDebugProcess process = createProcessWithoutTarget(node, sessionCalls);
      VirtualFile file = configurePricing();
      OtpErlangPid pid = new OtpErlangPid("nonode@nohost", 85, 0, 0);
      node.processSuspended(pid);

      process.runToPosition(XDebuggerUtil.getInstance().createPosition(file, 4), null);
      receive(peer);
      receive(peer);
      process.failedToSetBreakpoint("pricing", 4, "break_exists");
      assertTrue(sessionCalls.toString(), sessionCalls.contains("reportMessage"));

      process.breakpointReached(pid, List.of(snapshotAt(pid, "pricing", 3)));
      assertNoCommand(peer);
    }
    finally {
      node.stop();
    }
  }

  // #1541: a breakpoint elsewhere that stops the process first removes the temporary breakpoint too.
  public void testRunToPositionBreakpointIsRemovedWhenAnotherBreakpointComesFirst() throws Exception {
    ErlangDebuggerNode node = new ErlangDebuggerNode(new ResponseForwardingListener(new ErlangXDebugProcess[1], new CountDownLatch(0)));
    try (Socket peer = new Socket(InetAddress.getLoopbackAddress(), node.getLocalDebuggerPort())) {
      peer.setSoTimeout(TIMEOUT_SECONDS * 1000);
      ErlangXDebugProcess process = createProcessWithoutTarget(node);
      VirtualFile file = configurePricing();
      process.addBreakpoint(lineBreakpointAt(XDebuggerUtil.getInstance().createPosition(file, 3)));
      receive(peer);
      OtpErlangPid pid = new OtpErlangPid("nonode@nohost", 85, 0, 0);
      node.processSuspended(pid);

      process.runToPosition(XDebuggerUtil.getInstance().createPosition(file, 4), null);
      receive(peer);
      receive(peer);
      process.breakpointReached(pid, List.of(snapshotAt(pid, "pricing", 3)));
      assertEquals(breakpointCommand("remove_breakpoint", "pricing", 5), receive(peer));
    }
    finally {
      node.stop();
    }
  }

  @NotNull
  private VirtualFile configurePricing() {
    return myFixture.configureByText("pricing.erl", """
      -module(pricing).
      -export([cheapest/1]).

      cheapest([H | T]) ->
        lists:foldl(fun(I, Best) when I < Best -> I; (_, Best) -> Best end, H, T).
      """).getVirtualFile();
  }

  @NotNull
  private static ErlangProcessSnapshot snapshotAt(@NotNull OtpErlangPid pid, @NotNull String module, int line) {
    ErlangTraceElement frame = new ErlangTraceElement(new OtpErlangLong(1), module, "cheapest", new OtpErlangList(),
                                                      Collections.emptyList());
    return new ErlangProcessSnapshot(pid, frame, "break", module, line, null, List.of(frame));
  }

  @SuppressWarnings("unchecked")
  @NotNull
  private static XLineBreakpoint<ErlangLineBreakpointProperties> lineBreakpointAt(@NotNull XSourcePosition position) {
    return (XLineBreakpoint<ErlangLineBreakpointProperties>)Proxy.newProxyInstance(
      ErlangXDebugProcessTest.class.getClassLoader(), new Class[]{XLineBreakpoint.class},
      (proxy, method, args) -> switch (method.getName()) {
        case "getSourcePosition" -> position;
        case "equals" -> proxy == args[0];
        case "hashCode" -> System.identityHashCode(proxy);
        default -> null;
      });
  }

  @NotNull
  private static OtpErlangTuple breakpointCommand(@NotNull String name, @NotNull String module, int line) {
    return new OtpErlangTuple(new OtpErlangObject[]{atom(name), atom(module), new OtpErlangInt(line)});
  }

  private static void assertNoCommand(@NotNull Socket peer) throws Exception {
    peer.setSoTimeout(500);
    try {
      fail("an unexpected command was sent: " + receive(peer));
    }
    catch (SocketTimeoutException expected) {
    }
  }

  // {evaluate_response, {{...{[]}...}}} with depth one-element tuples
  private static byte[] tooDeeplyNestedEvaluateResponse(int depth) {
    ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    bytes.write(OtpExternal.smallTupleTag);
    bytes.write(2);
    byte[] name = new OtpOutputStream(atom("evaluate_response")).toByteArray();
    bytes.write(name, 0, name.length);
    for (int i = 0; i < depth; i++) {
      bytes.write(OtpExternal.smallTupleTag);
      bytes.write(1);
    }
    bytes.write(OtpExternal.nilTag);
    return bytes.toByteArray();
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
        return method.getReturnType() == boolean.class ? Boolean.TRUE : null;
      });
    return new ErlangXDebugProcess(session, node, new ErlangDebugLocationResolver(getProject(), null, false));
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

  // stands in for a watch whose rendering of a decoded value overflows the stack
  private static class OverflowingCallback extends RecordingCallback {
    @Override
    public void evaluated(@NotNull XValue result) {
      throw new StackOverflowError();
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

    @Override
    public void evaluationFailed(String errorMessage) {
      try {
        myProcess[0].evaluationFailed(errorMessage);
      }
      finally {
        myResponses.countDown();
      }
    }

    @Override public void debuggerStarted() { }
    @Override public void failedToInterpretModules(String nodeName, List<String> modules) { }
    @Override public void failedToDebugRemoteNode(String nodeName, String error) { }
    @Override public void unknownMessage(String messageText) { }
    @Override public void failedToReceiveMessage(String error) { }
    @Override public void failedToSetBreakpoint(String module, int line, String errorMessage) { }
    @Override public void breakpointIsSet(String module, int line) { }
    @Override public void breakpointReached(OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots) { }
    @Override public void debuggerStopped() { }
  }
}
