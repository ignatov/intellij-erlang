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
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.net.InetAddress;
import java.net.Socket;
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
      assertEquals(atom("evaluate"), ((OtpErlangTuple)receive(peer)).elementAt(0));

      send(peer, new OtpErlangTuple(new OtpErlangObject[]{atom("evaluate_response"), new OtpErlangLong(1)}));
      send(peer, new OtpErlangTuple(new OtpErlangObject[]{atom("evaluate_response"), atom("b")}));
      assertTrue("the node did not deliver both answers", responses.await(TIMEOUT_SECONDS, TimeUnit.SECONDS));

      assertEquals("watch A (watch B got " + watchB.myValues + ")", List.of(new OtpErlangLong(1)), watchA.myValues);
      assertEquals("watch B", List.of(atom("b")), watchB.myValues);
    }
    finally {
      node.stop();
    }
  }

  // The production constructor starts an Erlang node and a run configuration; this one only wires
  // the session and the debugger node, which is all evaluateExpression and handleEvaluationResponse use.
  @NotNull
  private static ErlangXDebugProcess createProcessWithoutTarget(@NotNull ErlangDebuggerNode node) throws Exception {
    Field unsafeField = Class.forName("sun.misc.Unsafe").getDeclaredField("theUnsafe");
    unsafeField.setAccessible(true);
    Object unsafe = unsafeField.get(null);
    Method allocateInstance = unsafe.getClass().getMethod("allocateInstance", Class.class);
    ErlangXDebugProcess process = (ErlangXDebugProcess)allocateInstance.invoke(unsafe, ErlangXDebugProcess.class);
    XDebugSession session = (XDebugSession)Proxy.newProxyInstance(
      ErlangXDebugProcessTest.class.getClassLoader(), new Class[]{XDebugSession.class}, (proxy, method, args) -> null);
    setField(process, "mySession", session);
    setField(process, "myDebuggerNode", node);
    return process;
  }

  private static void setField(Object target, String name, Object value) throws Exception {
    Field field = ErlangXDebugProcess.class.getDeclaredField(name);
    field.setAccessible(true);
    field.set(target, value);
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
