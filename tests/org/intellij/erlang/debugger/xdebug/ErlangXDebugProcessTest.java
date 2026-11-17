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
import com.intellij.xdebugger.breakpoints.XLineBreakpoint;
import com.intellij.xdebugger.evaluation.XDebuggerEvaluator;
import com.intellij.xdebugger.frame.XValue;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;
import org.intellij.erlang.debugger.node.ErlangProcessSnapshot;
import org.intellij.erlang.debugger.node.ErlangTraceElement;
import org.intellij.erlang.debugger.node.ErlangVariableBinding;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import sun.misc.Unsafe;

import java.lang.reflect.Field;
import java.lang.reflect.Proxy;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Drives {@link ErlangXDebugProcess} with the messages that the Erlang side sends, without starting
 * a debugger node or an Erlang process: the process and its node are allocated without their constructors.
 */
public class ErlangXDebugProcessTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String MODULE_NAME = "evaluate_at_breakpoint";
  private static final int BREAKPOINT_LINE = 6; // the line of "  B." as the interpreter reports it

  private final List<String> mySessionCalls = new ArrayList<>();

  @Override
  protected String getTestDataPath() {
    return "testData/debugger/";
  }

  // #1273: after a top-frame evaluation, the OTP interpreter marks the process `break` again and the notifier
  // announces the same breakpoint a second time (observed with OTP 27: `{evaluate, Pid, "A", TopSP}` is followed
  // by one more breakpoint_reached for the same pid and line). That repeat must not become a new suspend.
  public void testRepeatedBreakAnnouncementAfterEvaluationIsNotANewSuspend() throws Exception {
    myFixture.configureByFile(MODULE_NAME + ".erl");
    ErlangXDebugProcess process = createProcess();
    OtpErlangPid pid = new OtpErlangPid("debuggee@localhost", 85, 0, 0);
    ErlangTraceElement topFrame = new ErlangTraceElement(new OtpErlangLong(2), MODULE_NAME, "f",
                                                         new OtpErlangList(new OtpErlangLong(1)),
                                                         List.of(new ErlangVariableBinding("A", new OtpErlangLong(1))));
    List<ErlangProcessSnapshot> snapshots = List.of(
      new ErlangProcessSnapshot(pid, topFrame, "break", MODULE_NAME, BREAKPOINT_LINE, null, List.of(topFrame)));

    process.breakpointReached(pid, snapshots);
    getDebuggerNode(process).processSuspended(pid);
    assertEquals(List.of("breakpointReached"), suspends());

    List<XValue> results = new ArrayList<>();
    process.evaluateExpression("A", new XDebuggerEvaluator.XEvaluationCallback() {
      @Override
      public void evaluated(@NotNull XValue result) {
        results.add(result);
      }

      @Override
      public void errorOccurred(@NotNull String errorMessage) {
        fail(errorMessage);
      }
    }, topFrame);
    process.handleEvaluationResponse(new OtpErlangLong(1));
    assertSize(1, results);

    // the announcement that follows every non-restricted evaluation, for the same pid and position
    process.breakpointReached(pid, snapshots);

    assertEquals("the repeated announcement after an evaluation was reported as a new suspend; session calls: " + mySessionCalls,
                 List.of("breakpointReached"), suspends());
  }

  private List<String> suspends() {
    return mySessionCalls.stream().filter(c -> c.equals("breakpointReached") || c.equals("positionReached")).toList();
  }

  @NotNull
  private ErlangXDebugProcess createProcess() throws Exception {
    Unsafe unsafe = getUnsafe();
    XDebugSession session = createRecordingSession();
    ErlangDebugLocationResolver resolver = new ErlangDebugLocationResolver(getProject(), getModule(), false);

    ErlangDebuggerNode node = (ErlangDebuggerNode) unsafe.allocateInstance(ErlangDebuggerNode.class);
    setField(ErlangDebuggerNode.class, node, "myCommandsQueue", new LinkedList<>());

    ErlangXDebugProcess process = (ErlangXDebugProcess) unsafe.allocateInstance(ErlangXDebugProcess.class);
    setField(com.intellij.xdebugger.XDebugProcess.class, process, "mySession", session);
    setField(ErlangXDebugProcess.class, process, "mySession", session);
    setField(ErlangXDebugProcess.class, process, "myDebuggerNode", node);
    setField(ErlangXDebugProcess.class, process, "myLocationResolver", resolver);

    ConcurrentHashMap<ErlangSourcePosition, XLineBreakpoint<ErlangLineBreakpointProperties>> breakpoints = new ConcurrentHashMap<>();
    ErlangSourcePosition breakpointPosition = ErlangSourcePosition.create(resolver, MODULE_NAME, BREAKPOINT_LINE);
    assertNotNull(breakpointPosition);
    breakpoints.put(breakpointPosition, createLineBreakpoint());
    setField(ErlangXDebugProcess.class, process, "myPositionToLineBreakpointMap", breakpoints);
    return process;
  }

  @NotNull
  private XDebugSession createRecordingSession() {
    return (XDebugSession) Proxy.newProxyInstance(getClass().getClassLoader(), new Class<?>[]{XDebugSession.class}, (proxy, method, args) -> {
      String name = method.getName();
      switch (name) {
        case "hashCode": return System.identityHashCode(proxy);
        case "equals": return proxy == args[0];
        case "toString": return "recording session";
      }
      mySessionCalls.add(name);
      if (name.equals("breakpointReached")) return true;
      return defaultValue(method.getReturnType());
    });
  }

  @SuppressWarnings("unchecked")
  @NotNull
  private static XLineBreakpoint<ErlangLineBreakpointProperties> createLineBreakpoint() {
    return (XLineBreakpoint<ErlangLineBreakpointProperties>) Proxy.newProxyInstance(
      ErlangXDebugProcessTest.class.getClassLoader(), new Class<?>[]{XLineBreakpoint.class}, (proxy, method, args) -> {
        switch (method.getName()) {
          case "hashCode": return System.identityHashCode(proxy);
          case "equals": return proxy == args[0];
          case "toString": return "line breakpoint";
        }
        return defaultValue(method.getReturnType());
      });
  }

  private static Object defaultValue(Class<?> type) {
    if (type == boolean.class) return false;
    if (type == int.class) return 0;
    if (type == long.class) return 0L;
    return null;
  }

  @NotNull
  private static ErlangDebuggerNode getDebuggerNode(@NotNull ErlangXDebugProcess process) throws Exception {
    Field field = ErlangXDebugProcess.class.getDeclaredField("myDebuggerNode");
    field.setAccessible(true);
    return (ErlangDebuggerNode) field.get(process);
  }

  private static void setField(@NotNull Class<?> owner, @NotNull Object target, @NotNull String name, Object value) throws Exception {
    Field field = owner.getDeclaredField(name);
    field.setAccessible(true);
    field.set(target, value);
  }

  @NotNull
  private static Unsafe getUnsafe() throws Exception {
    Field field = Unsafe.class.getDeclaredField("theUnsafe");
    field.setAccessible(true);
    return (Unsafe) field.get(null);
  }
}
