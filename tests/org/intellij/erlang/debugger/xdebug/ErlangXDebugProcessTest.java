/*
 * Copyright 2012-2015 Sergey Ignatov
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
import com.intellij.xdebugger.XDebugProcess;
import com.intellij.xdebugger.XDebugSession;
import com.intellij.xdebugger.evaluation.XDebuggerEvaluator;
import com.intellij.xdebugger.frame.XSuspendContext;
import com.intellij.xdebugger.frame.XValue;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;
import org.intellij.erlang.debugger.node.ErlangTraceElement;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.io.DataInputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Proxy;
import java.net.InetAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Drives {@link ErlangXDebugProcess} against a scripted peer that stands in for the Erlang debugger node.
 * The process is allocated without its constructor, which would start {@code erl}.
 */
public class ErlangXDebugProcessTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private ErlangXDebugProcess myProcess;
  private ErlangDebuggerNode myNode;
  private Socket myPeer;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myProcess = allocateWithoutConstructor(ErlangXDebugProcess.class);
    XDebugSession session = createSession(myProcess);
    setField(XDebugProcess.class, myProcess, "mySession", session);
    setField(ErlangXDebugProcess.class, myProcess, "mySession", session);
    myNode = new ErlangDebuggerNode(myProcess);
    setField(ErlangXDebugProcess.class, myProcess, "myDebuggerNode", myNode);
    myPeer = new Socket(InetAddress.getLoopbackAddress(), myNode.getLocalDebuggerPort());
    myPeer.setSoTimeout(10_000);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      if (myNode != null) myNode.stop();
      if (myPeer != null) myPeer.close();
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  public void testEvaluationDoesNotResumeTheDebuggee() throws Exception {
    OtpErlangPid pid = new OtpErlangPid("debuggee@localhost", 1, 0, 0);
    myNode.processSuspended(pid);

    List<XValue> values = new ArrayList<>();
    ErlangTraceElement traceElement = new ErlangTraceElement(null, "m", "f", new OtpErlangList(), Collections.emptyList());
    myProcess.evaluateExpression("X", new XDebuggerEvaluator.XEvaluationCallback() {
      @Override
      public void evaluated(@NotNull XValue result) {
        values.add(result);
      }

      @Override
      public void errorOccurred(@NotNull String errorMessage) {
        fail(errorMessage);
      }
    }, traceElement);
    // what the server thread of the node does when the debugger node answers {evaluate_response, 1}
    myProcess.handleEvaluationResponse(new OtpErlangLong(1));
    assertEquals(1, values.size());

    myNode.removeBreakpoint("sentinel", 0);
    assertEquals("the debuggee must stay at its breakpoint after an evaluation",
                 List.of("evaluate", "remove_breakpoint"), readCommandsUpTo("remove_breakpoint"));
  }

  @NotNull
  private List<String> readCommandsUpTo(@NotNull String lastCommand) throws IOException, OtpErlangDecodeException {
    DataInputStream in = new DataInputStream(myPeer.getInputStream());
    List<String> commands = new ArrayList<>();
    while (true) {
      byte[] frame = new byte[in.readInt()];
      in.readFully(frame);
      OtpErlangTuple message = (OtpErlangTuple) new OtpInputStream(frame).read_any();
      String command = ((OtpErlangAtom) message.elementAt(0)).atomValue();
      commands.add(command);
      if (command.equals(lastCommand)) return commands;
    }
  }

  @NotNull
  private static XDebugSession createSession(@NotNull XDebugProcess process) {
    // as XDebugSessionImpl: pause() calls XDebugProcess.startPausing(), resume() calls XDebugProcess.resume(context)
    return (XDebugSession) Proxy.newProxyInstance(
      ErlangXDebugProcessTest.class.getClassLoader(), new Class<?>[]{XDebugSession.class}, (proxy, method, args) -> {
        switch (method.getName()) {
          case "pause" -> process.startPausing();
          case "resume" -> process.resume((XSuspendContext) null);
          case "toString" -> { return "test session"; }
          case "hashCode" -> { return System.identityHashCode(proxy); }
          case "equals" -> { return proxy == args[0]; }
        }
        return method.getReturnType() == boolean.class ? Boolean.FALSE : null;
      });
  }

  @NotNull
  private static <T> T allocateWithoutConstructor(@NotNull Class<T> aClass) throws Exception {
    Class<?> unsafeClass = Class.forName("sun.misc.Unsafe");
    Field theUnsafe = unsafeClass.getDeclaredField("theUnsafe");
    theUnsafe.setAccessible(true);
    Object unsafe = theUnsafe.get(null);
    return aClass.cast(unsafeClass.getMethod("allocateInstance", Class.class).invoke(unsafe, aClass));
  }

  private static void setField(@NotNull Class<?> owner, @NotNull Object target, @NotNull String name, Object value)
    throws Exception {
    Field field = owner.getDeclaredField(name);
    field.setAccessible(true);
    field.set(target, value);
  }
}
