/*
 * Copyright 2012-2014 Sergey Ignatov
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

package org.intellij.erlang.debugger.node.events;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.debugger.node.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.intellij.erlang.debugger.node.events.OtpErlangTermUtil.*;

class BreakpointReachedEvent extends ErlangDebuggerEvent {
  public static final String NAME = "breakpoint_reached";

  private final OtpErlangPid myActivePid;
  private final List<ErlangProcessSnapshot> mySnapshots;

  public BreakpointReachedEvent(OtpErlangTuple breakpointReachedMessage) throws DebuggerEventFormatException {
    OtpErlangPid activePid = getPidValue(elementAt(breakpointReachedMessage, 1));
    OtpErlangList snapshots = getListValue(elementAt(breakpointReachedMessage, 2));
    if (activePid == null || snapshots == null) throw new DebuggerEventFormatException();

    myActivePid = activePid;
    mySnapshots = new ArrayList<>(snapshots.arity());
    for (OtpErlangObject snapshot : snapshots) {
      OtpErlangTuple snapshotTuple = getTupleValue(snapshot); // {Pid, Function, Status, Info, Stack}

      OtpErlangPid pid = getPidValue(elementAt(snapshotTuple, 0));
      ErlangTraceElement init = getTraceElement(getTupleValue(elementAt(snapshotTuple, 1)), null, null);
      String status = getAtomText(elementAt(snapshotTuple, 2));
      OtpErlangObject info = elementAt(snapshotTuple, 3);
      List<ErlangTraceElement> stack = getStack(getListValue(elementAt(snapshotTuple, 4)));

      if (pid == null || init == null || status == null || info == null || stack == null) {
        throw new DebuggerEventFormatException();
      }

      if ("break".equals(status)) {
        OtpErlangTuple infoTuple = getTupleValue(info);
        String breakModule = getAtomText(elementAt(infoTuple, 0));
        Integer breakLine = getIntegerValue(elementAt(infoTuple, 1));
        if (breakLine == null || breakModule == null) throw new DebuggerEventFormatException();
        mySnapshots.add(new ErlangProcessSnapshot(pid, init, status, breakModule, breakLine - 1, null, stack));
      }
      else if ("exit".equals(status)) {
        String exitReason = OtpErlangTermUtil.toString(info);
        mySnapshots.add(new ErlangProcessSnapshot(pid, init, status, null, -1, exitReason, stack));
      }
      else {
        mySnapshots.add(new ErlangProcessSnapshot(pid, init, status, null, -1, null, stack));
      }
    }
  }

  @Override
  public void process(@NotNull ErlangDebuggerNode debuggerNode, @NotNull ErlangDebuggerEventListener eventListener) {
    debuggerNode.processSuspended(myActivePid);
    eventListener.breakpointReached(myActivePid, mySnapshots);
  }

  @Nullable
  private static List<ErlangTraceElement> getStack(@Nullable OtpErlangList traceElementsList) {
    if (traceElementsList == null) return null;
    List<ErlangTraceElement> stack = new ArrayList<>(traceElementsList.arity());
    for (OtpErlangObject traceElementObject : traceElementsList) {
      OtpErlangTuple traceElementTuple = getTupleValue(traceElementObject);
      OtpErlangObject stackPointer = getTupleValue(elementAt(traceElementTuple, 0));
      OtpErlangTuple moduleFunctionArgsTuple = getTupleValue(elementAt(traceElementTuple, 1));
      OtpErlangList bindingsList = getListValue(elementAt(traceElementTuple, 2));
      ErlangTraceElement traceElement = getTraceElement(moduleFunctionArgsTuple, stackPointer, bindingsList);
      if (traceElement == null) return null;
      stack.add(traceElement);
    }
    return stack;
  }

  @Nullable
  private static ErlangTraceElement getTraceElement(@Nullable OtpErlangTuple moduleFunctionArgsTuple,
                                                    @Nullable OtpErlangObject stackPointer,
                                                    @Nullable OtpErlangList bindingsList) {
    String moduleName = getAtomText(elementAt(moduleFunctionArgsTuple, 0));
    String functionName = getAtomText(elementAt(moduleFunctionArgsTuple, 1));
    OtpErlangList args = getListValue(elementAt(moduleFunctionArgsTuple, 2));
    Collection<ErlangVariableBinding> bindings = getBindings(bindingsList);
    if (moduleName == null || functionName == null || args == null) return null; // bindings are not necessarily present
    return new ErlangTraceElement(stackPointer, moduleName, functionName, args, bindings);
  }

  @NotNull
  private static Collection<ErlangVariableBinding> getBindings(@Nullable OtpErlangList bindingsList) {
    if (bindingsList == null) return ContainerUtil.emptyList();
    Collection<ErlangVariableBinding> bindings = new ArrayList<>(bindingsList.arity());
    for (OtpErlangObject bindingObject : bindingsList) {
      OtpErlangTuple bindingTuple = getTupleValue(bindingObject);
      String variableName = getAtomText(elementAt(bindingTuple, 0));
      OtpErlangObject variableValue = elementAt(bindingTuple, 1);
      if (variableName == null || variableValue == null) return ContainerUtil.emptyList();
      bindings.add(new ErlangVariableBinding(variableName, variableValue));
    }
    return bindings;
  }
}
