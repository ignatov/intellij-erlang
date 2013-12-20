/*
 * Copyright 2012-2013 Sergey Ignatov
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
import com.intellij.openapi.project.Project;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.debugger.node.*;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangModulesUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.intellij.erlang.debugger.node.events.OtpErlangTermUtil.*;

class BreakpointReachedEvent implements ErlangDebuggerEvent {
  public static final String NAME = "breakpoint_reached";

  private final OtpErlangPid myActivePid;
  private final List<ErlangProcessSnapshot> mySnapshots;

  public BreakpointReachedEvent(@NotNull Project project, OtpErlangTuple breakpointReachedMessage) throws DebuggerEventFormatException {
    OtpErlangPid activePid = getPidValue(elementAt(breakpointReachedMessage, 1));
    OtpErlangList snapshots = getListValue(elementAt(breakpointReachedMessage, 2));
    if (activePid == null || snapshots == null) throw new DebuggerEventFormatException();

    myActivePid = activePid;
    mySnapshots = new ArrayList<ErlangProcessSnapshot>(snapshots.arity());
    for (OtpErlangObject snapshot : snapshots) {
      OtpErlangTuple snapshotTuple = getTupleValue(snapshot); // {Pid, Function, Status, Info, Stack}

      OtpErlangPid pid = getPidValue(elementAt(snapshotTuple, 0));
      ErlangTraceElement init = getTraceElement(project, getTupleValue(elementAt(snapshotTuple, 1)), null);
      String status = getAtomText(elementAt(snapshotTuple, 2));
      OtpErlangObject info = elementAt(snapshotTuple, 3);
      List<ErlangTraceElement> stack = getStack(project, getListValue(elementAt(snapshotTuple, 4)));

      if (pid == null || init == null || status == null || info == null || stack == null) {
        throw new DebuggerEventFormatException();
      }

      if ("break".equals(status)) {
        OtpErlangTuple infoTuple = getTupleValue(info);
        String breakModuleName = getAtomText(elementAt(infoTuple, 0));
        Integer breakLine = getIntegerValue(elementAt(infoTuple, 1));
        ErlangFile breakModule = breakModuleName != null ? ErlangModulesUtil.getErlangModuleFile(project, breakModuleName) : null;
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
  private static List<ErlangTraceElement> getStack(@NotNull Project project, @Nullable OtpErlangList traceElementsList) {
    if (traceElementsList == null) return null;
    List<ErlangTraceElement> stack = new ArrayList<ErlangTraceElement>(traceElementsList.arity());
    for (OtpErlangObject traceElementObject : traceElementsList) {
      OtpErlangTuple traceElementTuple = getTupleValue(traceElementObject);
      // ignoring SP at 0
      OtpErlangTuple moduleFunctionArgsTuple = getTupleValue(elementAt(traceElementTuple, 1));
      OtpErlangList bindingsList = getListValue(elementAt(traceElementTuple, 2));
      ErlangTraceElement traceElement = getTraceElement(project, moduleFunctionArgsTuple, bindingsList);
      if (traceElement == null) return null;
      stack.add(traceElement);
    }
    return stack;
  }

  @Nullable
  private static ErlangTraceElement getTraceElement(@NotNull Project project,
                                                    @Nullable OtpErlangTuple moduleFunctionArgsTuple,
                                                    @Nullable OtpErlangList bindingsList) {
    String moduleName = getAtomText(elementAt(moduleFunctionArgsTuple, 0));
    ErlangFile module = moduleName != null ? ErlangModulesUtil.getErlangModuleFile(project, moduleName) : null;
    String functionName = getAtomText(elementAt(moduleFunctionArgsTuple, 1));
    OtpErlangList args = getListValue(elementAt(moduleFunctionArgsTuple, 2));
    Collection<ErlangVariableBinding> bindings = getBindings(bindingsList);
    if (module == null || functionName == null || args == null) return null; // bindings are not necessarily present
    return new ErlangTraceElement(module, functionName, args, bindings);
  }

  @NotNull
  private static Collection<ErlangVariableBinding> getBindings(@Nullable OtpErlangList bindingsList) {
    if (bindingsList == null) return ContainerUtil.emptyList();
    Collection<ErlangVariableBinding> bindings = new ArrayList<ErlangVariableBinding>(bindingsList.arity());
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
