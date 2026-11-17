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

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;
import org.jetbrains.annotations.Nullable;

public abstract class ErlangDebuggerEvent {

  public abstract void process(ErlangDebuggerNode debuggerNode, ErlangDebuggerEventListener eventListener);

  @Nullable
  public static ErlangDebuggerEvent create(OtpErlangObject message) {
    if (!(message instanceof OtpErlangTuple messageTuple)) return null;
    String messageName = OtpErlangTermUtil.getAtomText(messageTuple.elementAt(0));
    if (messageName == null) return null;

    try {
      switch (messageName) {
        case InterpretModulesResponseEvent.NAME:
          return new InterpretModulesResponseEvent(messageTuple);
        case SetBreakpointResponseEvent.NAME:
          return new SetBreakpointResponseEvent(messageTuple);
        case BreakpointReachedEvent.NAME:
          return new BreakpointReachedEvent(messageTuple);
        case DebugRemoteNodeResponseEvent.NAME:
          return new DebugRemoteNodeResponseEvent(messageTuple);
        case EvaluateResponseEvent.NAME:
          return new EvaluateResponseEvent(messageTuple);
      }
    } catch (DebuggerEventFormatException e) {
      return new UnknownMessageEvent(messageTuple);
    }
    return new UnknownMessageEvent(messageTuple);
  }
}
