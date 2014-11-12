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

import com.ericsson.otp.erlang.OtpErlangExit;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;

class DebuggerStoppedEvent extends ErlangDebuggerEvent {
  public DebuggerStoppedEvent(OtpErlangExit exitMessage) {
    //TODO set exit reasons in debugger and use them here to provide exit details
  }

  @Override
  public void process(ErlangDebuggerNode debuggerNode, ErlangDebuggerEventListener eventListener) {
    debuggerNode.stop();
    eventListener.debuggerStopped();
  }
}
