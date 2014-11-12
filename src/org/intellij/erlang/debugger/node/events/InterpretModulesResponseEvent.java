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
import com.ericsson.otp.erlang.OtpErlangTuple;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;

import java.util.ArrayList;
import java.util.List;

class InterpretModulesResponseEvent extends ErlangDebuggerEvent {
  public static final String NAME = "interpret_modules_response";
  private final String myNodeName;
  private final List<String> myFailedToInterpretModules = new ArrayList<String>();

  public InterpretModulesResponseEvent(OtpErlangTuple receivedMessage) throws DebuggerEventFormatException {
    myNodeName = OtpErlangTermUtil.getAtomText(receivedMessage.elementAt(1));
    OtpErlangList interpretModuleStatuses = OtpErlangTermUtil.getListValue(receivedMessage.elementAt(2));
    if (interpretModuleStatuses == null || myNodeName == null) throw new DebuggerEventFormatException();
    for (OtpErlangObject status : interpretModuleStatuses) {
      OtpErlangTuple statusTuple = OtpErlangTermUtil.getTupleValue(status);
      String module = OtpErlangTermUtil.getAtomText(OtpErlangTermUtil.elementAt(statusTuple, 0));
      if (module == null) throw new DebuggerEventFormatException();

      OtpErlangObject moduleStatusObject = OtpErlangTermUtil.elementAt(statusTuple, 1);
      if (moduleStatusObject instanceof OtpErlangTuple) {
        //here moduleStatusObject is a tuple {error, details} - we can use that to provide error messages.
        myFailedToInterpretModules.add(module);
      } else if (!OtpErlangTermUtil.isOkAtom(moduleStatusObject)) {
        throw new DebuggerEventFormatException();
      }
    }
  }

  @Override
  public void process(ErlangDebuggerNode debuggerNode, ErlangDebuggerEventListener eventListener) {
    if (!myFailedToInterpretModules.isEmpty()) {
      eventListener.failedToInterpretModules(myNodeName, myFailedToInterpretModules);
    }
  }
}
