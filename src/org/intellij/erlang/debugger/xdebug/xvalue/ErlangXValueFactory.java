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

package org.intellij.erlang.debugger.xdebug.xvalue;

import com.ericsson.otp.erlang.*;
import com.intellij.xdebugger.frame.XValue;
import com.intellij.xdebugger.frame.XValueChildrenList;

public final class ErlangXValueFactory {
  private ErlangXValueFactory() {
  }

  public static XValue create(OtpErlangObject object) {
    if (object instanceof OtpErlangLong || object instanceof OtpErlangDouble) {
      return new ErlangNumericXValue(object);
    }
    if (object instanceof OtpErlangAtom) {
      return new ErlangAtomXValue((OtpErlangAtom) object);
    }
    if (object instanceof OtpErlangPid) {
      return new ErlangPidXValue((OtpErlangPid) object);
    }
    if (object instanceof OtpErlangPort) {
      return new ErlangPortXValue((OtpErlangPort) object);
    }
    if (object instanceof OtpErlangRef) {
      return new ErlangRefXValue((OtpErlangRef) object);
    }
    if (object instanceof OtpErlangTuple) {
      return new ErlangTupleXValue((OtpErlangTuple) object);
    }
    if (object instanceof OtpErlangString) {
      return new ErlangStringXValue((OtpErlangString) object);
    }
    if (object instanceof OtpErlangList) {
      return new ErlangListXValue((OtpErlangList) object);
    }
    if (object instanceof OtpErlangBitstr) {
      return new ErlangBitStringXValue((OtpErlangBitstr) object);
    }
    if (object instanceof OtpErlangMap) {
      return new ErlangMapXValue((OtpErlangMap) object);
    }
    return new ErlangXValueBase<OtpErlangObject>(object);
  }

  public static XValueChildrenList createChildrenList(OtpErlangObject[] elements) {
    XValueChildrenList children = new XValueChildrenList(elements.length);
    for (int i = 0; i < elements.length; i++) {
      children.add("[" + (i + 1) + "]", create(elements[i]));
    }
    return children;
  }
}
