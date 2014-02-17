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

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.intellij.xdebugger.frame.XCompositeNode;
import com.intellij.xdebugger.frame.XValueChildrenList;
import org.jetbrains.annotations.NotNull;

public class ErlangRefXValue extends ErlangXValueBase<OtpErlangRef> {
  public ErlangRefXValue(OtpErlangRef value) {
    super(value, true);
  }

  @Override
  public void computeChildren(@NotNull XCompositeNode node) {
    int[] ids = getValue().ids();
    XValueChildrenList children = new XValueChildrenList(1 + ids.length);
    children.add("node", ErlangXValueFactory.create(new OtpErlangAtom(getValue().node())));
    for (int i = 0; i < ids.length; i++) {
      children.add("id" + i, ErlangXValueFactory.create(new OtpErlangLong(ids[i])));
    }
    node.addChildren(children, true);
  }
}
