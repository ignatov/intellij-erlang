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

import com.ericsson.otp.erlang.OtpErlangBitstr;
import com.ericsson.otp.erlang.OtpErlangByte;
import com.intellij.xdebugger.frame.XCompositeNode;
import com.intellij.xdebugger.frame.XValueNode;
import com.intellij.xdebugger.frame.XValuePlace;
import com.intellij.xdebugger.frame.presentation.XValuePresentation;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

class ErlangBitStringXValue extends ErlangXValueBase<OtpErlangBitstr> {
  public ErlangBitStringXValue(OtpErlangBitstr value) {
    super(value, true);
  }

  @Override
  public void computeChildren(@NotNull XCompositeNode node) {
    byte[] bytes = getValue().binaryValue();
    OtpErlangByte[] otpBytes = new OtpErlangByte[bytes.length];
    for (int i = 0; i < bytes.length; i++) {
      otpBytes[i] = new OtpErlangByte(bytes[i]);
    }
    node.addChildren(ErlangXValueFactory.createChildrenList(otpBytes), true);
  }

  @Nullable
  @Override
  protected XValuePresentation getPresentation(@NotNull XValueNode node, @NotNull XValuePlace place) {
    return new XValuePresentation() {
      @Override
      public void renderValue(@NotNull XValueTextRenderer renderer) {
        //TODO apply string detection heuristics (see http://www.erlang.org/doc/apps/stdlib/unicode_usage.html)
        renderer.renderSpecialSymbol("<<");
        renderer.renderStringValue(new String(getValue().binaryValue()));
        renderer.renderSpecialSymbol(">>");
      }
    };
  }
}
