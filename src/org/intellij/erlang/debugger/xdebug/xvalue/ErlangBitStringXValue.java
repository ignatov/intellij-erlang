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
import com.intellij.xdebugger.frame.ImmediateFullValueEvaluator;
import com.intellij.xdebugger.frame.XValueChildrenList;
import com.intellij.xdebugger.frame.XValueNode;
import com.intellij.xdebugger.frame.XValuePlace;
import com.intellij.xdebugger.frame.presentation.XValuePresentation;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

class ErlangBitStringXValue extends ErlangArrayXValueBase<OtpErlangBitstr> {
  public ErlangBitStringXValue(OtpErlangBitstr value) {
    super(value, value.binaryValue().length);
  }

  @Override
  protected void computeChild(XValueChildrenList children, int childIdx) {
    addIndexedChild(children, getValue().binaryValue()[childIdx], childIdx);
  }

  @Nullable
  @Override
  protected XValuePresentation getPresentation(@NotNull XValueNode node, @NotNull XValuePlace place) {
    //TODO apply string detection heuristics (see http://www.erlang.org/doc/apps/stdlib/unicode_usage.html)
    final String textValue = new String(getValue().binaryValue());
    if (textValue.length() > XValueNode.MAX_VALUE_LENGTH) {
      node.setFullValueEvaluator(new ImmediateFullValueEvaluator(textValue));
    }
    return new XValuePresentation() {
      @Override
      public void renderValue(@NotNull XValueTextRenderer renderer) {
        renderer.renderSpecialSymbol("<<");
        renderer.renderStringValue(textValue, "\"\\", XValueNode.MAX_VALUE_LENGTH);
        renderer.renderSpecialSymbol(">>");
      }
    };
  }
}
