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

import com.ericsson.otp.erlang.OtpErlangObject;
import com.intellij.icons.AllIcons;
import com.intellij.xdebugger.frame.XValue;
import com.intellij.xdebugger.frame.XValueNode;
import com.intellij.xdebugger.frame.XValuePlace;
import com.intellij.xdebugger.frame.presentation.XValuePresentation;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

class ErlangXValueBase<T extends OtpErlangObject> extends XValue {
  private final T myValue;
  private final boolean myHasChildren;

  public ErlangXValueBase(T value) {
    this(value, false);
  }

  public ErlangXValueBase(T value, boolean hasChildren) {
    myValue = value;
    myHasChildren = hasChildren;
  }

  protected T getValue() {
    return myValue;
  }

  @Override
  public void computePresentation(@NotNull XValueNode node, @NotNull XValuePlace place) {
    XValuePresentation presentation = getPresentation(node, place);
    if (presentation != null) {
      node.setPresentation(AllIcons.Debugger.Value, presentation, myHasChildren);
    }
    else {
      node.setPresentation(AllIcons.Debugger.Value, getType(), getStringRepr(), myHasChildren);
    }
  }

  @Override
  public boolean canNavigateToSource() {
    return false;
  }

  @Nullable
  protected XValuePresentation getPresentation(@NotNull XValueNode node, @NotNull XValuePlace place) {
    return null;
  }

  @Nullable
  protected String getType() {
    return null;
  }

  @NotNull
  protected String getStringRepr() {
    return myValue.toString();
  }
}
