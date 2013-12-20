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

package org.intellij.erlang.debugger.xdebug;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.ColoredTextContainer;
import com.intellij.ui.SimpleTextAttributes;
import com.intellij.xdebugger.XDebuggerUtil;
import com.intellij.xdebugger.XSourcePosition;
import com.intellij.xdebugger.frame.*;
import org.intellij.erlang.debugger.node.ErlangTraceElement;
import org.intellij.erlang.debugger.node.ErlangVariableBinding;
import org.intellij.erlang.psi.ErlangFunExpression;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;

public class ErlangStackFrame extends XStackFrame {
  private final ErlangSourcePosition mySourcePosition;
  private final Collection<ErlangVariableBinding> myBindings;

  public ErlangStackFrame(@NotNull ErlangTraceElement traceElement) {
    this(traceElement, new ErlangSourcePosition(traceElement.getModule(), traceElement.getFunction(), traceElement.getFunctionArgs().arity()));
  }

  public ErlangStackFrame(@NotNull ErlangTraceElement traceElement, @Nullable ErlangSourcePosition sourcePosition) {
    mySourcePosition = sourcePosition;
    myBindings = traceElement.getBindings();
  }

  @Nullable
  @Override
  public XSourcePosition getSourcePosition() {
    if (mySourcePosition == null) return null;
    VirtualFile virtualFile = mySourcePosition.getErlangFile().getVirtualFile();
    assert virtualFile != null;
    return XDebuggerUtil.getInstance().createPosition(virtualFile, mySourcePosition.getLine());
  }

  @Override
  public void customizePresentation(ColoredTextContainer component) {
    if (mySourcePosition != null) {
      ErlangFunction function = mySourcePosition.getFunction();
      ErlangFunExpression funExpression = mySourcePosition.getFunExpression();
      if (function != null) {
        String title = ErlangPsiImplUtil.getQualifiedFunctionName(function);
        if (funExpression != null) {
          int line = 1 + StringUtil.offsetToLineNumber(funExpression.getContainingFile().getText(), funExpression.getTextOffset());
          title += ": fun at line " + line;
        }
        component.append(title, SimpleTextAttributes.REGULAR_ATTRIBUTES);
        component.setIcon(AllIcons.Debugger.StackFrame);
        return;
      }
    }
    super.customizePresentation(component);
  }

  @Override
  public void computeChildren(@NotNull XCompositeNode node) {
    XValueChildrenList myVariables = new XValueChildrenList(myBindings.size());
    for (ErlangVariableBinding binding : myBindings) {
      myVariables.add(binding.getName(), getVariableValue(binding.getValue()));
    }
    node.addChildren(myVariables, true);
  }

  private static XValue getVariableValue(final OtpErlangObject value) {
    //TODO implement XValue to provide proper presentation of values of variables.
    return new XValue() {
      @Override
      public void computePresentation(@NotNull XValueNode node, @NotNull XValuePlace place) {
        node.setPresentation(AllIcons.Debugger.Value, "", value.toString(), false);
      }
    };
  }
}
