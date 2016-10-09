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

package org.intellij.erlang.debugger.xdebug;

import com.intellij.util.containers.ContainerUtil;
import com.intellij.xdebugger.frame.XExecutionStack;
import com.intellij.xdebugger.frame.XStackFrame;
import org.intellij.erlang.debugger.node.ErlangProcessSnapshot;
import org.intellij.erlang.debugger.node.ErlangTraceElement;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

public class ErlangExecutionStack extends XExecutionStack {
  private final ErlangDebugLocationResolver myResolver;
  private final ErlangProcessSnapshot myProcessSnapshot;
  private final List<ErlangStackFrame> myStack;

  public ErlangExecutionStack(ErlangDebugLocationResolver resolver, ErlangProcessSnapshot snapshot) {
    super(snapshot.getPidString());
    myResolver = resolver;
    myProcessSnapshot = snapshot;
    myStack = new ArrayList<>(snapshot.getStack().size());
  }

  @Nullable
  @Override
  public XStackFrame getTopFrame() {
    return ContainerUtil.getFirstItem(myStack);
  }

  @Override
  public void computeStackFrames(int firstFrameIndex, XStackFrameContainer container) {
    if (myStack.isEmpty()) {
      List<ErlangTraceElement> traceElements = myProcessSnapshot.getStack();
      for (ErlangTraceElement traceElement : traceElements) {
        boolean isTopStackFrame = myStack.isEmpty(); // if it's a top stack frame we can set a line that's being executed.
        ErlangStackFrame stackFrame = isTopStackFrame ?
          new ErlangStackFrame(myResolver, traceElement, ErlangSourcePosition.create(myResolver, myProcessSnapshot)) :
          new ErlangStackFrame(myResolver, traceElement);
        myStack.add(stackFrame);
      }
      container.addStackFrames(myStack, true);
    }
  }
}