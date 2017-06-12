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

import com.ericsson.otp.erlang.OtpErlangPid;
import com.intellij.xdebugger.frame.XExecutionStack;
import com.intellij.xdebugger.frame.XSuspendContext;
import org.intellij.erlang.debugger.node.ErlangProcessSnapshot;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class ErlangSuspendContext extends XSuspendContext {
  private final XExecutionStack[] myExecutionStacks;
  private final int myActiveStackIdx;

  public ErlangSuspendContext(@NotNull ErlangXDebugProcess debugProcess,
                              @NotNull OtpErlangPid activePid,
                              @NotNull List<ErlangProcessSnapshot> snapshots) {
    myExecutionStacks = new XExecutionStack[snapshots.size()];
    int activeStackIdx = 0;
    for (int i = 0; i < snapshots.size(); i++) {
      ErlangProcessSnapshot snapshot = snapshots.get(i);
      if (snapshot.getPid().equals(activePid)) {
        activeStackIdx = i;
      }
      myExecutionStacks[i] = new ErlangExecutionStack(debugProcess, snapshot);
    }
    myActiveStackIdx = activeStackIdx;
  }

  @Override
  public XExecutionStack[] getExecutionStacks() {
    return myExecutionStacks;
  }

  @Nullable
  @Override
  public XExecutionStack getActiveExecutionStack() {
    return myExecutionStacks[myActiveStackIdx];
  }
}
