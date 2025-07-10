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

package org.intellij.erlang.debugger.node;

import com.ericsson.otp.erlang.OtpErlangPid;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class ErlangProcessSnapshot {
  private final OtpErlangPid myPid;
  private final ErlangTraceElement myInit;
  private final String myStatus;
  private final String myBreakModule;
  private final int myBreakLine;
  private final String myExitReason;
  private final List<ErlangTraceElement> myStack;

  public ErlangProcessSnapshot(@NotNull OtpErlangPid pid, @NotNull ErlangTraceElement init, @NotNull String status,
                               @Nullable String breakModule, int breakLine,
                               @Nullable String exitReason, @NotNull List<ErlangTraceElement> stack) {
    myPid = pid;
    myInit = init;
    myStatus = status;
    myBreakModule = breakModule;
    myBreakLine = breakLine;
    myExitReason = exitReason;
    myStack = stack;
  }

  @NotNull
  public OtpErlangPid getPid() {
    return myPid;
  }

  @NotNull
  public String getPidString() {
    return myPid.toString();
  }

  @NotNull
  public ErlangTraceElement getInit() {
    return myInit;
  }

  @NotNull
  public String getStatus() {
    return myStatus;
  }

  @Nullable
  public String getBreakModule() {
    return myBreakModule;
  }

  public int getBreakLine() {
    return myBreakLine;
  }

  @Nullable
  public String getExitReason() {
    return myExitReason;
  }

  @NotNull
  public List<ErlangTraceElement> getStack() {
    return myStack;
  }

  public boolean isSameBreakpoint(@NotNull ErlangProcessSnapshot snapshot) {
    return myPid.equals(snapshot.getPid())
           && myBreakModule != null
           && myBreakModule.equals(snapshot.getBreakModule())
           && myBreakLine == snapshot.getBreakLine();
  }
}
