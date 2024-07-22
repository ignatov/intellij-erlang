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

import com.intellij.xdebugger.breakpoints.XBreakpointHandler;
import com.intellij.xdebugger.breakpoints.XLineBreakpoint;
import org.jetbrains.annotations.NotNull;

public class ErlangLineBreakpointHandler extends XBreakpointHandler<XLineBreakpoint<ErlangLineBreakpointProperties>> {
  private final ErlangXDebugProcess myDebugProcess;

  public ErlangLineBreakpointHandler(ErlangXDebugProcess debugProcess) {
    super(ErlangLineBreakpointType.class);
    myDebugProcess = debugProcess;
  }

  @Override
  public void registerBreakpoint(@NotNull XLineBreakpoint<ErlangLineBreakpointProperties> breakpoint) {
    myDebugProcess.addBreakpoint(breakpoint);
  }

  @Override
  public void unregisterBreakpoint(@NotNull XLineBreakpoint<ErlangLineBreakpointProperties> breakpoint, boolean temporary) {
    myDebugProcess.removeBreakpoint(breakpoint);
  }
}
