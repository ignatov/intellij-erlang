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
    myDebugProcess.removeBreakpoint(breakpoint, temporary);
  }
}
