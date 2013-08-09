package org.intellij.erlang.debugger.xdebug;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.xdebugger.breakpoints.XLineBreakpointType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @author savenko
 */
public class ErlangLineBreakpointType extends XLineBreakpointType<ErlangLineBreakpointProperties> {
  public static final String ID = "ErlangLineBreakpoint";
  public static final String NAME = "Line breakpoint";

  protected ErlangLineBreakpointType() {
    super(ID, NAME);
  }

  @Nullable
  @Override
  public ErlangLineBreakpointProperties createBreakpointProperties(@NotNull VirtualFile file, int line) {
    return new ErlangLineBreakpointProperties();
  }

  @Override
  public boolean canPutAt(@NotNull VirtualFile file, int line, @NotNull Project project) {
    //TODO check if line breakpoint can be set here (see "Executable Lines" at http://www.erlang.org/doc/apps/debugger/debugger_chapter.html)
    return true;
  }
}
