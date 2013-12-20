package org.intellij.erlang.debugger.node.events;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.intellij.openapi.project.Project;
import org.intellij.erlang.utils.ErlangModulesUtil;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangModule;
import org.jetbrains.annotations.NotNull;

class SetBreakpointResponseEvent implements ErlangDebuggerEvent {
  public static final String NAME = "set_breakpoint_response";

  private final ErlangFile myFile;
  private final int myLine;
  private final String myError;

  public SetBreakpointResponseEvent(@NotNull Project project, @NotNull OtpErlangTuple message) throws DebuggerEventFormatException {
    String moduleName = OtpErlangTermUtil.getAtomText(message.elementAt(1));
    ErlangModule module = moduleName != null ? ErlangModulesUtil.getErlangModule(project, moduleName) : null;
    ErlangFile file = module != null ? (ErlangFile) module.getContainingFile() : null;
    if (file == null) throw new DebuggerEventFormatException();
    myFile = file;

    Integer line = OtpErlangTermUtil.getIntegerValue(message.elementAt(2));
    if (line == null) throw new DebuggerEventFormatException();
    myLine = line.intValue() - 1;

    OtpErlangObject statusObject = message.elementAt(3);
    if (OtpErlangTermUtil.isOkAtom(statusObject)) {
      myError = null;
    }
    else if (statusObject instanceof OtpErlangTuple) {
      OtpErlangTuple errorTuple = (OtpErlangTuple) statusObject;
      if (!OtpErlangTermUtil.isErrorAtom(errorTuple.elementAt(0))) throw new DebuggerEventFormatException();
      myError = OtpErlangTermUtil.toString(errorTuple.elementAt(1));
    }
    else {
      throw new DebuggerEventFormatException();
    }
  }

  @Override
  public void process(ErlangDebuggerNode debuggerNode, @NotNull ErlangDebuggerEventListener eventListener) {
    if (myError == null) {
      eventListener.breakpointIsSet(myFile, myLine);
    }
    else {
      eventListener.failedToSetBreakpoint(myFile, myLine, myError);
    }
  }
}
