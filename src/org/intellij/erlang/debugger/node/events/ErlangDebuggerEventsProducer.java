package org.intellij.erlang.debugger.node.events;

import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.Nullable;

public final class ErlangDebuggerEventsProducer {
  private Project myProject;

  public ErlangDebuggerEventsProducer(@Nullable Project project) {
    myProject = project;
  }

  @Nullable
  public ErlangDebuggerEvent produce(OtpErlangObject message) {
    if (!(message instanceof OtpErlangTuple)) return null;
    OtpErlangTuple messageTuple = (OtpErlangTuple) message;
    String messageName = OtpErlangTermUtil.getAtomText(messageTuple.elementAt(0));
    if (messageName == null) return null;

    try {
      if (RegisterListenerEvent.NAME.equals(messageName))         return new RegisterListenerEvent(messageTuple);
      if (InterpretModulesResponseEvent.NAME.equals(messageName)) return new InterpretModulesResponseEvent(myProject, messageTuple);
      if (SetBreakpointResponseEvent.NAME.equals(messageName))    return new SetBreakpointResponseEvent(myProject, messageTuple);
      if (BreakpointReachedEvent.NAME.equals(messageName))        return new BreakpointReachedEvent(myProject, messageTuple);
      if (DebugRemoteNodeResponseEvent.NAME.equals(messageName))  return new DebugRemoteNodeResponseEvent(messageTuple);
    } catch (DebuggerEventFormatException e) {
      return new UnknownMessageEvent(messageTuple);
    }
    return new UnknownMessageEvent(messageTuple);
  }

  public static ErlangDebuggerEvent produce(OtpErlangExit exitMessage) {
    return new DebuggerStoppedEvent(exitMessage);
  }
}
