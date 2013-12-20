package org.intellij.erlang.debugger.node.events;

import com.ericsson.otp.erlang.OtpErlangExit;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;

class DebuggerStoppedEvent implements ErlangDebuggerEvent {
  public DebuggerStoppedEvent(OtpErlangExit exitMessage) {
    //TODO set exit reasons in debugger and use them here to provide exit details
  }

  @Override
  public void process(ErlangDebuggerNode debuggerNode, ErlangDebuggerEventListener eventListener) {
    debuggerNode.stop();
    eventListener.debuggerStopped();
  }
}
