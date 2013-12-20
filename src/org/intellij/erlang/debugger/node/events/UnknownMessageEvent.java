package org.intellij.erlang.debugger.node.events;

import com.ericsson.otp.erlang.OtpErlangTuple;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;

class UnknownMessageEvent implements ErlangDebuggerEvent {
  private final String myUnknownMessageText;

  public UnknownMessageEvent(OtpErlangTuple message) {
    myUnknownMessageText = message.toString();
  }

  @Override
  public void process(ErlangDebuggerNode debuggerNode, ErlangDebuggerEventListener eventListener) {
    eventListener.unknownMessage(myUnknownMessageText);
  }
}
