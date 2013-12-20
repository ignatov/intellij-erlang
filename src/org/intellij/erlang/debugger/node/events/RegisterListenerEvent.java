package org.intellij.erlang.debugger.node.events;

import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;

class RegisterListenerEvent implements ErlangDebuggerEvent {
  public static final String NAME = "register_listener";

  private final OtpErlangPid myRemoteCommandListener;

  public RegisterListenerEvent(OtpErlangTuple receivedMessage) throws DebuggerEventFormatException {
    OtpErlangObject remoteCommandsListenerObject = receivedMessage.elementAt(1);
    OtpErlangPid remoteCommandsListenerPid = remoteCommandsListenerObject instanceof OtpErlangPid ? (OtpErlangPid) remoteCommandsListenerObject : null;
    if (remoteCommandsListenerPid == null) throw new DebuggerEventFormatException();
    myRemoteCommandListener = remoteCommandsListenerPid;
  }

  @Override
  public void process(ErlangDebuggerNode debuggerNode, ErlangDebuggerEventListener eventListener) {
    try {
      debuggerNode.setRemoteCommandListener(myRemoteCommandListener);
      eventListener.debuggerStarted();
    } catch (OtpErlangExit otpErlangExit) {
      new DebuggerStoppedEvent(otpErlangExit).process(debuggerNode, eventListener);
    }
  }
}
