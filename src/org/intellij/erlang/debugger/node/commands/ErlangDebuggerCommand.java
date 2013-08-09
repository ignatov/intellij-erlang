package org.intellij.erlang.debugger.node.commands;

import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * @author savenko
 */
public interface ErlangDebuggerCommand {
  OtpErlangTuple toMessage();
}
