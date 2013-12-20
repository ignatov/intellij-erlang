package org.intellij.erlang.debugger.node;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlangVariableBinding {
  private final String myName;
  private final OtpErlangObject myValue;

  public ErlangVariableBinding(String name, OtpErlangObject value) {
    myName = name;
    myValue = value;
  }

  public String getName() {
    return myName;
  }

  public OtpErlangObject getValue() {
    return myValue;
  }
}
