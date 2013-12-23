package org.intellij.erlang.debugger.node;

import com.ericsson.otp.erlang.OtpErlangList;
import org.intellij.erlang.psi.ErlangFile;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;

public class ErlangTraceElement {
  private final ErlangFile myModule;
  private final String myFunction;
  private final OtpErlangList myFunctionArgs;
  private final Collection<ErlangVariableBinding> myBindings;

  public ErlangTraceElement(@NotNull ErlangFile module,
                            @NotNull String function,
                            @NotNull OtpErlangList functionArgs,
                            @NotNull Collection<ErlangVariableBinding> bindings) {
    myModule = module;
    myFunction = function;
    myFunctionArgs = functionArgs;
    myBindings = bindings;
  }

  @NotNull
  public ErlangFile getModule() {
    return myModule;
  }

  @NotNull
  public String getFunction() {
    return myFunction;
  }

  @NotNull
  public OtpErlangList getFunctionArgs() {
    return myFunctionArgs;
  }

  @NotNull
  public Collection<ErlangVariableBinding> getBindings() {
    return myBindings;
  }
}