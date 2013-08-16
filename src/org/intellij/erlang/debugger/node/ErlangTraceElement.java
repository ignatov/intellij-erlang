package org.intellij.erlang.debugger.node;

import com.ericsson.otp.erlang.OtpErlangList;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;

/**
 * @author savenko
 */
public class ErlangTraceElement {
  private final ErlangFile myModule;
  private final String myFunction;
  private final OtpErlangList myFunctionArgs;
  private final Collection<ErlangVariableBinding> myBindings;

  public ErlangTraceElement(@NotNull ErlangFile module,
                            @NotNull String function,
                            @NotNull OtpErlangList functionArgs,
                            @Nullable Collection<ErlangVariableBinding> bindings) {
    myModule = module;
    myFunction = function;
    myFunctionArgs = functionArgs;
    myBindings = bindings == null ? ContainerUtil.<ErlangVariableBinding>emptyList() : bindings;
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