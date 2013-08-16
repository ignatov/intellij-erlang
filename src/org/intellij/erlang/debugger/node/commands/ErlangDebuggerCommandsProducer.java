package org.intellij.erlang.debugger.node.commands;

import com.ericsson.otp.erlang.*;
import com.intellij.util.ArrayUtil;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;

import java.util.List;

/**
 * @author savenko
 */
public final class ErlangDebuggerCommandsProducer {
  private ErlangDebuggerCommandsProducer() {
  }

  public static ErlangDebuggerCommand getSetBreakpointCommand(String module, int line) {
    return new SetBreakpointCommand(module, line);
  }

  public static ErlangDebuggerCommand getRemoveBreakpointCommand(String module, int line) {
    return new RemoveBreakpointCommand(module, line);
  }

  public static ErlangDebuggerCommand getRunDebuggerCommand(String module, String function, String args) {
    return new RunDebuggerCommand(module, function, args);
  }

  public static ErlangDebuggerCommand getInterpretModulesCommand(List<String> moduleNames) {
    return new InterpretModulesCommand(moduleNames);
  }

  public static ErlangDebuggerCommand getStepIntoCommand(OtpErlangPid pid) {
    return new StepIntoCommand(pid);
  }

  public static ErlangDebuggerCommand getStepOverCommand(OtpErlangPid pid) {
    return new StepOverCommand(pid);
  }

  public static ErlangDebuggerCommand getStepOutCommand(OtpErlangPid pid) {
    return new StepOutCommand(pid);
  }

  public static ErlangDebuggerCommand getContinueCommand(OtpErlangPid pid) {
    return new ContinueCommand(pid);
  }

  private static class StepOverCommand extends AbstractPidCommand {
    public StepOverCommand(OtpErlangPid pid) {
      super("step_over", pid);
    }
  }

  private static class RunDebuggerCommand implements ErlangDebuggerCommand {
    private final String myModule;
    private final String myFunction;
    private final String myArgs;

    RunDebuggerCommand(String module, String function, String args) {
      myModule = module;
      myFunction = function;
      myArgs = args;
    }

    @Override
    public OtpErlangTuple toMessage() {
      return new OtpErlangTuple(new OtpErlangObject[] {
        new OtpErlangAtom("run_debugger"),
        new OtpErlangAtom(myModule),
        new OtpErlangAtom(myFunction),
        new OtpErlangString(myArgs)
      });
    }
  }

  private static class SetBreakpointCommand implements ErlangDebuggerCommand {
    private final String myModule;
    private final int myLine;

    SetBreakpointCommand(String module, int line) {
      myModule = module;
      myLine = line + 1;
    }

    @Override
    public OtpErlangTuple toMessage() {
      return new OtpErlangTuple(new OtpErlangObject[]{
        new OtpErlangAtom("set_breakpoint"),
        new OtpErlangAtom(myModule),
        new OtpErlangInt(myLine)
      });
    }
  }

  private static abstract class AbstractPidCommand implements ErlangDebuggerCommand {
    private final String myName;
    private final OtpErlangPid myPid;

    protected AbstractPidCommand(String cmdName, OtpErlangPid pid) {
      myName = cmdName;
      myPid = pid;
    }

    @Override
    public OtpErlangTuple toMessage() {
      return new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom(myName), myPid});
    }
  }

  private static class StepOutCommand extends AbstractPidCommand {
    public StepOutCommand(OtpErlangPid pid) {
      super("step_out", pid);
    }
  }

  private static class StepIntoCommand extends AbstractPidCommand {
    public StepIntoCommand(OtpErlangPid pid) {
      super("step_into", pid);
    }
  }

  private static class ContinueCommand extends AbstractPidCommand {
    protected ContinueCommand(OtpErlangPid pid) {
      super("continue", pid);
    }
  }

  private static class InterpretModulesCommand implements ErlangDebuggerCommand {
    private final List<String> myModuleNames;

    public InterpretModulesCommand(List<String> moduleNames) {
      myModuleNames = moduleNames;
    }

    @Override
    public OtpErlangTuple toMessage() {
      List<OtpErlangObject> moduleNameAtoms = ContainerUtil.map(myModuleNames, new Function<String, OtpErlangObject>() {
        @Override
        public OtpErlangObject fun(String moduleName) {
          return new OtpErlangAtom(moduleName);
        }
      });
      return new OtpErlangTuple(new OtpErlangObject[] {
        new OtpErlangAtom("interpret_modules"),
        new OtpErlangList(ArrayUtil.toObjectArray(moduleNameAtoms, OtpErlangObject.class))
      });
    }
  }

  private static class RemoveBreakpointCommand implements ErlangDebuggerCommand {
    private final String myModule;
    private final int myLine;

    public RemoveBreakpointCommand(String module, int line) {
      myModule = module;
      myLine = line + 1;
    }

    @Override
    public OtpErlangTuple toMessage() {
      return new OtpErlangTuple(new OtpErlangObject[] {
        new OtpErlangAtom("remove_breakpoint"),
        new OtpErlangAtom(myModule),
        new OtpErlangInt(myLine)
      });
    }
  }
}
