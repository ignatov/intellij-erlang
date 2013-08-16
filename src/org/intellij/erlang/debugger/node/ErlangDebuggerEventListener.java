package org.intellij.erlang.debugger.node;

import com.ericsson.otp.erlang.OtpErlangPid;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangModule;

import java.util.List;

/**
 * @author savenko
 */
public interface ErlangDebuggerEventListener {
  void debuggerStarted();
  void failedToInterpretModules(List<ErlangModule> modules);
  void unknownMessage(String messageText);
  void failedToSetBreakpoint(ErlangFile file, int line, String errorMessage);
  void breakpointIsSet(ErlangFile file, int line);
  void breakpointReached(OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots);
  void debuggerStopped();
}
