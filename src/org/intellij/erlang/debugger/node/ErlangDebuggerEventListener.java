/*
 * Copyright 2012-2014 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.debugger.node;

import com.ericsson.otp.erlang.OtpErlangPid;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangModule;

import java.util.List;

public interface ErlangDebuggerEventListener {
  void debuggerStarted();
  void failedToInterpretModules(String nodeName, List<ErlangModule> modules);
  void failedToDebugRemoteNode(String nodeName, String error);
  void unknownMessage(String messageText);
  void failedToSetBreakpoint(ErlangFile file, int line, String errorMessage);
  void breakpointIsSet(ErlangFile file, int line);
  void breakpointReached(OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots);
  void debuggerStopped();
}
