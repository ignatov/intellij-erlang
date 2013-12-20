package org.intellij.erlang.debugger.node.events;

import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;

public interface ErlangDebuggerEvent {
  void process(ErlangDebuggerNode debuggerNode, ErlangDebuggerEventListener eventListener);
}
