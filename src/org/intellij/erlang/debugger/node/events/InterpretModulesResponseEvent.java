package org.intellij.erlang.debugger.node.events;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.intellij.openapi.project.Project;
import org.intellij.erlang.utils.ErlangModulesUtil;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;
import org.intellij.erlang.psi.ErlangModule;

import java.util.ArrayList;
import java.util.List;

class InterpretModulesResponseEvent implements ErlangDebuggerEvent {
  public static final String NAME = "interpret_modules_response";
  private final String myNodeName;
  private final List<ErlangModule> myFailedToInterpretModules = new ArrayList<ErlangModule>();

  public InterpretModulesResponseEvent(Project project, OtpErlangTuple receivedMessage) throws DebuggerEventFormatException {
    myNodeName = OtpErlangTermUtil.getAtomText(receivedMessage.elementAt(1));
    OtpErlangList interpretModuleStatuses = OtpErlangTermUtil.getListValue(receivedMessage.elementAt(2));
    if (interpretModuleStatuses == null || myNodeName == null) throw new DebuggerEventFormatException();
    for (OtpErlangObject status : interpretModuleStatuses) {
      OtpErlangTuple statusTuple = OtpErlangTermUtil.getTupleValue(status);
      String moduleName = OtpErlangTermUtil.getAtomText(OtpErlangTermUtil.elementAt(statusTuple, 0));
      if (moduleName == null) throw new DebuggerEventFormatException();

      OtpErlangObject moduleStatusObject = OtpErlangTermUtil.elementAt(statusTuple, 1);
      if (OtpErlangTermUtil.isOkAtom(moduleStatusObject)) {
        //noinspection UnnecessaryContinue
        continue;
      }
      else if (moduleStatusObject instanceof OtpErlangTuple) {
        //here moduleStatusObject is a tuple {error, details} - we can use that to provide error messages.
        ErlangModule module = ErlangModulesUtil.getErlangModule(project, moduleName);
        if (module == null) throw new DebuggerEventFormatException();
        myFailedToInterpretModules.add(module);
      } else {
        throw new DebuggerEventFormatException();
      }
    }
  }

  @Override
  public void process(ErlangDebuggerNode debuggerNode, ErlangDebuggerEventListener eventListener) {
    if (!myFailedToInterpretModules.isEmpty()) {
      eventListener.failedToInterpretModules(myNodeName, myFailedToInterpretModules);
    }
  }
}
