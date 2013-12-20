package org.intellij.erlang.debugger.node;

import com.ericsson.otp.erlang.*;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import org.intellij.erlang.debugger.node.commands.ErlangDebuggerCommandsProducer;
import org.intellij.erlang.debugger.node.events.ErlangDebuggerEvent;
import org.intellij.erlang.debugger.node.events.ErlangDebuggerEventsProducer;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.atomic.AtomicBoolean;

public class ErlangDebuggerNode {
  private static final Logger LOG = Logger.getInstance(ErlangDebuggerNode.class);
  private static final String MESSAGE_BOX_NAME = "idea_dbg_box";
  private static final int RECEIVE_TIMEOUT = 50;

  private OtpNode myOtpNode;
  @SuppressWarnings("FieldAccessedSynchronizedAndUnsynchronized")
  private OtpMbox myMessageBox;
  private AtomicBoolean myStopped = new AtomicBoolean(false);
  @SuppressWarnings("FieldAccessedSynchronizedAndUnsynchronized")
  private OtpErlangPid myRemoteCommandListener;
  private OtpErlangPid myLastSuspendedPid;

  private final ErlangDebuggerEventsProducer myDebuggerEventsProducer;

  private final Queue<ErlangDebuggerCommandsProducer.ErlangDebuggerCommand> myCommandsQueue = new LinkedList<ErlangDebuggerCommandsProducer.ErlangDebuggerCommand>();
  private ErlangDebuggerEventListener myEventsListener;

  public ErlangDebuggerNode(@Nullable Project project) {
    myDebuggerEventsProducer = new ErlangDebuggerEventsProducer(project);
  }

  public void startNode() throws ErlangDebuggerNodeException {
    if (myOtpNode != null) return;
    try {
      myOtpNode = new OtpNode("idea_dbg_" + System.currentTimeMillis());
      myMessageBox = myOtpNode.createMbox(MESSAGE_BOX_NAME);
      assert myMessageBox != null;
    } catch (IOException e) {
      String failedToConnectMessage = "Failed to connect to epmd.";
      LOG.debug(failedToConnectMessage, e);
      throw new ErlangDebuggerNodeException(failedToConnectMessage, e);
    }
    ApplicationManager.getApplication().executeOnPooledThread(new Runnable() {
      @Override
      public void run() {
        loop();
      }
    });
  }

  public String getName() {
    return myOtpNode.alive();
  }

  public String getMessageBoxName() {
    return myMessageBox.getName();
  }

  public void stop() {
    myStopped.set(true);
  }

  public boolean isStopped() {
    return myStopped.get();
  }

  public void setRemoteCommandListener(OtpErlangPid pid) throws OtpErlangExit {
    myMessageBox.link(pid);
    myRemoteCommandListener = pid;
  }

  public void processSuspended(OtpErlangPid pid) {
    myLastSuspendedPid = pid;
  }

  public void setListener(ErlangDebuggerEventListener listener) {
    myEventsListener = listener;
  }

  public void setBreakpoint(String module, int line) {
    addCommand(ErlangDebuggerCommandsProducer.getSetBreakpointCommand(module, line));
  }

  public void removeBreakpoint(String module, int line) {
    addCommand(ErlangDebuggerCommandsProducer.getRemoveBreakpointCommand(module, line));
  }

  public void interpretModules(List<String> moduleNames) {
    addCommand(ErlangDebuggerCommandsProducer.getInterpretModulesCommand(moduleNames));
  }

  public void runDebugger(String module, String function, List<String> args) {
    addCommand(ErlangDebuggerCommandsProducer.getRunDebuggerCommand(module, function, args));
  }

  public void debugRemoteNode(String nodeName, String cookie) {
    addCommand(ErlangDebuggerCommandsProducer.getDebugRemoteNodeCommand(nodeName, cookie));
  }

  public void stepInto() {
    addCommand(ErlangDebuggerCommandsProducer.getStepIntoCommand(myLastSuspendedPid));
  }

  public void stepOver() {
    addCommand(ErlangDebuggerCommandsProducer.getStepOverCommand(myLastSuspendedPid));
  }

  public void stepOut() {
    addCommand(ErlangDebuggerCommandsProducer.getStepOutCommand(myLastSuspendedPid));
  }

  public void resume() {
    addCommand(ErlangDebuggerCommandsProducer.getContinueCommand(myLastSuspendedPid));
  }

  private void addCommand(ErlangDebuggerCommandsProducer.ErlangDebuggerCommand command) {
    synchronized (myCommandsQueue) {
      myCommandsQueue.add(command);
    }
  }

  private void loop() {
    while (!isStopped()) {
      if (!isStopped()) {
        receiveMessage();
      }
      if (!isStopped()) {
        sendMessages();
      }
    }
    myOtpNode.close();
  }

  private void receiveMessage() {
    try {
      OtpErlangObject receivedMessage = myMessageBox.receive(RECEIVE_TIMEOUT);
      ErlangDebuggerEvent event = myDebuggerEventsProducer.produce(receivedMessage);
      if (event != null && myEventsListener != null) {
        event.process(this, myEventsListener);
      } else if (receivedMessage != null) {
        unsupportedMessage(receivedMessage);
      }
    } catch (OtpErlangExit otpErlangExit) {
      if (myEventsListener != null) {
        ErlangDebuggerEventsProducer.produce(otpErlangExit).process(this, myEventsListener);
      }
      LOG.info("Erlang node exited.", otpErlangExit);
    } catch (OtpErlangDecodeException e) {
      LOG.debug("Failed to decode received message.", e);
    }
  }

  private void sendMessages() {
    if (myRemoteCommandListener == null) return;
    synchronized (myCommandsQueue) {
      while (!myCommandsQueue.isEmpty()) {
        myMessageBox.send(myRemoteCommandListener, myCommandsQueue.remove().toMessage());
      }
    }
  }

  private static void unsupportedMessage(OtpErlangObject message) {
    LOG.debug("Unsupported message: " + message.toString());
  }
}
