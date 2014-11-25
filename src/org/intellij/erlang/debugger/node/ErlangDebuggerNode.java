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

import com.ericsson.otp.erlang.*;
import com.intellij.openapi.application.ApplicationManager;
import org.intellij.erlang.debugger.node.commands.ErlangDebuggerCommandsProducer;
import org.intellij.erlang.debugger.node.events.ErlangDebuggerEvent;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.intellij.erlang.debugger.ErlangDebuggerLog.LOG;

public class ErlangDebuggerNode {
  private static final String MESSAGE_BOX_NAME = "idea_dbg_box";
  private static final int RECEIVE_TIMEOUT = 50;

  private OtpNode myOtpNode;
  @SuppressWarnings("FieldAccessedSynchronizedAndUnsynchronized")
  private OtpMbox myMessageBox;
  private AtomicBoolean myStopped = new AtomicBoolean(false);
  @SuppressWarnings("FieldAccessedSynchronizedAndUnsynchronized")
  private OtpErlangPid myRemoteCommandListener;
  private OtpErlangPid myLastSuspendedPid;

  private final Queue<ErlangDebuggerCommandsProducer.ErlangDebuggerCommand> myCommandsQueue = new LinkedList<ErlangDebuggerCommandsProducer.ErlangDebuggerCommand>();
  private ErlangDebuggerEventListener myEventsListener;

  public void startNode() throws ErlangDebuggerNodeException {
    if (myOtpNode != null) return;
    try {
      LOG.debug("Starting an OTP node.");
      myOtpNode = new OtpNode("idea_dbg_" + System.currentTimeMillis());
      LOG.debug("We're now running as an OTP node '" + myOtpNode.alive() + "'");

      myMessageBox = myOtpNode.createMbox(MESSAGE_BOX_NAME);
      assert myMessageBox != null : "A message box named " + MESSAGE_BOX_NAME + " was already registered on this node!";

      LOG.debug("Accepting messages at mailbox '" + MESSAGE_BOX_NAME + "'");
    } catch (IOException e) {
      String failedToConnectMessage = "Failed to connect to epmd.";
      LOG.debug(failedToConnectMessage, e);
      throw new ErlangDebuggerNodeException(failedToConnectMessage, e);
    }
    LOG.debug("Starting send/receive loop.");
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
      if (receivedMessage == null) return;

      LOG.debug("Message received: " + String.valueOf(receivedMessage));

      ErlangDebuggerEvent event = ErlangDebuggerEvent.create(receivedMessage);
      boolean messageRecognized = event != null && myEventsListener != null;
      if (messageRecognized) {
        event.process(this, myEventsListener);
      }

      LOG.debug("Message processed: " + messageRecognized);
    } catch (OtpErlangExit otpErlangExit) {
      LOG.info("Erlang node exited.", otpErlangExit);
      if (myEventsListener != null) {
        ErlangDebuggerEvent.create(otpErlangExit).process(this, myEventsListener);
      }
    } catch (OtpErlangDecodeException e) {
      LOG.debug("Failed to decode received message.", e);
    }
  }

  private void sendMessages() {
    if (myRemoteCommandListener == null) return;
    synchronized (myCommandsQueue) {
      while (!myCommandsQueue.isEmpty()) {
        OtpErlangTuple message = myCommandsQueue.remove().toMessage();
        LOG.debug("Sending message: " + message);
        myMessageBox.send(myRemoteCommandListener, message);
      }
    }
  }

}
