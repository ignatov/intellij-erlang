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
import com.intellij.concurrency.AsyncFutureFactory;
import com.intellij.concurrency.AsyncFutureResult;
import com.intellij.openapi.application.ApplicationManager;
import org.intellij.erlang.debugger.node.commands.ErlangDebuggerCommandsProducer;
import org.intellij.erlang.debugger.node.events.ErlangDebuggerEvent;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.nio.ByteBuffer;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.intellij.erlang.debugger.ErlangDebuggerLog.LOG;

public class ErlangDebuggerNode {
  private static final int RECEIVE_TIMEOUT = 50;

  private OtpErlangPid myLastSuspendedPid;

  private final Queue<ErlangDebuggerCommandsProducer.ErlangDebuggerCommand> myCommandsQueue = new LinkedList<ErlangDebuggerCommandsProducer.ErlangDebuggerCommand>();
  private int myLocalDebuggerPort = -1;
  private final ErlangDebuggerEventListener myEventListener;
  private AtomicBoolean myStopped = new AtomicBoolean(false);

  public ErlangDebuggerNode(@NotNull ErlangDebuggerEventListener eventListener) throws ErlangDebuggerNodeException {
    myEventListener = eventListener;
    LOG.debug("Starting debugger server.");
    try {
      myLocalDebuggerPort = runDebuggerServer().get();
    } catch (Throwable e) {
      throw new ErlangDebuggerNodeException("Failed to start debugger server", e);
    }
  }

  public int getLocalDebuggerPort() {
    return myLocalDebuggerPort;
  }

  public void stop() {
    myStopped.set(true);
  }

  public boolean isStopped() {
    return myStopped.get();
  }

  public void processSuspended(OtpErlangPid pid) {
    myLastSuspendedPid = pid;
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

  private Future<Integer> runDebuggerServer() {
    final AsyncFutureResult<Integer> portFuture = AsyncFutureFactory.getInstance().createAsyncFutureResult();
    ApplicationManager.getApplication().executeOnPooledThread(new Runnable() {
      @Override
      public void run() {
        runDebuggerServerImpl(portFuture);
      }
    });
    return portFuture;
  }

  private void runDebuggerServerImpl(AsyncFutureResult<Integer> portFuture) {
    try {
      Exception cachedException = null;
      LOG.debug("Opening a server socket.");
      ServerSocket serverSocket = new ServerSocket(0);
      try {
        portFuture.set(serverSocket.getLocalPort());

        LOG.debug("Listening on port " + serverSocket.getLocalPort() + ".");

        Socket debuggerSocket = serverSocket.accept();
        try {
          LOG.debug("Debugger connected, closing the server socket.");
          serverSocket.close();
          myEventListener.debuggerStarted();
          LOG.debug("Starting send/receive loop.");
          serverLoop(debuggerSocket);
        } catch (Exception e) {
          cachedException = e;
          throw e;
        } finally {
          debuggerSocket.close();
        }
      } catch (Exception e) {
        if (cachedException != null) {
          if (e.getCause() == null) {
            e.initCause(cachedException);
          }
          else {
            LOG.debug("Lost exception.", cachedException);
          }
        }
        throw e;
      } finally {
        serverSocket.close();
      }
    } catch (Exception th) {
      if (!portFuture.isDone()) {
        portFuture.setException(th);
      }
      else {
        LOG.debug(th);
      }
    }
  }

  private void serverLoop(@NotNull Socket debuggerSocket) throws IOException {
    debuggerSocket.setSoTimeout(RECEIVE_TIMEOUT);

    while (!isStopped()) {
      if (!isStopped()) {
        receiveMessage(debuggerSocket);
      }
      if (!isStopped()) {
        sendMessages(debuggerSocket);
      }
    }
  }

  private void receiveMessage(@NotNull Socket socket) {
    OtpErlangObject receivedMessage = receive(socket);
    if (receivedMessage == null) return;

    LOG.debug("Message received: " + String.valueOf(receivedMessage));

    ErlangDebuggerEvent event = ErlangDebuggerEvent.create(receivedMessage);
    boolean messageRecognized = event != null;
    if (messageRecognized) {
      event.process(this, myEventListener);
    }

    LOG.debug("Message processed: " + messageRecognized);
  }

  private void sendMessages(@NotNull Socket socket) {
    synchronized (myCommandsQueue) {
      while (!myCommandsQueue.isEmpty()) {
        OtpErlangTuple message = myCommandsQueue.remove().toMessage();
        LOG.debug("Sending message: " + message);
        send(socket, message);
      }
    }
  }

  private static void send(@NotNull Socket socket, @NotNull OtpErlangObject message) {
    try {
      OutputStream out = socket.getOutputStream();

      byte[] bytes = new OtpOutputStream(message).toByteArray();
      byte[] sizeBytes = ByteBuffer.allocate(4).putInt(1 + bytes.length).array();

      out.write(sizeBytes);
      out.write(OtpExternal.versionTag);
      out.write(bytes);
    } catch (IOException e) {
      LOG.debug(e);
    }
  }

  @Nullable
  private static OtpErlangObject receive(@NotNull Socket socket) {
    try {
      InputStream in = socket.getInputStream();

      int objectSize = readObjectSize(in);
      if (objectSize == -1) return null;

      LOG.debug("Incoming packet size: " + objectSize + " bytes");

      byte[] objectBytes = readBytes(in, objectSize);
      return objectBytes == null ? null : decode(objectBytes);
    } catch (IOException e) {
      LOG.debug(e);
    }
    return null;
  }

  private static int readObjectSize(InputStream in) {
    byte[] bytes = readBytes(in, 4);
    return bytes != null ? ByteBuffer.wrap(bytes).getInt() : -1;
  }

  @Nullable
  private static byte[] readBytes(InputStream in, int size) {
    try {
      byte[] buffer = new byte[size];
      return size == in.read(buffer) ? buffer : null;
    } catch (SocketTimeoutException ignore) {
      return null;
    } catch (IOException e) {
      LOG.debug(e);
      return null;
    }
  }

  @Nullable
  private static OtpErlangObject decode(byte[] bytes) {
    try {
      return new OtpInputStream(bytes).read_any();
    } catch (OtpErlangDecodeException e) {
      LOG.debug("Failed to decode an erlang term.", e);
      return null;
    }
  }
}
