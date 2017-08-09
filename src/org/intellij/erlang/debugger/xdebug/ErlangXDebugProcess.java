/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.debugger.xdebug;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.execution.ui.ExecutionConsole;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.MessageType;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.LightVirtualFile;
import com.intellij.util.PathUtil;
import com.intellij.util.ResourceUtil;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.io.URLUtil;
import com.intellij.xdebugger.XDebugProcess;
import com.intellij.xdebugger.XDebugSession;
import com.intellij.xdebugger.XSourcePosition;
import com.intellij.xdebugger.breakpoints.XBreakpointHandler;
import com.intellij.xdebugger.breakpoints.XLineBreakpoint;
import com.intellij.xdebugger.evaluation.EvaluationMode;
import com.intellij.xdebugger.evaluation.XDebuggerEditorsProvider;
import com.intellij.xdebugger.evaluation.XDebuggerEvaluator;
import com.intellij.xdebugger.frame.XSuspendContext;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.debugger.node.*;
import org.intellij.erlang.debugger.remote.ErlangRemoteDebugRunConfiguration;
import org.intellij.erlang.debugger.remote.ErlangRemoteDebugRunningState;
import org.intellij.erlang.debugger.xdebug.xvalue.ErlangXValueFactory;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.runconfig.ErlangRunConfigurationBase;
import org.intellij.erlang.runconfig.ErlangRunningState;
import org.intellij.erlang.utils.ErlangModulesUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.net.URL;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import static org.intellij.erlang.debugger.ErlangDebuggerLog.LOG;

public class ErlangXDebugProcess extends XDebugProcess implements ErlangDebuggerEventListener {
  private final ExecutionEnvironment myExecutionEnvironment;
  private final ErlangRunningState myRunningState;
  private final ErlangDebuggerNode myDebuggerNode;
  private final OSProcessHandler myErlangProcessHandler;
  private final ErlangDebugLocationResolver myLocationResolver;

  private XBreakpointHandler<?>[] myBreakpointHandlers = new XBreakpointHandler[]{new ErlangLineBreakpointHandler(this)};
  private ConcurrentHashMap<ErlangSourcePosition, XLineBreakpoint<ErlangLineBreakpointProperties>> myPositionToLineBreakpointMap =
    new ConcurrentHashMap<>();
  private XDebuggerEvaluator.XEvaluationCallback myEvalCallback = null;
  // right after evaluating an expression, the erlang debugger will trigger a new "breakpoint reached" event
  // unfortunately, that means this plugin will try to concurrently render the new state of the bindings as
  // well as the result from the evaluation, which Intellij doesn't seem to handle too well, resulting in
  // the debugging session losing track of what the current frame is... so we just ignore the next "breakpoint
  // reached" event after evaluating an expression if it's the same breakpoint - better ideas welcome!
  private ErlangProcessSnapshot myLastBreakpointReached = null;
  private boolean myIgnoreNextBreakpointIfSame = false;

  public ErlangXDebugProcess(@NotNull XDebugSession session, ExecutionEnvironment env) throws ExecutionException {
    //TODO add debug build targets and make sure the project is built using them.
    super(session);

    session.setPauseActionSupported(false);

    myExecutionEnvironment = env;
    myRunningState = getRunConfiguration().getState(myExecutionEnvironment.getExecutor(), myExecutionEnvironment);
    if (myRunningState == null) {
      throw new ExecutionException("Failed to execute a run configuration.");
    }

    try {
      //TODO add the debugger node to disposable hierarchy (we may fail to initialize session so the session will not be stopped!)
      myDebuggerNode = new ErlangDebuggerNode(this);
    }
    catch (ErlangDebuggerNodeException e) {
      throw new ExecutionException(e);
    }

    // it's important to set modules to interpret before running debug target
    setModulesToInterpret();
    //TODO split running debug target and debugger process spawning
    myErlangProcessHandler = runDebugTarget();

    ErlangRunConfigurationBase<?> runConfig = getRunConfiguration();
    myLocationResolver = new ErlangDebugLocationResolver(runConfig.getProject(),
                                                         runConfig.getConfigurationModule().getModule(),
                                                         runConfig.isUseTestCodePath());
  }

  @NotNull
  public ErlangDebugLocationResolver getLocationResolver() {
    return myLocationResolver;
  }

  public synchronized void evaluateExpression(@NotNull String expression,
                                              @NotNull XDebuggerEvaluator.XEvaluationCallback callback,
                                              @NotNull ErlangTraceElement traceElement) {
    myEvalCallback = callback;
    // see the comment about myIgnoreNextBreakpointIfSame
    myIgnoreNextBreakpointIfSame = true;
    myDebuggerNode.evaluate(expression, traceElement);
  }

  @Override
  public synchronized void handleEvaluationResponse(OtpErlangObject response) {
    if (myEvalCallback != null) {
      String error = maybeExtractErrorFromEvaluationResponse(response);

      if (error == null) {
        myEvalCallback.evaluated(ErlangXValueFactory.create(response));
      }
      else {
        myEvalCallback.errorOccurred(error);
      }
    }
  }

  // Parses the response from an evaluation and determines whether it's an error
  // response or not; if it is an error, formats it as a displayable string, if
  // it's not, just returns null
  private static String maybeExtractErrorFromEvaluationResponse(OtpErlangObject response) {
    // is it a parsing error?
    if (response instanceof OtpErlangAtom && ((OtpErlangAtom) response).atomValue().equals("Parse error")) {
      return "Parse error";
    }

    // is it an uncaught exception?
    if (response instanceof OtpErlangTuple) {
      OtpErlangObject[] elements = ((OtpErlangTuple) response).elements();
      if (elements.length == 2
          && elements[0] instanceof OtpErlangAtom
          && ((OtpErlangAtom) elements[0]).atomValue().equals("EXIT")) {
        return "Uncaught exception: " + elements[1];
      }
    }

    return null;
  }

  @Override
  public void debuggerStarted() {
    getSession().reportMessage("Debug process started", MessageType.INFO);
  }

  @Override
  public void failedToInterpretModules(String nodeName, List<String> modules) {
    String messagePrefix = "Failed to interpret modules on node " + nodeName + ": ";
    String modulesString = StringUtil.join(modules, ", ");
    String messageSuffix = ".\nMake sure they are compiled with debug_info option, their sources are located in same directory as .beam files, modules are available on the node.";
    String message = messagePrefix + modulesString + messageSuffix;
    getSession().reportMessage(message, MessageType.WARNING);
  }

  @Override
  public void failedToDebugRemoteNode(String nodeName, String error) {
    String message = "Failed to debug remote node '" + nodeName + "'. Details: " + error;
    getSession().reportMessage(message, MessageType.ERROR);
  }

  @Override
  public void unknownMessage(String messageText) {
    getSession().reportMessage("Unknown message received: " + messageText, MessageType.WARNING);
  }

  @Override
  public void failedToSetBreakpoint(String module, int line, String errorMessage) {
    ErlangSourcePosition sourcePosition = ErlangSourcePosition.create(myLocationResolver, module, line);
    XLineBreakpoint<ErlangLineBreakpointProperties> breakpoint = getLineBreakpoint(sourcePosition);
    if (breakpoint != null) {
      getSession().updateBreakpointPresentation(breakpoint, AllIcons.Debugger.Db_invalid_breakpoint, errorMessage);
    }
  }

  @Override
  public void breakpointIsSet(String module, int line) {
  }

  @Override
  public synchronized void breakpointReached(final OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots) {
    ErlangProcessSnapshot processInBreakpoint = ContainerUtil.find(snapshots, erlangProcessSnapshot -> erlangProcessSnapshot.getPid().equals(pid));
    assert processInBreakpoint != null;

    boolean ignoreIfSame = myIgnoreNextBreakpointIfSame;
    myIgnoreNextBreakpointIfSame = false;
    if (ignoreIfSame && myLastBreakpointReached.isSameBreakpoint(processInBreakpoint)) {
      // see the comment about myLastBreakpointReached
      return;
    }

    myLastBreakpointReached = processInBreakpoint;
    ErlangSourcePosition breakPosition = ErlangSourcePosition.create(myLocationResolver, processInBreakpoint);
    XLineBreakpoint<ErlangLineBreakpointProperties> breakpoint = getLineBreakpoint(breakPosition);
    ErlangSuspendContext suspendContext = new ErlangSuspendContext(this, pid, snapshots);
    if (breakpoint == null) {
      getSession().positionReached(suspendContext);
    }
    else {
      boolean shouldSuspend = getSession().breakpointReached(breakpoint, null, suspendContext);
      if (!shouldSuspend) {
        resume(suspendContext);
      }
    }
  }

  @Override
  public void debuggerStopped() {
    getSession().reportMessage("Debug process stopped", MessageType.INFO);
    getSession().stop();
  }

  @Nullable
  private XLineBreakpoint<ErlangLineBreakpointProperties> getLineBreakpoint(@Nullable ErlangSourcePosition sourcePosition) {
    return sourcePosition != null ? myPositionToLineBreakpointMap.get(sourcePosition) : null;
  }

  private void setModulesToInterpret() {
    Project project = myExecutionEnvironment.getProject();
    Collection<ErlangFile> erlangModules = ErlangModulesUtil.getErlangModules(project);
    ErlangRunConfigurationBase<?> runConfiguration = getRunConfiguration();
    if (runConfiguration.isUseTestCodePath()) {
      HashSet<ErlangFile> erlangTestModules = new HashSet<>();
      for (Module module : runConfiguration.getModules()) {
        erlangTestModules.addAll(ErlangModulesUtil.getErlangModules(module, true));
      }
      erlangTestModules.addAll(erlangModules);
      erlangModules = erlangTestModules;
    }
    Set<String> notToInterpret = runConfiguration.getDebugOptions().getModulesNotToInterpret();
    List<String> moduleSourcePaths = ContainerUtil.newArrayListWithCapacity(erlangModules.size());
    for (ErlangFile erlangModule : erlangModules) {
      VirtualFile file = erlangModule.getVirtualFile();
      if (file != null && !notToInterpret.contains(file.getNameWithoutExtension())) {
        moduleSourcePaths.add(PathUtil.getLocalPath(file));
      }
    }
    myDebuggerNode.interpretModules(moduleSourcePaths);
  }

  @NotNull
  @Override
  public ExecutionConsole createConsole() {
    ConsoleView consoleView = myRunningState.createConsoleView(myExecutionEnvironment.getExecutor());
    consoleView.attachToProcess(getProcessHandler());
    myErlangProcessHandler.startNotify();
    return consoleView;
  }

  @NotNull
  @Override
  public XBreakpointHandler<?>[] getBreakpointHandlers() {
    return myBreakpointHandlers;
  }

  @NotNull
  @Override
  public XDebuggerEditorsProvider getEditorsProvider() {
    return new XDebuggerEditorsProvider() {
      @NotNull
      @Override
      public FileType getFileType() {
        return ErlangFileType.MODULE;
      }

      @NotNull
      @Override
      public Document createDocument(@NotNull Project project,
                                     @NotNull String text,
                                     @Nullable XSourcePosition sourcePosition,
                                     @NotNull EvaluationMode mode) {
        LightVirtualFile file = new LightVirtualFile("plain-text-erlang-debugger.txt", text);
        //noinspection ConstantConditions
        return FileDocumentManager.getInstance().getDocument(file);
      }
    };
  }

  @Override
  public void startStepOver(@Nullable XSuspendContext context) {
    myDebuggerNode.stepOver();
  }

  @Override
  public void startStepInto(@Nullable XSuspendContext context) {
    myDebuggerNode.stepInto();
  }

  @Override
  public void startStepOut(@Nullable XSuspendContext context) {
    myDebuggerNode.stepOut();
  }

  @Override
  public void stop() {
    myDebuggerNode.stop();
  }

  @Override
  public void resume(@Nullable XSuspendContext context) {
    myDebuggerNode.resume();
  }

  @Override
  public void runToPosition(@NotNull XSourcePosition position, @Nullable XSuspendContext context) {
    //TODO implement me
  }

  @Nullable
  @Override
  protected ProcessHandler doGetProcessHandler() {
    return myErlangProcessHandler;
  }

  void addBreakpoint(XLineBreakpoint<ErlangLineBreakpointProperties> breakpoint) {
    ErlangSourcePosition breakpointPosition = getErlangSourcePosition(breakpoint);
    if (breakpointPosition == null) return;
    myPositionToLineBreakpointMap.put(breakpointPosition, breakpoint);
    myDebuggerNode.setBreakpoint(breakpointPosition.getErlangModuleName(), breakpointPosition.getLine());
  }

  void removeBreakpoint(XLineBreakpoint<ErlangLineBreakpointProperties> breakpoint,
                        @SuppressWarnings("UnusedParameters") boolean temporary) {
    ErlangSourcePosition breakpointPosition = getErlangSourcePosition(breakpoint);
    if (breakpointPosition == null) return;
    myPositionToLineBreakpointMap.remove(breakpointPosition);
    myDebuggerNode.removeBreakpoint(breakpointPosition.getErlangModuleName(), breakpointPosition.getLine());
  }

  @Nullable
  private static ErlangSourcePosition getErlangSourcePosition(XLineBreakpoint<ErlangLineBreakpointProperties> breakpoint) {
    XSourcePosition sourcePosition = breakpoint.getSourcePosition();
    return sourcePosition != null ? ErlangSourcePosition.create(sourcePosition) : null;
  }

  private ErlangRunConfigurationBase<?> getRunConfiguration() {
    ErlangRunConfigurationBase<?> runConfig = (ErlangRunConfigurationBase) getSession().getRunProfile();
    assert runConfig != null;
    return runConfig;
  }

  @NotNull
  private OSProcessHandler runDebugTarget() throws ExecutionException {
    OSProcessHandler erlangProcessHandler;
    LOG.debug("Preparing to run debug target.");
    try {
      GeneralCommandLine commandLine = new GeneralCommandLine();
      myRunningState.setExePath(commandLine);
      myRunningState.setWorkDirectory(commandLine);
      setUpErlangDebuggerCodePath(commandLine);
      myRunningState.setCodePath(commandLine);
      commandLine.addParameters("-run", "debugnode", "main", String.valueOf(myDebuggerNode.getLocalDebuggerPort()));
      myRunningState.setErlangFlags(commandLine);
      myRunningState.setNoShellMode(commandLine);
      myRunningState.setStopErlang(commandLine);

      LOG.debug("Running debugger process. Command line (platform-independent): ");
      LOG.debug(commandLine.getCommandLineString());

      Process process = commandLine.createProcess();
      erlangProcessHandler = new OSProcessHandler(process, commandLine.getCommandLineString());

      LOG.debug("Debugger process started.");

      if (myRunningState instanceof ErlangRemoteDebugRunningState) {
        LOG.debug("Initializing remote node debugging.");
        ErlangRemoteDebugRunConfiguration runConfiguration = (ErlangRemoteDebugRunConfiguration) getRunConfiguration();
        if (StringUtil.isEmptyOrSpaces(runConfiguration.getRemoteErlangNodeName())) {
          throw new ExecutionException("Bad run configuration: remote Erlang node is not specified.");
        }
        LOG.debug("Remote node: " + runConfiguration.getRemoteErlangNodeName());
        LOG.debug("Cookie: " + runConfiguration.getCookie());
        myDebuggerNode.debugRemoteNode(runConfiguration.getRemoteErlangNodeName(), runConfiguration.getCookie());
      }
      else {
        LOG.debug("Initializing local debugging.");
        ErlangRunningState.ErlangEntryPoint entryPoint = myRunningState.getDebugEntryPoint();
        LOG.debug("Entry point: " + entryPoint.getModuleName() + ":" + entryPoint.getFunctionName() +
                  "(" + StringUtil.join(entryPoint.getArgsList(), ", ") + ")");
        myDebuggerNode.runDebugger(entryPoint.getModuleName(), entryPoint.getFunctionName(), entryPoint.getArgsList());
      }
    }
    catch (ExecutionException e) {
      LOG.debug("Failed to run debug target.", e);
      throw e;
    }
    LOG.debug("Debug target should now be running.");
    return erlangProcessHandler;
  }

  private static void setUpErlangDebuggerCodePath(GeneralCommandLine commandLine) throws ExecutionException {
    LOG.debug("Setting up debugger environment.");
    try {
      String[] beams = {"debugnode.beam", "remote_debugger.beam", "remote_debugger_listener.beam", "remote_debugger_notifier.beam"};
      File tempDirectory = FileUtil.createTempDirectory("intellij_erlang_debugger_", null);
      LOG.debug("Debugger beams will be put to: " + tempDirectory.getPath());
      for (String beam : beams) {
        copyBeamTo(beam, tempDirectory);
      }
      LOG.debug("Debugger beams were copied successfully.");
      commandLine.addParameters("-pa", tempDirectory.getPath());
    }
    catch (IOException e) {
      throw new ExecutionException("Failed to setup debugger environment", e);
    }
  }

  private static void copyBeamTo(String beamName, File directory) throws IOException {
    URL beamUrl = ResourceUtil.getResource(ErlangXDebugProcess.class, "/debugger/beams", beamName);
    if (beamUrl == null) {
      throw new IOException("Failed to locate debugger module: " + beamName);
    }
    try (BufferedInputStream inputStream = new BufferedInputStream(URLUtil.openStream(beamUrl))) {
      try (BufferedOutputStream outputStream = new BufferedOutputStream(new FileOutputStream(new File(directory, beamName)))) {
        FileUtil.copy(inputStream, outputStream);
      }
    }
  }
}
