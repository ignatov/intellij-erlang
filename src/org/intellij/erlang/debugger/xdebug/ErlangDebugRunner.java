package org.intellij.erlang.debugger.xdebug;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.RunProfile;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.executors.DefaultDebugExecutor;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.GenericProgramRunner;
import com.intellij.execution.ui.RunContentDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.xdebugger.XDebugProcess;
import com.intellij.xdebugger.XDebugProcessStarter;
import com.intellij.xdebugger.XDebugSession;
import com.intellij.xdebugger.XDebuggerManager;
import org.intellij.erlang.application.ErlangApplicationConfiguration;
import org.intellij.erlang.eunit.ErlangUnitRunConfiguration;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @author savenko
 */
public class ErlangDebugRunner extends GenericProgramRunner {
  public static final String ERLANG_DEBUG_RUNNER_ID = "ErlangDebugRunner";

  @Nullable
  @Override
  protected RunContentDescriptor doExecute(Project project,
                                           Executor executor,
                                           RunProfileState state,
                                           RunContentDescriptor contentToReuse,
                                           final ExecutionEnvironment env) throws ExecutionException {
    XDebuggerManager xDebuggerManager = XDebuggerManager.getInstance(project);
    return xDebuggerManager.startSession(this, env, contentToReuse, new XDebugProcessStarter() {
      @NotNull
      @Override
      public XDebugProcess start(@NotNull XDebugSession session) throws ExecutionException {
        return new ErlangXDebugProcess(session, env);
      }
    }).getRunContentDescriptor();
  }

  @NotNull
  @Override
  public String getRunnerId() {
    return ERLANG_DEBUG_RUNNER_ID;
  }

  @Override
  public boolean canRun(@NotNull String executorId, @NotNull RunProfile profile) {
    return DefaultDebugExecutor.EXECUTOR_ID.equals(executorId) &&
      (profile instanceof ErlangApplicationConfiguration || profile instanceof ErlangUnitRunConfiguration);
  }
}