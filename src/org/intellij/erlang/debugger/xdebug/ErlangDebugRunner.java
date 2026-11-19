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

package org.intellij.erlang.debugger.xdebug;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.RunProfile;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.configurations.RunnerSettings;
import com.intellij.execution.executors.DefaultDebugExecutor;
import com.intellij.execution.runners.AsyncProgramRunner;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.ui.RunContentDescriptor;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.xdebugger.XDebugProcess;
import com.intellij.xdebugger.XDebugProcessStarter;
import com.intellij.xdebugger.XDebugSession;
import com.intellij.xdebugger.XDebuggerManager;
import org.intellij.erlang.application.ErlangApplicationConfiguration;
import org.intellij.erlang.debugger.remote.ErlangRemoteDebugRunConfiguration;
import org.intellij.erlang.eunit.ErlangUnitRunConfiguration;
import org.intellij.erlang.runconfig.ErlangRunningState;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.concurrency.AsyncPromise;
import org.jetbrains.concurrency.Promise;

public class ErlangDebugRunner extends AsyncProgramRunner<RunnerSettings> {
  private static final String ERLANG_DEBUG_RUNNER_ID = "ErlangDebugRunner";

  // The platform creates the debug process on the EDT, so what it needs from the disk is prepared on a background thread first.
  @NotNull
  @Override
  protected Promise<RunContentDescriptor> execute(@NotNull ExecutionEnvironment environment, @NotNull RunProfileState state) {
    AsyncPromise<RunContentDescriptor> result = new AsyncPromise<>();
    ModalityState modalityState = ModalityState.defaultModalityState();
    Application application = ApplicationManager.getApplication();
    application.executeOnPooledThread(() -> {
      ErlangXDebugProcess.SetUp setUp;
      try {
        setUp = ErlangXDebugProcess.prepare(environment, (ErlangRunningState) state);
      }
      catch (Throwable e) {
        // settled on the EDT, as below: the caller adds its error handler there after this method returns
        application.invokeLater(() -> result.setError(e), modalityState);
        return;
      }
      application.invokeLater(() -> {
        try {
          if (environment.getProject().isDisposed()) {
            throw new ExecutionException("The project is closed");
          }
          result.setResult(startSession(environment, setUp));
        }
        catch (Throwable e) {
          application.executeOnPooledThread(setUp::deleteBeams);
          result.setError(e);
        }
      }, modalityState);
    });
    return result;
  }

  @Nullable
  private static RunContentDescriptor startSession(@NotNull ExecutionEnvironment environment,
                                                   @NotNull ErlangXDebugProcess.SetUp setUp) throws ExecutionException {
    XDebuggerManager xDebuggerManager = XDebuggerManager.getInstance(environment.getProject());
    return xDebuggerManager.newSessionBuilder(new XDebugProcessStarter() {
      @NotNull
      @Override
      public XDebugProcess start(@NotNull XDebugSession session) throws ExecutionException {
        return new ErlangXDebugProcess(session, environment, setUp);
      }
    }).environment(environment).startSession().getRunContentDescriptor();
  }

  @NotNull
  @Override
  public String getRunnerId() {
    return ERLANG_DEBUG_RUNNER_ID;
  }

  @Override
  public boolean canRun(@NotNull String executorId, @NotNull RunProfile profile) {
    return DefaultDebugExecutor.EXECUTOR_ID.equals(executorId) &&
      (profile instanceof ErlangApplicationConfiguration ||
        profile instanceof ErlangUnitRunConfiguration ||
        profile instanceof ErlangRemoteDebugRunConfiguration);
  }
}