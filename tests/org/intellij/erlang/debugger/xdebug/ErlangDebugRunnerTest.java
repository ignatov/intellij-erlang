/*
 * Copyright 2012-2026 Sergey Ignatov
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
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.executors.DefaultDebugExecutor;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ExecutionEnvironmentBuilder;
import com.intellij.execution.ui.ExecutionConsole;
import com.intellij.execution.ui.RunContentDescriptor;
import com.intellij.openapi.util.Disposer;
import com.intellij.testFramework.ServiceContainerUtil;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import com.intellij.xdebugger.XDebugProcess;
import com.intellij.xdebugger.XDebugProcessStarter;
import com.intellij.xdebugger.XDebugSession;
import com.intellij.xdebugger.XDebugSessionBuilder;
import com.intellij.xdebugger.XDebuggerManager;
import com.intellij.xdebugger.XSessionStartedResult;
import com.intellij.xdebugger.breakpoints.XBreakpointManager;
import org.intellij.erlang.application.ErlangApplicationConfiguration;
import org.intellij.erlang.application.ErlangApplicationRunConfigurationType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.List;

public class ErlangDebugRunnerTest extends BasePlatformTestCase {
  // XDebugSession.getRunContentDescriptor() logs "[Split debugger] RunContentDescriptor should not be used in split mode"
  // in the IDE, but not in unit test mode. So the debugger manager is replaced with one that fails on the deprecated
  // XDebuggerManager.startSession(environment, starter) and never hands out the session.
  public void testReturnsDescriptorOfSessionBuilderResult() throws ExecutionException {
    RunContentDescriptor descriptor = new RunContentDescriptor(null, null, new JPanel(), "debug");
    Disposer.register(getTestRootDisposable(), descriptor);
    RecordingDebuggerManager manager = new RecordingDebuggerManager(XDebuggerManager.getInstance(getProject()), descriptor);
    ServiceContainerUtil.replaceService(getProject(), XDebuggerManager.class, manager, getTestRootDisposable());
    ErlangDebugRunner runner = new ErlangDebugRunner();
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "app", ErlangApplicationRunConfigurationType.getInstance());
    ExecutionEnvironment environment = ExecutionEnvironmentBuilder
      .create(getProject(), DefaultDebugExecutor.getDebugExecutorInstance(), configuration)
      .runner(runner)
      .build();
    RunProfileState state = (executor, programRunner) -> null;

    assertSame(descriptor, runner.doExecute(state, environment));
    assertNotNull(manager.myStarter);
    assertSame(environment, manager.myEnvironment);
  }

  private static class RecordingDebuggerManager extends XDebuggerManager {
    private final XDebuggerManager myDelegate;
    private final RunContentDescriptor myDescriptor;
    private XDebugProcessStarter myStarter;
    private ExecutionEnvironment myEnvironment;

    RecordingDebuggerManager(@NotNull XDebuggerManager delegate, @NotNull RunContentDescriptor descriptor) {
      myDelegate = delegate;
      myDescriptor = descriptor;
    }

    @Override
    public @NotNull XDebugSessionBuilder newSessionBuilder(@NotNull XDebugProcessStarter starter) {
      myStarter = starter;
      return new XDebugSessionBuilder() {
        @Override
        public @NotNull XSessionStartedResult startSession() {
          return new XSessionStartedResult() {
            @Override
            public @NotNull XDebugSession getSession() {
              throw new AssertionError("The runner must take the descriptor from the start result, not from the session");
            }

            @Override
            public @Nullable RunContentDescriptor getRunContentDescriptor() {
              return myDescriptor;
            }
          };
        }

        @Override
        public @NotNull XDebugSessionBuilder environment(@NotNull ExecutionEnvironment environment) {
          myEnvironment = environment;
          return this;
        }

        @Override
        public @NotNull XDebugSessionBuilder sessionName(@NotNull String sessionName) {
          throw unexpected();
        }

        @Override
        public @NotNull XDebugSessionBuilder icon(@Nullable Icon icon) {
          throw unexpected();
        }

        @Override
        public @NotNull XDebugSessionBuilder contentToReuse(@Nullable RunContentDescriptor contentToReuse) {
          throw unexpected();
        }

        @Override
        public @NotNull XDebugSessionBuilder showTab(boolean value) {
          throw unexpected();
        }

        @Override
        public @NotNull XDebugSessionBuilder showToolWindowOnSuspendOnly(boolean value) {
          throw unexpected();
        }

        private AssertionError unexpected() {
          return new AssertionError("The debug runner starts the session with the environment only, as startSession(environment, starter) did");
        }
      };
    }

    @Override
    public @NotNull XDebugSession startSession(@NotNull ExecutionEnvironment environment,
                                               @NotNull XDebugProcessStarter processStarter) {
      throw new AssertionError("Deprecated XDebuggerManager.startSession(environment, starter) is called; " +
                               "its XDebugSession.getRunContentDescriptor() logs a split debugger error");
    }

    @Override
    public @NotNull XDebugSession startSessionAndShowTab(@NotNull String sessionName,
                                                         @Nullable RunContentDescriptor contentToReuse,
                                                         @NotNull XDebugProcessStarter starter) {
      throw new AssertionError("Deprecated XDebuggerManager.startSessionAndShowTab is called");
    }

    @Override
    public @NotNull XDebugSession startSessionAndShowTab(@NotNull String sessionName,
                                                         @NotNull XDebugProcessStarter starter,
                                                         @NotNull ExecutionEnvironment environment) {
      throw new AssertionError("Deprecated XDebuggerManager.startSessionAndShowTab is called");
    }

    @Override
    public @NotNull XDebugSession startSessionAndShowTab(@NotNull String sessionName,
                                                         @Nullable RunContentDescriptor contentToReuse,
                                                         boolean showToolWindowOnSuspendOnly,
                                                         @NotNull XDebugProcessStarter starter) {
      throw new AssertionError("Deprecated XDebuggerManager.startSessionAndShowTab is called");
    }

    @Override
    public @NotNull XDebugSession startSessionAndShowTab(@NotNull String sessionName,
                                                         @Nullable Icon icon,
                                                         @Nullable RunContentDescriptor contentToReuse,
                                                         boolean showToolWindowOnSuspendOnly,
                                                         @NotNull XDebugProcessStarter starter) {
      throw new AssertionError("Deprecated XDebuggerManager.startSessionAndShowTab is called");
    }

    @Override
    public @NotNull XBreakpointManager getBreakpointManager() {
      return myDelegate.getBreakpointManager();
    }

    @Override
    public XDebugSession @NotNull [] getDebugSessions() {
      return myDelegate.getDebugSessions();
    }

    @Override
    public @Nullable XDebugSession getDebugSession(@NotNull ExecutionConsole executionConsole) {
      return myDelegate.getDebugSession(executionConsole);
    }

    @Override
    public @NotNull <T extends XDebugProcess> List<? extends T> getDebugProcesses(Class<T> processClass) {
      return myDelegate.getDebugProcesses(processClass);
    }

    @Override
    public @Nullable XDebugSession getCurrentSession() {
      return myDelegate.getCurrentSession();
    }
  }
}
