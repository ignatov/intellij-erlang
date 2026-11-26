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
import com.intellij.execution.executors.DefaultDebugExecutor;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ExecutionEnvironmentBuilder;
import com.intellij.execution.ui.ExecutionConsole;
import com.intellij.execution.ui.RunContentDescriptor;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.PlatformTestUtil;
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
import org.jetbrains.concurrency.Promise;

import javax.swing.*;
import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ErlangDebugRunnerTest extends BasePlatformTestCase {
  // XDebugSession.getRunContentDescriptor() logs "[Split debugger] RunContentDescriptor should not be used in split mode"
  // in the IDE, but not in unit test mode. So the debugger manager is replaced with one that fails on the deprecated
  // XDebuggerManager.startSession(environment, starter) and never hands out the session.
  public void testReturnsDescriptorOfSessionBuilderResult() throws ExecutionException {
    RecordingDebuggerManager manager = replaceDebuggerManager();
    ExecutionEnvironment environment = createEnvironment();

    Promise<RunContentDescriptor> result = ((ErlangDebugRunner) environment.getRunner()).execute(environment, environment.getState());

    assertSame(manager.myDescriptor, PlatformTestUtil.waitForPromise(result));
    assertNotNull(manager.myStarter);
    assertSame(environment, manager.myEnvironment);
  }

  // The platform starts the session and creates the debug process on the EDT. The debug process copied the debugger's
  // beams to a new directory of the system temp directory there, which froze the IDE for seconds when the temp directory
  // was large, so the beams must be in place before the session starts.
  public void testCopiesBeamsBeforeSessionStarts() throws ExecutionException {
    RecordingDebuggerManager manager = replaceDebuggerManager();
    ExecutionEnvironment environment = createEnvironment();
    Set<File> before = beamDirectories();

    environment.getRunner().execute(environment);
    PlatformTestUtil.waitWithEventsDispatching("the debug session is not started", () -> manager.myBeamsAtSessionStart != null, 60);

    Set<File> copied = new HashSet<>(manager.myBeamsAtSessionStart);
    copied.removeAll(before);
    assertSize(1, copied);
    assertTrue(new File(copied.iterator().next(), "debugnode.beam").isFile());
  }

  public void testFailedSessionStartDeletesBeams() throws ExecutionException {
    RecordingDebuggerManager manager = replaceDebuggerManager();
    manager.myStartError = new ExecutionException("the session cannot start");
    ExecutionEnvironment environment = createEnvironment();
    Set<File> before = beamDirectories();

    Promise<RunContentDescriptor> result = ((ErlangDebugRunner) environment.getRunner()).execute(environment, environment.getState());
    List<Throwable> errors = new ArrayList<>();
    result.onError(errors::add);
    PlatformTestUtil.waitWithEventsDispatching("the debug start did not finish", () -> result.getState() != Promise.State.PENDING, 60);

    assertEquals(List.of(manager.myStartError), errors);
    assertSize(1, manager.myBeamsAtSessionStart);
    PlatformTestUtil.waitWithEventsDispatching("the beams of a failed debug start are left in the temp directory",
                                               () -> before.containsAll(beamDirectories()), 60);
  }

  @NotNull
  private RecordingDebuggerManager replaceDebuggerManager() {
    RunContentDescriptor descriptor = new RunContentDescriptor(null, null, new JPanel(), "debug");
    Disposer.register(getTestRootDisposable(), descriptor);
    RecordingDebuggerManager manager = new RecordingDebuggerManager(XDebuggerManager.getInstance(getProject()), descriptor);
    ServiceContainerUtil.replaceService(getProject(), XDebuggerManager.class, manager, getTestRootDisposable());
    return manager;
  }

  @NotNull
  private ExecutionEnvironment createEnvironment() throws ExecutionException {
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "app", ErlangApplicationRunConfigurationType.getInstance());
    configuration.getConfigurationModule().setModule(getModule());
    return ExecutionEnvironmentBuilder
      .create(getProject(), DefaultDebugExecutor.getDebugExecutorInstance(), configuration)
      .runner(new ErlangDebugRunner())
      .build();
  }

  @NotNull
  private static Set<File> beamDirectories() {
    File[] directories = new File(FileUtil.getTempDirectory()).listFiles(file -> file.getName().startsWith("intellij_erlang_debugger_"));
    return directories != null ? Set.of(directories) : Set.of();
  }

  private static class RecordingDebuggerManager extends XDebuggerManager {
    private final XDebuggerManager myDelegate;
    private final RunContentDescriptor myDescriptor;
    private XDebugProcessStarter myStarter;
    private ExecutionEnvironment myEnvironment;
    private volatile Set<File> myBeamsAtSessionStart;
    private ExecutionException myStartError;

    RecordingDebuggerManager(@NotNull XDebuggerManager delegate, @NotNull RunContentDescriptor descriptor) {
      myDelegate = delegate;
      myDescriptor = descriptor;
    }

    @Override
    public @NotNull XDebugSessionBuilder newSessionBuilder(@NotNull XDebugProcessStarter starter) {
      myStarter = starter;
      return new XDebugSessionBuilder() {
        @Override
        public @NotNull XSessionStartedResult startSession() throws ExecutionException {
          myBeamsAtSessionStart = beamDirectories();
          if (myStartError != null) {
            throw myStartError;
          }
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
