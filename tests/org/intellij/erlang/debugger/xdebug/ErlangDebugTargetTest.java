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

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.executors.DefaultDebugExecutor;
import com.intellij.execution.process.NopProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ExecutionEnvironmentBuilder;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.util.PathUtil;
import com.intellij.util.TimeoutUtil;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.xdebugger.XDebugSession;
import org.intellij.erlang.application.ErlangApplicationConfiguration;
import org.intellij.erlang.application.ErlangApplicationRunConfigurationType;
import org.intellij.erlang.application.ErlangApplicationRunningState;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;
import org.intellij.erlang.debugger.node.ErlangProcessSnapshot;
import org.intellij.erlang.eunit.ErlangEunitReporterModule;
import org.intellij.erlang.eunit.ErlangUnitRunConfiguration;
import org.intellij.erlang.eunit.ErlangUnitRunConfigurationType;
import org.intellij.erlang.eunit.ErlangUnitRunningState;
import org.intellij.erlang.runconfig.ErlangRunConfigurationBase;
import org.intellij.erlang.runconfig.ErlangRunningState;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

public class ErlangDebugTargetTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/", ErlangSdkRelease.V_R15B02);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  // #616: "-run debugnode main" blocks until the session ends, so the user's flags must come before it
  public void testErlFlagsPrecedeDebugNode() throws Exception {
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "app", ErlangApplicationRunConfigurationType.getInstance());
    configuration.setModuleAndFunction("riak_dt_runner boot");
    configuration.setErlFlags("-s lager start -extra foo bar");
    ExecutionEnvironment environment = createEnvironment(configuration);
    ErlangApplicationRunningState state = new ErlangApplicationRunningState(environment, getModule(), configuration);

    GeneralCommandLine commandLine = new GeneralCommandLine();
    ErlangXDebugProcess.addDebugTargetParameters(commandLine, state, prepare(environment, state).codePath(), 4242);

    assertEquals(List.of("-pa", "/src", "-s", "lager", "start", "-run", "debugnode", "main", "4242", "-noshell",
                         "-s", "init", "stop", "-extra", "foo", "bar"), commandLine.getParametersList().getList());
  }

  public void testPrepareFindsCodePathAndModulesToInterpret() throws Exception {
    VirtualFile debugged = myFixture.addFileToProject("debugged.erl", "-module(debugged).").getVirtualFile();
    myFixture.addFileToProject("skipped.erl", "-module(skipped).");
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "app", ErlangApplicationRunConfigurationType.getInstance());
    configuration.getDebugOptions().setModulesNotToInterpret(Set.of("skipped"));
    ExecutionEnvironment environment = createEnvironment(configuration);

    ErlangXDebugProcess.SetUp setUp = prepare(environment, new ErlangApplicationRunningState(environment, getModule(), configuration));

    assertEquals(List.of("-pa", "/src"), setUp.codePath());
    assertEquals(List.of(PathUtil.getLocalPath(debugged)), setUp.moduleSourcePaths());
    assertTrue(new File(setUp.beamsDirectory(), "debugnode.beam").isFile());
  }

  // a write action waits for a blocking read action, and the EDT with it, so the scan of the sources must give way
  public void testScanForModulesToInterpretGivesWayToWriteActions() throws Exception {
    myFixture.addFileToProject("debugged.erl", "-module(debugged).");
    CountDownLatch inScan = new CountDownLatch(1);
    AtomicBoolean written = new AtomicBoolean();
    AtomicBoolean waited = new AtomicBoolean();
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "app", ErlangApplicationRunConfigurationType.getInstance()) {
        @Override
        public ErlangDebugOptions getDebugOptions() {
          if (waited.compareAndSet(false, true)) {
            inScan.countDown();
            long end = System.currentTimeMillis() + 10_000;
            while (!written.get() && System.currentTimeMillis() < end) {
              ProgressManager.checkCanceled();
              TimeoutUtil.sleep(10);
            }
          }
          return super.getDebugOptions();
        }
      };
    ExecutionEnvironment environment = createEnvironment(configuration);
    ErlangApplicationRunningState state = new ErlangApplicationRunningState(environment, getModule(), configuration);
    Future<ErlangXDebugProcess.SetUp> setUp = ApplicationManager.getApplication().executeOnPooledThread(() -> ErlangXDebugProcess.prepare(environment, state));
    PlatformTestUtil.waitWithEventsDispatching("the scan did not start", () -> inScan.getCount() == 0, 30);

    long start = System.currentTimeMillis();
    WriteAction.run(() -> written.set(true));
    long writeWait = System.currentTimeMillis() - start;

    PlatformTestUtil.waitWithEventsDispatching("prepare did not finish", setUp::isDone, 30);
    assertNotEmpty(setUp.get().moduleSourcePaths());
    assertTrue("a write action waited " + writeWait + " ms for the scan of the sources", writeWait < 5_000);
  }

  // EUnit puts its reporter module on the code path in a directory of its own, made for one session
  public void testEunitReporterDirectoryIsDeletedWhenTargetTerminates() throws Exception {
    ErlangUnitRunConfiguration configuration =
      new ErlangUnitRunConfiguration(getProject(), "eunit", ErlangUnitRunConfigurationType.getInstance());
    ExecutionEnvironment environment = createEnvironment(configuration);
    ErlangXDebugProcess.SetUp setUp = prepare(environment, new ErlangUnitRunningState(environment, getModule(), configuration));
    File reporter = ContainerUtil.find(ContainerUtil.map(setUp.codePath(), File::new),
                                       file -> file.getName().startsWith(ErlangEunitReporterModule.MODULE_NAME));
    assertNotNull("no reporter directory on the code path " + setUp.codePath(), reporter);
    assertTrue(new File(reporter, ErlangEunitReporterModule.MODULE_NAME + ".beam").isFile());
    NopProcessHandler processHandler = new NopProcessHandler();
    ErlangXDebugProcess.deleteBeamsOnTermination(processHandler, setUp);
    processHandler.startNotify();

    processHandler.destroyProcess();

    PlatformTestUtil.waitWithEventsDispatching("the reporter directory of the session is left after the debug target terminated",
                                               () -> !reporter.exists() && !setUp.beamsDirectory().exists(), 30);
  }

  // a write action waits for a read action, and the EDT with it, so the reporter directory must be made outside one
  public void testEunitReporterDirectoryIsCreatedOutsideReadAction() throws Exception {
    ErlangUnitRunConfiguration configuration =
      new ErlangUnitRunConfiguration(getProject(), "eunit", ErlangUnitRunConfigurationType.getInstance());
    ExecutionEnvironment environment = createEnvironment(configuration);
    AtomicReference<Boolean> readAccess = new AtomicReference<>();
    ErlangUnitRunningState state = new ErlangUnitRunningState(environment, getModule(), configuration) {
      @Override
      public @NotNull List<File> createCodePathDirectories() throws ExecutionException {
        readAccess.set(ApplicationManager.getApplication().isReadAccessAllowed());
        return super.createCodePathDirectories();
      }
    };

    prepare(environment, state);

    assertEquals("the reporter directory is created under a read action", Boolean.FALSE, readAccess.get());
  }

  @NotNull
  private ExecutionEnvironment createEnvironment(@NotNull ErlangRunConfigurationBase<?> configuration) throws ExecutionException {
    configuration.getConfigurationModule().setModule(getModule());
    return ExecutionEnvironmentBuilder
      .create(getProject(), DefaultDebugExecutor.getDebugExecutorInstance(), configuration)
      .runner(new ErlangDebugRunner())
      .build();
  }

  // the runner calls prepare on a pooled thread; the EDT dispatches events meanwhile, as in the IDE
  @NotNull
  private static ErlangXDebugProcess.SetUp prepare(@NotNull ExecutionEnvironment environment,
                                                   @NotNull ErlangRunningState state) throws Exception {
    Future<ErlangXDebugProcess.SetUp> setUp =
      ApplicationManager.getApplication().executeOnPooledThread(() -> ErlangXDebugProcess.prepare(environment, state));
    PlatformTestUtil.waitWithEventsDispatching("prepare did not finish", setUp::isDone, 30);
    return setUp.get();
  }

  public void testStopUnblocksDebuggerNodeBeforeConnect() throws Exception {
    CountDownLatch stopped = new CountDownLatch(1);
    ErlangDebuggerNode node = new ErlangDebuggerNode(new StoppedListener(stopped));

    node.stop();

    assertTrue("the debugger node must stop without a connection", stopped.await(10, TimeUnit.SECONDS));
  }

  // a user flag that fails at boot kills the debug target before it connects
  public void testDebuggerNodeStopsWhenTargetTerminatesBeforeConnect() throws Exception {
    CountDownLatch stopped = new CountDownLatch(1);
    ErlangDebuggerNode node = new ErlangDebuggerNode(new StoppedListener(stopped));
    NopProcessHandler processHandler = new NopProcessHandler();
    ErlangXDebugProcess.stopDebuggerNodeOnTermination(processHandler, node);
    processHandler.startNotify();

    processHandler.destroyProcess();

    assertTrue("the debugger node must stop when the debug target terminates", stopped.await(10, TimeUnit.SECONDS));
  }

  public void testBeamsAreDeletedWhenTargetTerminates() throws Exception {
    File beams = FileUtil.createTempDirectory("intellij_erlang_debugger_", null);
    FileUtil.writeToFile(new File(beams, "debugnode.beam"), "");
    NopProcessHandler processHandler = new NopProcessHandler();
    ErlangXDebugProcess.deleteBeamsOnTermination(processHandler, new ErlangXDebugProcess.SetUp(beams, List.of(), List.of(), List.of()));
    processHandler.startNotify();

    processHandler.destroyProcess();

    PlatformTestUtil.waitWithEventsDispatching("the beams of the session must be deleted when the debug target terminates",
                                               () -> !beams.exists(), 30);
  }

  // #1324: the mock SDK has no bin/erl, so starting the debug target fails after the debugger node has opened its port;
  // the failed start must stop the node instead of leaving its server thread in accept() until the IDE exits
  public void testFailedStartStopsDebuggerNode() throws Exception {
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "app", ErlangApplicationRunConfigurationType.getInstance());
    configuration.getConfigurationModule().setModule(getModule());
    configuration.setModuleAndFunction("app main");
    ExecutionEnvironment environment = ExecutionEnvironmentBuilder
      .create(getProject(), DefaultDebugExecutor.getDebugExecutorInstance(), configuration)
      .build();
    List<String> sessionCalls = Collections.synchronizedList(new ArrayList<>());
    XDebugSession session = (XDebugSession)Proxy.newProxyInstance(
      getClass().getClassLoader(), new Class<?>[]{XDebugSession.class},
      (proxy, method, args) -> {
        if (method.getName().equals("reportMessage") || method.getName().equals("stop")) {
          sessionCalls.add(method.getName() + (args != null && args.length > 0 ? "(" + args[0] + ")" : "()"));
        }
        if (method.getName().equals("getRunProfile")) return configuration;
        if (method.getName().equals("getProject")) return getProject();
        if (method.getName().equals("toString")) return "session";
        if (method.getName().equals("hashCode")) return System.identityHashCode(proxy);
        if (method.getName().equals("equals")) return proxy == args[0];
        Class<?> type = method.getReturnType();
        if (type == boolean.class) return false;
        if (type == int.class) return 0;
        if (type == long.class) return 0L;
        return null;
      });
    Set<Thread> serversBefore = debuggerServerThreads();

    File beams = FileUtil.createTempDirectory("intellij_erlang_debugger_", null);
    ErlangXDebugProcess.SetUp setUp = new ErlangXDebugProcess.SetUp(beams, List.of(), List.of(), List.of());

    String startError = null;
    try {
      new ErlangXDebugProcess(session, environment, setUp);
    }
    catch (ExecutionException e) {
      startError = e.getMessage();
    }
    assertNotNull("starting the debug target must fail without bin/erl in the SDK", startError);

    Set<Thread> leaked;
    long deadline = System.currentTimeMillis() + 10_000;
    do {
      leaked = debuggerServerThreads();
      leaked.removeAll(serversBefore);
      if (leaked.isEmpty()) break;
      Thread.sleep(100);
    }
    while (System.currentTimeMillis() < deadline);
    assertTrue("a failed debug start (" + startError + ") left " + leaked.size() + " debugger server thread(s) in accept(): " + leaked, leaked.isEmpty());
    synchronized (sessionCalls) {
      assertEquals("a failed debug start must report only its error to the session", List.of(), sessionCalls);
    }
  }

  private static Set<Thread> debuggerServerThreads() {
    Set<Thread> threads = new HashSet<>();
    for (Map.Entry<Thread, StackTraceElement[]> entry : Thread.getAllStackTraces().entrySet()) {
      for (StackTraceElement element : entry.getValue()) {
        if (element.getClassName().equals(ErlangDebuggerNode.class.getName()) &&
            element.getMethodName().equals("runDebuggerServerImpl")) {
          threads.add(entry.getKey());
          break;
        }
      }
    }
    return threads;
  }

  private static class StoppedListener implements ErlangDebuggerEventListener {
    private final CountDownLatch myStopped;

    StoppedListener(CountDownLatch stopped) {
      myStopped = stopped;
    }

    @Override
    public void debuggerStopped() {
      myStopped.countDown();
    }

    @Override
    public void debuggerStarted() {
    }

    @Override
    public void failedToReceiveMessage(String error) {
    }

    @Override
    public void failedToInterpretModules(String nodeName, List<String> modules) {
    }

    @Override
    public void failedToDebugRemoteNode(String nodeName, String error) {
    }

    @Override
    public void unknownMessage(String messageText) {
    }

    @Override
    public void failedToSetBreakpoint(String module, int line, String errorMessage) {
    }

    @Override
    public void breakpointIsSet(String module, int line) {
    }

    @Override
    public void breakpointReached(OtpErlangPid pid, List<ErlangProcessSnapshot> snapshots) {
    }

    @Override
    public void handleEvaluationResponse(OtpErlangObject response) {
    }

    @Override
    public void evaluationFailed(String errorMessage) {
    }
  }
}
