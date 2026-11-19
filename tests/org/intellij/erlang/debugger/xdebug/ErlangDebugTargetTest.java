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
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ExecutionEnvironmentBuilder;
import com.intellij.xdebugger.XDebugSession;
import com.intellij.execution.process.NopProcessHandler;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.application.ErlangApplicationConfiguration;
import org.intellij.erlang.application.ErlangApplicationRunConfigurationType;
import org.intellij.erlang.application.ErlangApplicationRunningState;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;
import org.intellij.erlang.debugger.node.ErlangProcessSnapshot;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.lang.reflect.Proxy;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

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

    GeneralCommandLine commandLine = new GeneralCommandLine();
    ErlangXDebugProcess.addDebugTargetParameters(commandLine, new ErlangApplicationRunningState(null, getModule(), configuration), 4242);

    assertEquals(List.of("-pa", "/src", "-s", "lager", "start", "-run", "debugnode", "main", "4242", "-noshell",
                         "-s", "init", "stop", "-extra", "foo", "bar"), commandLine.getParametersList().getList());
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
    XDebugSession session = (XDebugSession)Proxy.newProxyInstance(
      getClass().getClassLoader(), new Class<?>[]{XDebugSession.class},
      (proxy, method, args) -> {
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

    String startError = null;
    try {
      new ErlangXDebugProcess(session, environment);
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
  }
}
