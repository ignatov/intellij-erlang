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

package org.intellij.erlang.rebar.runner;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.configuration.EnvironmentVariablesData;
import com.intellij.execution.configurations.ModuleRunProfile;
import com.intellij.execution.configurations.RunConfigurationBase;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.configurations.WrappingRunConfiguration;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ExecutionEnvironmentBuilder;
import com.intellij.execution.testframework.AbstractTestProxy;
import com.intellij.execution.testframework.Filter;
import com.intellij.execution.testframework.TestConsoleProperties;
import com.intellij.execution.testframework.TestFrameworkRunningModel;
import com.intellij.execution.testframework.TestTreeView;
import com.intellij.execution.testframework.sm.runner.SMTestProxy;
import com.intellij.execution.testframework.ui.AbstractTestTreeBuilderBase;
import com.intellij.notification.Notification;
import com.intellij.notification.Notifications;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.ui.ComponentContainer;
import com.intellij.openapi.util.Disposer;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import com.intellij.util.ReflectionUtil;
import com.intellij.util.messages.MessageBus;
import org.intellij.erlang.eunit.ErlangTestConsoleProperties;
import org.intellij.erlang.eunit.ErlangTestLocationProvider;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class RebarEunitRerunFailedTestsActionTest extends BasePlatformTestCase {
  public void testRerunKeepsEnvironment() {
    EnvironmentVariablesData envData = EnvironmentVariablesData.create(Map.of("ERL_LIBS", "/opt/x"), false);
    RebarEunitRunConfiguration original = new RebarEunitRunConfiguration(getProject(), "eunit");
    original.setEnvData(envData);
    original.setModule(getModule());

    RebarEunitRunConfiguration rerun = RebarEunitRerunFailedTestsAction.createRerunConfiguration(original, "eunit --module=sample_tests");

    assertEquals(envData, rerun.getEnvData());
    assertEquals(getModule(), rerun.getConfigurationModule().getModule());
    assertEquals("eunit --module=sample_tests", rerun.getCommand());
    assertTrue(rerun.isSkipDependencies());
  }

  private static final String A = "-module(a).\n-include_lib(\"eunit/include/eunit.hrl\").\na1_test() -> ok.\na2_test() -> ?assert(false).\n";
  private static final String C = """
    -module(c).
    -include_lib("eunit/include/eunit.hrl").
    c4_test_() -> [fun fhelper/0, {with, 1, [fun with1/1]}, ?_assert(false)].
    fhelper() -> error(bad).
    with1(_) -> error(bad).
    """;

  public void testRebar3RerunsModuleOfFailedFunctionThatIsNotATest() {
    useRebar("rebar3");
    myFixture.addFileToProject("a.erl", A);
    myFixture.addFileToProject("c.erl", C);
    List<ErlangFunction> failedGeneratedTests = new ArrayList<>();
    String command = createRerunCommand(failedGeneratedTests, "eunit://c:fhelper:-1", "eunit://c:with1:-1", "eunit://a:a2_test");
    assertEquals("eunit --module=c --test=a:a2_test", command);
    assertEquals(List.of("with1"), List.of(failedGeneratedTests.getFirst().getName()));
  }

  public void testRebar3RerunsGeneratorOfFailedFunOfGenerator() {
    useRebar("rebar3");
    myFixture.addFileToProject("a.erl", A);
    myFixture.addFileToProject("c.erl", C);
    assertEquals("eunit --test=a:a2_test --generator=c:c4_test_",
                 createRerunCommand(new ArrayList<>(), "eunit://c:-c4_test_/0-fun-2-:3", "eunit://a:a2_test"));
    // no generator function in the project: the module named in the hint is rerun
    assertEquals("eunit --module=d --test=a:a2_test",
                 createRerunCommand(new ArrayList<>(), "eunit://d:-d_test_/0-fun-0-:3", "eunit://a:a2_test"));
  }

  public void testRebar3RerunsFailedTestNotItsSuite() {
    useRebar("rebar3");
    myFixture.addFileToProject("a.erl", A);
    myFixture.addFileToProject("b.erl", "-module(b).\nb_test() -> ok.\n");
    SMTestProxy suiteA = createSuite("eunit://a", "eunit://a:a2_test");
    SMTestProxy suiteB = createSuite("eunit://b", "eunit://b:b_test");
    List<SMTestProxy> failed = List.of(suiteA, suiteA.getChildren().getFirst(), suiteB, suiteB.getChildren().getFirst());
    assertEquals("eunit --test=a:a2_test,b:b_test",
                 RebarEunitRerunFailedTestsAction.createRerunCommand(getProject(), failed, new ArrayList<>(), new ArrayList<>()));
  }

  private static SMTestProxy createSuite(String suiteUrl, String failedTestUrl) {
    SMTestProxy suite = new SMTestProxy(suiteUrl, true, suiteUrl);
    suite.setLocator(ErlangTestLocationProvider.INSTANCE);
    SMTestProxy test = new SMTestProxy(failedTestUrl, false, failedTestUrl);
    test.setLocator(ErlangTestLocationProvider.INSTANCE);
    suite.addChild(test);
    suite.setStarted();
    test.setStarted();
    test.setTestFailed("failed", null, false);
    suite.setFinished();
    return suite;
  }

  public void testRebar2RerunCommandUnchanged() {
    useRebar("rebar");
    myFixture.addFileToProject("a.erl", A);
    assertEquals("eunit suites=a tests=a2_test", createRerunCommand(new ArrayList<>(), "eunit://a:a2_test"));
  }

  private String createRerunCommand(List<ErlangFunction> failedGeneratedTests, String... locationUrls) {
    List<SMTestProxy> proxies = new ArrayList<>();
    for (String url : locationUrls) {
      SMTestProxy proxy = new SMTestProxy(url, false, url);
      proxy.setLocator(ErlangTestLocationProvider.INSTANCE);
      proxies.add(proxy);
    }
    return RebarEunitRerunFailedTestsAction.createRerunCommand(getProject(), proxies, failedGeneratedTests, new ArrayList<>());
  }

  private void useRebar(String rebarName) {
    RebarSettings rebarSettings = RebarSettings.getInstance(getProject());
    String oldRebarPath = rebarSettings.getRebarPath();
    rebarSettings.setRebarPath("/opt/rebar/" + rebarName);
    Disposer.register(getTestRootDisposable(), () -> rebarSettings.setRebarPath(oldRebarPath));
  }

  @SuppressWarnings("unchecked")
  public void testFilterAcceptsFailedAndInterruptedTestsOnly() {
    Filter<AbstractTestProxy> filter = createAction().getFilter(getProject(), GlobalSearchScope.allScope(getProject()));
    SMTestProxy notRun = new SMTestProxy("not_run_test", false, null);
    SMTestProxy passed = new SMTestProxy("passed_test", false, null);
    passed.setStarted();
    passed.setFinished();
    SMTestProxy failed = new SMTestProxy("failed_test", false, null);
    failed.setStarted();
    failed.setTestFailed("failed", null, false);
    SMTestProxy ignored = new SMTestProxy("ignored_test", false, null);
    ignored.setStarted();
    ignored.setTestIgnored("ignored", null);
    SMTestProxy terminated = new SMTestProxy("terminated_test", false, null);
    terminated.setStarted();
    terminated.setTerminated();

    assertFalse(filter.shouldAccept(notRun));
    assertFalse(filter.shouldAccept(passed));
    assertTrue(filter.shouldAccept(failed));
    assertFalse(filter.shouldAccept(ignored));
    assertTrue(filter.shouldAccept(terminated));
  }

  public void testNoRunProfileWithoutTestResults() throws ExecutionException {
    RebarEunitRunConfiguration configuration = new RebarEunitRunConfiguration(getProject(), "eunit");
    assertNull(createAction().getRunProfile(createEnvironment(configuration)));
  }

  public void testRebar3RunProfileRerunsFailedTestsAndReportsGeneratedOnes() throws ExecutionException {
    useRebar("rebar3");
    myFixture.addFileToProject("a.erl", A);
    myFixture.addFileToProject("c.erl", C);
    RebarEunitRunConfiguration configuration = new RebarEunitRunConfiguration(getProject(), "eunit");
    configuration.setCommand("eunit");
    configuration.setModule(getModule());
    SMTestProxy.SMRootTestProxy root = new SMTestProxy.SMRootTestProxy();
    root.addChild(finishedTest("eunit://a:a1_test", false));
    root.addChild(finishedTest("eunit://a:a2_test", true));
    root.addChild(finishedTest("eunit://c:with1:-1", true));
    SMTestProxy ignored = new SMTestProxy("eunit://a:a1_test", false, "eunit://a:a1_test");
    ignored.setLocator(ErlangTestLocationProvider.INSTANCE);
    ignored.setStarted();
    ignored.setTestIgnored("ignored", null);
    root.addChild(ignored);
    List<Notification> notifications = collectNotifications(ApplicationManager.getApplication().getMessageBus());

    RebarEunitRunConfiguration rerun = getRerunConfiguration(configuration, root);

    assertEquals("eunit --module=c --test=a:a2_test", rerun.getCommand());
    assertTrue(rerun.isSkipDependencies());
    assertEquals(getModule(), rerun.getConfigurationModule().getModule());
    PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
    assertSize(1, notifications);
    assertEquals("Some tests cannot be rerun directly", notifications.get(0).getTitle());
    assertTrue(notifications.get(0).getContent(), notifications.get(0).getContent().endsWith("Tests failed: c:with1"));
  }

  public void testRebar2RunProfileReportsTestsThatCannotBeRerun() throws ExecutionException {
    useRebar("rebar");
    myFixture.addFileToProject("a.erl", A);
    RebarEunitRunConfiguration configuration = new RebarEunitRunConfiguration(getProject(), "eunit");
    SMTestProxy.SMRootTestProxy root = new SMTestProxy.SMRootTestProxy();
    root.addChild(finishedTest("eunit://a:a2_test", true));
    root.addChild(finishedTest("eunit://zz:zz_test", true));
    List<Notification> notifications = collectNotifications(getProject().getMessageBus());

    RebarEunitRunConfiguration rerun = getRerunConfiguration(configuration, root);

    assertEquals("eunit suites=a tests=a2_test", rerun.getCommand());
    PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
    assertSize(1, notifications);
    assertEquals("Some failed tests cannot be rerun", notifications.get(0).getTitle());
    assertTrue(notifications.get(0).getContent(), notifications.get(0).getContent().contains("eunit://zz:zz_test"));
  }

  public void testRunProfileRunsInModulesOfConfiguration() throws ExecutionException {
    RebarEunitRunConfiguration configuration = new RebarEunitRunConfiguration(getProject(), "eunit");
    configuration.setModule(getModule());
    RebarEunitRerunFailedTestsAction action = createAction();
    action.setModel(createModel(configuration, new SMTestProxy.SMRootTestProxy()));

    RunConfigurationBase<?> profile = action.getRunProfile(createEnvironment(configuration));

    assertNotNull(profile);
    assertSame(configuration, ((WrappingRunConfiguration<?>) profile).getPeer());
    assertOrderedEquals(((ModuleRunProfile) profile).getModules(), getModule());
  }

  public void testRebar3RerunsModuleOfFailedSuiteWithoutFailedTests() {
    useRebar("rebar3");
    myFixture.addFileToProject("a.erl", A);
    myFixture.addFileToProject("b.erl", "-module(b).\nb_test() -> ok.\n");
    SMTestProxy suiteB = new SMTestProxy("b", true, "eunit://b");
    suiteB.setLocator(ErlangTestLocationProvider.INSTANCE);
    suiteB.addChild(finishedTest("eunit://b:b_test", false));
    List<String> testsNotRerun = new ArrayList<>();
    assertEquals("eunit --module=b --test=a:a2_test", RebarEunitRerunFailedTestsAction.createRerunCommand(
      getProject(), List.of(suiteB, finishedTest("eunit://a:a2_test", true)), new ArrayList<>(), testsNotRerun));
    assertEmpty(testsNotRerun);
  }

  public void testRerunsTestReportedTwiceOnce() {
    useRebar("rebar3");
    myFixture.addFileToProject("a.erl", A);
    assertEquals("eunit --test=a:a2_test", createRerunCommand(new ArrayList<>(), "eunit://a:a2_test", "eunit://a:a2_test"));
  }

  public void testGeneratedTestReportedTwiceIsListedOnce() {
    useRebar("rebar3");
    myFixture.addFileToProject("c.erl", C);
    List<ErlangFunction> failedGeneratedTests = new ArrayList<>();
    assertEquals("eunit --module=c", createRerunCommand(failedGeneratedTests, "eunit://c:with1:-1", "eunit://c:with1:-1"));
    assertSize(1, failedGeneratedTests);
  }

  public void testRebar3ReportsFailedTestsWithoutModule() {
    useRebar("rebar3");
    myFixture.addFileToProject("a.erl", A);
    SMTestProxy noLocation = new SMTestProxy("no_location", false, null);
    noLocation.setLocator(ErlangTestLocationProvider.INSTANCE);
    SMTestProxy otherProtocol = new SMTestProxy("other_protocol", false, "file://a.erl");
    otherProtocol.setLocator(ErlangTestLocationProvider.INSTANCE);
    SMTestProxy noModule = new SMTestProxy("no_module", false, "eunit://:x");
    noModule.setLocator(ErlangTestLocationProvider.INSTANCE);
    List<String> testsNotRerun = new ArrayList<>();
    assertEquals("eunit --test=a:a2_test", RebarEunitRerunFailedTestsAction.createRerunCommand(
      getProject(), List.of(noLocation, otherProtocol, noModule, finishedTest("eunit://a:a2_test", true)),
      new ArrayList<>(), testsNotRerun));
    assertEquals(List.of("no_location", "other_protocol", "no_module"), testsNotRerun);
  }

  private RebarEunitRunConfiguration getRerunConfiguration(RebarEunitRunConfiguration configuration,
                                                           SMTestProxy.SMRootTestProxy root) throws ExecutionException {
    RebarEunitRerunFailedTestsAction action = createAction();
    action.setModel(createModel(configuration, root));
    ExecutionEnvironment environment = createEnvironment(configuration);
    RunConfigurationBase<?> profile = action.getRunProfile(environment);
    assertNotNull(profile);
    RunProfileState state = profile.getState(environment.getExecutor(), environment);
    assertInstanceOf(state, RebarEunitRunningState.class);
    return ReflectionUtil.getField(RebarEunitRunningState.class, state, RebarEunitRunConfiguration.class, "myConfiguration");
  }

  private static SMTestProxy finishedTest(String url, boolean failed) {
    SMTestProxy test = new SMTestProxy(url, false, url);
    test.setLocator(ErlangTestLocationProvider.INSTANCE);
    test.setStarted();
    if (failed) {
      test.setTestFailed("failed", null, false);
    }
    else {
      test.setFinished();
    }
    return test;
  }

  private List<Notification> collectNotifications(MessageBus messageBus) {
    List<Notification> notifications = new ArrayList<>();
    messageBus.connect(getTestRootDisposable()).subscribe(Notifications.TOPIC, new Notifications() {
      @Override
      public void notify(@NotNull Notification notification) {
        notifications.add(notification);
      }
    });
    return notifications;
  }

  private ExecutionEnvironment createEnvironment(RebarEunitRunConfiguration configuration) throws ExecutionException {
    return ExecutionEnvironmentBuilder.create(getProject(), DefaultRunExecutor.getRunExecutorInstance(), configuration)
      .runner(new RebarRunner())
      .build();
  }

  private RebarEunitRerunFailedTestsAction createAction() {
    JPanel component = new JPanel();
    ComponentContainer container = new ComponentContainer() {
      @Override
      public @NotNull JComponent getComponent() {
        return component;
      }

      @Override
      public JComponent getPreferredFocusableComponent() {
        return component;
      }

      @Override
      public void dispose() {
      }
    };
    Disposer.register(getTestRootDisposable(), container);
    return new RebarEunitRerunFailedTestsAction(container);
  }

  private TestFrameworkRunningModel createModel(RebarEunitRunConfiguration configuration, AbstractTestProxy root) {
    Executor executor = DefaultRunExecutor.getRunExecutorInstance();
    TestConsoleProperties properties = new ErlangTestConsoleProperties(configuration, executor);
    Disposer.register(getTestRootDisposable(), properties);
    return new TestFrameworkRunningModel() {
      @Override
      public TestConsoleProperties getProperties() {
        return properties;
      }

      @Override
      public void setFilter(@NotNull Filter<?> filter) {
      }

      @Override
      public boolean isRunning() {
        return false;
      }

      @Override
      public TestTreeView getTreeView() {
        return null;
      }

      @Override
      public AbstractTestTreeBuilderBase<?> getTreeBuilder() {
        return null;
      }

      @Override
      public boolean hasTestSuites() {
        return false;
      }

      @Override
      public AbstractTestProxy getRoot() {
        return root;
      }

      @Override
      public void selectAndNotify(AbstractTestProxy testProxy) {
      }

      @Override
      public void dispose() {
      }
    };
  }
}
