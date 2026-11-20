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

package org.intellij.erlang.eunit;

import com.intellij.execution.configuration.EnvironmentVariablesData;
import com.intellij.execution.testframework.sm.runner.SMTestProxy;
import com.intellij.notification.Notification;
import com.intellij.notification.Notifications;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class ErlangUnitRerunFailedTestsActionTest extends BasePlatformTestCase {
  public void testRerunKeepsEnvironment() {
    EnvironmentVariablesData envData = EnvironmentVariablesData.create(Map.of("ERL_LIBS", "/opt/x"), false);
    ErlangUnitRunConfiguration original =
      new ErlangUnitRunConfiguration(getProject(), "eunit", ErlangUnitRunConfigurationType.getInstance());
    original.setEnvData(envData);

    ErlangUnitRunConfiguration rerun =
      ErlangUnitRerunFailedTestsAction.createRerunConfiguration(original, Set.of("sample_tests:failing_test"));

    assertEquals(envData, rerun.getEnvData());
    assertEquals(envData, new ErlangUnitRunningState(null, getModule(), rerun).getEnvData());
    assertEquals(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION, rerun.getConfigData().getKind());
    assertEquals(Set.of("sample_tests:failing_test"), rerun.getConfigData().getFunctionNames());
  }

  public void testRerunGeneratorTests() {
    myFixture.configureByText("g_tests.erl", """
      -module(g_tests).
      -include_lib("eunit/include/eunit.hrl").
      plain_test() -> ok.
      gen_test_() -> [?_assert(true),
        ?_assertEqual(1, 2)].
      fixture_test_() -> {setup, fun() -> ok end,
        fun(_) -> [?_assertEqual(1, helper())] end}.
      helper() -> [?_assert(false)].
      """);

    SMTestProxy suite = new SMTestProxy("g_tests", true, ErlangUnitRunConfigurationType.PROTOCOL + "://g_tests");
    suite.setLocator(ErlangTestLocationProvider.INSTANCE);
    suite.addChild(failedTest("-gen_test_/0-fun-1-", "g_tests:-gen_test_/0-fun-1-:5"));
    List<SMTestProxy> failedTests = List.of(
      failedTest("-gen_test_/0-fun-1-", "g_tests:-gen_test_/0-fun-1-:5"),
      failedTest("plain_test", "g_tests:plain_test:-1"),
      failedTest("-fixture_test_/0-fun-1-", "g_tests:-fixture_test_/0-fun-1-:7"),
      failedTest("-helper/0-fun-0-", "g_tests:-helper/0-fun-0-:8"),
      suite);

    List<Notification> notifications = collectNotifications();
    assertEquals(List.of("g_tests:gen_test_", "g_tests:plain_test", "g_tests:fixture_test_"),
                 List.copyOf(ErlangUnitRerunFailedTestsAction.collectTestsToRerun(getProject(), failedTests)));
    assertNotification(notifications, "Some failed tests cannot be rerun", "-helper/0-fun-0-");
  }

  // a test made by a fixture instantiator of arity 1 has no zero-arity test function to rerun
  public void testRerunReportsTestsThatCannotBeRerun() {
    configureFixtureModule();

    List<Notification> notifications = collectNotifications();
    Set<String> tests = ErlangUnitRerunFailedTestsAction.collectTestsToRerun(getProject(), List.of(
      failedTest("plain_test", "q_tests:plain_test:-1"),
      failedTest("-inst_a/1-fun-1-", "q_tests:-inst_a/1-fun-1-:6")));

    assertEquals(Set.of("q_tests:plain_test"), tests);
    assertNotification(notifications, "Some failed tests cannot be rerun", "-inst_a/1-fun-1-");
  }

  public void testRerunWithNothingToRerunNotifiesAndRunsNothing() {
    configureFixtureModule();

    List<Notification> notifications = collectNotifications();
    Set<String> tests = ErlangUnitRerunFailedTestsAction.collectTestsToRerun(getProject(), List.of(
      failedTest("-inst_a/1-fun-1-", "q_tests:-inst_a/1-fun-1-:6")));

    assertNull(tests);
    assertNotification(notifications, "Failed tests cannot be rerun", "-inst_a/1-fun-1-");
  }

  public void testRerunQuotesFunctionNames() throws Exception {
    configureFixtureModule();

    Set<String> tests = ErlangUnitRerunFailedTestsAction.collectTestsToRerun(getProject(), List.of(
      failedTest("-quo-ted_test_/0-fun-1-", "q_tests:-quo-ted_test_/0-fun-1-:7"),
      failedTest("plain_test", "q_tests:plain_test:-1")));
    assertNotNull(tests);
    ErlangUnitRunConfiguration original =
      new ErlangUnitRunConfiguration(getProject(), "eunit", ErlangUnitRunConfigurationType.getInstance());
    ErlangUnitRunConfiguration rerun = ErlangUnitRerunFailedTestsAction.createRerunConfiguration(original, tests);

    assertEquals("[{\"module 'q_tests'\", [{generator, fun q_tests:'quo-ted_test_'/0}, fun q_tests:plain_test/0]}]",
                 new ErlangUnitRunningState(null, getModule(), rerun).getEntryPoint().getArgsList().get(0));
  }

  private void configureFixtureModule() {
    myFixture.configureByText("q_tests.erl", """
      -module(q_tests).
      -include_lib("eunit/include/eunit.hrl").
      plain_test() -> 1 = 2.
      fe_test_() -> {foreach, fun() -> ok end, fun(_) -> ok end, [fun inst_a/1]}.
      inst_a(_) ->
        ?_assertEqual(1, 2).
      'quo-ted_test_'() -> [?_assert(false)].
      """);
  }

  private List<Notification> collectNotifications() {
    List<Notification> notifications = new ArrayList<>();
    getProject().getMessageBus().connect(getTestRootDisposable()).subscribe(Notifications.TOPIC, new Notifications() {
      @Override
      public void notify(@NotNull Notification notification) {
        notifications.add(notification);
      }
    });
    return notifications;
  }

  private static void assertNotification(List<Notification> notifications, String title, String testName) {
    PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
    assertEquals(1, notifications.size());
    assertEquals(title, notifications.get(0).getTitle());
    assertTrue(notifications.get(0).getContent(), notifications.get(0).getContent().contains(testName));
  }

  private static SMTestProxy failedTest(String name, String location) {
    SMTestProxy proxy = new SMTestProxy(name, false, ErlangUnitRunConfigurationType.PROTOCOL + "://" + location);
    proxy.setLocator(ErlangTestLocationProvider.INSTANCE);
    return proxy;
  }
}
