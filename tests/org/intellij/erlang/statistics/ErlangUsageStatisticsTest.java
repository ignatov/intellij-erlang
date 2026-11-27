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

package org.intellij.erlang.statistics;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.codeInsight.lookup.Lookup;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.codeInsight.lookup.LookupEvent;
import com.intellij.codeInsight.lookup.LookupListener;
import com.intellij.codeInsight.template.Template;
import com.intellij.codeInsight.template.TemplateManager;
import com.intellij.codeInsight.template.impl.TemplateManagerImpl;
import com.intellij.codeInsight.template.impl.TemplateState;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.RunProfile;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.executors.DefaultDebugExecutor;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.process.NopProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ExecutionEnvironmentBuilder;
import com.intellij.openapi.fileTypes.PlainTextFileType;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowId;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.task.ProjectTaskManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkModificator;
import com.intellij.openapi.projectRoots.impl.ProjectJdkImpl;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.testFramework.LightVirtualFile;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.util.TimeoutUtil;
import com.intellij.util.concurrency.AppExecutorUtil;
import com.intellij.util.ui.UIUtil;
import com.intellij.util.xmlb.XmlSerializer;
import kotlin.coroutines.Continuation;
import kotlin.coroutines.CoroutineContext;
import kotlin.coroutines.EmptyCoroutineContext;
import org.intellij.erlang.application.ErlangApplicationConfiguration;
import org.intellij.erlang.application.ErlangApplicationRunConfigurationType;
import org.intellij.erlang.console.ErlangConsoleRunConfiguration;
import org.intellij.erlang.debugger.remote.ErlangRemoteDebugRunConfiguration;
import org.intellij.erlang.dialyzer.ErlangDialyzerExternalAnnotator;
import org.intellij.erlang.eunit.ErlangUnitRunConfiguration;
import org.intellij.erlang.eunit.ErlangUnitRunConfigurationType;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.rebar.runner.RebarEunitRunConfigurationFactory;
import org.intellij.erlang.rebar.runner.RebarRunConfiguration;
import org.intellij.erlang.runconfig.ErlangRunner;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.plaf.ColorUIResource;
import java.awt.*;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.io.File;
import java.lang.reflect.Proxy;
import java.time.LocalDate;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Delayed;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

public class ErlangUsageStatisticsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String MODULE = """
    -module(hostile_module).
    hostile_fun_one() -> ok.
    hostile_fun_two() -> ok.
    main() -> hostile_fun_<caret>
    """;
  private final ErlangUsageCollectorTest.FakeSender mySender = new ErlangUsageCollectorTest.FakeSender();

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    ErlangUsageStatistics.getInstance().resetForTests();
    collector().setSender(mySender);
    ErlangUsageStatistics.setAllowedForTests(true);
    ErlangUsageStatistics.setErlangProjectForTests(getProject(), true);
    // the input listener runs and the user has been quiet for a minute
    ErlangUsageIdle.setInstalledForTests(true, System.currentTimeMillis() - 60_000);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ErlangUsageStatistics.getInstance().resetForTests();
      collector().setSender(new ErlangAmplitudeSender());
      ErlangUsageStatistics.setAllowedForTests(null);
      ErlangUsageStatistics.setErlangProjectForTests(getProject(), null);
      ErlangUsageIdle.setInstalledForTests(false, 0);
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  public void testNothingIsAllowedInTestsByDefault() {
    ErlangUsageStatistics.setAllowedForTests(null);
    assertFalse(ErlangUsageStatistics.isAllowed());
    collector().setEnabled(true);
    ErlangUsageCounters.count(getProject(), ErlangUsageKey.RENAME);
    assertFalse(ErlangUsageCounters.isCounting());
    assertTrue(collector().getState().days.isEmpty());
  }

  // the startup activity of a test project runs beside setUp and can come after the test allowed the statistics; then
  // it started the timer for the rest of the tests, and its check of the project, which has no Erlang files, took away
  // what setErlangProjectForTests set
  public void testStartupActivityDoesNothingInTests() {
    new ErlangUsageListeners.Startup().execute(getProject(), new Continuation<>() {
      @Override
      public @NotNull CoroutineContext getContext() {
        return EmptyCoroutineContext.INSTANCE;
      }

      @Override
      public void resumeWith(@NotNull Object result) {
      }
    });
    assertFalse(ErlangUsageStatistics.getInstance().isTimerStartedForTests());
  }

  public void testEnvironmentSwitches() {
    assertTrue(ErlangUsageStatistics.isAllowedEnvironment("0.11.1304", false, false, null, null));
    assertTrue(ErlangUsageStatistics.isAllowedEnvironment("0.11.1304", false, false, "true", "0"));
    assertFalse("SNAPSHOT", ErlangUsageStatistics.isAllowedEnvironment("0.11.SNAPSHOT", false, false, null, null));
    assertFalse("no version", ErlangUsageStatistics.isAllowedEnvironment(null, false, false, null, null));
    assertFalse("unit tests", ErlangUsageStatistics.isAllowedEnvironment("0.11.1304", true, false, null, null));
    assertFalse("headless", ErlangUsageStatistics.isAllowedEnvironment("0.11.1304", false, true, null, null));
    assertFalse("property", ErlangUsageStatistics.isAllowedEnvironment("0.11.1304", false, false, "false", null));
    assertFalse("property", ErlangUsageStatistics.isAllowedEnvironment("0.11.1304", false, false, "FALSE", null));
    assertFalse("DO_NOT_TRACK", ErlangUsageStatistics.isAllowedEnvironment("0.11.1304", false, false, null, "1"));
    assertFalse("DO_NOT_TRACK", ErlangUsageStatistics.isAllowedEnvironment("0.11.1304", false, false, null, "true"));
  }

  public void testProjectWithoutErlangFilesDoesNotCount() {
    ErlangUsageStatistics.setErlangProjectForTests(getProject(), false);
    collector().setEnabled(true);
    myFixture.configureByText("web.config", "<configuration><appSettings/></configuration>");
    myFixture.performEditorAction("ReformatCode");
    completeFunction();
    ErlangUsageCounters.countQuickFix(getProject(), ErlangUsageCollectorTest.FakeSender.class);
    assertTrue(collector().getState().days.isEmpty());
    JsonObject properties = previewProperties();
    assertFalse(properties.has("opened_config"));
    assertFalse(properties.has("reformat"));
    assertFalse(properties.has("completion"));
  }

  // the preview says that today is sent only when sendPending would send it
  public void testPreviewSaysWhetherTodayIsSent() {
    assertEquals(ErlangUsageStatisticsPanel.PREVIEW_SENT, ErlangUsageStatisticsPanel.previewNote(collector()));
    collector().setEnabled(true);
    collector().setProperty(ErlangUsageProperty.OTP, "27");
    ErlangUsageCounters.count(getProject(), ErlangUsageKey.OPENED_CONFIG);
    assertEquals(ErlangUsageStatisticsPanel.PREVIEW_NOT_SENT, ErlangUsageStatisticsPanel.previewNote(collector()));
    ErlangUsageCounters.count(getProject(), ErlangUsageKey.RENAME);
    assertEquals(ErlangUsageStatisticsPanel.PREVIEW_SENT, ErlangUsageStatisticsPanel.previewNote(collector()));
  }

  public void testPreviewStoresNothing() {
    collector().previewToday();
    assertTrue(collector().getState().days.isEmpty());
    assertNull(collector().getInstallationId());
  }

  public void testAskDialogButtonsAndChoices() {
    ErlangUsageAskDialog first = new ErlangUsageAskDialog(getProject(), false);
    ErlangUsageAskDialog reminder = new ErlangUsageAskDialog(getProject(), true);
    try {
      assertEquals(ErlangUsageAskDialog.TITLE, first.getTitle());
      assertFalse(first.isExampleShownForTests());
      assertEquals(List.of(ErlangUsageAskDialog.YES, ErlangUsageAskDialog.NO, ErlangUsageAskDialog.LATER), first.getButtonTextsForTests());
      assertEquals(ErlangUsageAskUi.Choice.LATER, ErlangUsageAskDialog.choiceOf(DialogWrapper.CANCEL_EXIT_CODE));
      // Esc and the close button of the window cancel the dialog
      first.doCancelAction();
      assertEquals(ErlangUsageAskUi.Choice.LATER, ErlangUsageAskDialog.choiceOf(first.getExitCode()));
      reminder.clickNoForTests();
      assertFalse("No during the guard time", reminder.isDisposed());
      reminder.endGuardForTests();
      reminder.clickNoForTests();
      assertEquals(ErlangUsageAskUi.Choice.NO, ErlangUsageAskDialog.choiceOf(reminder.getExitCode()));
    }
    finally {
      Disposer.dispose(first.getDisposable());
      Disposer.dispose(reminder.getDisposable());
    }
  }

  public void testYesIsTheDefaultAfterAShortGuard() {
    ErlangUsageAskDialog dialog = new ErlangUsageAskDialog(getProject(), false);
    try {
      Action yes = dialog.getActionsForTests().get(0);
      Action no = dialog.getActionsForTests().get(1);
      assertEquals(ErlangUsageAskDialog.YES, yes.getValue(Action.NAME));
      assertNotNull(yes.getValue(DialogWrapper.DEFAULT_ACTION));
      assertNotNull(yes.getValue(DialogWrapper.FOCUSED_ACTION));
      // Yes has its own exit code; the hidden OK action means later
      assertEquals(ErlangUsageAskUi.Choice.LATER, ErlangUsageAskDialog.choiceOf(DialogWrapper.OK_EXIT_CODE));

      // the buttons are disabled when the dialog opens, until the guard timer, which starts when the window is shown, ends
      assertTrue(dialog.isGuardedForTests());
      assertFalse(yes.isEnabled());
      assertFalse(no.isEnabled());
      assertFalse(dialog.isGuardTimerRunningForTests());
      dialog.startGuardForTests();
      assertTrue(dialog.isGuardTimerRunningForTests());
      dialog.pressYesForTests();
      assertFalse(dialog.isDisposed());

      // a press during the guard with a release after it does not click
      JButton yesButton = dialog.getButtonForTests(yes);
      if (yesButton != null) {
        assertFalse(yesButton.isEnabled());
        yesButton.getModel().setArmed(true);
        yesButton.getModel().setPressed(true);
      }
      dialog.endGuardForTests();
      assertTrue(yes.isEnabled());
      assertTrue(no.isEnabled());
      if (yesButton != null) {
        assertTrue(yesButton.isEnabled());
        yesButton.getModel().setPressed(false);
        assertFalse(dialog.isDisposed());
      }
      assertSame(yesButton, dialog.getPreferredFocusedComponent());

      dialog.pressYesForTests();
      assertEquals(ErlangUsageAskUi.Choice.YES, ErlangUsageAskDialog.choiceOf(dialog.getExitCode()));
    }
    finally {
      if (!dialog.isDisposed()) Disposer.dispose(dialog.getDisposable());
    }
  }

  // the focus goes to Yes at the end of the guard only if the user did not move it to another control
  public void testGuardEndKeepsTheFocusThatTheUserMoved() {
    ErlangUsageAskDialog moved = new ErlangUsageAskDialog(getProject(), false);
    ErlangUsageAskDialog untouched = new ErlangUsageAskDialog(getProject(), false);
    ErlangUsageAskDialog outside = new ErlangUsageAskDialog(getProject(), false);
    try {
      assertFalse("the user opened the example during the guard", moved.endGuardForTests(moved.getToggleForTests()));
      assertTrue(untouched.endGuardForTests(untouched.getContentForTests()));
      assertTrue("not a control of the content", outside.endGuardForTests(new JButton()));
    }
    finally {
      Disposer.dispose(moved.getDisposable());
      Disposer.dispose(untouched.getDisposable());
      Disposer.dispose(outside.getDisposable());
    }
  }

  public void testButtonTextsAndOrder() {
    ErlangUsageAskDialog dialog = new ErlangUsageAskDialog(getProject(), false);
    try {
      for (String text : dialog.getButtonTextsForTests()) {
        // DialogWrapper binds the bare Y and N keys to buttons named exactly "Yes" and "No"
        assertFalse(text, text.equals("Yes") || text.equals("No") || text.contains("&") || text.contains("_"));
      }
      assertEquals(List.of(ErlangUsageAskDialog.LATER, ErlangUsageAskDialog.NO, ErlangUsageAskDialog.YES), dialog.getMacOrderForTests());
    }
    finally {
      Disposer.dispose(dialog.getDisposable());
    }
  }

  public void testDialogShowsTheExample() {
    ErlangUsageAskDialog dialog = new ErlangUsageAskDialog(getProject(), false);
    try {
      String text = dialog.getExampleTextForTests();
      assertEquals(ErlangUsageAskDialog.exampleText(), text);
      assertTrue(text, text.startsWith(ErlangUsageStatisticsPanel.prettyPrint(ErlangUsagePayload.exampleEventProperties())));
      // the counters from the first visible line on
      assertTrue(text, text.startsWith("{\n  \"completion\": "));
      assertTrue(text, text.endsWith(ErlangUsageAskDialog.SENT_TOGETHER));
    }
    finally {
      Disposer.dispose(dialog.getDisposable());
    }
  }

  public void testNoInputListenerMeansBusy() {
    FakeAskUi ui = new FakeAskUi();
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    statistics.setAskUiForTests(ui);
    ErlangUsageIdle.setInstalledForTests(false, 0);
    statistics.ask(getProject());
    assertEquals(List.of(), ui.asked);
    assertTrue(ErlangUsageIdle.isInstalled());
    ErlangUsageIdle.setLastInputForTests(System.currentTimeMillis() - 6_000);
    statistics.ask(getProject());
    assertEquals(List.of(false), ui.asked);
    assertFalse("the listener stops after the ask", ErlangUsageIdle.isInstalled());
  }

  // a drag is input, a pointer that only moves is not
  public void testMouseDragIsInput() {
    assertTrue("drags do not reach the listener", (ErlangUsageIdle.MASK & AWTEvent.MOUSE_MOTION_EVENT_MASK) != 0);
    JPanel source = new JPanel();
    long quiet = System.currentTimeMillis() - 60_000;
    ErlangUsageIdle.setLastInputForTests(quiet);
    ErlangUsageIdle.dispatchForTests(new MouseEvent(source, MouseEvent.MOUSE_MOVED, System.currentTimeMillis(), 0, 1, 1, 0, false));
    assertTrue("a move is not input", ErlangUsageIdle.idleMillis() >= 60_000);
    ErlangUsageIdle.dispatchForTests(new MouseEvent(source, MouseEvent.MOUSE_DRAGGED, System.currentTimeMillis(),
                                                    InputEvent.BUTTON1_DOWN_MASK, 1, 1, 0, false));
    assertTrue("a drag is input", ErlangUsageIdle.idleMillis() < 10_000);
  }

  public void testAskLaterStartsTheInputListener() {
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    ErlangUsageIdle.setInstalledForTests(false, 0);
    ErlangUsageStatistics.setErlangProjectForTests(getProject(), false);
    statistics.askLaterForTests(getProject());
    assertFalse("not for a project without Erlang files", ErlangUsageIdle.isInstalled());
    ErlangUsageStatistics.setErlangProjectForTests(getProject(), true);
    statistics.askLaterForTests(getProject());
    assertTrue(ErlangUsageIdle.isInstalled());
    assertEquals(1, statistics.getScheduledForTests());
    statistics.setEnabled(false);
    assertFalse("an answer stops the listener", ErlangUsageIdle.isInstalled());
  }

  public void testAskIsNotCountedWhenTheDialogCannotBeCreated() {
    FakeAskUi ui = new FakeAskUi();
    ui.fail = true;
    ErlangUsageStatistics.getInstance().setAskUiForTests(ui);
    try {
      ErlangUsageStatistics.getInstance().ask(getProject());
      fail("the fake dialog throws");
    }
    catch (IllegalStateException expected) {
    }
    assertEquals(0, collector().getState().askCount);
  }

  public void testNoAskAfterThePluginIsUnloaded() {
    FakeAskUi ui = new FakeAskUi();
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    statistics.setAskUiForTests(ui);
    statistics.setDisposedForTests(true);
    try {
      statistics.ask(getProject());
      assertEquals(List.of(), ui.asked);
    }
    finally {
      statistics.setDisposedForTests(false);
    }
  }

  public void testBusyChecksAreLimited() {
    FakeAskUi ui = new FakeAskUi();
    ui.canAsk = false;
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    statistics.setAskUiForTests(ui);
    for (int i = 0; i < ErlangUsageStatistics.MAX_RETRIES + 10; i++) {
      statistics.ask(getProject());
    }
    assertEquals(ErlangUsageStatistics.MAX_RETRIES, statistics.getScheduledForTests());
    assertFalse(ErlangUsageIdle.isInstalled());
    assertEquals(List.of(), ui.asked);
    assertEquals(0, collector().getState().askCount);
  }

  // review point 1: after the busy checks of one chain ran out, the next chain of the timer has its own checks
  public void testTimerAsksAgainAfterTheBusyChecksRanOut() {
    FakeAskUi ui = new FakeAskUi();
    ui.canAsk = false;
    FakeScheduler scheduler = new FakeScheduler();
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    statistics.setAskUiForTests(ui);
    statistics.setSchedulerForTests(scheduler);
    ErlangUsageIdle.setInstalledForTests(false, 0);
    for (int chain = 0; chain < 2; chain++) {
      // the timer starts a chain, and the user stays busy during all of its checks
      statistics.askLaterForTests(getProject());
      assertEquals("checks of chain " + chain, ErlangUsageStatistics.MAX_RETRIES + 1, scheduler.runAll());
      assertFalse(ErlangUsageIdle.isInstalled());
    }
    ui.canAsk = true;
    statistics.askLaterForTests(getProject());
    scheduler.runAll();
    assertEquals(List.of(false), ui.asked);
  }

  // the listener starts with the chain, so its first check waits until the user was quiet for long enough
  public void testFirstCheckOfAChainComesAfterTheIdleTime() {
    FakeAskUi ui = new FakeAskUi();
    FakeScheduler scheduler = new FakeScheduler();
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    statistics.setAskUiForTests(ui);
    statistics.setSchedulerForTests(scheduler);
    ErlangUsageIdle.setInstalledForTests(false, 0);
    statistics.askLaterForTests(getProject());
    assertEquals("checks until the question", 1, scheduler.runAll());
    assertEquals(List.of(false), ui.asked);
  }

  public void testOneChainAtATime() {
    FakeAskUi ui = new FakeAskUi();
    ui.canAsk = false;
    FakeScheduler scheduler = new FakeScheduler();
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    statistics.setAskUiForTests(ui);
    statistics.setSchedulerForTests(scheduler);
    // the project check and the timer both want to ask
    statistics.askLaterForTests(getProject());
    statistics.askLaterForTests(getProject());
    assertEquals(1, statistics.getScheduledForTests());
    assertEquals(ErlangUsageStatistics.MAX_RETRIES + 1, scheduler.runAll());
  }

  // with two Erlang projects open, the chain asks in the one whose window the user works in
  public void testChainAsksInAnotherProjectThatCanBeAsked() {
    FakeAskUi ui = new FakeAskUi();
    ui.cannotAskIn = getProject();
    FakeScheduler scheduler = new FakeScheduler();
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    statistics.setAskUiForTests(ui);
    statistics.setSchedulerForTests(scheduler);
    Project other = fakeProject(new boolean[]{false});
    ErlangUsageStatistics.setErlangProjectForTests(other, true);
    statistics.setOpenProjectsForTests(() -> new Project[]{getProject(), other});
    statistics.askLaterForTests(getProject());
    scheduler.runAll();
    assertEquals(List.of(other), ui.askedIn);
  }

  public void testChainEndsWhenItsProjectCloses() {
    FakeScheduler scheduler = new FakeScheduler();
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    statistics.setAskUiForTests(new FakeAskUi());
    statistics.setSchedulerForTests(scheduler);
    ErlangUsageIdle.setInstalledForTests(false, 0);
    boolean[] disposed = {false};
    Project closing = fakeProject(disposed);
    ErlangUsageStatistics.setErlangProjectForTests(closing, true);
    // the only open project
    statistics.setOpenProjectsForTests(() -> new Project[]{closing});
    statistics.askLaterForTests(closing);
    assertTrue(ErlangUsageIdle.isInstalled());
    disposed[0] = true;
    scheduler.runAll();
    assertFalse("no question is due, so the listener stops", ErlangUsageIdle.isInstalled());
    // another project can start a chain
    statistics.askLaterForTests(getProject());
    assertEquals(2, statistics.getScheduledForTests());
  }

  // review point 5: a check with no delay can run before the scheduler returns its future
  public void testCheckThatRunsBeforeTheSchedulerReturns() {
    FakeAskUi ui = new FakeAskUi();
    FakeScheduler scheduler = new FakeScheduler();
    scheduler.immediately = true;
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    statistics.setAskUiForTests(ui);
    statistics.setSchedulerForTests(scheduler);
    statistics.askLaterForTests(getProject());
    PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
    assertEquals(List.of(false), ui.asked);
  }

  // review point 7: a project without Erlang files at the start is checked again by the timer while a question is due
  public void testProjectThatGetsItsFirstErlangFileIsAsked() {
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    statistics.setSchedulerForTests(new FakeScheduler());
    ErlangUsageStatistics.setErlangProjectForTests(getProject(), false);
    myFixture.addFileToProject("first.erl", "-module(first).\n");
    statistics.tickForTests();
    PlatformTestUtil.waitWithEventsDispatching("the timer did not start a question", () -> statistics.getScheduledForTests() == 1, 10);
    assertTrue(ErlangUsageStatistics.isErlangProject(getProject()));
  }

  // review point 8: an Error, not only an Exception, would end the timer for the rest of the session
  public void testTimerSurvivesAnError() {
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    collector().setEnabled(true);
    collector().setSender(json -> {
      throw new AssertionError("a broken sender");
    });
    ErlangUsageStatisticsState state = collector().getState();
    ErlangUsageStatisticsState.Day day = new ErlangUsageStatisticsState.Day();
    day.date = LocalDate.now().minusDays(1).toString();
    day.counters.put(ErlangUsageKey.RENAME.name(), 1L);
    state.days.add(day);
    collector().loadState(state);
    statistics.tickForTests();
    // the tick also checks the project, which has no Erlang files
    PlatformTestUtil.waitWithEventsDispatching("the project check did not end", () -> !ErlangUsageStatistics.isErlangProject(getProject()), 10);
  }

  public void testEscIsLaterAlsoDuringTheGuard() {
    ErlangUsageAskDialog dialog = new ErlangUsageAskDialog(getProject(), false);
    try {
      assertTrue(dialog.isGuardedForTests());
      dialog.doCancelAction();
      assertEquals(ErlangUsageAskUi.Choice.LATER, ErlangUsageAskDialog.choiceOf(dialog.getExitCode()));
    }
    finally {
      if (!dialog.isDisposed()) Disposer.dispose(dialog.getDisposable());
    }
  }

  public void testExampleClosedAgainHasTheFirstSize() {
    ErlangUsageAskDialog dialog = new ErlangUsageAskDialog(getProject(), false);
    try {
      Dimension collapsed = dialog.getContentPreferredSizeForTests();
      dialog.toggleExampleForTests();
      assertTrue(dialog.isExampleShownForTests());
      assertTrue(dialog.getContentPreferredSizeForTests().height > collapsed.height);
      dialog.toggleExampleForTests();
      assertFalse(dialog.isExampleShownForTests());
      assertEquals(collapsed, dialog.getContentPreferredSizeForTests());
    }
    finally {
      Disposer.dispose(dialog.getDisposable());
    }
  }

  // a switch of the theme puts the colours of the new theme into the defaults of the look and feel; the IDE then
  // updates the components of its windows, but not the example, which is not in the window while it is closed
  public void testExampleFollowsTheThemeThatChangedWhileItWasClosed() {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();
    Object oldForeground = defaults.get("TextArea.foreground");
    Object oldBackground = defaults.get("Panel.background");
    Color darkForeground = new ColorUIResource(0xDFE1E5);
    Color darkBackground = new ColorUIResource(0x191A1C);
    Color lightForeground = new ColorUIResource(0x000000);
    Color lightBackground = new ColorUIResource(0xF7F8FA);
    ErlangUsageAskDialog dialog = null;
    try {
      defaults.put("TextArea.foreground", darkForeground);
      defaults.put("Panel.background", darkBackground);
      assertEquals(darkForeground, UIManager.getColor("TextArea.foreground"));
      assertEquals(darkBackground, UIManager.getColor("Panel.background"));
      dialog = new ErlangUsageAskDialog(getProject(), false);
      // the theme changes while the example is closed
      defaults.put("TextArea.foreground", lightForeground);
      defaults.put("Panel.background", lightBackground);
      dialog.toggleExampleForTests();
      JTextArea text = UIUtil.findComponentOfType(dialog.getContentForTests(), JTextArea.class);
      assertNotNull(text);
      assertEquals(ErlangUsageAskDialog.exampleText(), text.getText());
      assertColors(text, lightForeground, lightBackground);
      // and back while it is open: the IDE updates the components of the window
      defaults.put("TextArea.foreground", darkForeground);
      defaults.put("Panel.background", darkBackground);
      SwingUtilities.updateComponentTreeUI(dialog.getContentForTests());
      assertColors(text, darkForeground, darkBackground);
    }
    finally {
      defaults.put("TextArea.foreground", oldForeground);
      defaults.put("Panel.background", oldBackground);
      if (dialog != null) Disposer.dispose(dialog.getDisposable());
    }
  }

  private static void assertColors(@NotNull JTextArea text, @NotNull Color foreground, @NotNull Color background) {
    Color actualForeground = text.getForeground();
    Color actualBackground = text.getBackground();
    assertEquals("foreground", foreground.getRGB(), actualForeground.getRGB());
    assertEquals("background", background.getRGB(), actualBackground.getRGB());
    // the WCAG ratio for normal text
    double contrast = contrast(actualForeground, actualBackground);
    assertTrue("contrast " + contrast, contrast >= 4.5);
  }

  private static double contrast(@NotNull Color a, @NotNull Color b) {
    double la = luminance(a);
    double lb = luminance(b);
    return (Math.max(la, lb) + 0.05) / (Math.min(la, lb) + 0.05);
  }

  private static double luminance(@NotNull Color color) {
    double[] c = {color.getRed() / 255.0, color.getGreen() / 255.0, color.getBlue() / 255.0};
    for (int i = 0; i < c.length; i++) {
      c[i] = c[i] <= 0.03928 ? c[i] / 12.92 : Math.pow((c[i] + 0.055) / 1.055, 2.4);
    }
    return 0.2126 * c[0] + 0.7152 * c[1] + 0.0722 * c[2];
  }

  public void testAskWaitsAMinuteAfterTheProjectIsReady() {
    FakeAskUi ui = new FakeAskUi();
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    statistics.setAskUiForTests(ui);
    ErlangUsageStatistics.setReadySinceForTests(getProject(), System.currentTimeMillis() - ErlangUsageStatistics.ASK_DELAY_MS / 2);
    statistics.ask(getProject());
    assertEquals(List.of(), ui.asked);
    ErlangUsageStatistics.setReadySinceForTests(getProject(), System.currentTimeMillis() - ErlangUsageStatistics.ASK_DELAY_MS - 1_000);
    statistics.ask(getProject());
    assertEquals(List.of(false), ui.asked);
  }

  public void testAskWaitsUntilTheUserStopsTyping() {
    FakeAskUi ui = new FakeAskUi();
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    statistics.setAskUiForTests(ui);
    try {
      ErlangUsageIdle.setLastInputForTests(System.currentTimeMillis() - 1_000);
      statistics.ask(getProject());
      assertEquals(List.of(), ui.asked);
      ErlangUsageIdle.setLastInputForTests(System.currentTimeMillis() - 6_000);
      statistics.ask(getProject());
      assertEquals(List.of(false), ui.asked);
    }
    finally {
      ErlangUsageIdle.setLastInputForTests(0);
    }
  }

  public void testAskWaitsUntilTheUserIsNotBusyAndAsksOncePerSession() {
    FakeAskUi ui = new FakeAskUi();
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    statistics.setAskUiForTests(ui);
    ui.canAsk = false;
    statistics.ask(getProject());
    assertEquals(List.of(), ui.asked);
    assertEquals(0, collector().getState().askCount);

    ui.canAsk = true;
    ui.choice = ErlangUsageAskUi.Choice.LATER;
    statistics.ask(getProject());
    assertEquals(List.of(false), ui.asked);
    assertEquals(1, collector().getState().askCount);
    assertEquals(ErlangUsageCollector.Consent.UNSET, collector().getConsent());

    statistics.ask(getProject());
    assertEquals(List.of(false), ui.asked);
  }

  public void testAskOnlyInErlangProjects() {
    FakeAskUi ui = new FakeAskUi();
    ErlangUsageStatistics.getInstance().setAskUiForTests(ui);
    ErlangUsageStatistics.setErlangProjectForTests(getProject(), false);
    ErlangUsageStatistics.getInstance().ask(getProject());
    assertEquals(List.of(), ui.asked);
  }

  public void testYesFromTheDialog() {
    FakeAskUi ui = new FakeAskUi();
    ui.choice = ErlangUsageAskUi.Choice.YES;
    ErlangUsageStatistics.getInstance().setAskUiForTests(ui);
    ErlangUsageStatistics.getInstance().ask(getProject());
    assertEquals(ErlangUsageCollector.Consent.YES, collector().getConsent());
    assertNotNull(collector().getInstallationId());
  }

  public void testNoFromTheDialog() {
    FakeAskUi ui = new FakeAskUi();
    ui.choice = ErlangUsageAskUi.Choice.NO;
    ErlangUsageStatistics.getInstance().setAskUiForTests(ui);
    ErlangUsageStatistics.getInstance().ask(getProject());
    assertEquals(ErlangUsageCollector.Consent.NO, collector().getConsent());
    assertFalse(collector().shouldAsk());
  }

  private static final class FakeAskUi implements ErlangUsageAskUi {
    final List<Boolean> asked = new ArrayList<>();
    final List<Project> askedIn = new ArrayList<>();
    boolean canAsk = true;
    // the user does not work in the window of this project
    Project cannotAskIn;
    boolean fail;
    Choice choice = Choice.LATER;

    @Override
    public boolean canAskNow(@NotNull Project project) {
      return canAsk && project != cannotAskIn;
    }

    @Override
    public @NotNull Choice ask(@NotNull Project project, boolean reminder, @NotNull Runnable beforeShow) {
      // like a dialog whose construction throws before it is shown
      if (fail) throw new IllegalStateException("no dialog");
      beforeShow.run();
      asked.add(reminder);
      askedIn.add(project);
      return choice;
    }
  }

  // runs the checks of the ask chain when the test says so; while a check waits, the time passes without input
  private static final class FakeScheduler implements ErlangUsageStatistics.AskScheduler {
    private record Task(Runnable task, long delay) {
    }

    private final ArrayDeque<Task> myTasks = new ArrayDeque<>();
    // like a check with no delay that runs before schedule returns
    boolean immediately;

    @Override
    public @NotNull ScheduledFuture<?> schedule(@NotNull Runnable task, long delayMillis) {
      if (immediately) task.run();
      else myTasks.add(new Task(task, delayMillis));
      return new DoneFuture();
    }

    // runs the checks until none is due; returns how many ran
    int runAll() {
      int checks = 0;
      while (!myTasks.isEmpty()) {
        Task task = myTasks.poll();
        ErlangUsageIdle.setLastInputForTests(System.currentTimeMillis() - ErlangUsageIdle.idleMillis() - task.delay());
        task.task().run();
        PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
        checks++;
      }
      return checks;
    }
  }

  private static final class DoneFuture implements ScheduledFuture<Object> {
    @Override
    public long getDelay(@NotNull TimeUnit unit) {
      return 0;
    }

    @Override
    public int compareTo(@NotNull Delayed o) {
      return 0;
    }

    @Override
    public boolean cancel(boolean mayInterruptIfRunning) {
      return false;
    }

    @Override
    public boolean isCancelled() {
      return false;
    }

    @Override
    public boolean isDone() {
      return true;
    }

    @Override
    public Object get() {
      return null;
    }

    @Override
    public Object get(long timeout, @NotNull TimeUnit unit) {
      return null;
    }
  }

  // a project that the test can close; it keeps only its user data
  @NotNull
  private static Project fakeProject(boolean @NotNull [] disposed) {
    Map<Object, Object> data = new HashMap<>();
    return (Project)Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class}, (proxy, method, args) ->
      switch (method.getName()) {
        case "isDisposed" -> disposed[0];
        case "getUserData" -> data.get(args[0]);
        case "putUserData" -> {
          data.put(args[0], args[1]);
          yield null;
        }
        case "hashCode" -> System.identityHashCode(proxy);
        case "equals" -> proxy == args[0];
        case "toString" -> "fake project";
        default -> method.getReturnType() == boolean.class ? false : null;
      });
  }

  public void testTwoSettingsDialogs() {
    ErlangUsageStatisticsPanel first = new ErlangUsageStatisticsPanel();
    ErlangUsageStatisticsPanel second = new ErlangUsageStatisticsPanel();
    first.setSelectedForTests(true);
    assertTrue(first.isModified());
    first.apply();
    assertEquals(ErlangUsageCollector.Consent.YES, collector().getConsent());
    // the second dialog still shows "off", but the user did not touch it
    assertFalse(second.isModified());
    second.apply();
    assertEquals(ErlangUsageCollector.Consent.YES, collector().getConsent());
  }

  public void testSettingsCheckBoxIsDisabledWhenTheEnvironmentSwitchesItOff() {
    ErlangUsageStatistics.setAllowedForTests(false);
    ErlangUsageStatisticsPanel panel = new ErlangUsageStatisticsPanel();
    assertFalse(panel.isCheckBoxEnabledForTests());
    panel.setSelectedForTests(true);
    panel.apply();
    assertEquals("UNSET", collector().getState().consent);
    assertNull(collector().getInstallationId());
    ErlangUsageStatistics.setAllowedForTests(true);
    assertTrue(new ErlangUsageStatisticsPanel().isCheckBoxEnabledForTests());
  }

  // a missing dialyzer executable gives an empty output without an exit code: Dialyzer did not run, so it is not ok
  public void testDialyzerThatNeverStartedIsNotOk() {
    collector().setEnabled(true);
    String missing = FileUtil.getTempDirectory() + "/no-such-sdk";
    ErlangDialyzerExternalAnnotator.State state =
      new ErlangDialyzerExternalAnnotator.State(getProject(), missing + "/bin/dialyzer", null, missing + "/foo.erl", FileUtil.getTempDirectory());
    new ErlangDialyzerExternalAnnotator().doAnnotate(state);
    JsonObject properties = previewProperties();
    assertFalse(properties.toString(), properties.has("dialyzer_ok"));
    assertEquals(1, properties.get("dialyzer_failed").getAsInt());
  }

  public void testOtpReleaseNeverStartsErl() throws Exception {
    File home = FileUtil.createTempDirectory("erlang-sdk", null);
    File marker = new File(home, "erl-started");
    File erl = new File(home, "bin/erl");
    FileUtil.writeToFile(erl, "#!/bin/sh\ntouch '" + marker.getPath() + "'\n");
    assertTrue(erl.setExecutable(true));
    ProjectJdkImpl sdk = new ProjectJdkImpl("Erlang without version", ErlangSdkType.getInstance());
    ProjectRootManager rootManager = ProjectRootManager.getInstance(getProject());
    Sdk old = rootManager.getProjectSdk();
    WriteAction.run(() -> {
      SdkModificator modificator = sdk.getSdkModificator();
      modificator.setHomePath(home.getPath());
      modificator.commitChanges();
      ProjectJdkTable.getInstance().addJdk(sdk, getTestRootDisposable());
      rootManager.setProjectSdk(sdk);
    });
    assertSame(sdk, rootManager.getProjectSdk());
    try {
      assertEquals("unknown", ErlangUsageStatistics.otp(getProject(), true));
      assertFalse(marker.exists());
      WriteAction.run(() -> {
        SdkModificator modificator = sdk.getSdkModificator();
        modificator.setVersionString(ErlangSdkRelease.V_27_0.toString());
        modificator.commitChanges();
      });
      assertEquals("27", ErlangUsageStatistics.otp(getProject(), true));
      assertEquals("none", ErlangUsageStatistics.otp(getProject(), false));
      assertFalse(marker.exists());
    }
    finally {
      WriteAction.run(() -> rootManager.setProjectSdk(old));
    }
  }

  // the SDK of the project is read in a read action, so it waits while a write action changes the roots
  public void testSdkIsReadInAReadAction() throws Exception {
    collector().setEnabled(true);
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    Future<?>[] record = new Future<?>[1];
    WriteAction.run(() -> {
      record[0] = AppExecutorUtil.getAppExecutorService().submit(() -> statistics.recordProjectForTests(getProject(), 5));
      long end = System.currentTimeMillis() + 1_000;
      while (System.currentTimeMillis() < end && !hasSdkConfigured()) {
        TimeoutUtil.sleep(10);
      }
      assertFalse("the SDK was read during a write action", hasSdkConfigured());
    });
    record[0].get(10, TimeUnit.SECONDS);
    assertTrue(hasSdkConfigured());
  }

  private static boolean hasSdkConfigured() {
    return collector().getState().days.stream().anyMatch(day -> day.properties.containsKey(ErlangUsageProperty.SDK_CONFIGURED.name()));
  }

  public void testOnlyClassesOfThisPluginCount() {
    collector().setEnabled(true);
    ErlangUsageCounters.countQuickFix(getProject(), LocalQuickFix.class);
    ErlangUsageCounters.countIntention(getProject(), String.class);
    ErlangUsageCounters.countQuickFix(getProject(), ErlangUsageCollector.class);
    collector().flush();
    assertEquals(Map.of("quick_fix:ErlangUsageCollector", 1L), collector().getState().days.get(0).counters);
  }

  // hostile values through every setter and through a changed state file never reach the JSON
  public void testHostileValuesInTheStateFile() throws Exception {
    ErlangUsageCollectorTest.FakeSender sender = new ErlangUsageCollectorTest.FakeSender();
    LocalDate[] today = {LocalDate.of(2026, 11, 19)};
    ErlangUsageCollector collector =
      new ErlangUsageCollector(() -> today[0], ErlangUsageCollectorTest::appProperties, () -> "Germany", () -> true, sender);
    collector.setEnabled(true);
    String[] hostile = {"/Users/hostile", "hostile\",\"ip\":\"1.2.3.4", "27,/Users/hostile", "Erlang/../hostile", "ErlangHostile\u0000x",
      "true,hostile", "IU\nhostile", "263 hostile", "0.11.1\"hostile"};
    for (ErlangUsageProperty property : ErlangUsageProperty.values()) {
      for (String value : hostile) collector.setProperty(property, value);
    }
    collector.count(ErlangUsageKey.RENAME);
    collector.flush();
    ErlangUsageStatisticsState state = collector.getState();
    ErlangUsageStatisticsState.Day day = state.days.get(0);
    day.counters.put("quick_fix:/Users/hostile", 1L);
    day.counters.put("intention:Hostile name", 1L);
    day.counters.put("hostile_counter", 1L);
    day.counters.put("RENAME\",\"ip\":\"1.2.3.4", 1L);
    day.properties.put("hostile_property", "hostile");
    day.properties.put("OTP", "27,/Users/hostile,R16");
    day.properties.put("CHANGED_INSPECTIONS", "ErlangUnusedVariable,hostile path,Erlang\"x");
    day.country = "Hostile\",\"ip\":\"1.2.3.4";
    ErlangUsageStatisticsState.Day bad = new ErlangUsageStatisticsState.Day();
    bad.date = "2026-11-18\",\"ip\":\"1.2.3.4";
    bad.counters.put("RENAME", 1L);
    state.days.add(bad);
    String xml = JDOMUtil.write(XmlSerializer.serialize(state));
    collector.loadState(XmlSerializer.deserialize(JDOMUtil.load(xml), ErlangUsageStatisticsState.class));
    today[0] = today[0].plusDays(1);
    assertEquals(1, collector.sendPending());
    String json = String.join("\n", sender.bodies);
    assertFalse(json, json.contains("hostile") || json.contains("Hostile") || json.contains("/Users") || json.contains("\"ip\""));
    assertTrue(json, json.contains("\"otp\":[\"27\",\"R16\"]"));
  }

  public void testDefaultIsOff() {
    completeFunction();
    myFixture.performEditorAction("ReformatCode");
    ErlangUsageCounters.count(getProject(), ErlangUsageKey.RENAME);
    assertFalse(ErlangUsageCounters.isCounting());
    assertTrue(collector().getState().days.isEmpty());
    assertEquals(0, collector().sendPending());
    assertTrue(mySender.bodies.isEmpty());
  }

  public void testCompletionInErlangFileIsCounted() {
    collector().setEnabled(true);
    completeFunction();
    JsonObject properties = previewProperties();
    assertEquals(1, properties.get("completion").getAsInt());
    assertEquals(1, properties.get("completion_function").getAsInt());
  }

  public void testActionInErlangFileIsCounted() {
    collector().setEnabled(true);
    myFixture.configureByText("a.erl", "-module(a).\nfoo()->ok.\n");
    myFixture.performEditorAction("ReformatCode");
    assertEquals(1, previewProperties().get("reformat").getAsInt());
  }

  public void testActionInOtherFileIsNotCounted() {
    collector().setEnabled(true);
    myFixture.configureByText("a.txt", "text");
    myFixture.performEditorAction("ReformatCode");
    assertFalse(previewProperties().has("reformat"));
  }

  public void testTurningOffDeletesTheData() {
    collector().setEnabled(true);
    ErlangUsageCounters.count(getProject(), ErlangUsageKey.RENAME);
    assertNotNull(collector().getInstallationId());
    ErlangUsageStatisticsPanel panel = new ErlangUsageStatisticsPanel();
    assertFalse(panel.isModified());
    ErlangUsageStatistics.getInstance().setEnabled(false);
    assertNull(collector().getInstallationId());
    assertTrue(collector().getState().days.isEmpty());
    assertFalse(previewProperties().has("rename"));
  }

  public void testStateIsStoredAndRead() {
    collector().setEnabled(true);
    ErlangUsageCounters.count(getProject(), ErlangUsageKey.RENAME);
    ErlangUsageCounters.countQuickFix(getProject(), ErlangUsageCollectorTest.FakeSender.class);
    collector().setProperty(ErlangUsageProperty.OTP, "27");
    collector().markAsked();
    ErlangUsageStatisticsState state = ErlangUsageStatistics.getInstance().getState();
    ErlangUsageStatisticsState read = XmlSerializer.deserialize(XmlSerializer.serialize(state), ErlangUsageStatisticsState.class);
    assertEquals("YES", read.consent);
    assertEquals(1, read.askCount);
    assertEquals(state.lastAskDate, read.lastAskDate);
    assertEquals(state.installationId, read.installationId);
    assertEquals(1, read.days.size());
    assertEquals(state.days.get(0).date, read.days.get(0).date);
    assertEquals(Map.of("RENAME", 1L, "quick_fix:FakeSender", 1L), read.days.get(0).counters);
    assertEquals("27", read.days.get(0).properties.get("OTP"));
  }

  public void testRunConfigurationKinds() {
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "hostile_run", ErlangApplicationRunConfigurationType.getInstance());
    assertEquals(ErlangUsageKey.RUN_APPLICATION, ErlangUsageListeners.runKey("Run", configuration));
    assertEquals(ErlangUsageKey.DEBUG_APPLICATION, ErlangUsageListeners.runKey("Debug", configuration));
    assertNull(ErlangUsageListeners.runKey("Coverage", configuration));
    assertNull(ErlangUsageListeners.runKey("Run", null));
  }

  public void testOpenedFileKinds() {
    assertEquals(ErlangUsageKey.OPENED_ERL, ErlangUsageListeners.openedFileKey(new LightVirtualFile("a.erl")));
    assertEquals(ErlangUsageKey.OPENED_HRL, ErlangUsageListeners.openedFileKey(new LightVirtualFile("a.hrl")));
    assertEquals(ErlangUsageKey.OPENED_APP, ErlangUsageListeners.openedFileKey(new LightVirtualFile("a.app.src")));
    assertEquals(ErlangUsageKey.OPENED_CONFIG, ErlangUsageListeners.openedFileKey(new LightVirtualFile("sys.config")));
    assertNull(ErlangUsageListeners.openedFileKey(new LightVirtualFile("a.txt")));
  }

  public void testRunConfigurationKindsOfEveryErlangConfiguration() {
    Project project = getProject();
    assertRunKeys(new ErlangConsoleRunConfiguration("console", project), ErlangUsageKey.RUN_CONSOLE, ErlangUsageKey.DEBUG_CONSOLE);
    assertRunKeys(new ErlangUnitRunConfiguration(project, "eunit", ErlangUnitRunConfigurationType.getInstance()),
                  ErlangUsageKey.RUN_EUNIT, ErlangUsageKey.DEBUG_EUNIT);
    assertRunKeys(RebarEunitRunConfigurationFactory.getInstance().createTemplateConfiguration(project),
                  ErlangUsageKey.RUN_REBAR_EUNIT, ErlangUsageKey.DEBUG_REBAR_EUNIT);
    assertRunKeys(new RebarRunConfiguration("rebar", project), ErlangUsageKey.RUN_REBAR, ErlangUsageKey.DEBUG_REBAR);
    assertRunKeys(new ErlangRemoteDebugRunConfiguration(project, "remote"), ErlangUsageKey.RUN_REMOTE_NODE, ErlangUsageKey.DEBUG_REMOTE_NODE);
    // a run configuration of another plugin
    RunProfile other = new RunProfile() {
      @Override
      public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment environment) {
        return null;
      }

      @Override
      public @NotNull String getName() {
        return "other";
      }

      @Override
      public Icon getIcon() {
        return null;
      }
    };
    assertNull(ErlangUsageListeners.runKey(DefaultRunExecutor.EXECUTOR_ID, other));
    assertNull(ErlangUsageListeners.runKey(DefaultDebugExecutor.EXECUTOR_ID, other));
  }

  private static void assertRunKeys(@NotNull RunProfile profile, @NotNull ErlangUsageKey run, @NotNull ErlangUsageKey debug) {
    assertEquals(run, ErlangUsageListeners.runKey(DefaultRunExecutor.EXECUTOR_ID, profile));
    assertEquals(debug, ErlangUsageListeners.runKey(DefaultDebugExecutor.EXECUTOR_ID, profile));
  }

  public void testExecutionListenerCountsTheStartedErlangRun() {
    collector().setEnabled(true);
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "app", ErlangApplicationRunConfigurationType.getInstance());
    ExecutionEnvironment environment = new ExecutionEnvironmentBuilder(getProject(), DefaultRunExecutor.getRunExecutorInstance())
      .runProfile(configuration).runner(new ErlangRunner()).build();
    ErlangUsageListeners.Execution listener = new ErlangUsageListeners.Execution();
    listener.processStarted(DefaultRunExecutor.EXECUTOR_ID, environment, new NopProcessHandler());
    // an executor that the statistics do not know, such as coverage, counts nothing
    listener.processStarted("Coverage", environment, new NopProcessHandler());
    JsonObject properties = previewProperties();
    assertEquals(1, properties.get(ErlangUsageKey.RUN_APPLICATION.jsonName()).getAsInt());
    assertFalse(properties.has(ErlangUsageKey.DEBUG_APPLICATION.jsonName()));
  }

  public void testCompletionKindOfPsiElements() {
    PsiFile file = myFixture.configureByText("kinds.erl", """
      -module(kinds).
      -record(rec, {a}).
      -define(MAC, 1).
      -type t() :: integer().
      f(X) -> X.
      """);
    assertEquals(ErlangUsageKey.COMPLETION_FUNCTION, completionKindOf(PsiTreeUtil.findChildOfType(file, ErlangFunction.class)));
    assertEquals(ErlangUsageKey.COMPLETION_MODULE, completionKindOf(PsiTreeUtil.findChildOfType(file, ErlangModule.class)));
    assertEquals(ErlangUsageKey.COMPLETION_MODULE, completionKindOf(file));
    assertEquals(ErlangUsageKey.COMPLETION_RECORD, completionKindOf(PsiTreeUtil.findChildOfType(file, ErlangRecordDefinition.class)));
    assertEquals(ErlangUsageKey.COMPLETION_MACRO, completionKindOf(PsiTreeUtil.findChildOfType(file, ErlangMacrosDefinition.class)));
    assertEquals(ErlangUsageKey.COMPLETION_VARIABLE, completionKindOf(PsiTreeUtil.findChildOfType(file, ErlangQVar.class)));
    assertEquals(ErlangUsageKey.COMPLETION_TYPE, completionKindOf(PsiTreeUtil.findChildOfType(file, ErlangTypeDefinition.class)));
    // an element of another kind is told by its lookup string and its icon
    assertEquals(ErlangUsageKey.COMPLETION_OTHER, completionKindOf(PsiTreeUtil.findChildOfType(file, ErlangAttribute.class)));
  }

  @NotNull
  private static ErlangUsageKey completionKindOf(@Nullable PsiElement element) {
    assertNotNull(element);
    return ErlangUsageListeners.completionKind(LookupElementBuilder.create(element, "name"));
  }

  public void testCompletionKindOfPlainItems() {
    assertEquals(ErlangUsageKey.COMPLETION_KEYWORD, completionKindOf(LookupElementBuilder.create("case")));
    assertEquals(ErlangUsageKey.COMPLETION_KEYWORD, completionKindOf(LookupElementBuilder.create("receive")));
    assertEquals(ErlangUsageKey.COMPLETION_FUNCTION, completionKindOf(LookupElementBuilder.create("f").withIcon(ErlangIcons.FUNCTION)));
    assertEquals(ErlangUsageKey.COMPLETION_MODULE, completionKindOf(LookupElementBuilder.create("m").withIcon(ErlangIcons.MODULE)));
    assertEquals(ErlangUsageKey.COMPLETION_MODULE, completionKindOf(LookupElementBuilder.create("m").withIcon(ErlangIcons.FILE)));
    assertEquals(ErlangUsageKey.COMPLETION_RECORD, completionKindOf(LookupElementBuilder.create("r").withIcon(ErlangIcons.RECORD)));
    assertEquals(ErlangUsageKey.COMPLETION_MACRO, completionKindOf(LookupElementBuilder.create("M").withIcon(ErlangIcons.MACROS)));
    assertEquals(ErlangUsageKey.COMPLETION_VARIABLE, completionKindOf(LookupElementBuilder.create("V").withIcon(ErlangIcons.VARIABLE)));
    assertEquals(ErlangUsageKey.COMPLETION_TYPE, completionKindOf(LookupElementBuilder.create("t").withIcon(ErlangIcons.TYPE)));
    // a bold item without an icon is a keyword that the lexer does not list, such as andalso
    assertEquals(ErlangUsageKey.COMPLETION_KEYWORD, completionKindOf(LookupElementBuilder.create("andalso").bold()));
    assertEquals(ErlangUsageKey.COMPLETION_OTHER, completionKindOf(LookupElementBuilder.create("andalso")));
    assertEquals(ErlangUsageKey.COMPLETION_OTHER, completionKindOf(LookupElementBuilder.create("a").withIcon(ErlangIcons.ATOM).bold()));
  }

  @NotNull
  private static ErlangUsageKey completionKindOf(@NotNull LookupElement item) {
    return ErlangUsageListeners.completionKind(item);
  }

  public void testCompletionListenerCountsTheItemSelectedInAnErlangFile() {
    collector().setEnabled(true);
    PsiFile erlang = myFixture.configureByText("a.erl", "-module(a).\n");
    PsiFile text = PsiFileFactory.getInstance(getProject()).createFileFromText("a.txt", PlainTextFileType.INSTANCE, "text");
    List<LookupListener> listeners = new ArrayList<>();
    ErlangUsageListeners.Completion completion = new ErlangUsageListeners.Completion();
    completion.activeLookupChanged(null, null);
    completion.activeLookupChanged(null, fakeLookup(text, listeners));
    assertEmpty(listeners);

    Lookup lookup = fakeLookup(erlang, listeners);
    completion.activeLookupChanged(null, lookup);
    assertSize(1, listeners);
    listeners.get(0).itemSelected(new LookupEvent(lookup, null, Lookup.NORMAL_SELECT_CHAR));
    assertFalse(previewProperties().has(ErlangUsageKey.COMPLETION.jsonName()));
    listeners.get(0).itemSelected(new LookupEvent(lookup, LookupElementBuilder.create("case"), Lookup.NORMAL_SELECT_CHAR));
    JsonObject properties = previewProperties();
    assertEquals(1, properties.get(ErlangUsageKey.COMPLETION.jsonName()).getAsInt());
    assertEquals(1, properties.get(ErlangUsageKey.COMPLETION_KEYWORD.jsonName()).getAsInt());
  }

  public void testCompletionListenerIgnoresLookupsWhileOff() {
    PsiFile erlang = myFixture.configureByText("a.erl", "-module(a).\n");
    List<LookupListener> listeners = new ArrayList<>();
    new ErlangUsageListeners.Completion().activeLookupChanged(null, fakeLookup(erlang, listeners));
    assertEmpty(listeners);
  }

  // a lookup in the file that keeps the listeners added to it
  @NotNull
  private Lookup fakeLookup(@NotNull PsiFile file, @NotNull List<LookupListener> listeners) {
    Project project = getProject();
    return (Lookup)Proxy.newProxyInstance(Lookup.class.getClassLoader(), new Class<?>[]{Lookup.class}, (proxy, method, args) ->
      switch (method.getName()) {
        case "getPsiFile" -> file;
        case "getProject" -> project;
        case "addLookupListener" -> {
          listeners.add((LookupListener)args[0]);
          yield null;
        }
        case "hashCode" -> System.identityHashCode(proxy);
        case "equals" -> proxy == args[0];
        case "toString" -> "fake lookup";
        default -> method.getReturnType() == boolean.class ? false : null;
      });
  }

  public void testStructureViewIsCountedForAnErlangEditor() {
    collector().setEnabled(true);
    ErlangUsageListeners.StructureView listener = new ErlangUsageListeners.StructureView(getProject());
    myFixture.configureByText("a.txt", "text");
    listener.toolWindowShown(toolWindow(ToolWindowId.STRUCTURE_VIEW));
    assertFalse(previewProperties().has(ErlangUsageKey.STRUCTURE_VIEW.jsonName()));
    myFixture.configureByText("a.erl", "-module(a).\n");
    listener.toolWindowShown(toolWindow(ToolWindowId.PROJECT_VIEW));
    assertFalse(previewProperties().has(ErlangUsageKey.STRUCTURE_VIEW.jsonName()));
    listener.toolWindowShown(toolWindow(ToolWindowId.STRUCTURE_VIEW));
    assertEquals(1, previewProperties().get(ErlangUsageKey.STRUCTURE_VIEW.jsonName()).getAsInt());
  }

  public void testStructureViewWithoutAnEditorIsNotCounted() {
    collector().setEnabled(true);
    ErlangUsageListeners.StructureView listener = new ErlangUsageListeners.StructureView(getProject());
    listener.toolWindowShown(toolWindow(ToolWindowId.STRUCTURE_VIEW));
    assertFalse(previewProperties().has(ErlangUsageKey.STRUCTURE_VIEW.jsonName()));
    // a closed project has no editors to ask; asking them would fail
    new ErlangUsageListeners.StructureView(fakeProject(new boolean[]{true})).toolWindowShown(toolWindow(ToolWindowId.STRUCTURE_VIEW));
    assertFalse(previewProperties().has(ErlangUsageKey.STRUCTURE_VIEW.jsonName()));
  }

  @NotNull
  private static ToolWindow toolWindow(@NotNull String id) {
    return (ToolWindow)Proxy.newProxyInstance(ToolWindow.class.getClassLoader(), new Class<?>[]{ToolWindow.class}, (proxy, method, args) ->
      switch (method.getName()) {
        case "getId" -> id;
        case "hashCode" -> System.identityHashCode(proxy);
        case "equals" -> proxy == args[0];
        case "toString" -> "tool window " + id;
        default -> method.getReturnType() == boolean.class ? false : null;
      });
  }

  public void testLiveTemplateIsCountedInAnErlangEditor() {
    TemplateManagerImpl.setTemplateTesting(getTestRootDisposable());
    collector().setEnabled(true);
    myFixture.configureByText("a.erl", "-module(a).\n<caret>");
    TemplateManager manager = TemplateManager.getInstance(getProject());
    // the templates of in-place rename and of other code have no abbreviation
    startTemplate(manager.createTemplate("", "", "x$END$"));
    assertFalse(previewProperties().has(ErlangUsageKey.LIVE_TEMPLATE.jsonName()));
    startTemplate(manager.createTemplate("hello", "test", "hello()$END$"));
    assertEquals(1, previewProperties().get(ErlangUsageKey.LIVE_TEMPLATE.jsonName()).getAsInt());
  }

  public void testLiveTemplateIsNotCountedInOtherFilesOrWhileOff() {
    TemplateManagerImpl.setTemplateTesting(getTestRootDisposable());
    TemplateManager manager = TemplateManager.getInstance(getProject());
    myFixture.configureByText("a.erl", "-module(a).\n<caret>");
    Template stopping = manager.createTemplate("hello", "test", "hello($X$)$END$");
    stopping.addVariable("X", "", "\"a\"", true);
    startTemplate(stopping);
    TemplateState state = TemplateManagerImpl.getTemplateState(myFixture.getEditor());
    assertNotNull(state);
    // while the statistics are off, the listener returns before it asks the project for its editor
    new ErlangUsageListeners.LiveTemplates(ErlangUsageBuildListenerTest.projectThatMustNotBeAsked()).templateStarted(state);
    state.cancelTemplate();
    collector().setEnabled(true);
    myFixture.configureByText("a.txt", "<caret>");
    startTemplate(manager.createTemplate("hello", "test", "hello()$END$"));
    assertFalse(previewProperties().has(ErlangUsageKey.LIVE_TEMPLATE.jsonName()));
  }

  private void startTemplate(@NotNull Template template) {
    TemplateManager.getInstance(getProject()).startTemplate(myFixture.getEditor(), template);
  }

  public void testBuildWithoutErlangModulesIsNotCounted() {
    collector().setEnabled(true);
    new ErlangUsageListeners.Build(getProject()).finished(buildResult(false));
    JsonObject properties = previewProperties();
    assertFalse(properties.has(ErlangUsageKey.BUILD_BUILT_IN.jsonName()));
    assertFalse(properties.has(ErlangUsageKey.BUILD_REBAR.jsonName()));
  }

  @NotNull
  private static ProjectTaskManager.Result buildResult(boolean aborted) {
    return (ProjectTaskManager.Result)Proxy.newProxyInstance(
      ProjectTaskManager.Result.class.getClassLoader(), new Class<?>[]{ProjectTaskManager.Result.class}, (proxy, method, args) ->
        switch (method.getName()) {
          case "isAborted" -> aborted;
          case "hashCode" -> System.identityHashCode(proxy);
          case "equals" -> proxy == args[0];
          case "toString" -> "build result";
          default -> method.getReturnType() == boolean.class ? false : null;
        });
  }

  // names and paths pushed through every public way to count must not get into the JSON or the stored state
  public void testNoNamesOrPathsLeak() throws Exception {
    collector().setEnabled(true);
    completeFunction();
    myFixture.performEditorAction("ReformatCode");
    myFixture.performEditorAction("RenameElement");
    myFixture.configureByText("hostile_text.txt", "hostile_text");
    myFixture.performEditorAction("ReformatCode");
    ErlangUsageCounters.countNewFile(getProject(), "hostile_template /Users/hostile_user/hostile_dir");
    ErlangUsageCounters.countNewFile(getProject(), null);
    ErlangUsageCounters.countQuickFix(getProject(), String.class);
    ErlangUsageCounters.countQuickFix(getProject(), new Object() {}.getClass());
    ErlangUsageCounters.countIntention(getProject(), ErlangUsageCollectorTest.FakeSender.class);
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "hostile_run /tmp/hostile_path", ErlangApplicationRunConfigurationType.getInstance());
    ErlangUsageCounters.count(getProject(), ErlangUsageListeners.runKey("Run", configuration));

    String json = collector().previewToday();
    String state = JDOMUtil.write(XmlSerializer.serialize(ErlangUsageStatistics.getInstance().getState()));
    for (String text : List.of(json, state)) {
      for (String hostile : List.of("hostile", "/Users", "/tmp", "String", getProject().getName())) {
        assertFalse(hostile + " in " + text, text.contains(hostile));
      }
    }
    JsonObject properties = previewProperties();
    ErlangUsagePayloadTest.assertAllowedKeys(properties);
    assertEquals(1, properties.get("completion_function").getAsInt());
    assertEquals(1, properties.get("reformat").getAsInt());
    // the fixture opens the module in an editor; the text file does not count
    assertEquals(1, properties.get("opened_erl").getAsInt());
    assertEquals(2, properties.get("new_file_other").getAsInt());
    assertEquals(1, properties.get("run_application").getAsInt());
    assertEquals(1, properties.getAsJsonObject("intentions").get("FakeSender").getAsInt());
    assertFalse(properties.has("quick_fixes"));
  }

  private void completeFunction() {
    myFixture.configureByText("hostile_module.erl", MODULE);
    myFixture.completeBasic();
    assertNotNull(myFixture.getLookup());
    myFixture.finishLookup(Lookup.NORMAL_SELECT_CHAR);
  }

  @NotNull
  private JsonObject previewProperties() {
    String json = collector().previewToday();
    return JsonParser.parseString(json).getAsJsonObject().getAsJsonArray("events").get(0).getAsJsonObject()
      .getAsJsonObject("event_properties");
  }

  @NotNull
  private static ErlangUsageCollector collector() {
    return ErlangUsageStatistics.getInstance().getCollector();
  }
}
