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
import com.intellij.codeInspection.LocalQuickFix;
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
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.application.ErlangApplicationConfiguration;
import org.intellij.erlang.application.ErlangApplicationRunConfigurationType;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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

  public void testAskWaitsAMinuteAfterTheProjectIsReady() {
    FakeAskUi ui = new FakeAskUi();
    ErlangUsageStatistics statistics = ErlangUsageStatistics.getInstance();
    statistics.setAskUiForTests(ui);
    ErlangUsageStatistics.setReadySinceForTests(getProject(), System.currentTimeMillis() - 50_000);
    statistics.ask(getProject());
    assertEquals(List.of(), ui.asked);
    ErlangUsageStatistics.setReadySinceForTests(getProject(), System.currentTimeMillis() - 61_000);
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
    boolean canAsk = true;
    boolean fail;
    Choice choice = Choice.LATER;

    @Override
    public boolean canAskNow(@NotNull Project project) {
      return canAsk;
    }

    @Override
    public @NotNull Choice ask(@NotNull Project project, boolean reminder, @NotNull Runnable beforeShow) {
      // like a dialog whose construction throws before it is shown
      if (fail) throw new IllegalStateException("no dialog");
      beforeShow.run();
      asked.add(reminder);
      return choice;
    }
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
