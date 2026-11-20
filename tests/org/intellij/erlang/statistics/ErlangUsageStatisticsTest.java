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
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.testFramework.LightVirtualFile;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.application.ErlangApplicationConfiguration;
import org.intellij.erlang.application.ErlangApplicationRunConfigurationType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

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
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ErlangUsageStatistics.getInstance().resetForTests();
      collector().setSender(new ErlangAmplitudeSender());
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  public void testNoSendingOrAskingInTests() {
    assertFalse(ErlangUsageStatistics.isSendingAllowed());
  }

  public void testDefaultIsOff() {
    completeFunction();
    myFixture.performEditorAction("ReformatCode");
    ErlangUsageCounters.count(ErlangUsageKey.RENAME);
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
    ErlangUsageCounters.count(ErlangUsageKey.RENAME);
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
    ErlangUsageCounters.count(ErlangUsageKey.RENAME);
    ErlangUsageCounters.countQuickFix(ErlangUsageCollectorTest.FakeSender.class);
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
    ErlangUsageCounters.countNewFile("hostile_template /Users/hostile_user/hostile_dir");
    ErlangUsageCounters.countNewFile(null);
    ErlangUsageCounters.countQuickFix(String.class);
    ErlangUsageCounters.countQuickFix(new Object() {}.getClass());
    ErlangUsageCounters.countIntention(ErlangUsageCollectorTest.FakeSender.class);
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "hostile_run /tmp/hostile_path", ErlangApplicationRunConfigurationType.getInstance());
    ErlangUsageCounters.count(ErlangUsageListeners.runKey("Run", configuration));

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
