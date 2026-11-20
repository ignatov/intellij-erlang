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

import com.intellij.openapi.util.JDOMUtil;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.time.LocalDate;

public class ErlangUsageStatisticsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private final ErlangUsageCollectorTest.FakeSender mySender = new ErlangUsageCollectorTest.FakeSender();

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    ErlangUsageStatistics.getInstance().resetForTests();
    collector().setSender(mySender);
    ErlangUsageStatistics.setAllowedForTests(true);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ErlangUsageStatistics.getInstance().resetForTests();
      collector().setSender(new ErlangAmplitudeSender());
      ErlangUsageStatistics.setAllowedForTests(null);
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

  public void testPreviewStoresNothing() {
    collector().previewToday();
    assertTrue(collector().getState().days.isEmpty());
    assertNull(collector().getInstallationId());
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

  @NotNull
  private static ErlangUsageCollector collector() {
    return ErlangUsageStatistics.getInstance().getCollector();
  }
}
