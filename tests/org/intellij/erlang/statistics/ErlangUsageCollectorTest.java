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

import com.google.gson.JsonArray;
import com.google.gson.JsonParser;
import junit.framework.TestCase;
import org.jetbrains.annotations.NotNull;

import java.time.LocalDate;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.function.BooleanSupplier;

public class ErlangUsageCollectorTest extends TestCase {
  private static final LocalDate DAY = LocalDate.of(2026, 11, 19);

  private LocalDate myToday = DAY;
  private ErlangUsageCollector myCollector;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myCollector = newCollector(() -> true);
  }

  @NotNull
  private ErlangUsageCollector newCollector(@NotNull BooleanSupplier allowed) {
    return new ErlangUsageCollector(() -> myToday, ErlangUsageCollectorTest::appProperties, () -> "Germany", allowed);
  }

  static Map<ErlangUsageProperty, String> appProperties() {
    Map<ErlangUsageProperty, String> properties = new EnumMap<>(ErlangUsageProperty.class);
    properties.put(ErlangUsageProperty.PLUGIN_VERSION, "0.11.1304");
    properties.put(ErlangUsageProperty.IDE_PRODUCT, "IU");
    properties.put(ErlangUsageProperty.IDE_BASELINE, "263");
    properties.put(ErlangUsageProperty.SMALL_IDE, "false");
    properties.put(ErlangUsageProperty.OS, "mac");
    return properties;
  }

  public void testDefaultIsOff() {
    assertEquals(ErlangUsageCollector.Consent.UNSET, myCollector.getConsent());
    assertFalse(myCollector.isCounting());
    myCollector.count(ErlangUsageKey.COMPLETION);
    myCollector.setProperty(ErlangUsageProperty.OTP, "27");
    assertTrue(myCollector.getState().days.isEmpty());
    assertNull(myCollector.getInstallationId());
  }

  public void testYesCreatesIdAndCounts() {
    myCollector.setEnabled(true);
    assertEquals(ErlangUsageCollector.Consent.YES, myCollector.getConsent());
    assertTrue(myCollector.isCounting());
    String id = myCollector.getInstallationId();
    assertNotNull(id);
    assertTrue(id, id.matches("[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}"));
    myCollector.count(ErlangUsageKey.COMPLETION);
    myCollector.count(ErlangUsageKey.COMPLETION);
    assertEquals(Long.valueOf(2), myCollector.getState().days.get(0).counters.get("COMPLETION"));
  }

  public void testNoCollectsNothing() {
    myCollector.setEnabled(false);
    assertEquals(ErlangUsageCollector.Consent.NO, myCollector.getConsent());
    assertFalse(myCollector.shouldAsk());
    myCollector.count(ErlangUsageKey.COMPLETION);
    assertTrue(myCollector.getState().days.isEmpty());
    assertNull(myCollector.getInstallationId());
  }

  public void testLaterAsksAgainAfterThirtyDays() {
    assertTrue(myCollector.shouldAsk());
    assertFalse(myCollector.isReminder());
    myCollector.markAsked();
    // "Ask me later", Esc or the dialog closed without an answer: the consent stays unset
    assertFalse(myCollector.shouldAsk());
    myToday = DAY.plusDays(29);
    assertFalse(myCollector.shouldAsk());
    myToday = DAY.plusDays(30);
    assertTrue(myCollector.shouldAsk());
    assertTrue(myCollector.isReminder());
    assertEquals(ErlangUsageCollector.Consent.UNSET, myCollector.getConsent());
    assertFalse(myCollector.isCounting());
  }

  public void testNoNeverAsksAgain() {
    myCollector.markAsked();
    myCollector.setEnabled(false);
    for (int days : new int[]{0, 30, 365, 3650}) {
      myToday = DAY.plusDays(days);
      assertFalse(myCollector.shouldAsk());
    }
    assertEquals(ErlangUsageCollector.Consent.NO, myCollector.getConsent());
  }

  public void testAtMostThreeAsks() {
    for (int i = 0; i < ErlangUsageCollector.MAX_ASKS; i++) {
      assertTrue(myCollector.shouldAsk());
      assertEquals(i > 0, myCollector.isReminder());
      myCollector.markAsked();
      myToday = myToday.plusDays(ErlangUsageCollector.ASK_AGAIN_DAYS);
    }
    for (int days : new int[]{0, 30, 365}) {
      assertFalse(myCollector.shouldAsk());
      myToday = myToday.plusDays(days);
    }
    assertEquals(ErlangUsageCollector.Consent.NO, myCollector.getConsent());
    assertFalse(myCollector.isCounting());
    myCollector.count(ErlangUsageKey.COMPLETION);
    assertTrue(myCollector.getState().days.isEmpty());
    // the settings can still turn it on
    myCollector.setEnabled(true);
    assertEquals(ErlangUsageCollector.Consent.YES, myCollector.getConsent());
  }

  public void testTurningOffDeletesIdAndCounters() {
    myCollector.setEnabled(true);
    String id = myCollector.getInstallationId();
    myCollector.count(ErlangUsageKey.RENAME);
    myCollector.flush();
    assertEquals(1, myCollector.getState().days.size());
    myCollector.count(ErlangUsageKey.RENAME);

    myCollector.setEnabled(false);
    assertNull(myCollector.getInstallationId());
    assertTrue(myCollector.getState().days.isEmpty());

    myCollector.setEnabled(true);
    assertNotNull(myCollector.getInstallationId());
    assertFalse(id.equals(myCollector.getInstallationId()));
    // the counts made before turning off are gone, also those that were only in memory
    assertTrue(myCollector.getState().days.isEmpty());
  }

  public void testCountsStayInTheirDay() {
    myCollector.setEnabled(true);
    myCollector.count(ErlangUsageKey.RENAME);
    myToday = DAY.plusDays(1);
    myCollector.count(ErlangUsageKey.RENAME);
    myCollector.count(ErlangUsageKey.INLINE);
    myCollector.flush();
    List<ErlangUsageStatisticsState.Day> days = myCollector.getState().days;
    assertEquals(2, days.size());
    assertEquals(DAY.toString(), days.get(0).date);
    assertEquals(Map.of("RENAME", 1L), days.get(0).counters);
    assertEquals(Map.of("RENAME", 1L, "INLINE", 1L), days.get(1).counters);
  }

  public void testListAndBooleanProperties() {
    myCollector.setEnabled(true);
    myCollector.setProperty(ErlangUsageProperty.OTP, "27");
    myCollector.setProperty(ErlangUsageProperty.OTP, "R16");
    myCollector.setProperty(ErlangUsageProperty.OTP, "27");
    myCollector.setProperty(ErlangUsageProperty.OTP, "/usr/lib/erlang");
    myCollector.setProperty(ErlangUsageProperty.SDK_CONFIGURED, "true");
    myCollector.setProperty(ErlangUsageProperty.SDK_CONFIGURED, "false");
    Map<String, String> properties = myCollector.getState().days.get(0).properties;
    assertEquals("27,R16", properties.get("OTP"));
    assertEquals("true", properties.get("SDK_CONFIGURED"));
  }

  public void testLoadedStateWithoutConsentIsCleared() {
    ErlangUsageStatisticsState state = new ErlangUsageStatisticsState();
    state.installationId = "8f1c2b0e-1111-4222-8333-444455556666";
    ErlangUsageStatisticsState.Day day = new ErlangUsageStatisticsState.Day();
    day.date = DAY.toString();
    day.counters.put("RENAME", 3L);
    state.days.add(day);
    myCollector.loadState(state);
    assertFalse(myCollector.isCounting());
    assertNull(myCollector.getInstallationId());
    assertTrue(myCollector.getState().days.isEmpty());
  }

  public void testLoadedStateWithConsentCounts() {
    ErlangUsageStatisticsState state = new ErlangUsageStatisticsState();
    state.consent = "YES";
    state.installationId = "8f1c2b0e-1111-4222-8333-444455556666";
    myCollector.loadState(state);
    assertTrue(myCollector.isCounting());
  }

  public void testCountsBeforeConsentAreNeverKept() {
    myCollector.count(ErlangUsageKey.RENAME);
    assertEquals(0, myCollector.countsInMemory());
    myCollector.setEnabled(true);
    myCollector.flush();
    assertTrue(myCollector.getState().days.isEmpty());
  }

  public void testCountsBeforeTurningOffAreNotSentAfterTurningOnAgain() {
    myCollector.setEnabled(true);
    myCollector.count(ErlangUsageKey.RENAME);
    myCollector.setEnabled(false);
    myCollector.setEnabled(true);
    myCollector.flush();
    assertTrue(myCollector.getState().days.isEmpty());
  }

  public void testNothingIsCountedWhenTheEnvironmentSwitchesItOff() {
    ErlangUsageCollector collector = newCollector(() -> false);
    collector.setEnabled(true);
    assertFalse(collector.isCounting());
    collector.count(ErlangUsageKey.RENAME);
    collector.setProperty(ErlangUsageProperty.OTP, "27");
    assertEquals(0, collector.countsInMemory());
    assertTrue(collector.getState().days.isEmpty());
  }

  public void testOldDaysAreDroppedWithoutSending() {
    myCollector.setEnabled(true);
    for (int i = 0; i < 60; i++) {
      myCollector.count(ErlangUsageKey.RENAME);
      myToday = myToday.plusDays(1);
    }
    myCollector.flush();
    // the last 7 days; today has no counts yet
    assertEquals(ErlangUsageCollector.MAX_DAYS, myCollector.getState().days.size());
  }

  @NotNull
  static JsonArray events(@NotNull String body) {
    return JsonParser.parseString(body).getAsJsonObject().getAsJsonArray("events");
  }
}
