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
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import junit.framework.TestCase;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.BooleanSupplier;

public class ErlangUsageCollectorTest extends TestCase {
  private static final LocalDate DAY = LocalDate.of(2026, 11, 19);

  private LocalDate myToday = DAY;
  private final FakeSender mySender = new FakeSender();
  private ErlangUsageCollector myCollector;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myCollector = newCollector(() -> true);
  }

  @NotNull
  private ErlangUsageCollector newCollector(@NotNull BooleanSupplier allowed) {
    return new ErlangUsageCollector(() -> myToday, ErlangUsageCollectorTest::appProperties, () -> "Germany", allowed, mySender);
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
    myCollector.countQuickFix(ErlangUsageCollectorTest.class);
    myCollector.setProperty(ErlangUsageProperty.OTP, "27");
    myToday = DAY.plusDays(1);
    assertEquals(0, myCollector.sendPending());
    assertTrue(mySender.bodies.isEmpty());
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

  public void testAtMostTwoAsks() {
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

  public void testDayIsSentOnTheNextDayOnce() {
    myCollector.setEnabled(true);
    String id = myCollector.getInstallationId();
    myCollector.count(ErlangUsageKey.FIND_USAGES);
    assertEquals(0, myCollector.sendPending());
    assertTrue(mySender.bodies.isEmpty());

    myToday = DAY.plusDays(1);
    assertEquals(1, myCollector.sendPending());
    assertEquals(1, mySender.bodies.size());
    JsonObject event = events(mySender.bodies.get(0)).get(0).getAsJsonObject();
    assertEquals(id + "-2026-11-19", event.get("insert_id").getAsString());
    assertEquals(DAY.atTime(LocalTime.NOON).toInstant(ZoneOffset.UTC).toEpochMilli(), event.get("time").getAsLong());
    assertEquals(1, event.getAsJsonObject("event_properties").get("find_usages").getAsInt());

    assertEquals(0, myCollector.sendPending());
    assertEquals(1, mySender.bodies.size());
  }

  public void testRetryUsesTheSameInsertId() {
    myCollector.setEnabled(true);
    myCollector.count(ErlangUsageKey.FIND_USAGES);
    myToday = DAY.plusDays(1);
    mySender.status = 503;
    assertEquals(0, myCollector.sendPending());
    mySender.status = 200;
    assertEquals(1, myCollector.sendPending());
    assertEquals(2, mySender.bodies.size());
    assertEquals(events(mySender.bodies.get(0)).get(0).getAsJsonObject().get("insert_id"),
                 events(mySender.bodies.get(1)).get(0).getAsJsonObject().get("insert_id"));
  }

  public void testFailedSendKeepsTheDay() {
    myCollector.setEnabled(true);
    myCollector.count(ErlangUsageKey.RENAME);
    myToday = DAY.plusDays(1);
    mySender.status = 503;
    assertEquals(0, myCollector.sendPending());
    assertEquals(1, myCollector.getState().days.size());

    mySender.status = 200;
    mySender.fail = true;
    assertEquals(0, myCollector.sendPending());
    assertEquals(1, myCollector.getState().days.size());

    mySender.fail = false;
    assertEquals(1, myCollector.sendPending());
    assertTrue(myCollector.getState().days.isEmpty());
  }

  public void testAtMostSevenDaysAreSent() {
    myCollector.setEnabled(true);
    for (int i = 0; i < 10; i++) {
      myToday = DAY.plusDays(i);
      myCollector.count(ErlangUsageKey.REFORMAT);
    }
    myToday = DAY.plusDays(10);
    assertEquals(ErlangUsageCollector.MAX_DAYS, myCollector.sendPending());
    JsonArray events = events(mySender.bodies.get(0));
    assertEquals(ErlangUsageCollector.MAX_DAYS, events.size());
    String id = myCollector.getInstallationId();
    assertEquals(id + "-" + DAY.plusDays(3), events.get(0).getAsJsonObject().get("insert_id").getAsString());
    assertEquals(id + "-" + DAY.plusDays(9), events.get(6).getAsJsonObject().get("insert_id").getAsString());
    assertTrue(myCollector.getState().days.isEmpty());
  }

  public void testDayWithoutActivitySendsNothing() {
    myCollector.setEnabled(true);
    myCollector.setProperty(ErlangUsageProperty.OTP, "27");
    myCollector.setProperty(ErlangUsageProperty.PROJECT_SIZE, "up_to_100");
    myToday = DAY.plusDays(1);
    assertEquals(0, myCollector.sendPending());
    assertTrue(mySender.bodies.isEmpty());
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

  public void testOnlyNamedClassesOfThePluginAreCounted() {
    myCollector.setEnabled(true);
    myCollector.countQuickFix(String.class);
    myCollector.countQuickFix(new Object() {}.getClass());
    myCollector.countIntention(ErlangUsageCollector.class);
    myCollector.countQuickFix(FakeSender.class);
    myCollector.flush();
    assertEquals(Map.of("intention:ErlangUsageCollector", 1L, "quick_fix:FakeSender", 1L), myCollector.getState().days.get(0).counters);
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
    myCollector.count(ErlangUsageKey.RENAME);
    myToday = DAY.plusDays(1);
    assertEquals(1, myCollector.sendPending());
    assertEquals("8f1c2b0e-1111-4222-8333-444455556666",
                 events(mySender.bodies.get(0)).get(0).getAsJsonObject().get("device_id").getAsString());
  }

  public void testCountsBeforeConsentAreNeverKept() {
    myCollector.count(ErlangUsageKey.RENAME);
    myCollector.countQuickFix(FakeSender.class);
    assertEquals(0, myCollector.countsInMemory());
    myCollector.setEnabled(true);
    myCollector.flush();
    assertTrue(myCollector.getState().days.isEmpty());
    myToday = DAY.plusDays(1);
    assertEquals(0, myCollector.sendPending());
    assertTrue(mySender.bodies.isEmpty());
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
    myToday = DAY.plusDays(1);
    assertEquals(0, collector.sendPending());
    assertTrue(mySender.bodies.isEmpty());
  }

  // review point 2: the settings cannot store a consent or make an id while the environment switches the statistics off
  public void testSwitchedOffEnvironmentStoresNoConsent() {
    ErlangUsageCollector collector = newCollector(() -> false);
    collector.setEnabled(true);
    assertEquals(ErlangUsageCollector.Consent.UNSET, collector.getConsent());
    assertNull(collector.getInstallationId());
    assertEquals("UNSET", collector.getState().consent);
  }

  // review points 2 and 3: a stored yes gets no new id, and the days not sent yet are deleted
  public void testStateLoadedInASwitchedOffEnvironment() {
    ErlangUsageCollector collector = newCollector(() -> false);
    ErlangUsageStatisticsState state = new ErlangUsageStatisticsState();
    state.consent = "YES";
    ErlangUsageStatisticsState.Day day = new ErlangUsageStatisticsState.Day();
    day.date = DAY.minusDays(1).toString();
    day.counters.put("RENAME", 3L);
    state.days.add(day);
    collector.loadState(state);
    assertNull(collector.getInstallationId());
    assertTrue(collector.getState().days.isEmpty());
    // the answer itself stays for a later session that allows the statistics
    assertEquals("YES", collector.getState().consent);
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

  public void testOpenedConfigFilesAloneDoNotMakeADayUsed() {
    myCollector.setEnabled(true);
    myCollector.count(ErlangUsageKey.OPENED_CONFIG);
    myCollector.count(ErlangUsageKey.OPENED_APP);
    myToday = DAY.plusDays(1);
    assertEquals(0, myCollector.sendPending());
    assertTrue(mySender.bodies.isEmpty());
    myCollector.count(ErlangUsageKey.OPENED_CONFIG);
    myCollector.count(ErlangUsageKey.OPENED_ERL);
    myToday = DAY.plusDays(2);
    assertEquals(1, myCollector.sendPending());
  }

  public void testRefusedDayIsDropped() {
    myCollector.setEnabled(true);
    myCollector.count(ErlangUsageKey.RENAME);
    myToday = DAY.plusDays(1);
    mySender.status = 400;
    assertEquals(0, myCollector.sendPending());
    assertTrue(myCollector.getState().days.isEmpty());
    assertEquals(0, myCollector.sendPending());
    assertEquals(1, mySender.bodies.size());
  }

  public void testTooLargeRequestIsSplit() {
    myCollector.setEnabled(true);
    for (int i = 0; i < 4; i++) {
      myToday = DAY.plusDays(i);
      myCollector.count(ErlangUsageKey.RENAME);
    }
    myToday = DAY.plusDays(4);
    ErlangUsageSender sender = json -> {
      mySender.bodies.add(json);
      return events(json).size() > 1 ? 413 : 200;
    };
    myCollector.setSender(sender);
    assertEquals(4, myCollector.sendPending());
    // 4 days, then 2 and 2, then 1, 1, 1 and 1
    assertEquals(7, mySender.bodies.size());
    assertTrue(myCollector.getState().days.isEmpty());
  }

  public void testTurningOffDuringASendStopsTheSplitRequests() {
    myCollector.setEnabled(true);
    for (int i = 0; i < 2; i++) {
      myToday = DAY.plusDays(i);
      myCollector.count(ErlangUsageKey.RENAME);
    }
    myToday = DAY.plusDays(2);
    // the user turns the statistics off while the first request is on its way, and the service answers 413
    myCollector.setSender(json -> {
      mySender.bodies.add(json);
      myCollector.setEnabled(false);
      return 413;
    });
    assertEquals(0, myCollector.sendPending());
    assertEquals(1, mySender.bodies.size());
    assertTrue(myCollector.getState().days.isEmpty());
  }

  public void testFailuresBackOffUntilTheNextDay() {
    myCollector.setEnabled(true);
    myCollector.count(ErlangUsageKey.RENAME);
    myToday = DAY.plusDays(1);
    for (int status : new int[]{429, 500, 503}) {
      mySender.status = status;
      for (int tick = 0; tick < 32; tick++) {
        myCollector.sendPending();
      }
    }
    assertEquals(ErlangUsageCollector.MAX_FAILED_SENDS_PER_DAY, mySender.bodies.size());
    assertEquals(1, myCollector.getState().days.size());
    myToday = DAY.plusDays(2);
    mySender.status = 200;
    assertEquals(1, myCollector.sendPending());
    assertEquals(ErlangUsageCollector.MAX_FAILED_SENDS_PER_DAY + 1, mySender.bodies.size());
  }

  // the day changes back and forth while several threads count: no count may land in a batch that was already moved
  public void testNoCountIsLostAtTheDayChange() throws Exception {
    AtomicLong calls = new AtomicLong();
    ErlangUsageCollector collector = new ErlangUsageCollector(() -> DAY.plusDays(calls.incrementAndGet() / 500 % 2),
                                                              ErlangUsageCollectorTest::appProperties, () -> "Germany", () -> true, mySender);
    collector.setEnabled(true);
    int threads = 4;
    int counts = 100_000;
    ExecutorService executor = Executors.newFixedThreadPool(threads);
    try {
      List<Future<?>> futures = new ArrayList<>();
      for (int t = 0; t < threads; t++) {
        futures.add(executor.submit(() -> {
          for (int i = 0; i < counts; i++) collector.count(ErlangUsageKey.RENAME);
        }));
      }
      for (Future<?> future : futures) future.get(1, TimeUnit.MINUTES);
    }
    finally {
      executor.shutdownNow();
    }
    collector.flush();
    long stored = collector.getState().days.stream().mapToLong(day -> day.counters.getOrDefault("RENAME", 0L)).sum();
    assertEquals((long)threads * counts, stored);
  }

  public void testCountryIsThatOfTheDay() {
    String[] country = {"Germany"};
    ErlangUsageCollector collector =
      new ErlangUsageCollector(() -> myToday, ErlangUsageCollectorTest::appProperties, () -> country[0], () -> true, mySender);
    collector.setEnabled(true);
    collector.count(ErlangUsageKey.RENAME);
    country[0] = "Japan";
    myToday = DAY.plusDays(1);
    assertEquals(1, collector.sendPending());
    assertEquals("Germany", events(mySender.bodies.get(0)).get(0).getAsJsonObject().get("country").getAsString());
  }

  @NotNull
  static JsonArray events(@NotNull String body) {
    return JsonParser.parseString(body).getAsJsonObject().getAsJsonArray("events");
  }

  static final class FakeSender implements ErlangUsageSender {
    final List<String> bodies = new ArrayList<>();
    int status = 200;
    boolean fail;

    @Override
    public int send(@NotNull String json) throws IOException {
      bodies.add(json);
      if (fail) throw new IOException("offline");
      return status;
    }
  }
}
