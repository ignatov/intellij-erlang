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

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.time.LocalDate;
import java.time.format.DateTimeParseException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.BooleanSupplier;
import java.util.function.Supplier;

/**
 * The consent and the counters of the usage statistics, without the IDE around them.
 * <p>
 * Counters live in memory and move to the stored state on {@link #flush()}. Nothing is counted or stored unless the
 * user said yes and the environment allows statistics. The stored state keeps at most the last {@link #MAX_DAYS} days.
 */
public final class ErlangUsageCollector {
  // the first ask and two reminders
  public static final int MAX_ASKS = 3;
  // "Ask me later", Esc or a dialog closed without an answer asks again after this many days
  public static final int ASK_AGAIN_DAYS = 30;
  public static final int MAX_DAYS = 7;

  public enum Consent {UNSET, YES, NO}

  private record Batch(@NotNull LocalDate date, @Nullable String country, @NotNull Map<String, AtomicLong> counts) {
  }

  private final Supplier<LocalDate> myToday;
  private final Supplier<Map<ErlangUsageProperty, String>> myAppProperties;
  private final Supplier<String> myCountry;
  private final BooleanSupplier myAllowed;
  // guarded by this
  private ErlangUsageStatisticsState myState = new ErlangUsageStatisticsState();
  // the consent is yes
  private volatile boolean myConsented;
  private volatile Batch myBatch;

  /**
   * @param allowed false when the environment switches the statistics off; then nothing is counted or sent
   */
  public ErlangUsageCollector(@NotNull Supplier<LocalDate> today,
                              @NotNull Supplier<Map<ErlangUsageProperty, String>> appProperties,
                              @NotNull Supplier<String> country,
                              @NotNull BooleanSupplier allowed) {
    myToday = today;
    myAppProperties = appProperties;
    myCountry = country;
    myAllowed = allowed;
  }

  // consent

  @NotNull
  public synchronized Consent getConsent() {
    Consent consent = storedConsent();
    return consent == Consent.UNSET && myState.askCount >= MAX_ASKS ? Consent.NO : consent;
  }

  public synchronized boolean shouldAsk() {
    if (storedConsent() != Consent.UNSET || myState.askCount >= MAX_ASKS) return false;
    LocalDate lastAsk = parse(myState.lastAskDate);
    return lastAsk == null || !myToday.get().isBefore(lastAsk.plusDays(ASK_AGAIN_DAYS));
  }

  // true when the plugin asked before and got no answer, so the next ask is a reminder
  public synchronized boolean isReminder() {
    return myState.askCount > 0;
  }

  public synchronized void markAsked() {
    myState.askCount++;
    myState.lastAskDate = myToday.get().toString();
  }

  public synchronized void setEnabled(boolean enabled) {
    if (enabled) {
      myState.consent = Consent.YES.name();
      if (myState.installationId == null) myState.installationId = UUID.randomUUID().toString();
    }
    else {
      myState.consent = Consent.NO.name();
      myState.installationId = null;
      myState.days.clear();
    }
    // counts never move from before a change of the consent to after it
    myBatch = null;
    myConsented = enabled;
  }

  public boolean isCounting() {
    return myConsented && myAllowed.getAsBoolean();
  }

  @Nullable
  public synchronized String getInstallationId() {
    return myState.installationId;
  }

  // counting

  public void count(@NotNull ErlangUsageKey key) {
    add(key.name());
  }

  private void add(@NotNull String key) {
    if (!isCounting()) return;
    LocalDate today = myToday.get();
    Batch batch = myBatch;
    if (batch == null || !batch.date().equals(today)) batch = newBatch(today);
    if (batch != null) batch.counts().computeIfAbsent(key, k -> new AtomicLong()).incrementAndGet();
  }

  @Nullable
  private synchronized Batch newBatch(@NotNull LocalDate today) {
    if (!isCounting()) return null;
    Batch batch = myBatch;
    if (batch != null && batch.date().equals(today)) return batch;
    flushLocked();
    batch = new Batch(today, myCountry.get(), new ConcurrentHashMap<>());
    myBatch = batch;
    return batch;
  }

  @TestOnly
  long countsInMemory() {
    Batch batch = myBatch;
    return batch == null ? 0 : batch.counts().values().stream().mapToLong(AtomicLong::get).sum();
  }

  /**
   * Sets a property of today. A LIST property adds the value to the set of the day; a BOOLEAN one keeps true once set.
   */
  public synchronized void setProperty(@NotNull ErlangUsageProperty property, @NotNull String value) {
    if (!isCounting() || !property.accepts(value)) return;
    Map<String, String> properties = dayLocked(myToday.get(), myCountry.get()).properties;
    String old = properties.get(property.name());
    String merged = switch (property.kind()) {
      case TEXT, NUMBER -> value;
      case BOOLEAN -> "true".equals(old) ? old : value;
      case LIST -> {
        Set<String> values = new TreeSet<>(old != null ? split(old) : List.of());
        if (values.size() < ErlangUsageProperty.MAX_LIST_SIZE) values.add(value);
        yield String.join(",", values);
      }
    };
    properties.put(property.name(), merged);
  }

  // moves the counters in memory to the stored state and drops the days that are too old to be sent
  public synchronized void flush() {
    flushLocked();
  }

  private void flushLocked() {
    Batch batch = myBatch;
    if (batch != null) {
      ErlangUsageStatisticsState.Day day = null;
      for (Map.Entry<String, AtomicLong> entry : batch.counts().entrySet()) {
        long count = entry.getValue().getAndSet(0);
        if (count <= 0) continue;
        if (day == null) day = dayLocked(batch.date(), batch.country());
        day.counters.merge(entry.getKey(), count, Long::sum);
      }
    }
    LocalDate oldest = myToday.get().minusDays(MAX_DAYS);
    myState.days.removeIf(day -> {
      LocalDate date = parse(day.date);
      return date == null || date.isBefore(oldest);
    });
  }

  @NotNull
  private ErlangUsageStatisticsState.Day dayLocked(@NotNull LocalDate date, @Nullable String country) {
    String text = date.toString();
    for (ErlangUsageStatisticsState.Day day : myState.days) {
      if (text.equals(day.date)) return day;
    }
    ErlangUsageStatisticsState.Day day = newDay(text, country);
    myState.days.add(day);
    return day;
  }

  @NotNull
  private ErlangUsageStatisticsState.Day newDay(@NotNull String date, @Nullable String country) {
    ErlangUsageStatisticsState.Day day = new ErlangUsageStatisticsState.Day();
    day.date = date;
    day.country = country;
    myAppProperties.get().forEach((property, value) -> {
      if (property.accepts(value)) day.properties.put(property.name(), value);
    });
    return day;
  }

  // state

  // a copy, so that the IDE can write it to disk while the counters change
  @NotNull
  public synchronized ErlangUsageStatisticsState getState() {
    flushLocked();
    ErlangUsageStatisticsState copy = new ErlangUsageStatisticsState();
    copy.consent = myState.consent;
    copy.askCount = myState.askCount;
    copy.lastAskDate = myState.lastAskDate;
    copy.installationId = myState.installationId;
    for (ErlangUsageStatisticsState.Day day : myState.days) {
      copy.days.add(copy(day));
    }
    return copy;
  }

  public synchronized void loadState(@NotNull ErlangUsageStatisticsState state) {
    myState = state;
    myBatch = null;
    boolean yes = storedConsent() == Consent.YES;
    if (!yes) {
      state.installationId = null;
      state.days.clear();
    }
    else if (state.installationId == null) {
      state.installationId = UUID.randomUUID().toString();
    }
    myConsented = yes;
  }

  @NotNull
  private Consent storedConsent() {
    try {
      return Consent.valueOf(myState.consent);
    }
    catch (IllegalArgumentException | NullPointerException e) {
      return Consent.UNSET;
    }
  }

  @NotNull
  static List<String> split(@NotNull String list) {
    List<String> result = new ArrayList<>();
    for (String item : list.split(",")) {
      if (!item.isEmpty()) result.add(item);
    }
    return result;
  }

  @Nullable
  private static LocalDate parse(@Nullable String date) {
    if (date == null) return null;
    try {
      return LocalDate.parse(date);
    }
    catch (DateTimeParseException e) {
      return null;
    }
  }

  @NotNull
  private static ErlangUsageStatisticsState.Day copy(@NotNull ErlangUsageStatisticsState.Day day) {
    ErlangUsageStatisticsState.Day copy = new ErlangUsageStatisticsState.Day();
    copy.date = day.date;
    copy.country = day.country;
    copy.counters = new TreeMap<>(day.counters);
    copy.properties = new TreeMap<>(day.properties);
    return copy;
  }
}
