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

import com.intellij.ide.plugins.cl.PluginAwareClassLoader;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.time.LocalDate;
import java.time.format.DateTimeParseException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.BooleanSupplier;
import java.util.function.Supplier;

/**
 * The consent, the counters and the daily send of the usage statistics, without the IDE around them.
 * <p>
 * Counters live in memory and move to the stored state on {@link #flush()}. Nothing is counted or stored unless the
 * user said yes and the environment allows statistics. Once a day, the summaries of the previous days (at most
 * {@link #MAX_DAYS}) are sent, one event per day, and are removed when the service accepted or refused them. A day
 * without any counted use is never sent.
 */
public final class ErlangUsageCollector {
  private static final Logger LOG = Logger.getInstance(ErlangUsageCollector.class);
  public static final String PLUGIN_ID = "org.jetbrains.erlang";
  // the first ask and one reminder
  public static final int MAX_ASKS = 2;
  // "Ask me later", Esc or a dialog closed without an answer asks again after this many days
  public static final int ASK_AGAIN_DAYS = 30;
  public static final int MAX_DAYS = 7;
  // after this many failed sends on one day, the next try is on the next day
  public static final int MAX_FAILED_SENDS_PER_DAY = 3;
  // counters that do not make a day "used": these file types also match files of other tools, for example web.config
  private static final Set<String> NOT_ACTIVITY = Set.of(ErlangUsageKey.OPENED_CONFIG.name(), ErlangUsageKey.OPENED_APP.name());
  private static final int HTTP_TOO_LARGE = 413;

  public enum Consent {UNSET, YES, NO}

  private record Batch(@NotNull LocalDate date, @Nullable String country, @NotNull Map<String, AtomicLong> counts) {
  }

  private final Supplier<LocalDate> myToday;
  private final Supplier<Map<ErlangUsageProperty, String>> myAppProperties;
  private final Supplier<String> myCountry;
  private final BooleanSupplier myAllowed;
  private volatile ErlangUsageSender mySender;
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
                              @NotNull BooleanSupplier allowed,
                              @NotNull ErlangUsageSender sender) {
    myToday = today;
    myAppProperties = appProperties;
    myCountry = country;
    myAllowed = allowed;
    mySender = sender;
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
      // an environment that switches the statistics off stores no consent and makes no id
      if (!myAllowed.getAsBoolean()) return;
      myState.consent = Consent.YES.name();
      if (myState.installationId == null) myState.installationId = UUID.randomUUID().toString();
    }
    else {
      myState.consent = Consent.NO.name();
      myState.installationId = null;
      myState.days.clear();
      myState.failedSendDate = null;
      myState.failedSends = 0;
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

  public void countQuickFix(@NotNull Class<?> fixClass) {
    String name = pluginClassName(fixClass);
    if (name != null) add(ErlangUsagePayload.QUICK_FIX_PREFIX + name);
  }

  public void countIntention(@NotNull Class<?> intentionClass) {
    String name = pluginClassName(intentionClass);
    if (name != null) add(ErlangUsagePayload.INTENTION_PREFIX + name);
  }

  // the simple name of a named class of this plugin, or null for any other class
  @Nullable
  static String pluginClassName(@NotNull Class<?> aClass) {
    if (!isPluginClass(aClass) || aClass.isAnonymousClass() || aClass.isLocalClass()) return null;
    String name = aClass.getSimpleName();
    return ErlangUsagePayload.CLASS_NAME.matcher(name).matches() ? name : null;
  }

  // the plugin class loader of the IDE decides; without it (tests, a run from sources) the class must come from the
  // class loader of this plugin and from its package
  private static boolean isPluginClass(@NotNull Class<?> aClass) {
    ClassLoader loader = aClass.getClassLoader();
    if (loader instanceof PluginAwareClassLoader pluginLoader) {
      return PLUGIN_ID.equals(pluginLoader.getPluginDescriptor().getPluginId().getIdString());
    }
    return loader != null && loader == ErlangUsageCollector.class.getClassLoader() &&
           aClass.getName().startsWith("org.intellij.erlang.");
  }

  // under the lock of newBatch and flush, so that a count never lands in a batch that was already moved to the state,
  // and never in the batch of a later consent
  private synchronized void add(@NotNull String key) {
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

  // a day counts as used when it has a counter other than the files opened that may not be Erlang ones
  static boolean hasActivity(@NotNull ErlangUsageStatisticsState.Day day) {
    for (Map.Entry<String, Long> entry : day.counters.entrySet()) {
      if (!NOT_ACTIVITY.contains(entry.getKey()) && entry.getValue() != null && entry.getValue() > 0) return true;
    }
    return false;
  }

  // sending

  /**
   * Sends the summaries of the days before today. A day that the service refused as invalid (HTTP 400) is dropped;
   * a request that is too large (HTTP 413) is split. Any other failure keeps the days; after
   * {@link #MAX_FAILED_SENDS_PER_DAY} failures the next try is on the next day.
   *
   * @return the number of days the service accepted
   */
  @RequiresBackgroundThread
  public int sendPending() {
    String id;
    List<ErlangUsageStatisticsState.Day> pending;
    LocalDate today = myToday.get();
    synchronized (this) {
      flushLocked();
      id = myState.installationId;
      if (!isCounting() || id == null) return 0;
      if (today.toString().equals(myState.failedSendDate) && myState.failedSends >= MAX_FAILED_SENDS_PER_DAY) return 0;
      myState.days.removeIf(day -> {
        LocalDate date = parse(day.date);
        return date == null || date.isBefore(today) && !hasActivity(day);
      });
      pending = new ArrayList<>();
      for (ErlangUsageStatisticsState.Day day : myState.days) {
        LocalDate date = parse(day.date);
        if (date != null && date.isBefore(today)) pending.add(copy(day));
      }
    }
    pending.sort(Comparator.comparing(day -> day.date));
    Deque<List<ErlangUsageStatisticsState.Day>> requests = new ArrayDeque<>();
    for (int from = 0; from < pending.size(); from += ErlangUsagePayload.MAX_EVENTS_PER_REQUEST) {
      requests.add(pending.subList(from, Math.min(pending.size(), from + ErlangUsagePayload.MAX_EVENTS_PER_REQUEST)));
    }
    int sent = 0;
    while (!requests.isEmpty()) {
      List<ErlangUsageStatisticsState.Day> chunk = requests.poll();
      // the user may have turned the statistics off, or off and on again, while the previous request was on its way
      synchronized (this) {
        if (!isCounting() || !id.equals(myState.installationId)) break;
      }
      int status;
      try {
        status = mySender.send(ErlangUsagePayload.build(id, chunk));
      }
      catch (IOException e) {
        LOG.debug("Cannot send the Erlang usage statistics", e);
        status = -1;
      }
      if (status == HTTP_TOO_LARGE && chunk.size() > 1) {
        int half = chunk.size() / 2;
        requests.addFirst(chunk.subList(half, chunk.size()));
        requests.addFirst(chunk.subList(0, half));
        continue;
      }
      boolean accepted = status == HttpURLConnection.HTTP_OK;
      boolean refused = status == HttpURLConnection.HTTP_BAD_REQUEST || status == HTTP_TOO_LARGE;
      if (refused) LOG.debug("The statistics service refused " + chunk.size() + " day(s) with HTTP " + status);
      synchronized (this) {
        if (!id.equals(myState.installationId)) break;
        if (!accepted && !refused) {
          if (!today.toString().equals(myState.failedSendDate)) myState.failedSends = 0;
          myState.failedSendDate = today.toString();
          myState.failedSends++;
          break;
        }
        Set<String> dates = new HashSet<>();
        for (ErlangUsageStatisticsState.Day day : chunk) {
          dates.add(day.date);
        }
        myState.days.removeIf(day -> dates.contains(day.date));
      }
      if (accepted) sent += chunk.size();
    }
    return sent;
  }

  /**
   * The JSON that the summary of today would be sent as, for the settings page. It stores nothing.
   */
  @NotNull
  public synchronized String previewToday() {
    flushLocked();
    String today = myToday.get().toString();
    ErlangUsageStatisticsState.Day preview = null;
    for (ErlangUsageStatisticsState.Day day : myState.days) {
      if (today.equals(day.date)) preview = copy(day);
    }
    if (preview == null) preview = newDay(today, myCountry.get());
    String id = myState.installationId != null ? myState.installationId : ErlangUsagePayload.EXAMPLE_ID;
    return ErlangUsagePayload.build(id, List.of(preview));
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
    copy.failedSendDate = myState.failedSendDate;
    copy.failedSends = myState.failedSends;
    for (ErlangUsageStatisticsState.Day day : myState.days) {
      copy.days.add(copy(day));
    }
    return copy;
  }

  public synchronized void loadState(@NotNull ErlangUsageStatisticsState state) {
    myState = state;
    myBatch = null;
    boolean yes = storedConsent() == Consent.YES;
    boolean allowed = myAllowed.getAsBoolean();
    // in a session that switches the statistics off, the days not sent yet are deleted, so that a later session does
    // not send them; a stored yes stays, but gets no new id
    if (!yes || !allowed) state.days.clear();
    if (!yes) {
      state.installationId = null;
    }
    else if (state.installationId == null && allowed) {
      state.installationId = UUID.randomUUID().toString();
    }
    myConsented = yes;
  }

  @TestOnly
  public void setSender(@NotNull ErlangUsageSender sender) {
    mySender = sender;
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
