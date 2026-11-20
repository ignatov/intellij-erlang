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

import java.time.LocalDate;
import java.time.format.DateTimeParseException;
import java.util.*;
import java.util.function.BooleanSupplier;
import java.util.function.Supplier;

/**
 * The consent of the usage statistics, without the IDE around it.
 * <p>
 * Nothing is counted or stored unless the user said yes and the environment allows statistics.
 */
public final class ErlangUsageCollector {
  // the first ask and two reminders
  public static final int MAX_ASKS = 3;
  // "Ask me later", Esc or a dialog closed without an answer asks again after this many days
  public static final int ASK_AGAIN_DAYS = 30;

  public enum Consent {UNSET, YES, NO}

  private final Supplier<LocalDate> myToday;
  private final BooleanSupplier myAllowed;
  // guarded by this
  private ErlangUsageStatisticsState myState = new ErlangUsageStatisticsState();
  // the consent is yes
  private volatile boolean myConsented;

  /**
   * @param allowed false when the environment switches the statistics off; then nothing is counted or sent
   */
  public ErlangUsageCollector(@NotNull Supplier<LocalDate> today, @NotNull BooleanSupplier allowed) {
    myToday = today;
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
    }
    myConsented = enabled;
  }

  public boolean isCounting() {
    return myConsented && myAllowed.getAsBoolean();
  }

  @Nullable
  public synchronized String getInstallationId() {
    return myState.installationId;
  }

  // state

  // a copy, so that the IDE can write it to disk while the counters change
  @NotNull
  public synchronized ErlangUsageStatisticsState getState() {
    ErlangUsageStatisticsState copy = new ErlangUsageStatisticsState();
    copy.consent = myState.consent;
    copy.askCount = myState.askCount;
    copy.lastAskDate = myState.lastAskDate;
    copy.installationId = myState.installationId;
    return copy;
  }

  public synchronized void loadState(@NotNull ErlangUsageStatisticsState state) {
    myState = state;
    boolean yes = storedConsent() == Consent.YES;
    if (!yes) {
      state.installationId = null;
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
}
