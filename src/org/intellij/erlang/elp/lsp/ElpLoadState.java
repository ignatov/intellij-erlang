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

package org.intellij.erlang.elp.lsp;

import com.intellij.util.concurrency.AppExecutorUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

/**
 * Whether one ELP server process has loaded its project. The LSP client is {@code Running} about 0.01 s after the start, but ELP
 * answers only "elp is still loading", or nothing while it is stalled, until it has loaded the project: measured 1.5–2 s on a small
 * project, 67–78 s on the first open of an umbrella whose dependencies are fetched and built, and forever when it cannot find
 * {@code erl}. So ELP serves a file only in {@link State#RUNNING}, and the plugin answers until then.
 * <p>
 * The client advertises {@code experimental.serverStatusNotification}, and ELP sends {@code elp/status} with {@code Loading}, then
 * {@code Running}; {@code ShuttingDown} on shutdown. ELP has no status for a failed load: its loading progress ends normally and
 * {@code Loading}/{@code Running} never come; the only signal is a {@code window/showMessage} of type Error ("No such file or
 * directory (os error 2)" without {@code erl}). {@link State#FAILED} is therefore derived:
 * <ul>
 *   <li>an Error message before {@code Running}, and then no loading progress for {@link #ERROR_GRACE_MS}: without rebar3 ELP
 *   shows such a message and still reaches {@code Running} 30 ms later, on a fallback project;</li>
 *   <li>no {@code Running} and no loading progress for {@link #IDLE_TIMEOUT_MS}: a load that fetches and builds dependencies
 *   reports progress all the time (up to 77 s measured), so it is never cut short while it reports;</li>
 *   <li>no {@code Running} within {@link #HARD_TIMEOUT_MS} of the start, even with progress.</li>
 * </ul>
 * {@code FAILED} is not final: when {@code Running} comes later, ELP takes over.
 */
final class ElpLoadState {
  enum State {
    /** The process is started; the client has not initialized it yet. */
    STARTING,
    /** Initialized; ELP loads the project, or reloads it after a change. */
    LOADING,
    /** ELP has loaded the project and answers. */
    RUNNING,
    /** ELP did not load the project, see the class comment. */
    FAILED,
    /** The process is shutting down or has ended. */
    STOPPED
  }

  /** Told about every change of the state, outside the lock. */
  interface Listener {
    void stateChanged(@NotNull ElpLoadState loadState, @NotNull State oldState, @NotNull State newState);
  }

  static final long ERROR_GRACE_MS = 10_000;
  static final long IDLE_TIMEOUT_MS = 60_000;
  static final long HARD_TIMEOUT_MS = 300_000;
  private static final int LOG_LINES = 20;
  private static final String OTP_NOT_FOUND = "Could not find OTP";

  private final Listener myListener;
  private State myState = State.STARTING;
  private final List<State> myHistory = new ArrayList<>(List.of(State.STARTING));
  private long myStartTime;
  private long myLastProgressTime;
  private final Set<String> myActiveProgress = new HashSet<>();
  private boolean myEverRunning;
  private String myFirstError;
  private boolean myOtpNotFound;
  private String myErrorToReport;
  private boolean myErrorReported;
  private final Deque<String> myLog = new ArrayDeque<>();
  private ScheduledFuture<?> myWatchdog;

  ElpLoadState(@NotNull Listener listener) {
    myListener = listener;
  }

  /** A new server process: back to {@link State#STARTING}, and the watchdog checks the load once a second. */
  void started(long now) {
    synchronized (this) {
      cancelWatchdog();
      myStartTime = now;
      myLastProgressTime = now;
      myActiveProgress.clear();
      myEverRunning = false;
      myFirstError = null;
      myOtpNotFound = false;
      myErrorToReport = null;
      myErrorReported = false;
      myLog.clear();
      myWatchdog = AppExecutorUtil.getAppScheduledExecutorService()
        .scheduleWithFixedDelay(() -> tick(System.currentTimeMillis()), 1, 1, TimeUnit.SECONDS);
    }
    moveTo(State.STARTING);
  }

  void initialized() {
    State state = state();
    if (state == State.STARTING) moveTo(State.LOADING);
  }

  /** {@code elp/status}. */
  void status(@Nullable String status, long now) {
    if ("Running".equals(status)) {
      synchronized (this) {
        myEverRunning = true;
        cancelWatchdog();
      }
      moveTo(State.RUNNING);
    }
    else if ("Loading".equals(status)) {
      if (state() != State.STOPPED) moveTo(State.LOADING);
      synchronized (this) {
        myLastProgressTime = now;
      }
    }
    else if ("ShuttingDown".equals(status)) {
      moveTo(State.STOPPED);
    }
  }

  /** {@code $/progress}: {@code kind} is {@code begin}, {@code report} or {@code end}. */
  synchronized void progress(@NotNull String token, @Nullable String kind, long now) {
    myLastProgressTime = now;
    if ("end".equals(kind)) myActiveProgress.remove(token);
    else myActiveProgress.add(token);
  }

  /**
   * {@code window/showMessage} and {@code window/logMessage}: an Error message before {@code Running} may mean a failed load; every
   * message is kept for the text of the failure.
   *
   * @param type the LSP message type: 1 Error, 2 Warning, 3 Info, 4 Log
   */
  synchronized void message(int type, @NotNull String message, boolean shown) {
    log(message);
    if (message.contains(OTP_NOT_FOUND)) myOtpNotFound = true;
    if (shown && type == 1 && myFirstError == null && !myEverRunning) myFirstError = message.trim();
    if (type == 1 && myErrorToReport == null) myErrorToReport = message.trim();
  }

  /**
   * @return the first Error message of the session, once, after ELP is running; ELP sends the real failures of a running server,
   * such as "Failed to set up project ... for parsing", only as log messages
   */
  @Nullable
  synchronized String takeErrorToReport() {
    if (myErrorReported || !myEverRunning || myErrorToReport == null) return null;
    myErrorReported = true;
    return myErrorToReport;
  }

  /** A line of the server's standard error. */
  synchronized void log(@NotNull String line) {
    String trimmed = line.strip();
    if (trimmed.isEmpty()) return;
    if (myLog.size() == LOG_LINES) myLog.removeFirst();
    myLog.addLast(trimmed);
  }

  void stopped() {
    synchronized (this) {
      cancelWatchdog();
    }
    moveTo(State.STOPPED);
  }

  /** Decides whether a load that has not reached {@code Running} has failed, see the class comment. */
  void tick(long now) {
    boolean failed;
    synchronized (this) {
      if (myState != State.STARTING && myState != State.LOADING || myEverRunning) return;
      boolean quiet = myActiveProgress.isEmpty();
      long sinceProgress = now - myLastProgressTime;
      failed = myFirstError != null && quiet && sinceProgress >= ERROR_GRACE_MS ||
               quiet && sinceProgress >= IDLE_TIMEOUT_MS ||
               now - myStartTime >= HARD_TIMEOUT_MS;
      if (failed) cancelWatchdog();
    }
    if (failed) moveTo(State.FAILED);
  }

  synchronized boolean isRunning() {
    return myState == State.RUNNING;
  }

  @NotNull
  synchronized State state() {
    return myState;
  }

  @NotNull
  synchronized List<State> history() {
    return List.copyOf(myHistory);
  }

  /** The first Error message ELP showed before it was running, if any. */
  @Nullable
  synchronized String firstError() {
    return myFirstError;
  }

  /** Whether ELP said that it cannot find Erlang/OTP. */
  synchronized boolean isOtpNotFound() {
    return myOtpNotFound;
  }

  /** The last lines of ELP's messages and standard error, oldest first. */
  @NotNull
  synchronized List<String> lastLogLines() {
    return List.copyOf(myLog);
  }

  /** Puts the state without telling the listener, for a test client that stands in for a loaded ELP. */
  @TestOnly
  synchronized void setStateForTests(@NotNull State state) {
    myState = state;
    myHistory.add(state);
    if (state == State.RUNNING) myEverRunning = true;
  }

  private void moveTo(@NotNull State newState) {
    State oldState;
    synchronized (this) {
      oldState = myState;
      if (oldState == newState) return;
      myState = newState;
      myHistory.add(newState);
    }
    myListener.stateChanged(this, oldState, newState);
  }

  private void cancelWatchdog() {
    if (myWatchdog != null) {
      myWatchdog.cancel(false);
      myWatchdog = null;
    }
  }
}
