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

package org.intellij.erlang.update;

import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.util.text.StringUtil;
import org.intellij.erlang.statistics.ErlangUsageKey;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.function.LongSupplier;
import java.util.regex.Pattern;

/**
 * The decisions of the notification about a new version of the plugin: when to ask the plugin repository, when to
 * stay silent, and what to show. The IDE facts, the clock and the user interface are given to it, so it has no IDE
 * state of its own; {@link ErlangUpdateNoticeService} connects it to the IDE.
 */
public final class ErlangUpdateNotice {
  public static final String TITLE = "A new version of the Erlang plugin is out";
  public static final String INSTALLED_TITLE = "The Erlang plugin is updated";
  public static final String NOT_INSTALLED_TITLE = "The Erlang plugin is not updated";
  public static final String SWITCH_PROPERTY = "erlang.update.notification";
  // at most one question to the plugin repository a day
  static final long CHECK_INTERVAL_MS = TimeUnit.HOURS.toMillis(24);
  // at most one notification a week: releases come out almost every day
  static final long SHOW_INTERVAL_MS = TimeUnit.DAYS.toMillis(7);
  static final int WHATS_NEW_LINES = 2;
  static final int WHATS_NEW_LENGTH = 80;
  // the tags that start or end a line of the change notes
  private static final Pattern LINE_BREAK = Pattern.compile("(?i)<\\s*/?\\s*(li|br|p|ul|ol|div|h\\d)\\b[^>]*>|\\r?\\n");
  private static final Pattern TAG = Pattern.compile("<[^>]*>");
  private static final Pattern SPACES = Pattern.compile("\\s+");
  // the issue or pull request number at the end of a line, "(#1401)"
  private static final Pattern ISSUE_SUFFIX = Pattern.compile("\\s*\\(\\s*#\\d+\\s*\\)$");

  /**
   * What the decisions need to know about the IDE.
   */
  interface Platform {
    // false in tests, in headless mode, in local builds and with -Derlang.update.notification=false
    boolean isAllowedEnvironment();

    // the IDE setting "Update plugins automatically"
    boolean isAutoUpdateEnabled();

    // the IDE checks for plugin updates at all
    boolean isPluginCheckEnabled();

    // a new version of the plugin is already installed and waits for a restart
    boolean isUpdatePending();

    boolean isRestartCapable();

    // restarts the IDE; with exitConfirmed it does not ask "Are you sure you want to exit?"
    void restart(boolean exitConfirmed);

    // asks the plugin repository, on a background thread; null when there is no update or the question failed
    @Nullable
    Update findUpdate();
  }

  /**
   * The notifications and the usage counters of one project, or of no project.
   */
  interface Ui {
    // the notification with the actions Install and Don't show again
    void offer(@NotNull Update update);

    // the update is installed; with the action Restart when offerRestart is true
    void installed(@NotNull Update update, boolean offerRestart);

    // the update is not installed: it failed or the user cancelled it
    void notInstalled(@NotNull Update update, @NotNull String contentHtml);

    void count(@NotNull ErlangUsageKey key);
  }

  /**
   * Downloads, checks and installs the new version; it is applied when the IDE restarts. Runs on a background thread.
   */
  interface Installer {
    @NotNull
    InstallResult install(@Nullable ProgressIndicator indicator);
  }

  /**
   * @param reasonHtml why the update was not installed, as notification HTML; null when installed or when unknown
   */
  record InstallResult(boolean installed, @Nullable String reasonHtml) {
    static final InstallResult INSTALLED = new InstallResult(true, null);

    @NotNull
    static InstallResult failed(@Nullable String reasonHtml) {
      return new InstallResult(false, reasonHtml);
    }
  }

  /**
   * A new version that the plugin repository offers.
   *
   * @param whatsNew at most {@link #WHATS_NEW_LINES} short lines of plain text from the change notes
   */
  record Update(@NotNull String version, @NotNull String currentVersion, @NotNull List<String> whatsNew, @NotNull Installer installer) {
  }

  private final LongSupplier myClock;
  private final Platform myPlatform;
  private volatile ErlangUpdateNoticeState myState = new ErlangUpdateNoticeState();

  ErlangUpdateNotice(@NotNull LongSupplier clock, @NotNull Platform platform) {
    myClock = clock;
    myPlatform = platform;
  }

  @NotNull
  ErlangUpdateNoticeState getState() {
    return myState;
  }

  synchronized void loadState(@NotNull ErlangUpdateNoticeState state) {
    myState = state;
  }

  /**
   * Whether it is time to ask the plugin repository. Cheap and without network, on any thread.
   */
  synchronized boolean isCheckDue() {
    long now = myClock.getAsLong();
    return !isRecent(myState.lastCheck, now, CHECK_INTERVAL_MS) && mayShow(now);
  }

  /**
   * Asks the plugin repository when it is time to, and shows the notification when it finds a new version and may
   * show it. Runs on a background thread. A question that fails counts as a question, so the plugin asks again only a
   * day later.
   */
  void check(@NotNull Ui ui) {
    synchronized (this) {
      if (!isCheckDue()) return;
      myState.lastCheck = myClock.getAsLong();
    }
    Update update = myPlatform.findUpdate();
    if (update == null) return;
    synchronized (this) {
      // the user may have installed the update or changed a setting while the question ran
      long now = myClock.getAsLong();
      if (!mayShow(now)) return;
      myState.lastShown = now;
    }
    ui.offer(update);
    ui.count(ErlangUsageKey.UPDATE_NOTICE_SHOWN);
  }

  // the silence rules, except the one for the last question
  private boolean mayShow(long now) {
    if (!myPlatform.isAllowedEnvironment()) return false;
    if (myState.dontShowAgain) return false;
    if (isRecent(myState.lastShown, now, SHOW_INTERVAL_MS)) return false;
    if (myPlatform.isAutoUpdateEnabled() || !myPlatform.isPluginCheckEnabled()) return false;
    return !myPlatform.isUpdatePending();
  }

  // a time less than the interval ago; a stored time in the future by more than the interval (a clock that was wrong)
  // does not count
  private static boolean isRecent(long time, long now, long interval) {
    return time > 0 && Math.abs(now - time) < interval;
  }

  synchronized void dontShowAgain(@NotNull Ui ui) {
    myState.dontShowAgain = true;
    ui.count(ErlangUsageKey.UPDATE_NOTICE_DONT_SHOW);
  }

  // the user pressed Install; the install task starts next
  void installPressed(@NotNull Ui ui) {
    ui.count(ErlangUsageKey.UPDATE_NOTICE_INSTALL);
  }

  /**
   * Installs the update and tells the user how it went. Runs on the background thread of the install task.
   */
  void install(@NotNull Update update, @Nullable ProgressIndicator indicator, @NotNull Ui ui) {
    InstallResult result = update.installer().install(indicator);
    if (result.installed()) ui.installed(update, myPlatform.isRestartCapable());
    else ui.notInstalled(update, notInstalledText(update, result.reasonHtml()));
  }

  // the user pressed Restart: the question whether to exit is already answered
  void restartPressed() {
    myPlatform.restart(true);
  }

  void installCancelled(@NotNull Update update, @NotNull Ui ui) {
    ui.notInstalled(update, cancelledText(update));
  }

  // the install task failed with an unexpected error
  void installFailed(@NotNull Update update, @NotNull Ui ui) {
    ui.notInstalled(update, notInstalledText(update, null));
  }

  /**
   * False in tests, in headless mode, in local and CI builds (a SNAPSHOT version, or a version that cannot be read)
   * and with -Derlang.update.notification=false.
   */
  static boolean isAllowedEnvironment(@Nullable String pluginVersion, boolean unitTestMode, boolean headless, @Nullable String switchProperty) {
    if (unitTestMode || headless) return false;
    if ("false".equalsIgnoreCase(switchProperty)) return false;
    return pluginVersion != null && !pluginVersion.contains("SNAPSHOT");
  }

  // HTML: who writes on the first line, the versions on the second, then what is new
  @NotNull
  static String offerText(@NotNull Update update) {
    StringBuilder text = new StringBuilder("Hi, it's Sergey, the author of the Erlang plugin.<br>")
      .append("Version ").append(StringUtil.escapeXmlEntities(update.version()))
      .append(" is available; you have ").append(StringUtil.escapeXmlEntities(update.currentVersion())).append('.');
    for (String line : update.whatsNew()) {
      text.append("<br>• ").append(StringUtil.escapeXmlEntities(line));
    }
    return text.toString();
  }

  @NotNull
  static String installedText(@NotNull Update update) {
    return "Version " + StringUtil.escapeXmlEntities(update.version()) + " is installed. Restart the IDE to use it.";
  }

  @NotNull
  static String notInstalledText(@NotNull Update update, @Nullable String reasonHtml) {
    String text = "Version " + StringUtil.escapeXmlEntities(update.version()) + " was not installed.";
    return StringUtil.isEmptyOrSpaces(reasonHtml) ? text : text + "<br>" + reasonHtml;
  }

  @NotNull
  static String cancelledText(@NotNull Update update) {
    return "The update to version " + StringUtil.escapeXmlEntities(update.version()) + " was cancelled.";
  }

  /**
   * At most {@link #WHATS_NEW_LINES} lines of plain text from the change notes of the new version: the first items,
   * without HTML and without the issue numbers at their end, each cut to {@link #WHATS_NEW_LENGTH} characters.
   */
  @NotNull
  static List<String> whatsNew(@Nullable String changeNotes) {
    List<String> lines = new ArrayList<>();
    if (changeNotes == null) return lines;
    for (String part : LINE_BREAK.split(changeNotes)) {
      String line = TAG.matcher(part).replaceAll("");
      line = StringUtil.unescapeXmlEntities(line).replace(' ', ' ').replace("&nbsp;", " ");
      line = SPACES.matcher(line).replaceAll(" ").trim();
      line = ISSUE_SUFFIX.matcher(line).replaceAll("");
      if (line.isEmpty()) continue;
      if (line.length() > WHATS_NEW_LENGTH) line = line.substring(0, WHATS_NEW_LENGTH - 1).trim() + "…";
      lines.add(line);
      if (lines.size() == WHATS_NEW_LINES) break;
    }
    return lines;
  }
}
