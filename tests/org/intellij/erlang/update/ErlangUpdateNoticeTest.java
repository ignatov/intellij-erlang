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
import junit.framework.TestCase;
import org.intellij.erlang.statistics.ErlangUsageKey;
import org.intellij.erlang.update.ErlangUpdateNotice.InstallResult;
import org.intellij.erlang.update.ErlangUpdateNotice.Update;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class ErlangUpdateNoticeTest extends TestCase {
  private static final long START = 1_795_000_000_000L;
  private static final long HOUR = TimeUnit.HOURS.toMillis(1);
  private static final long DAY = TimeUnit.DAYS.toMillis(1);

  private long myNow = START;
  private final FakePlatform myPlatform = new FakePlatform();
  private final FakeUi myUi = new FakeUi();
  private final FakeInstaller myInstaller = new FakeInstaller();
  private final ErlangUpdateNotice myNotice = new ErlangUpdateNotice(() -> myNow, myPlatform);

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myPlatform.update = update();
  }

  @NotNull
  private Update update() {
    return new Update("0.11.1500", "0.11.1400", myInstaller);
  }

  public void testFirstCheckShowsTheUpdate() {
    assertTrue(myNotice.isCheckDue());
    myNotice.check(myUi);
    assertEquals(1, myPlatform.questions);
    assertEquals(List.of("offer 0.11.1500"), myUi.events);
    assertEquals(List.of(ErlangUsageKey.UPDATE_NOTICE_SHOWN), myUi.counted);
    assertEquals(START, myNotice.getState().lastCheck);
    assertEquals(START, myNotice.getState().lastShown);
  }

  public void testNoUpdateShowsNothing() {
    myPlatform.update = null;
    myNotice.check(myUi);
    assertEquals(1, myPlatform.questions);
    assertNothingShown();
    assertEquals(START, myNotice.getState().lastCheck);
    assertEquals(0, myNotice.getState().lastShown);
  }

  public void testSilentWhenTheEnvironmentDoesNotAllowIt() {
    myPlatform.allowed = false;
    assertSilent();
  }

  public void testSilentWhenPluginsUpdateAutomatically() {
    myPlatform.autoUpdate = true;
    assertSilent();
  }

  public void testSilentWhenTheIdeDoesNotCheckForPluginUpdates() {
    myPlatform.checkEnabled = false;
    assertSilent();
  }

  public void testSilentWhenTheUpdateWaitsForARestart() {
    myPlatform.pending = true;
    assertSilent();
  }

  public void testDontShowAgain() {
    myNotice.dontShowAgain(myUi);
    assertTrue(myNotice.getState().dontShowAgain);
    assertEquals(List.of(ErlangUsageKey.UPDATE_NOTICE_DONT_SHOW), myUi.counted);
    myUi.counted.clear();
    myNow += 30 * DAY;
    assertSilent();
  }

  public void testAtMostOneQuestionADay() {
    myPlatform.update = null;
    myNotice.check(myUi);
    myNow += 23 * HOUR;
    assertFalse(myNotice.isCheckDue());
    myNotice.check(myUi);
    assertEquals(1, myPlatform.questions);
    myNow += HOUR;
    assertTrue(myNotice.isCheckDue());
    myNotice.check(myUi);
    assertEquals(2, myPlatform.questions);
    assertEquals(START + DAY, myNotice.getState().lastCheck);
  }

  public void testFailedQuestionWaitsADay() {
    // the platform reports a failed question as no update
    myPlatform.update = null;
    myNotice.check(myUi);
    myNow += HOUR;
    myPlatform.update = update();
    myNotice.check(myUi);
    assertEquals(1, myPlatform.questions);
    assertNothingShown();
  }

  public void testAtMostOneNotificationAWeek() {
    myNotice.check(myUi);
    assertEquals(1, myUi.events.size());
    for (int day = 1; day < 7; day++) {
      myNow = START + day * DAY;
      assertFalse(myNotice.isCheckDue());
      myNotice.check(myUi);
    }
    // not even a question while the notification was shown less than a week ago
    assertEquals(1, myPlatform.questions);
    assertEquals(1, myUi.events.size());
    myNow = START + 7 * DAY;
    myNotice.check(myUi);
    assertEquals(2, myPlatform.questions);
    assertEquals(List.of("offer 0.11.1500", "offer 0.11.1500"), myUi.events);
    assertEquals(START + 7 * DAY, myNotice.getState().lastShown);
  }

  public void testUpdateInstalledDuringTheQuestionIsNotShown() {
    myPlatform.pendingAfterQuestion = true;
    myNotice.check(myUi);
    assertEquals(1, myPlatform.questions);
    assertNothingShown();
    assertEquals(0, myNotice.getState().lastShown);
  }

  public void testOfferIsNotShownWhenPluginsStartToUpdateAutomaticallyBeforeItAppears() {
    myUi.deferOffers = true;
    myNotice.check(myUi);
    // the offer waits for the EDT, for example until the Settings dialog closes
    myPlatform.autoUpdate = true;
    myUi.showQueuedOffers();
    assertNothingShown();
    assertEquals(START, myNotice.getState().lastCheck);
    assertEquals(0, myNotice.getState().lastShown);
  }

  public void testOfferIsNotShownWhenTheUpdateIsInstalledBeforeItAppears() {
    myUi.deferOffers = true;
    myNotice.check(myUi);
    myPlatform.pending = true;
    myUi.showQueuedOffers();
    assertNothingShown();
    assertEquals(0, myNotice.getState().lastShown);
  }

  public void testOfferRecordsTheTimeWhenItAppears() {
    myUi.deferOffers = true;
    myNotice.check(myUi);
    assertNothingShown();
    assertEquals(0, myNotice.getState().lastShown);
    myNow += HOUR;
    myUi.showQueuedOffers();
    assertEquals(List.of("offer 0.11.1500"), myUi.events);
    assertEquals(List.of(ErlangUsageKey.UPDATE_NOTICE_SHOWN), myUi.counted);
    assertEquals(START + HOUR, myNotice.getState().lastShown);
  }

  public void testStoredTimesFromAWrongClockDoNotBlockForever() {
    ErlangUpdateNoticeState state = new ErlangUpdateNoticeState();
    state.lastCheck = START + 365 * DAY;
    state.lastShown = START + 365 * DAY;
    myNotice.loadState(state);
    myNotice.check(myUi);
    assertEquals(List.of("offer 0.11.1500"), myUi.events);
  }

  public void testInstallThenOfferRestart() {
    myNotice.check(myUi);
    assertTrue(myNotice.installPressed(myUi));
    myNotice.install(myPlatform.update, null, myUi);
    assertEquals(1, myInstaller.runs);
    assertEquals(List.of("offer 0.11.1500", "installed 0.11.1500 restart"), myUi.events);
    assertEquals(List.of(ErlangUsageKey.UPDATE_NOTICE_SHOWN, ErlangUsageKey.UPDATE_NOTICE_INSTALL), myUi.counted);
  }

  public void testOneInstallAtATime() {
    // Install pressed again, in the same notification or in an older one that is still on screen
    assertTrue(myNotice.installPressed(myUi));
    assertFalse(myNotice.installPressed(myUi));
    assertFalse(myNotice.installPressed(new FakeUi()));
    assertEquals(List.of(ErlangUsageKey.UPDATE_NOTICE_INSTALL), myUi.counted);
    myNotice.installFinished();
    assertTrue(myNotice.installPressed(myUi));
    assertEquals(List.of(ErlangUsageKey.UPDATE_NOTICE_INSTALL, ErlangUsageKey.UPDATE_NOTICE_INSTALL), myUi.counted);
  }

  public void testRestartDoesNotAskAgain() {
    myNotice.restartPressed();
    // the user pressed Restart: the IDE must not ask "Are you sure you want to exit?"
    assertEquals(List.of("restart, exit confirmed"), myPlatform.restarts);
  }

  public void testInstallWithoutRestartWhenTheIdeCannotRestart() {
    myPlatform.restartCapable = false;
    myNotice.install(myPlatform.update, null, myUi);
    assertEquals(List.of("installed 0.11.1500 no restart"), myUi.events);
  }

  public void testInstallFailure() {
    myInstaller.result = InstallResult.failed("Plugin \"Erlang\" was not installed: Unknown error");
    myNotice.install(myPlatform.update, null, myUi);
    assertEquals(List.of("not installed: Version 0.11.1500 was not installed.<br>Plugin \"Erlang\" was not installed: Unknown error"),
                 myUi.events);
  }

  public void testInstallFailureWithoutReason() {
    myInstaller.result = InstallResult.failed(null);
    myNotice.install(myPlatform.update, null, myUi);
    assertEquals(List.of("not installed: Version 0.11.1500 was not installed."), myUi.events);
    myUi.events.clear();
    myNotice.installFailed(myPlatform.update, myUi);
    assertEquals(List.of("not installed: Version 0.11.1500 was not installed."), myUi.events);
  }

  public void testInstallCancelled() {
    myNotice.installCancelled(myPlatform.update, myUi);
    assertEquals(List.of("not installed: The update to version 0.11.1500 was cancelled."), myUi.events);
  }

  public void testAllowedEnvironment() {
    assertTrue(ErlangUpdateNotice.isAllowedEnvironment("0.11.1500", false, false, null));
    assertTrue(ErlangUpdateNotice.isAllowedEnvironment("0.11.1500", false, false, "true"));
    assertFalse(ErlangUpdateNotice.isAllowedEnvironment("0.11.1500", true, false, null));
    assertFalse(ErlangUpdateNotice.isAllowedEnvironment("0.11.1500", false, true, null));
    assertFalse(ErlangUpdateNotice.isAllowedEnvironment("0.11.1500", false, false, "false"));
    assertFalse(ErlangUpdateNotice.isAllowedEnvironment("0.11.1500", false, false, "FALSE"));
    assertFalse(ErlangUpdateNotice.isAllowedEnvironment("0.11-SNAPSHOT", false, false, null));
    assertFalse(ErlangUpdateNotice.isAllowedEnvironment(null, false, false, null));
  }

  public void testTexts() {
    Update update = update();
    // the collapsed balloon shows the title and the text in full: one short line each
    assertEquals("Erlang plugin 0.11.1500 is available", ErlangUpdateNotice.offerTitle(update));
    assertEquals("You have version 0.11.1400.", ErlangUpdateNotice.offerText(update));
    assertEquals("Version 0.11.1500 is installed. Restart the IDE to use it.", ErlangUpdateNotice.installedText(update));
  }

  public void testTextsEscapeTheVersions() {
    Update update = new Update("1.0<b>", "0.9&1", myInstaller);
    assertEquals("Erlang plugin 1.0&lt;b&gt; is available", ErlangUpdateNotice.offerTitle(update));
    assertEquals("You have version 0.9&amp;1.", ErlangUpdateNotice.offerText(update));
  }

  private void assertSilent() {
    assertFalse(myNotice.isCheckDue());
    myNotice.check(myUi);
    assertEquals(0, myPlatform.questions);
    assertNothingShown();
    assertEquals(0, myNotice.getState().lastCheck);
  }

  private void assertNothingShown() {
    assertEquals(List.of(), myUi.events);
    assertEquals(List.of(), myUi.counted);
  }

  private final class FakePlatform implements ErlangUpdateNotice.Platform {
    boolean allowed = true;
    boolean autoUpdate;
    boolean checkEnabled = true;
    boolean pending;
    boolean pendingAfterQuestion;
    boolean restartCapable = true;
    Update update;
    int questions;
    final List<String> restarts = new ArrayList<>();

    @Override
    public boolean isAllowedEnvironment() {
      return allowed;
    }

    @Override
    public boolean isAutoUpdateEnabled() {
      return autoUpdate;
    }

    @Override
    public boolean isPluginCheckEnabled() {
      return checkEnabled;
    }

    @Override
    public boolean isUpdatePending() {
      return pending;
    }

    @Override
    public boolean isRestartCapable() {
      return restartCapable;
    }

    @Override
    public void restart(boolean exitConfirmed) {
      restarts.add(exitConfirmed ? "restart, exit confirmed" : "restart, ask to exit");
    }

    @Nullable
    @Override
    public Update findUpdate() {
      questions++;
      if (pendingAfterQuestion) pending = true;
      return update;
    }
  }

  private final class FakeUi implements ErlangUpdateNotice.Ui {
    final List<String> events = new ArrayList<>();
    final List<ErlangUsageKey> counted = new ArrayList<>();
    // the offers wait, as on the EDT, until showQueuedOffers()
    boolean deferOffers;
    private final List<Update> myQueuedOffers = new ArrayList<>();

    @Override
    public void offer(@NotNull Update update) {
      if (deferOffers) myQueuedOffers.add(update);
      else show(update);
    }

    void showQueuedOffers() {
      myQueuedOffers.forEach(this::show);
      myQueuedOffers.clear();
    }

    private void show(@NotNull Update update) {
      if (myNotice.mayShowOfferNow(this)) events.add("offer " + update.version());
    }

    @Override
    public void installed(@NotNull Update update, boolean offerRestart) {
      events.add("installed " + update.version() + (offerRestart ? " restart" : " no restart"));
    }

    @Override
    public void notInstalled(@NotNull Update update, @NotNull String contentHtml) {
      events.add("not installed: " + contentHtml);
    }

    @Override
    public void count(@NotNull ErlangUsageKey key) {
      counted.add(key);
    }
  }

  private static final class FakeInstaller implements ErlangUpdateNotice.Installer {
    InstallResult result = InstallResult.INSTALLED;
    int runs;

    @NotNull
    @Override
    public InstallResult install(@Nullable ProgressIndicator indicator) {
      runs++;
      return result;
    }
  }
}
