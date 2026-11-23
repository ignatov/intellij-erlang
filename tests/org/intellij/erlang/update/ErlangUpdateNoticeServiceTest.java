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

import com.intellij.notification.Notification;
import com.intellij.notification.NotificationDisplayType;
import com.intellij.notification.NotificationGroup;
import com.intellij.notification.NotificationGroupManager;
import com.intellij.notification.Notifications;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.impl.SimpleDataContext;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.updateSettings.impl.UpdateSettings;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.statistics.ErlangUsageKey;
import org.intellij.erlang.update.ErlangUpdateNotice.InstallResult;
import org.intellij.erlang.update.ErlangUpdateNotice.Update;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Supplier;

public class ErlangUpdateNoticeServiceTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testNotificationGroupIsAStickyBalloon() {
    NotificationGroup group = NotificationGroupManager.getInstance().getNotificationGroup(ErlangUpdateNoticeService.NOTIFICATION_GROUP);
    assertNotNull(group);
    assertEquals(NotificationDisplayType.STICKY_BALLOON, group.getDisplayType());
  }

  public void testOpenedErlangFileStartsNoQuestionInTests() {
    ErlangUpdateNoticeService service = ErlangUpdateNoticeService.getInstance();
    int started = service.getChecksStartedForTests();
    myFixture.configureByText("module.erl", "-module(module).");
    new ErlangUpdateNoticeListener().fileOpened(FileEditorManager.getInstance(getProject()), myFixture.getFile().getVirtualFile());
    assertEquals(started, service.getChecksStartedForTests());
  }

  public void testRestartRunsAfterTheNotificationActionReturns() {
    List<Boolean> restarts = new ArrayList<>();
    new ErlangUpdateNoticeService.IdePlatform(restarts::add).restart(true);
    // an exit inside the notification action logs an error on 2026.2
    assertEquals(List.of(), restarts);
    PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
    assertEquals(List.of(true), restarts);
  }

  public void testQueuedInstallDoesNothingAfterThePluginIsUnloaded() {
    ErlangUpdateNoticeService service = new ErlangUpdateNoticeService();
    service.dispose();
    AtomicInteger installs = new AtomicInteger();
    Update update = new Update("0.11.1500", "0.11.1400", indicator -> {
      installs.incrementAndGet();
      return InstallResult.INSTALLED;
    });
    List<String> events = new ArrayList<>();
    EmptyProgressIndicator indicator = new EmptyProgressIndicator();
    service.runInstall(update, indicator, new ErlangUpdateNotice.Ui() {
      @Override
      public void offer(@NotNull Update update) {
        events.add("offer");
      }

      @Override
      public void installed(@NotNull Update update, boolean offerRestart) {
        events.add("installed");
      }

      @Override
      public void notInstalled(@NotNull Update update, @NotNull String contentHtml) {
        events.add("not installed");
      }

      @Override
      public void count(@NotNull ErlangUsageKey key) {
        events.add(key.name());
      }
    });
    assertEquals(0, installs.get());
    assertEquals(List.of(), events);
  }

  public void testStateIsStored() {
    ErlangUpdateNoticeState state = new ErlangUpdateNoticeState();
    state.lastCheck = 1_795_000_000_000L;
    state.lastShown = 1_795_000_000_001L;
    state.dontShowAgain = true;
    Element element = XmlSerializer.serialize(state);
    ErlangUpdateNoticeState loaded = XmlSerializer.deserialize(element, ErlangUpdateNoticeState.class);
    assertEquals(state.lastCheck, loaded.lastCheck);
    assertEquals(state.lastShown, loaded.lastShown);
    assertTrue(loaded.dontShowAgain);
  }

  private static final long DAY_MS = TimeUnit.DAYS.toMillis(1);
  private final List<Notification> myNotifications = new CopyOnWriteArrayList<>();
  private final List<ErlangUpdateNoticeService> myServices = new ArrayList<>();
  private final AtomicLong myNow = new AtomicLong(1_795_000_000_000L);
  private final FakePlatform myPlatform = new FakePlatform();

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    getProject().getMessageBus().connect(getTestRootDisposable()).subscribe(Notifications.TOPIC, new Notifications() {
      @Override
      public void notify(@NotNull Notification notification) {
        if (ErlangUpdateNoticeService.NOTIFICATION_GROUP.equals(notification.getGroupId())) myNotifications.add(notification);
      }
    });
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      myPlatform.release.countDown();
      for (ErlangUpdateNoticeService service : myServices) service.dispose();
      for (Notification notification : myNotifications) notification.expire();
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  public void testLoadedStateIsTheStoredState() {
    ErlangUpdateNoticeService service = new ErlangUpdateNoticeService();
    ErlangUpdateNoticeState state = new ErlangUpdateNoticeState();
    state.lastCheck = 1_795_000_000_000L;
    state.dontShowAgain = true;
    service.loadState(state);
    assertSame(state, service.getState());
    service.dispose();
  }

  public void testUnloadingThePluginCancelsTheRunningInstall() {
    ErlangUpdateNoticeService service = new ErlangUpdateNoticeService();
    List<String> events = new ArrayList<>();
    EmptyProgressIndicator indicator = new EmptyProgressIndicator();
    service.runInstall(update(indicator1 -> InstallResult.INSTALLED), indicator, recordingUi(events));
    assertEquals(List.of("installed " + ApplicationManager.getApplication().isRestartCapable()), events);
    assertFalse(indicator.isCanceled());
    service.dispose();
    assertTrue(indicator.isCanceled());
  }

  public void testIdePlatformReadsTheUpdateSettings() {
    UpdateSettings settings = UpdateSettings.getInstance();
    boolean autoUpdate = settings.isPluginsAutoUpdateEnabled();
    boolean check = settings.isPluginsCheckNeeded();
    ErlangUpdateNoticeService.IdePlatform platform = new ErlangUpdateNoticeService.IdePlatform(exitConfirmed -> {});
    try {
      settings.setPluginsAutoUpdateEnabled(true);
      settings.setPluginsCheckNeeded(false);
      assertTrue(platform.isAutoUpdateEnabled());
      assertFalse(platform.isPluginCheckEnabled());
      settings.setPluginsAutoUpdateEnabled(false);
      settings.setPluginsCheckNeeded(true);
      assertFalse(platform.isAutoUpdateEnabled());
      assertTrue(platform.isPluginCheckEnabled());
    }
    finally {
      settings.setPluginsAutoUpdateEnabled(autoUpdate);
      settings.setPluginsCheckNeeded(check);
    }
  }

  public void testIdePlatformInTests() {
    ErlangUpdateNoticeService.IdePlatform platform = new ErlangUpdateNoticeService.IdePlatform(exitConfirmed -> {});
    // the plugin under test is not updated, and unit test mode never shows the notification
    assertFalse(platform.isUpdatePending());
    assertFalse(platform.isAllowedEnvironment());
    assertEquals(ApplicationManager.getApplication().isRestartCapable(), platform.isRestartCapable());
  }

  public void testOpenedErlangFileShowsTheOffer() throws Exception {
    myPlatform.update = update(indicator -> InstallResult.INSTALLED);
    ErlangUpdateNoticeService service = service();
    service.erlangFileOpened(getProject());
    assertEquals(1, service.getChecksStartedForTests());
    Notification offer = waitForNotification(0);
    assertEquals("Erlang plugin 0.11.1500 is available", offer.getTitle());
    assertEquals("You have version 0.11.1400.", offer.getContent());
    assertEquals(List.of("Install", "Don't show again"), actionNames(offer));
    assertEquals(1, myPlatform.questions.get());
    assertEquals(myNow.get(), service.getState().lastShown);
    // the next question is a day later
    service.erlangFileOpened(getProject());
    assertEquals(1, service.getChecksStartedForTests());
  }

  public void testNewerOfferExpiresTheOlderOne() throws Exception {
    myPlatform.update = update(indicator -> InstallResult.INSTALLED);
    ErlangUpdateNoticeService service = service();
    service.erlangFileOpened(getProject());
    Notification first = waitForNotification(0);
    waitUntilCheckEnds(service);
    myNow.addAndGet(8 * DAY_MS);
    service.erlangFileOpened(getProject());
    Notification second = waitForNotification(1);
    assertTrue(first.isExpired());
    assertFalse(second.isExpired());
    assertEquals(2, service.getChecksStartedForTests());
  }

  public void testOfferIsNotShownWhenTheSettingChangedWhileItWaited() throws Exception {
    myPlatform.update = update(indicator -> InstallResult.INSTALLED);
    // the IDE updates plugins by itself from the moment the offer is about to appear: the 4th look at the setting,
    // after the one when the file is opened and the two of the question
    myPlatform.autoUpdateFromLook = 4;
    ErlangUpdateNoticeService service = service();
    service.erlangFileOpened(getProject());
    waitUntilCheckEnds(service);
    PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
    assertEquals(4, myPlatform.autoUpdateLooks.get());
    assertEquals(1, myPlatform.questions.get());
    assertEquals(List.of(), myNotifications);
    assertEquals(0, service.getState().lastShown);
  }

  public void testDontShowAgainSilencesTheNotice() throws Exception {
    myPlatform.update = update(indicator -> InstallResult.INSTALLED);
    ErlangUpdateNoticeService service = service();
    service.erlangFileOpened(getProject());
    Notification offer = waitForNotification(0);
    fire(offer, "Don't show again");
    assertTrue(service.getState().dontShowAgain);
    assertTrue(offer.isExpired());
    waitUntilCheckEnds(service);
    myNow.addAndGet(30 * DAY_MS);
    service.erlangFileOpened(getProject());
    assertEquals(1, service.getChecksStartedForTests());
  }

  public void testInstallShowsInstalledWithRestart() throws Exception {
    AtomicReferenceIndicator seen = new AtomicReferenceIndicator();
    myPlatform.update = update(indicator -> {
      seen.indicator = indicator;
      return InstallResult.INSTALLED;
    });
    ErlangUpdateNoticeService service = service();
    service.erlangFileOpened(getProject());
    Notification offer = waitForNotification(0);
    fire(offer, "Install");
    Notification installed = waitForNotification(1);
    assertNotNull(seen.indicator);
    assertTrue(offer.isExpired());
    assertEquals(ErlangUpdateNotice.INSTALLED_TITLE, installed.getTitle());
    assertEquals("Version 0.11.1500 is installed. Restart the IDE to use it.", installed.getContent());
    assertEquals(List.of("Restart"), actionNames(installed));
    fire(installed, "Restart");
    assertEquals(List.of(true), myPlatform.restarts);
    // the finished install lets the user press Install again
    waitUntilInstallEnds(service);
    fire(offer, "Install");
    waitForNotification(2);
  }

  public void testInstalledWithoutRestartWhenTheIdeCannotRestart() throws Exception {
    myPlatform.restartCapable = false;
    myPlatform.update = update(indicator -> InstallResult.INSTALLED);
    ErlangUpdateNoticeService service = service();
    service.erlangFileOpened(getProject());
    fire(waitForNotification(0), "Install");
    Notification installed = waitForNotification(1);
    assertEquals(ErlangUpdateNotice.INSTALLED_TITLE, installed.getTitle());
    assertEquals(List.of(), actionNames(installed));
  }

  public void testFailedInstallShowsTheReason() throws Exception {
    myPlatform.update = update(indicator -> InstallResult.failed("No space left"));
    ErlangUpdateNoticeService service = service();
    service.erlangFileOpened(getProject());
    Notification offer = waitForNotification(0);
    fire(offer, "Install");
    Notification notInstalled = waitForNotification(1);
    assertEquals(ErlangUpdateNotice.NOT_INSTALLED_TITLE, notInstalled.getTitle());
    assertEquals("Version 0.11.1500 was not installed.<br>No space left", notInstalled.getContent());
    assertFalse(offer.isExpired());
  }

  public void testInstallThatThrowsShowsNotInstalled() throws Exception {
    myPlatform.update = update(indicator -> {
      throw new IllegalStateException("broken download");
    });
    ErlangUpdateNoticeService service = service();
    service.erlangFileOpened(getProject());
    fire(waitForNotification(0), "Install");
    Notification notInstalled = waitForNotification(1);
    assertEquals(ErlangUpdateNotice.NOT_INSTALLED_TITLE, notInstalled.getTitle());
    assertEquals("Version 0.11.1500 was not installed.", notInstalled.getContent());
  }

  public void testCancelledInstallShowsCancelled() throws Exception {
    myPlatform.update = update(indicator -> {
      assertNotNull(indicator);
      indicator.cancel();
      indicator.checkCanceled();
      return InstallResult.INSTALLED;
    });
    ErlangUpdateNoticeService service = service();
    service.erlangFileOpened(getProject());
    fire(waitForNotification(0), "Install");
    Notification notInstalled = waitForNotification(1);
    assertEquals(ErlangUpdateNotice.NOT_INSTALLED_TITLE, notInstalled.getTitle());
    assertEquals("The update to version 0.11.1500 was cancelled.", notInstalled.getContent());
  }

  public void testInstallPressedDuringTheInstallStartsNoSecondOne() throws Exception {
    AtomicInteger installs = new AtomicInteger();
    List<Notification> offers = new CopyOnWriteArrayList<>();
    myPlatform.update = update(indicator -> {
      // the user presses Install again while the first install runs
      if (installs.incrementAndGet() == 1) fire(offers.get(0), "Install");
      return InstallResult.INSTALLED;
    });
    ErlangUpdateNoticeService service = service();
    service.erlangFileOpened(getProject());
    offers.add(waitForNotification(0));
    fire(offers.get(0), "Install");
    waitForNotification(1);
    waitUntilInstallEnds(service);
    PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
    assertEquals(1, installs.get());
    assertEquals(2, myNotifications.size());
  }

  public void testInstallPressedAfterThePluginIsUnloadedDoesNothing() throws Exception {
    AtomicInteger installs = new AtomicInteger();
    myPlatform.update = update(indicator -> {
      installs.incrementAndGet();
      return InstallResult.INSTALLED;
    });
    ErlangUpdateNoticeService service = service();
    service.erlangFileOpened(getProject());
    Notification offer = waitForNotification(0);
    service.dispose();
    fire(offer, "Install");
    PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
    assertEquals(0, installs.get());
    assertEquals(1, myNotifications.size());
  }

  public void testOfferWithoutAProject() throws Exception {
    // a notification of no project goes to the application
    ApplicationManager.getApplication().getMessageBus().connect(getTestRootDisposable()).subscribe(Notifications.TOPIC, new Notifications() {
      @Override
      public void notify(@NotNull Notification notification) {
        if (ErlangUpdateNoticeService.NOTIFICATION_GROUP.equals(notification.getGroupId())) myNotifications.add(notification);
      }
    });
    myPlatform.update = update(indicator -> InstallResult.INSTALLED);
    ErlangUpdateNoticeService service = service();
    service.erlangFileOpened(null);
    Notification offer = waitForNotification(0);
    assertEquals("Erlang plugin 0.11.1500 is available", offer.getTitle());
  }

  public void testNoQuestionAfterThePluginIsUnloaded() {
    myPlatform.update = update(indicator -> InstallResult.INSTALLED);
    ErlangUpdateNoticeService service = service();
    service.dispose();
    service.erlangFileOpened(getProject());
    assertEquals(0, service.getChecksStartedForTests());
    assertEquals(0, myPlatform.questions.get());
  }

  public void testOneQuestionAtATime() throws Exception {
    myPlatform.update = update(indicator -> InstallResult.INSTALLED);
    myPlatform.block = true;
    ErlangUpdateNoticeService service = service();
    service.erlangFileOpened(getProject());
    assertTrue(myPlatform.asked.await(30, TimeUnit.SECONDS));
    // a day later, while the first question still runs
    myNow.addAndGet(2 * DAY_MS);
    service.erlangFileOpened(getProject());
    assertEquals(1, service.getChecksStartedForTests());
    myPlatform.release.countDown();
    waitForNotification(0);
    assertEquals(1, myPlatform.questions.get());
  }

  public void testUnloadingThePluginCancelsTheRunningQuestion() throws Exception {
    myPlatform.update = update(indicator -> InstallResult.INSTALLED);
    myPlatform.waitForCancel = true;
    ErlangUpdateNoticeService service = service();
    service.erlangFileOpened(getProject());
    assertTrue(myPlatform.asked.await(30, TimeUnit.SECONDS));
    service.dispose();
    waitUntilCheckEnds(service);
    assertTrue(myPlatform.cancelSeen.get());
    PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
    assertEquals(List.of(), myNotifications);
  }

  public void testFailedQuestionIsAskedAgainADayLater() throws Exception {
    myPlatform.failure = () -> new IllegalStateException("the repository is down");
    ErlangUpdateNoticeService service = service();
    service.erlangFileOpened(getProject());
    waitFor("the first question", () -> myPlatform.questions.get() == 1);
    waitUntilCheckEnds(service);
    myNow.addAndGet(2 * DAY_MS);
    myPlatform.failure = ProcessCanceledException::new;
    service.erlangFileOpened(getProject());
    waitFor("the second question", () -> myPlatform.questions.get() == 2);
    waitUntilCheckEnds(service);
    myNow.addAndGet(2 * DAY_MS);
    myPlatform.failure = null;
    myPlatform.update = update(indicator -> InstallResult.INSTALLED);
    service.erlangFileOpened(getProject());
    assertEquals(3, service.getChecksStartedForTests());
    waitForNotification(0);
    assertEquals(3, myPlatform.questions.get());
  }

  @NotNull
  private ErlangUpdateNoticeService service() {
    ErlangUpdateNoticeService service = new ErlangUpdateNoticeService();
    myServices.add(service);
    try {
      Field notice = ErlangUpdateNoticeService.class.getDeclaredField("myNotice");
      notice.setAccessible(true);
      notice.set(service, new ErlangUpdateNotice(myNow::get, myPlatform));
    }
    catch (ReflectiveOperationException e) {
      throw new AssertionError(e);
    }
    return service;
  }

  @NotNull
  private Notification waitForNotification(int index) throws Exception {
    waitFor("notification " + index, () -> myNotifications.size() > index);
    return myNotifications.get(index);
  }

  private static void waitUntilCheckEnds(@NotNull ErlangUpdateNoticeService service) throws Exception {
    AtomicBoolean checking = (AtomicBoolean)field(service, "myChecking");
    waitFor("the question ends", () -> !checking.get());
  }

  private static void waitUntilInstallEnds(@NotNull ErlangUpdateNoticeService service) throws Exception {
    waitFor("the install ends", () -> {
      try {
        return field(service, "myInstallIndicator") == null;
      }
      catch (ReflectiveOperationException e) {
        throw new AssertionError(e);
      }
    });
  }

  @Nullable
  private static Object field(@NotNull ErlangUpdateNoticeService service, @NotNull String name) throws ReflectiveOperationException {
    Field field = ErlangUpdateNoticeService.class.getDeclaredField(name);
    field.setAccessible(true);
    return field.get(service);
  }

  private static void waitFor(@NotNull String what, @NotNull Supplier<Boolean> condition) {
    long deadline = System.currentTimeMillis() + TimeUnit.SECONDS.toMillis(30);
    while (!condition.get()) {
      if (System.currentTimeMillis() > deadline) fail("timed out waiting for " + what);
      PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
      try {
        Thread.sleep(10);
      }
      catch (InterruptedException e) {
        throw new RuntimeException(e);
      }
    }
    PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
  }

  @NotNull
  private static List<String> actionNames(@NotNull Notification notification) {
    List<String> names = new ArrayList<>();
    for (AnAction action : notification.getActions()) names.add(action.getTemplatePresentation().getText());
    return names;
  }

  private static void fire(@NotNull Notification notification, @NotNull String actionName) {
    for (AnAction action : notification.getActions()) {
      if (actionName.equals(action.getTemplatePresentation().getText())) {
        Notification.fire(notification, action, SimpleDataContext.getSimpleContext(Notification.KEY, notification));
        return;
      }
    }
    fail("no action " + actionName + " in " + actionNames(notification));
  }

  @NotNull
  private static Update update(@NotNull ErlangUpdateNotice.Installer installer) {
    return new Update("0.11.1500", "0.11.1400", installer);
  }

  @NotNull
  private static ErlangUpdateNotice.Ui recordingUi(@NotNull List<String> events) {
    return new ErlangUpdateNotice.Ui() {
      @Override
      public void offer(@NotNull Update update) {
        events.add("offer");
      }

      @Override
      public void installed(@NotNull Update update, boolean offerRestart) {
        events.add("installed " + offerRestart);
      }

      @Override
      public void notInstalled(@NotNull Update update, @NotNull String contentHtml) {
        events.add("not installed");
      }

      @Override
      public void count(@NotNull ErlangUsageKey key) {
        events.add(key.name());
      }
    };
  }

  private static final class AtomicReferenceIndicator {
    volatile ProgressIndicator indicator;
  }

  private static final class FakePlatform implements ErlangUpdateNotice.Platform {
    volatile Update update;
    volatile Supplier<RuntimeException> failure;
    volatile int autoUpdateFromLook = Integer.MAX_VALUE;
    final AtomicInteger autoUpdateLooks = new AtomicInteger();
    volatile boolean restartCapable = true;
    volatile boolean block;
    volatile boolean waitForCancel;
    final AtomicInteger questions = new AtomicInteger();
    final AtomicBoolean cancelSeen = new AtomicBoolean();
    final CountDownLatch asked = new CountDownLatch(1);
    final CountDownLatch release = new CountDownLatch(1);
    final List<Boolean> restarts = new CopyOnWriteArrayList<>();

    @Override
    public boolean isAllowedEnvironment() {
      return true;
    }

    @Override
    public boolean isAutoUpdateEnabled() {
      return autoUpdateLooks.incrementAndGet() >= autoUpdateFromLook;
    }

    @Override
    public boolean isPluginCheckEnabled() {
      return true;
    }

    @Override
    public boolean isUpdatePending() {
      return false;
    }

    @Override
    public boolean isRestartCapable() {
      return restartCapable;
    }

    @Override
    public void restart(boolean exitConfirmed) {
      restarts.add(exitConfirmed);
    }

    @Nullable
    @Override
    public Update findUpdate() {
      questions.incrementAndGet();
      asked.countDown();
      try {
        if (block) release.await(30, TimeUnit.SECONDS);
        if (waitForCancel) {
          ProgressIndicator indicator = ProgressManager.getGlobalProgressIndicator();
          long deadline = System.currentTimeMillis() + TimeUnit.SECONDS.toMillis(30);
          while (indicator != null && !indicator.isCanceled() && System.currentTimeMillis() < deadline) Thread.sleep(10);
          cancelSeen.set(indicator != null && indicator.isCanceled());
        }
      }
      catch (InterruptedException e) {
        throw new RuntimeException(e);
      }
      Supplier<RuntimeException> fail = failure;
      if (fail != null) throw fail.get();
      return update;
    }
  }
}
