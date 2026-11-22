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

import com.intellij.notification.NotificationDisplayType;
import com.intellij.notification.NotificationGroup;
import com.intellij.notification.NotificationGroupManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.statistics.ErlangUsageKey;
import org.intellij.erlang.update.ErlangUpdateNotice.InstallResult;
import org.intellij.erlang.update.ErlangUpdateNotice.Update;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

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
}
