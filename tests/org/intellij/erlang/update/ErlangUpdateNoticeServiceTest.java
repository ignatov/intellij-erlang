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
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jdom.Element;

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
