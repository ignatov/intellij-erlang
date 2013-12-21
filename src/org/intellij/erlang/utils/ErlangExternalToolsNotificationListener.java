/*
 * Copyright 2012-2013 Sergey Ignatov
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

package org.intellij.erlang.utils;

import com.intellij.notification.Notification;
import com.intellij.notification.NotificationListener;
import com.intellij.openapi.options.ShowSettingsUtil;
import com.intellij.openapi.project.Project;
import org.intellij.erlang.settings.ErlangExternalToolsConfigurable;
import org.jetbrains.annotations.NotNull;

import javax.swing.event.HyperlinkEvent;

public class ErlangExternalToolsNotificationListener implements NotificationListener {
  @NotNull
  private final Project myProject;

  public ErlangExternalToolsNotificationListener(@NotNull Project project) {
    myProject = project;
  }

  @Override
  public void hyperlinkUpdate(@NotNull Notification notification, @NotNull HyperlinkEvent event) {
    if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
      if (event.getDescription().equals("configure") && !myProject.isDisposed()) {
        ShowSettingsUtil.getInstance().showSettingsDialog(myProject, ErlangExternalToolsConfigurable.ERLANG_RELATED_TOOLS);
      }
    }
  }
}
