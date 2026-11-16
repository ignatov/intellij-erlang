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

import com.intellij.notification.Notification;
import com.intellij.notification.NotificationAction;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.options.ShowSettingsUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import org.intellij.erlang.elp.ElpPreflight;
import org.intellij.erlang.settings.ErlangExternalToolsConfigurable;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * The balloons of the ELP integration, in the {@value #GROUP} group registered in {@code erlang-elp-lsp.xml}. None is modal.
 */
final class ElpNotifications {
  static final String GROUP = "Erlang ELP";

  private ElpNotifications() {
  }

  /**
   * ELP is not started for the project, and the plugin answers there; told once per set of reasons, see
   * {@link ElpPreflight#shouldTell}.
   */
  static void notStarted(@NotNull Project project, @NotNull ElpPreflight.Result result) {
    StringBuilder content = new StringBuilder("The plugin answers in ").append(StringUtil.escapeXmlEntities(project.getName()))
      .append(" instead:");
    appendList(content, result.blocking().stream().map(ElpPreflight.Problem::text).toList());
    notify(project, new Notification(GROUP, "ELP is not started", content.toString(), NotificationType.WARNING)
      .addAction(openSettings(project)));
  }

  static void appendList(@NotNull StringBuilder content, @NotNull List<String> items) {
    content.append("<ul>");
    for (String item : items) content.append("<li>").append(StringUtil.escapeXmlEntities(item)).append("</li>");
    content.append("</ul>");
  }

  @NotNull
  static NotificationAction openSettings(@NotNull Project project) {
    return NotificationAction.createSimple("Erlang External Tools settings",
                                           () -> ShowSettingsUtil.getInstance().showSettingsDialog(project, ErlangExternalToolsConfigurable.class));
  }

  static void notify(@NotNull Project project, @NotNull Notification notification) {
    if (!project.isDisposed()) notification.notify(project);
  }
}
