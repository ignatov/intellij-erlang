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
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.options.ShowSettingsUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import org.intellij.erlang.elp.ElpFeedback;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.elp.ElpPreflight;
import org.intellij.erlang.elp.ElpSettings;
import org.intellij.erlang.settings.ErlangExternalToolsConfigurable;
import org.jetbrains.annotations.NotNull;

import java.nio.file.Path;
import java.util.List;

/**
 * The balloons of the ELP integration, in the {@value #GROUP} group registered in {@code erlang-elp-lsp.xml}. None is modal.
 */
final class ElpNotifications {
  static final String GROUP = "Erlang ELP";
  private static final int LAST_LINES = 8;

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

  /**
   * ELP did not load the project, see {@link ElpLoadState}; the plugin answers there.
   */
  static void couldNotLoad(@NotNull Project project, @NotNull ElpLoadState loadState, @NotNull String root) {
    StringBuilder content = new StringBuilder();
    if (loadState.isOtpNotFound()) {
      content.append("ELP cannot find Erlang/OTP: it runs erl from the bin directory of the project's Erlang SDK. ")
        .append("Check the SDK of the project.");
    }
    else {
      String error = loadState.firstError();
      content.append(error != null ? "ELP said: " + StringUtil.escapeXmlEntities(error)
                                   : "ELP did not finish loading " + StringUtil.escapeXmlEntities(root) + ".");
    }
    content.append(" The plugin answers in ").append(StringUtil.escapeXmlEntities(project.getName())).append(" instead.");
    List<String> lines = loadState.lastLogLines();
    if (!lines.isEmpty()) {
      content.append("<br>Last messages:");
      appendList(content, lines.subList(Math.max(0, lines.size() - LAST_LINES), lines.size()));
    }
    notify(project, new Notification(GROUP, "ELP could not load the project", content.toString(), NotificationType.ERROR)
      .addAction(openSettings(project))
      .addAction(showLog(project))
      .addAction(switchBack())
      .addAction(feedback(project)));
  }

  /** Sets the engine to the plugin at once: the way back from ELP is one click. Whoever goes back is not offered ELP again. */
  @NotNull
  static NotificationAction switchBack() {
    return NotificationAction.createSimpleExpiring("Switch back to the plugin", () -> {
      ElpSettings.getInstance().setOfferNeverAsk(true);
      ElpSettings.getInstance().setMode(ElpMode.PLUGIN);
    });
  }

  /** Opens a prefilled new issue in the browser, see {@link ElpFeedback}; the plugin sends nothing. */
  @NotNull
  static NotificationAction feedback(@NotNull Project project) {
    return NotificationAction.createSimple("Tell us how it went", () -> ElpFeedback.open(project));
  }

  /**
   * The first Error message of a running ELP in a session; the platform logs ELP's messages but shows none of its log messages.
   */
  static void elpError(@NotNull Project project, @NotNull String message) {
    notify(project, new Notification(GROUP, "ELP reported an error",
                                     StringUtil.escapeXmlEntities(StringUtil.shortenTextWithEllipsis(message, 500, 0)),
                                     NotificationType.WARNING)
      .addAction(showLog(project)));
  }

  /** Opens the IDE log, where the platform writes ELP's standard error and messages. */
  @NotNull
  static NotificationAction showLog(@NotNull Project project) {
    return NotificationAction.createSimple("Show IDE log", () -> ApplicationManager.getApplication().executeOnPooledThread(() -> {
      VirtualFile log = LocalFileSystem.getInstance().refreshAndFindFileByNioFile(Path.of(PathManager.getLogPath(), "idea.log"));
      if (log == null) return;
      ApplicationManager.getApplication().invokeLater(() -> FileEditorManager.getInstance(project).openFile(log, true),
                                                      ModalityState.nonModal(), project.getDisposed());
    }));
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
