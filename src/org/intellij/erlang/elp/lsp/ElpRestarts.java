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
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.platform.lsp.api.LspServerManager;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;

/**
 * What happens when an ELP server stops without being asked to. The platform's client hands every feature back to the plugin
 * within a second, but tells the user nothing and does not start ELP again (measured: no restart in 100 s; opening files or typing
 * does not restart it). The plugin says so and restarts ELP once; when ELP stops again within {@link #WINDOW_MS}, it stays with
 * the plugin and says so, with a Restart action. A server that stops before it was initialized did not start at all: that is told
 * with the path and the reason, and not restarted.
 */
final class ElpRestarts {
  private static final Logger LOG = Logger.getInstance(ElpRestarts.class);
  private static final Key<ElpRestarts> KEY = Key.create("ElpRestarts");

  static final int MAX_AUTOMATIC_RESTARTS = 1;
  static final long WINDOW_MS = 5 * 60_000;

  private final Deque<Long> myRestarts = new ArrayDeque<>();

  @NotNull
  static synchronized ElpRestarts getInstance(@NotNull Project project) {
    ElpRestarts restarts = project.getUserData(KEY);
    if (restarts == null) {
      restarts = new ElpRestarts();
      project.putUserData(KEY, restarts);
    }
    return restarts;
  }

  @TestOnly
  static void resetForTests(@NotNull Project project) {
    project.putUserData(KEY, null);
  }

  /**
   * @return whether an automatic restart is allowed now, counting it if so
   */
  synchronized boolean claimAutomaticRestart(long now) {
    while (!myRestarts.isEmpty() && now - myRestarts.peekFirst() >= WINDOW_MS) myRestarts.removeFirst();
    if (myRestarts.size() >= MAX_AUTOMATIC_RESTARTS) return false;
    myRestarts.addLast(now);
    return true;
  }

  /**
   * The server of {@code project} stopped without being asked to, after it had been initialized. Under a root that ELP serves
   * ({@code served}) the plugin does not answer while ELP is down, so the user is offered the way back to the plugin.
   */
  static void crashed(@NotNull Project project, @Nullable Integer exitCode, @NotNull List<String> lastLines, boolean served) {
    if (project.isDisposed()) return;
    boolean restart = getInstance(project).claimAutomaticRestart(System.currentTimeMillis());
    LOG.info("ELP stopped unexpectedly (exit code " + exitCode + "); " + (restart ? "restarting it" : "not restarting it again"));
    StringBuilder content = new StringBuilder("ELP stopped");
    if (exitCode != null) content.append(" with exit code ").append(exitCode);
    content.append(restart ? "; the plugin started it again." :
                   " again within " + WINDOW_MS / 60_000 + " minutes" +
                   (served ? "." + ElpNotifications.STAYS_OFF
                           : "; the plugin answers in " + StringUtil.escapeXmlEntities(project.getName()) + " until ELP is restarted."));
    if (!lastLines.isEmpty()) {
      content.append("<br>Last messages:");
      ElpNotifications.appendList(content, lastLines.subList(Math.max(0, lastLines.size() - 5), lastLines.size()));
    }
    Notification notification = new Notification(ElpNotifications.GROUP, "ELP stopped unexpectedly", content.toString(),
                                                 restart ? NotificationType.WARNING : NotificationType.ERROR);
    if (!restart) notification.addAction(NotificationAction.createSimpleExpiring("Restart ELP", () -> restart(project)));
    if (!restart && served) notification.addAction(ElpNotifications.switchBack());
    notification.addAction(ElpNotifications.showLog(project));
    ElpNotifications.notify(project, notification);
    if (restart) restart(project);
  }

  /**
   * The server process ended before the client initialized it, or could not be started: ELP did not start. Not restarted.
   */
  static void didNotStart(@NotNull Project project, @NotNull String path, @NotNull String reason, @NotNull List<String> lastLines) {
    if (project.isDisposed()) return;
    StringBuilder content = new StringBuilder(StringUtil.escapeXmlEntities(path)).append(": ").append(StringUtil.escapeXmlEntities(reason))
      .append(". The plugin answers in ").append(StringUtil.escapeXmlEntities(project.getName())).append(" instead.");
    if (!lastLines.isEmpty()) {
      content.append("<br>Last messages:");
      ElpNotifications.appendList(content, lastLines.subList(Math.max(0, lastLines.size() - 5), lastLines.size()));
    }
    ElpNotifications.notify(project, new Notification(ElpNotifications.GROUP, "ELP did not start", content.toString(),
                                                      NotificationType.ERROR)
      .addAction(ElpNotifications.openSettings(project))
      .addAction(ElpNotifications.showLog(project)));
  }

  /** Stops the ELP servers of the project and starts the ones that the open Erlang files need, as the widget's Restart does. */
  static void restart(@NotNull Project project) {
    ApplicationManager.getApplication().invokeLater(
      () -> LspServerManager.getInstance(project).stopAndRestartIfNeeded(ElpLspIntegrationProvider.class),
      ModalityState.nonModal(), project.getDisposed());
  }
}
