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
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import com.intellij.util.concurrency.annotations.RequiresEdt;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpFeedback;
import org.intellij.erlang.elp.ElpFiles;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.elp.ElpPreflight;
import org.intellij.erlang.elp.ElpSettings;
import org.intellij.erlang.elp.distribution.ElpAvailability;
import org.intellij.erlang.elp.distribution.ElpDistribution;
import org.intellij.erlang.elp.distribution.ElpRelease;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.io.File;
import java.nio.file.Path;
import java.util.List;

/**
 * The offer to move to ELP, shown at most once per plugin version when an Erlang file is opened in the Plugin position and ELP
 * would work for the project: {@link ElpPreflight} finds nothing against it, and a server is set or can be downloaded. Nothing
 * switches by itself: "Try ELP" downloads the server when needed and only then sets the ELP position; "Switch back to the
 * plugin" undoes it. The {@value #PROPERTY} system property set to {@code false} turns the offer off.
 */
final class ElpOffer {
  private static final Logger LOG = Logger.getInstance(ElpOffer.class);
  static final String PROPERTY = "erlang.elp.offer";
  /** Registered as a sticky balloon in {@code erlang-elp-lsp.xml}: the offer stays until the user answers it. */
  static final String GROUP = "Erlang ELP offer";
  static final String TITLE = "Try ELP, the Erlang language server";
  static final String TRY = "Try ELP";
  static final String NOT_NOW = "Not now";
  static final String NEVER = "Don't ask again";
  static final String ON_TITLE = "ELP is on";
  static final String FAILED_TITLE = "ELP is not on";
  static final String RETRY = "Retry";

  /** A project that ELP does not fit is checked again after this time at the earliest, when another file is opened. */
  private static final long RECHECK_MS = 60_000;
  static final Key<Long> LAST_CHECK = Key.create("erlang.elp.offer.lastCheck");
  private static volatile boolean ourEnabledInTests;

  private ElpOffer() {
  }

  /** Lets {@link #fileOpened} check the project in tests until {@code parent} is disposed. */
  @TestOnly
  static void enableInTests(@NotNull Disposable parent) {
    ourEnabledInTests = true;
    Disposer.register(parent, () -> ourEnabledInTests = false);
  }

  /**
   * Whether the offer may be shown for {@code pluginVersion}: it is not switched off, the engine is the plugin and not forced by
   * a system property, the user did not choose "Don't ask again" and the offer was not shown for this version yet. Touches
   * neither the file system nor the project model.
   */
  static boolean shouldAsk(@NotNull String pluginVersion) {
    return whyNotAsk(pluginVersion) == null;
  }

  /** @return why {@link #shouldAsk} says no, for the log; {@code null} when the offer may be shown */
  @Nullable
  static String whyNotAsk(@NotNull String pluginVersion) {
    if (Boolean.FALSE.equals(ElpConfiguration.parseBoolean(System.getProperty(PROPERTY)))) return "-D" + PROPERTY + "=false";
    String forcedMode = System.getProperty(ElpConfiguration.MODE_PROPERTY);
    if (ElpMode.parse(forcedMode) != null) return "the engine is forced by -D" + ElpConfiguration.MODE_PROPERTY + "=" + forcedMode;
    ElpSettings settings = ElpSettings.getInstance();
    if (settings.getStoredMode() != ElpMode.PLUGIN) return "the engine is " + settings.getStoredMode();
    if (settings.isOfferNeverAsk()) return "the user chose \"" + NEVER + "\"";
    if (pluginVersion.equals(settings.getOfferShownForVersion())) {
      return "already shown for version " + settings.getOfferShownForVersion() + " (plugin version " + pluginVersion + ")";
    }
    return null;
  }

  /**
   * ELP would work for the workspace root that {@code result} was checked for. With a server that is not downloaded yet, the
   * missing download is not held against it: "Try ELP" downloads it.
   */
  static boolean isEligible(@NotNull ElpPreflight.Result result, @NotNull ElpAvailability availability) {
    ElpAvailability.Kind kind = availability.kind();
    if (kind == ElpAvailability.Kind.UNAVAILABLE || kind == ElpAvailability.Kind.CONFIGURED && !availability.hasServer()) return false;
    List<ElpPreflight.Problem> problems = result.problems().stream()
      .filter(p -> kind != ElpAvailability.Kind.DOWNLOADABLE || p.reason() != ElpPreflight.Reason.SERVER_NOT_DOWNLOADED)
      .toList();
    return new ElpPreflight.Result(problems).offerEligible();
  }

  /** Called for every Erlang file the user opens; checks the project off the EDT, at most once a minute per project. */
  static void fileOpened(@NotNull Project project, @NotNull VirtualFile file) {
    // Tests that open files for other reasons must not get the offer: it would land among their notifications and settings.
    if (ApplicationManager.getApplication().isUnitTestMode() && !ourEnabledInTests) return;
    String version = ElpFeedback.pluginVersion();
    LOG.debug("fileOpened " + file.getName() + ", engine " + ElpSettings.getInstance().getStoredMode() + ", plugin version " + version);
    String whyNot = whyNotAsk(version);
    if (whyNot != null) {
      LOG.debug("No ELP offer: " + whyNot);
      return;
    }
    synchronized (LAST_CHECK) {
      Long last = project.getUserData(LAST_CHECK);
      long now = System.currentTimeMillis();
      if (last != null && now - last < RECHECK_MS) {
        LOG.debug("No ELP offer check: the project was checked less than a minute ago");
        return;
      }
      project.putUserData(LAST_CHECK, now);
    }
    ApplicationManager.getApplication().executeOnPooledThread(() -> {
      try {
        check(project, file, version);
      }
      catch (ProcessCanceledException e) {
        LOG.debug("The ELP offer check was cancelled");
      }
      catch (Throwable e) {
        LOG.warn("The ELP offer check failed", e);
      }
    });
  }

  @RequiresBackgroundThread
  private static void check(@NotNull Project project, @NotNull VirtualFile file, @NotNull String version) {
    VirtualFile root = ReadAction.computeBlocking(
      () -> project.isDisposed() || !file.isValid() ? null : ElpFiles.findWorkspaceRoot(project, file));
    if (root == null) {
      LOG.debug("No ELP offer: no workspace root for " + file.getName());
      return;
    }
    ElpPreflight.Facts facts = ElpPreflight.getInstance(project).gatherFacts();
    ElpPreflight.Layout layout = ReadAction.computeBlocking(() -> root.isValid() ? ElpPreflight.Layout.of(root) : null);
    ElpPreflight.Result result = ElpPreflight.evaluate(facts, layout);
    ElpAvailability availability = ElpDistribution.getInstance().availability(project);
    LOG.debug("ELP offer check: OTP " + facts.otpRelease() + ", rebar3 " + facts.rebar3Version() + ", layout " + layout +
              ", problems " + result.reasons() + ", availability " + availability.kind() +
              (availability.choice().reason() != null ? " (" + availability.choice().message() + ")" : ""));
    ApplicationManager.getApplication().invokeLater(() -> offerIfEligible(project, version, result, availability),
                                                    ModalityState.nonModal(), project.getDisposed());
  }

  /**
   * Shows the offer and remembers that it was shown for {@code pluginVersion}, when it may be shown and ELP would work. A
   * project that ELP does not fit leaves the version unmarked, so that another project can get the offer.
   *
   * @return whether the offer was shown
   */
  @RequiresEdt
  static boolean offerIfEligible(@NotNull Project project, @NotNull String pluginVersion, @NotNull ElpPreflight.Result result,
                                 @NotNull ElpAvailability availability) {
    if (project.isDisposed()) return false;
    String whyNot = whyNotAsk(pluginVersion);
    if (whyNot != null) {
      LOG.debug("No ELP offer: " + whyNot);
      return false;
    }
    if (!isEligible(result, availability)) {
      LOG.info("No ELP offer for " + project.getName() + ": " + StringUtil.join(result.problems(), ElpPreflight.Problem::text, " ") +
               " ELP server: " + availability.kind() +
               (availability.choice().reason() != null ? ", " + availability.choice().message() : "") + ".");
      return false;
    }
    LOG.info("ELP offer shown for plugin version " + pluginVersion + ", ELP server: " + availability.kind());
    ElpSettings.getInstance().setOfferShownForVersion(pluginVersion);
    ElpNotifications.notify(project, new Notification(GROUP, TITLE, content(availability), NotificationType.INFORMATION)
      .addAction(NotificationAction.createSimpleExpiring(TRY, () -> tryElp(project)))
      .addAction(NotificationAction.createSimpleExpiring(NOT_NOW, () -> ElpSettings.getInstance().setOfferShownForVersion(pluginVersion)))
      .addAction(NotificationAction.createSimpleExpiring(NEVER, () -> ElpSettings.getInstance().setOfferNeverAsk(true))));
    return true;
  }

  /** The text of the offer; it says what "Try ELP" does, because the click is the user's consent. */
  @NotNull
  static String content(@NotNull ElpAvailability availability) {
    StringBuilder text = new StringBuilder(
      "ELP, the Erlang language server from WhatsApp, will answer diagnostics, navigation, find usages and rename. " +
      "The plugin keeps completion, formatting, refactorings, run and debug.<br>");
    ElpRelease.Asset asset = availability.choice().asset();
    if (availability.kind() == ElpAvailability.Kind.DOWNLOADABLE && asset != null) {
      text.append("Try ELP downloads ELP ").append(availability.release().version()).append(" (").append(asset.sizeInMegabytes())
        .append(") from GitHub. ");
    }
    text.append("When a project is opened, ELP runs rebar3 to fetch and build its dependencies and writes to _build.<br>")
      .append("You can switch back in Settings | Other Settings | Erlang External Tools.");
    return text.toString();
  }

  /** "Try ELP" and "Retry": looks for the server off the EDT again, because the offer may have waited for hours. */
  static void tryElp(@NotNull Project project) {
    ApplicationManager.getApplication().executeOnPooledThread(() -> {
      if (project.isDisposed()) return;
      ElpAvailability availability = ElpDistribution.getInstance().availability(project);
      String problem = serverProblem(availability);
      ApplicationManager.getApplication().invokeLater(() -> tryElp(project, availability, problem), ModalityState.nonModal(),
                                                      project.getDisposed());
    });
  }

  /** @return why the server of {@code availability} cannot be used now, {@code null} when it can or can be downloaded */
  @Nullable
  @RequiresBackgroundThread
  static String serverProblem(@NotNull ElpAvailability availability) {
    return switch (availability.kind()) {
      case INSTALLED, DOWNLOADABLE -> null;
      case UNAVAILABLE -> availability.choice().message();
      case CONFIGURED -> {
        String path = availability.serverPath();
        if (path == null) yield "The path to the ELP binary is empty.";
        File file = new File(path);
        yield file.isFile() && file.canExecute() ? null : "The ELP binary " + path + " is not an executable file.";
      }
    };
  }

  /**
   * Switches to ELP when its server is there, downloads it first when it is not; on a failed or cancelled download the engine
   * stays the plugin.
   */
  @RequiresEdt
  static void tryElp(@NotNull Project project, @NotNull ElpAvailability availability, @Nullable String problem) {
    if (problem != null) {
      failed(project, problem);
      return;
    }
    ElpRelease.Asset asset = availability.choice().asset();
    if (availability.kind() != ElpAvailability.Kind.DOWNLOADABLE || asset == null) {
      switchToElp(project);
      return;
    }
    ElpDistribution.getInstance().download(project, asset, new ElpDistribution.DownloadCallback() {
      @Override
      public void installed(@NotNull Path elp) {
        switchToElp(project);
      }

      @Override
      public void failed(@NotNull String message) {
        ElpOffer.failed(project, message);
      }

      @Override
      public void cancelled() {
        ElpOffer.failed(project, "The download of ELP was cancelled.");
      }
    });
  }

  @RequiresEdt
  static void switchToElp(@NotNull Project project) {
    ElpSettings.getInstance().setMode(ElpMode.ELP);
    ElpNotifications.notify(project, new Notification(
      ElpNotifications.GROUP, ON_TITLE,
      "ELP answers diagnostics, navigation, find usages and rename in Erlang files. The language services widget in the status " +
      "bar shows whether it is loading the project, ready or failed.", NotificationType.INFORMATION)
      .addAction(ElpNotifications.switchBack())
      .addAction(ElpNotifications.feedback(project)));
  }

  private static void failed(@NotNull Project project, @NotNull String reason) {
    ElpNotifications.notify(project, new Notification(
      ElpNotifications.GROUP, FAILED_TITLE, StringUtil.escapeXmlEntities(reason) + "<br>The plugin keeps answering.",
      NotificationType.WARNING)
      .addAction(NotificationAction.createSimpleExpiring(RETRY, () -> tryElp(project))));
  }
}
