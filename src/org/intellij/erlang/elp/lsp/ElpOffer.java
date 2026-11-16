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
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.util.concurrency.AppExecutorUtil;
import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import com.intellij.util.concurrency.annotations.RequiresEdt;
import com.intellij.util.concurrency.annotations.RequiresReadLock;
import com.intellij.util.text.VersionComparatorUtil;
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

import javax.swing.*;
import java.io.File;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.function.BooleanSupplier;
import java.util.function.LongSupplier;

/**
 * The offer to move to ELP, a dialog shown when an Erlang file is opened in the Plugin position
 * and ELP would work for the project: {@link ElpPreflight} finds nothing against it, and a server is set or can be downloaded.
 * It waits for a moment when the user can read it: the project is indexed, its frame is active and no other dialog is open;
 * otherwise the next opened file tries again. Nothing switches by itself: "Try ELP" downloads the server when needed and only
 * then sets the ELP position; "Switch back to the plugin" undoes it. After "Not now" the offer comes again in a newer plugin
 * version, and not sooner than 30 days after the answer; after "Don't ask again", or a switch from ELP back to the plugin, it
 * never comes again. The {@value #PROPERTY} system property set to {@code false} turns the offer off.
 */
final class ElpOffer {
  private static final Logger LOG = Logger.getInstance(ElpOffer.class);
  static final String PROPERTY = "erlang.elp.offer";
  static final String TRY = "Try ELP";
  static final String NOT_NOW = "Not now";
  static final String NEVER = "Don't ask again";
  static final String ON_TITLE = "ELP is on";
  static final String FAILED_TITLE = "ELP is not on";
  static final String RETRY = "Retry";

  /** A project that ELP does not fit is checked again after this time at the earliest, when another file is opened. */
  private static final long RECHECK_MS = 60_000;
  static final Key<Long> LAST_CHECK = Key.create("erlang.elp.offer.lastCheck");
  /** Set on a project that the check found ELP does not fit; it is not checked again in this IDE session. */
  static final Key<Boolean> NOT_ELIGIBLE = Key.create("erlang.elp.offer.notEligible");
  /** Set while a check of the project waits for the end of indexing or for its frame; at most one waits per project. */
  static final Key<Boolean> PENDING = Key.create("erlang.elp.offer.pending");
  private static volatile boolean ourEnabledInTests;
  /** A dialog that cannot be shown now is looked at again this often, and this many times: ten minutes. */
  private static final long RETRY_MS = 5_000;
  private static final int RETRIES = 120;
  private static volatile long ourRetryMs = RETRY_MS;
  private static volatile ElpPreflight.Result ourPreflightInTests;
  private static volatile BooleanSupplier ourMacOs = () -> SystemInfo.isMac;
  /** After "Not now" the offer comes again in a newer plugin version, and not sooner than this after the answer. */
  static final int ASK_AGAIN_DAYS = 30;
  static final long ASK_AGAIN_MS = TimeUnit.DAYS.toMillis(ASK_AGAIN_DAYS);
  private static volatile LongSupplier ourClock = System::currentTimeMillis;

  /**
   * The answers of the dialog; each does what the action of the same name on the balloon did before. {@link #TRY_AND_NEVER} is
   * "Try ELP" with "Don't ask again" selected.
   */
  enum Choice {TRY, NOT_NOW, NEVER, TRY_AND_NEVER}

  /** Shows the offer and returns the answer; tests replace it, so that no window opens. */
  interface Asker {
    @NotNull
    @RequiresEdt
    Choice ask(@NotNull Project project, @NotNull List<ElpOfferDialog.Block> content);
  }

  static final Asker DIALOG = (project, content) -> {
    ElpOfferDialog dialog = new ElpOfferDialog(project, content);
    dialog.show();
    return dialog.choice();
  };
  private static volatile Asker ourAsker = DIALOG;
  private static volatile BooleanSupplier ourFrameActiveInTests;

  private ElpOffer() {
  }

  /**
   * Lets tests answer the offer with {@code asker} and decide with {@code frameActive} whether the frame of the project is
   * active (tests have no frame), until {@code parent} is disposed.
   */
  @TestOnly
  static void setUpForTests(@NotNull Asker asker, @NotNull BooleanSupplier frameActive, @NotNull Disposable parent) {
    ourAsker = asker;
    ourFrameActiveInTests = frameActive;
    ourRetryMs = 10;
    Disposer.register(parent, () -> {
      ourAsker = DIALOG;
      ourFrameActiveInTests = null;
      ourRetryMs = RETRY_MS;
    });
  }

  /** A test has ended, and a check that it started must not open the real dialog. */
  private static boolean testsAreOver() {
    return ApplicationManager.getApplication().isUnitTestMode() && ourAsker == DIALOG;
  }

  /** @return {@code false} when a check of the project already waits */
  private static boolean markPending(@NotNull Project project) {
    synchronized (LAST_CHECK) {
      if (project.getUserData(PENDING) != null) return false;
      project.putUserData(PENDING, Boolean.TRUE);
      return true;
    }
  }

  private static void clearPending(@NotNull Project project) {
    synchronized (LAST_CHECK) {
      project.putUserData(PENDING, null);
    }
  }

  /**
   * Runs {@code offer} again when the user can read the dialog: at the end of indexing, or, for an inactive frame or another
   * dialog, at the first of {@value #RETRIES} looks every {@value #RETRY_MS} ms that finds the frame active. A wait that gives
   * up leaves the check to the next opened file.
   */
  @RequiresEdt
  private static void retryLater(@NotNull Project project, @NotNull Runnable offer) {
    if (!markPending(project)) return;
    if (DumbService.isDumb(project)) {
      DumbService.getInstance(project).runWhenSmart(() -> {
        clearPending(project);
        if (!project.isDisposed()) offer.run();
      });
    }
    else {
      lookAgain(project, offer, RETRIES);
    }
  }

  private static void lookAgain(@NotNull Project project, @NotNull Runnable offer, int looksLeft) {
    AppExecutorUtil.getAppScheduledExecutorService().schedule(() -> ApplicationManager.getApplication().invokeLater(() -> {
      if (testsAreOver()) return;
      if (whyNotNow(project) == null) {
        clearPending(project);
        offer.run();
      }
      else if (looksLeft > 1) {
        lookAgain(project, offer, looksLeft - 1);
      }
      else {
        clearPending(project);
        LOG.info("The ELP offer waits for the next opened file");
      }
    }, ModalityState.nonModal(), project.getDisposed()), ourRetryMs, TimeUnit.MILLISECONDS);
  }

  /** Makes the check of a project in tests find {@code result} instead of running the preflight, until {@code parent} is disposed. */
  @TestOnly
  static void preflightInTests(@NotNull ElpPreflight.Result result, @NotNull Disposable parent) {
    ourPreflightInTests = result;
    Disposer.register(parent, () -> ourPreflightInTests = null);
  }

  /** Makes the offer read the time from {@code clock}, in milliseconds since the epoch, until {@code parent} is disposed. */
  @TestOnly
  static void clockInTests(@NotNull LongSupplier clock, @NotNull Disposable parent) {
    ourClock = clock;
    Disposer.register(parent, () -> ourClock = System::currentTimeMillis);
  }

  /** Makes the offer think that the IDE runs on macOS, or not, until {@code parent} is disposed. */
  @TestOnly
  static void macOsInTests(boolean macOs, @NotNull Disposable parent) {
    ourMacOs = () -> macOs;
    Disposer.register(parent, () -> ourMacOs = () -> SystemInfo.isMac);
  }

  /** Lets {@link #fileOpened} check the project in tests until {@code parent} is disposed. */
  @TestOnly
  static void enableInTests(@NotNull Disposable parent) {
    ourEnabledInTests = true;
    Disposer.register(parent, () -> ourEnabledInTests = false);
  }

  /**
   * Whether the offer may be shown for {@code pluginVersion}: the IDE runs on macOS, the offer is not switched off, the engine is
   * the plugin and not forced by a system property, the user did not choose "Don't ask again", and the offer was not shown yet or
   * was last shown for an older version {@value #ASK_AGAIN_DAYS} days ago or earlier. Touches neither the file system nor the
   * project model.
   */
  static boolean shouldAsk(@NotNull String pluginVersion) {
    return whyNotAsk(pluginVersion) == null;
  }

  /** @return why {@link #shouldAsk} says no, for the log; {@code null} when the offer may be shown */
  @Nullable
  static String whyNotAsk(@NotNull String pluginVersion) {
    if (!ourMacOs.getAsBoolean()) return "the IDE does not run on macOS";
    if (Boolean.FALSE.equals(ElpConfiguration.parseBoolean(System.getProperty(PROPERTY)))) return "-D" + PROPERTY + "=false";
    String forcedMode = System.getProperty(ElpConfiguration.MODE_PROPERTY);
    if (ElpMode.parse(forcedMode) != null) return "the engine is forced by -D" + ElpConfiguration.MODE_PROPERTY + "=" + forcedMode;
    ElpSettings settings = ElpSettings.getInstance();
    if (settings.getStoredMode() != ElpMode.PLUGIN) return "the engine is " + settings.getStoredMode();
    if (settings.isOfferNeverAsk()) return "the user chose \"" + NEVER + "\"";
    String shownForVersion = settings.getOfferShownForVersion();
    if (!shownForVersion.isEmpty()) {
      if (VersionComparatorUtil.compare(pluginVersion, shownForVersion) <= 0) {
        return "already shown for version " + shownForVersion + " (plugin version " + pluginVersion + ")";
      }
      long since = ourClock.getAsLong() - settings.getOfferAnsweredTime();
      if (since < ASK_AGAIN_MS) {
        return "answered " + TimeUnit.MILLISECONDS.toDays(since) + " days ago; asked again after " + ASK_AGAIN_DAYS + " days";
      }
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

  /**
   * Called, with read access, for every Erlang file the user opens; checks the project off the EDT, at most once a minute per
   * project, and not again in this IDE session once ELP did not fit it.
   */
  @RequiresReadLock
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
    // The check runs the project's rebar3, whose path is a project setting.
    if (!ElpPreflight.isTrusted(project)) {
      LOG.debug("No ELP offer check: the project is not trusted");
      return;
    }
    if (project.getUserData(NOT_ELIGIBLE) != null) {
      LOG.debug("No ELP offer check: ELP did not fit the project at the last check");
      return;
    }
    // The dialog would wait for the end of indexing anyway; the check is not used up on a file opened while the project loads.
    if (DumbService.isDumb(project)) {
      // The restored editors open while the project is indexed, and nothing opens a file afterwards: check this one later.
      if (markPending(project)) {
        LOG.debug("The ELP offer check of " + file.getName() + " waits for the end of indexing");
        DumbService.getInstance(project).runWhenSmart(() -> {
          clearPending(project);
          if (project.isDisposed() || !file.isValid() || testsAreOver()) return;
          ReadAction.runBlocking(() -> fileOpened(project, file));
        });
      }
      else {
        LOG.debug("No ELP offer check: the project is being indexed, and a check already waits");
      }
      return;
    }
    // Before the once-a-minute limit: the files of the last session open in a burst, and the first one may be outside the
    // project, such as a module of the SDK; it must not use up the check for the project files that follow.
    VirtualFile root = ElpFiles.findWorkspaceRoot(project, file);
    if (root == null) {
      LOG.debug("No ELP offer check: no workspace root for " + file.getName());
      return;
    }
    long now = System.currentTimeMillis();
    synchronized (LAST_CHECK) {
      Long last = project.getUserData(LAST_CHECK);
      if (last != null && now - last < RECHECK_MS) {
        LOG.debug("No ELP offer check: the project was checked less than a minute ago");
        return;
      }
      project.putUserData(LAST_CHECK, now);
    }
    ApplicationManager.getApplication().executeOnPooledThread(() -> {
      try {
        check(project, root, version, now);
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
  private static void check(@NotNull Project project, @NotNull VirtualFile root, @NotNull String version, long started) {
    if (project.isDisposed()) return;
    ElpPreflight.Result result = ourPreflightInTests;
    if (result == null) {
      ElpPreflight.Facts facts = ElpPreflight.getInstance(project).gatherFacts();
      ElpPreflight.Layout layout = ReadAction.computeBlocking(() -> root.isValid() ? ElpPreflight.Layout.of(root) : null);
      result = ElpPreflight.evaluate(facts, layout);
      LOG.debug("ELP offer check: OTP " + facts.otpRelease() + ", rebar3 " + facts.rebar3Version() + ", layout " + layout);
    }
    ElpAvailability availability = ElpDistribution.getInstance().availability(project);
    LOG.debug("ELP offer check: problems " + result.reasons() + ", availability " + availability.kind() +
              (availability.choice().reason() != null ? " (" + availability.choice().message() + ")" : ""));
    if (!isEligible(result, availability)) {
      synchronized (LAST_CHECK) {
        // Unless the check was started over in the meantime, as the tests do on a project that they share.
        if (Long.valueOf(started).equals(project.getUserData(LAST_CHECK))) project.putUserData(NOT_ELIGIBLE, Boolean.TRUE);
      }
    }
    ElpPreflight.Result checked = result;
    ApplicationManager.getApplication().invokeLater(() -> {
      // A check that a test started may end after the test, whose answers to the dialog are gone by then.
      if (ApplicationManager.getApplication().isUnitTestMode() && !ourEnabledInTests) return;
      offerIfEligible(project, version, checked, availability);
    }, ModalityState.nonModal(), project.getDisposed());
  }

  /**
   * Shows the offer and remembers that it was shown for {@code pluginVersion}, and when, if it may be shown, ELP would work and the user
   * can read it now, see {@link #whyNotNow}. A project that ELP does not fit leaves the version unmarked, so that another
   * project can get the offer; an offer that has to wait leaves it unmarked too, and the next opened file checks again.
   *
   * @return whether the offer was shown
   */
  @RequiresEdt
  static boolean offerIfEligible(@NotNull Project project, @NotNull String pluginVersion, @NotNull ElpPreflight.Result result,
                                 @NotNull ElpAvailability availability) {
    if (project.isDisposed() || testsAreOver()) return false;
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
    String notNow = whyNotNow(project);
    if (notNow != null) {
      LOG.info("The ELP offer waits: " + notNow);
      // Also an opened file checks again, should the wait below give up.
      synchronized (LAST_CHECK) {
        project.putUserData(LAST_CHECK, null);
      }
      retryLater(project, () -> offerIfEligible(project, pluginVersion, result, availability));
      return false;
    }
    LOG.info("ELP offer shown for plugin version " + pluginVersion + ", ELP server: " + availability.kind());
    ElpSettings settings = ElpSettings.getInstance();
    settings.setOfferShownForVersion(pluginVersion);
    settings.setOfferAnsweredTime(ourClock.getAsLong());
    Choice choice = ourAsker.ask(project, ElpOfferDialog.content(availability));
    LOG.info("ELP offer answered: " + choice);
    switch (choice) {
      case TRY -> tryElp(project);
      case NOT_NOW -> {
        settings.setOfferShownForVersion(pluginVersion);
        settings.setOfferAnsweredTime(ourClock.getAsLong());
      }
      case NEVER -> settings.setOfferNeverAsk(true);
      case TRY_AND_NEVER -> {
        settings.setOfferNeverAsk(true);
        tryElp(project);
      }
    }
    return true;
  }

  /**
   * @return why the user cannot read the offer now, {@code null} when the dialog may open: the project is indexed, no other
   * dialog is open and the frame of the project is the active window
   */
  @Nullable
  @RequiresEdt
  static String whyNotNow(@NotNull Project project) {
    if (DumbService.isDumb(project)) return "the project is being indexed";
    if (!ModalityState.nonModal().equals(ModalityState.current())) return "another dialog is open";
    BooleanSupplier frameActiveInTests = ourFrameActiveInTests;
    boolean frameActive;
    if (frameActiveInTests != null) {
      frameActive = frameActiveInTests.getAsBoolean();
    }
    else {
      JFrame frame = WindowManager.getInstance().getFrame(project);
      frameActive = frame != null && frame.isActive();
    }
    return frameActive ? null : "the frame of the project is not the active window";
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
