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

import com.intellij.ide.plugins.InstalledPluginsState;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationAction;
import com.intellij.notification.NotificationGroupManager;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.application.ex.ApplicationManagerEx;
import com.intellij.openapi.components.*;
import com.intellij.openapi.diagnostic.ControlFlowException;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.updateSettings.PluginUpdateCheckService;
import com.intellij.openapi.updateSettings.PluginUpdateInfo;
import com.intellij.openapi.updateSettings.impl.PluginDownloader;
import com.intellij.openapi.updateSettings.impl.UpdateSettings;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.concurrency.AppExecutorUtil;
import org.intellij.erlang.statistics.ErlangUsageCollector;
import org.intellij.erlang.statistics.ErlangUsageCounters;
import org.intellij.erlang.statistics.ErlangUsageKey;
import org.intellij.erlang.statistics.ErlangUsageStatistics;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.io.IOException;
import java.lang.ref.WeakReference;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.intellij.erlang.update.ErlangUpdateNotice.*;

/**
 * Tells the user that a new version of the plugin is out, with an action that installs it. It asks the plugin
 * repository through the update service of the IDE, at most once a day and only after an Erlang file was opened.
 * The decisions are in {@link ErlangUpdateNotice}.
 */
@Service(Service.Level.APP)
@State(name = "ErlangUpdateNotice", storages = @Storage(value = "erlangUpdateNotice.xml", roamingType = RoamingType.DISABLED))
public final class ErlangUpdateNoticeService implements PersistentStateComponent<ErlangUpdateNoticeState>, Disposable {
  public static final String NOTIFICATION_GROUP = "Erlang plugin updates";
  private static final Logger LOG = Logger.getInstance(ErlangUpdateNoticeService.class);
  private static final PluginId PLUGIN_ID = PluginId.getId(ErlangUsageCollector.PLUGIN_ID);

  private final ErlangUpdateNotice myNotice = new ErlangUpdateNotice(System::currentTimeMillis, new IdePlatform());
  // one question to the plugin repository at a time
  private final AtomicBoolean myChecking = new AtomicBoolean();
  private volatile boolean myDisposed;
  // the running question and the running install, cancelled when the plugin is unloaded
  private volatile Future<?> myPendingCheck;
  private volatile ProgressIndicator myCheckIndicator;
  private volatile ProgressIndicator myInstallIndicator;
  private volatile int myChecksStartedForTests;

  @NotNull
  public static ErlangUpdateNoticeService getInstance() {
    return ApplicationManager.getApplication().getService(ErlangUpdateNoticeService.class);
  }

  @NotNull
  @Override
  public ErlangUpdateNoticeState getState() {
    return myNotice.getState();
  }

  @Override
  public void loadState(@NotNull ErlangUpdateNoticeState state) {
    myNotice.loadState(state);
  }

  @Override
  public void dispose() {
    myDisposed = true;
    Future<?> check = myPendingCheck;
    if (check != null) check.cancel(false);
    cancel(myCheckIndicator);
    cancel(myInstallIndicator);
  }

  private static void cancel(@Nullable ProgressIndicator indicator) {
    if (indicator != null) indicator.cancel();
  }

  /**
   * An Erlang module or header was opened in an editor of the project. When it is time to, asks the plugin repository
   * on a background thread.
   */
  void erlangFileOpened(@Nullable Project project) {
    if (myDisposed || !myNotice.isCheckDue() || !myChecking.compareAndSet(false, true)) return;
    myChecksStartedForTests++;
    WeakReference<Project> projectRef = new WeakReference<>(project);
    ProgressIndicator indicator = new EmptyProgressIndicator();
    myCheckIndicator = indicator;
    try {
      myPendingCheck = AppExecutorUtil.getAppExecutorService().submit(() -> {
        try {
          if (!myDisposed) ProgressManager.getInstance().runProcess(() -> myNotice.check(new IdeUi(projectRef)), indicator);
        }
        catch (Throwable e) {
          if (!(e instanceof ControlFlowException)) LOG.debug("Erlang plugin update check", e);
        }
        finally {
          myCheckIndicator = null;
          myChecking.set(false);
        }
      });
    }
    catch (RuntimeException e) {
      // the executor refuses tasks while the IDE shuts down
      myChecking.set(false);
      LOG.debug("Erlang plugin update check", e);
    }
  }

  private void startInstall(@NotNull Update update, @NotNull IdeUi ui) {
    if (myDisposed || !ui.myInstalling.compareAndSet(false, true)) return;
    myNotice.installPressed(ui);
    new Task.Backgroundable(ui.project(), "Updating the Erlang plugin to version " + update.version(), true) {
      @Override
      public void run(@NotNull ProgressIndicator indicator) {
        myInstallIndicator = indicator;
        myNotice.install(update, indicator, ui);
      }

      @Override
      public void onCancel() {
        myNotice.installCancelled(update, ui);
      }

      @Override
      public void onThrowable(@NotNull Throwable error) {
        LOG.warn("Erlang plugin update", error);
        myNotice.installFailed(update, ui);
      }

      @Override
      public void onFinished() {
        myInstallIndicator = null;
        ui.myInstalling.set(false);
      }
    }.queue();
  }

  // downloads the new version, checks its signature and installs it for the next start of the IDE
  @NotNull
  private static InstallResult installUpdate(@NotNull PluginDownloader update, @Nullable ProgressIndicator indicator) {
    // the IDE would show its errors in a dialog; the plugin shows them in its notification
    AtomicReference<String> error = new AtomicReference<>();
    PluginDownloader downloader = update.withErrorsConsumer(error::set);
    try {
      if (!downloader.prepareToInstall(indicator)) return InstallResult.failed(error.get());
      downloader.install();
      return InstallResult.INSTALLED;
    }
    catch (IOException e) {
      LOG.debug("Erlang plugin update", e);
      String reason = error.get();
      return InstallResult.failed(reason != null ? reason : StringUtil.escapeXmlEntities(StringUtil.notNullize(e.getMessage())));
    }
  }

  @TestOnly
  int getChecksStartedForTests() {
    return myChecksStartedForTests;
  }

  private static final class IdePlatform implements Platform {
    @Override
    public boolean isAllowedEnvironment() {
      Application application = ApplicationManager.getApplication();
      return application != null &&
             ErlangUpdateNotice.isAllowedEnvironment(ErlangUsageStatistics.pluginVersion(), application.isUnitTestMode(),
                                                     application.isHeadlessEnvironment(), System.getProperty(SWITCH_PROPERTY));
    }

    @Override
    public boolean isAutoUpdateEnabled() {
      return UpdateSettings.getInstance().isPluginsAutoUpdateEnabled();
    }

    @Override
    public boolean isPluginCheckEnabled() {
      return UpdateSettings.getInstance().isPluginsCheckNeeded();
    }

    @Override
    public boolean isUpdatePending() {
      InstalledPluginsState state = InstalledPluginsState.getInstance();
      return state.wasUpdated(PLUGIN_ID) || state.wasUpdatedWithRestart(PLUGIN_ID);
    }

    @Override
    public boolean isRestartCapable() {
      return ApplicationManager.getApplication().isRestartCapable();
    }

    @Override
    public void restart(boolean exitConfirmed) {
      // after the notification action returns: an exit inside the action logs "CE must not be thrown from a dispose()
      // implementation" on 2026.2 and blames the plugin for it
      ApplicationManager.getApplication().invokeLater(() -> ApplicationManagerEx.getApplicationEx().restart(exitConfirmed),
                                                      ModalityState.nonModal());
    }

    @Nullable
    @Override
    public Update findUpdate() {
      PluginUpdateInfo info = PluginUpdateCheckService.getInstance().getPluginUpdate(PLUGIN_ID, ProgressManager.getGlobalProgressIndicator());
      if (info instanceof PluginUpdateInfo.UpdateAvailable available) {
        PluginDownloader downloader = available.getUpdate();
        String version = downloader.getPluginVersion();
        if (version == null) return null;
        return new Update(version, StringUtil.notNullize(ErlangUsageStatistics.pluginVersion()),
                          whatsNew(downloader.getDescriptor().getChangeNotes()),
                          indicator -> installUpdate(downloader, indicator));
      }
      if (info instanceof PluginUpdateInfo.CheckFailed failed) {
        LOG.debug("Erlang plugin update check failed: " + failed.getErrors());
      }
      return null;
    }
  }

  // the notifications and the counters of the project where the Erlang file was opened
  private final class IdeUi implements Ui {
    private final WeakReference<Project> myProject;
    // one install at a time from the same notification
    private final AtomicBoolean myInstalling = new AtomicBoolean();
    private volatile Notification myOffer;

    IdeUi(@NotNull WeakReference<Project> project) {
      myProject = project;
    }

    @Nullable
    Project project() {
      Project project = myProject.get();
      return project == null || project.isDisposed() ? null : project;
    }

    @Override
    public void offer(@NotNull Update update) {
      later(() -> {
        Notification notification = notification(TITLE, offerText(update), NotificationType.INFORMATION)
          .addAction(NotificationAction.createSimple("Install", () -> startInstall(update, this)))
          .addAction(NotificationAction.createSimpleExpiring("Don't show again", () -> myNotice.dontShowAgain(this)));
        myOffer = notification;
        notification.notify(project());
      });
    }

    @Override
    public void installed(@NotNull Update update, boolean offerRestart) {
      later(() -> {
        Notification offer = myOffer;
        if (offer != null) offer.expire();
        Notification notification = notification(INSTALLED_TITLE, installedText(update), NotificationType.INFORMATION);
        if (offerRestart) notification.addAction(NotificationAction.createSimpleExpiring("Restart", myNotice::restartPressed));
        notification.notify(project());
      });
    }

    @Override
    public void notInstalled(@NotNull Update update, @NotNull String contentHtml) {
      later(() -> notification(NOT_INSTALLED_TITLE, contentHtml, NotificationType.ERROR).notify(project()));
    }

    @Override
    public void count(@NotNull ErlangUsageKey key) {
      ErlangUsageCounters.count(project(), key);
    }

    @NotNull
    private static Notification notification(@NotNull String title, @NotNull String content, @NotNull NotificationType type) {
      return NotificationGroupManager.getInstance().getNotificationGroup(NOTIFICATION_GROUP).createNotification(title, content, type);
    }

    private void later(@NotNull Runnable runnable) {
      ApplicationManager.getApplication().invokeLater(() -> {
        if (!myDisposed) runnable.run();
      }, ModalityState.nonModal());
    }
  }
}
