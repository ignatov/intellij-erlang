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

package org.intellij.erlang.statistics;

import com.intellij.codeInsight.daemon.HighlightDisplayKey;
import com.intellij.codeInsight.hints.declarative.DeclarativeInlayHintsSettings;
import com.intellij.codeInspection.LocalInspectionEP;
import com.intellij.codeInspection.ex.InspectionProfileImpl;
import com.intellij.ide.plugins.cl.PluginAwareClassLoader;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationAction;
import com.intellij.notification.NotificationGroupManager;
import com.intellij.notification.NotificationListener;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationInfo;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.components.*;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.options.ShowSettingsUtil;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.project.ProjectUtil;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.profile.codeInspection.InspectionProjectProfileManager;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.concurrency.AppExecutorUtil;
import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import com.intellij.util.concurrency.annotations.RequiresEdt;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.hints.ErlangParameterHintsProvider;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.sdk.ErlangSystemUtil;
import org.intellij.erlang.settings.ErlangExternalToolsConfigurable;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import javax.swing.event.HyperlinkEvent;
import java.time.LocalDate;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

/**
 * The opt-in usage statistics of the plugin: one summary of counts per day, sent to Amplitude.
 * See PRIVACY.md at the root of the repository for what is sent.
 */
@Service(Service.Level.APP)
@State(name = "ErlangUsageStatistics", storages = @Storage(value = "erlangUsageStatistics.xml", roamingType = RoamingType.DISABLED))
public final class ErlangUsageStatistics implements PersistentStateComponent<ErlangUsageStatisticsState>, Disposable {
  public static final String NOTIFICATION_GROUP = "Erlang Usage Statistics";
  public static final String PRIVACY_URL = "https://github.com/ignatov/intellij-erlang/blob/master/PRIVACY.md";
  public static final String TITLE = "Help improve the Erlang plugin";
  public static final String TEXT = "Hi, this is Sergey, the author of the Erlang plugin. " +
                                    "I want to make the plugin better, and for that I need to know which features people use. " +
                                    "May the plugin send anonymous usage counts once a day? It never sends code, file names or paths.";
  public static final String REMINDER_TEXT = "Hi, it's Sergey again, the author of the Erlang plugin. " +
                                             "I still want to make the plugin better, and it would help a lot to know which features people use. " +
                                             "May the plugin send anonymous usage counts once a day? It never sends code, file names or paths.";
  public static final String WHAT_IS_SENT_LINK = "<a href=\"settings\">See what is sent</a>";
  public static final String YES = "Yes, send statistics";
  public static final String LATER = "Later";
  public static final String NO = "No, thanks";
  private static final Logger LOG = Logger.getInstance(ErlangUsageStatistics.class);
  private static final long TIMER_MINUTES = 45;
  private static final int FILE_COUNT_LIMIT = 1001;
  private static final Key<String> SNAPSHOT_DATE = Key.create("ErlangUsageStatistics.snapshotDate");

  private final ErlangUsageCollector myCollector =
    new ErlangUsageCollector(LocalDate::now, ErlangUsageStatistics::appProperties, ErlangUsageCountry::current, new ErlangAmplitudeSender());
  private volatile ScheduledFuture<?> myTimer;
  private volatile Notification myNotification;

  @NotNull
  public static ErlangUsageStatistics getInstance() {
    return ApplicationManager.getApplication().getService(ErlangUsageStatistics.class);
  }

  @NotNull
  public ErlangUsageCollector getCollector() {
    return myCollector;
  }

  @NotNull
  @Override
  public ErlangUsageStatisticsState getState() {
    return myCollector.getState();
  }

  @Override
  public void loadState(@NotNull ErlangUsageStatisticsState state) {
    myCollector.loadState(state);
  }

  @Override
  public void dispose() {
    ScheduledFuture<?> timer = myTimer;
    if (timer != null) timer.cancel(false);
  }

  /**
   * False in tests, in headless mode, in local and CI builds (a SNAPSHOT version), with -Derlang.usage.statistics=false
   * and with DO_NOT_TRACK=1. Then the plugin neither asks nor sends.
   */
  public static boolean isSendingAllowed() {
    var application = ApplicationManager.getApplication();
    if (application == null || application.isUnitTestMode() || application.isHeadlessEnvironment()) return false;
    if ("false".equalsIgnoreCase(System.getProperty("erlang.usage.statistics"))) return false;
    String doNotTrack = System.getenv("DO_NOT_TRACK");
    if ("1".equals(doNotTrack) || "true".equalsIgnoreCase(doNotTrack)) return false;
    String version = pluginVersion();
    return version != null && !version.contains("SNAPSHOT");
  }

  public void setEnabled(boolean enabled) {
    myCollector.setEnabled(enabled);
    Notification notification = myNotification;
    if (notification != null) notification.expire();
    if (enabled) {
      for (Project project : ProjectManager.getInstance().getOpenProjects()) {
        project.putUserData(SNAPSHOT_DATE, null);
        recordProjectLater(project);
      }
    }
  }

  // called once for each opened project
  void projectOpened(@NotNull Project project) {
    if (myCollector.isCounting()) recordProjectLater(project);
    if (!isSendingAllowed()) return;
    startTimer();
    if (!myCollector.shouldAsk()) return;
    ReadAction.nonBlocking(() -> FileTypeIndex.containsFileOfType(ErlangFileType.MODULE, GlobalSearchScope.projectScope(project)))
      .inSmartMode(project)
      .expireWhen(project::isDisposed)
      .finishOnUiThread(ModalityState.nonModal(), hasErlangFiles -> {
        if (hasErlangFiles) ask(project);
      })
      .submit(AppExecutorUtil.getAppExecutorService());
  }

  /**
   * Asks once. "Later", closing the notification and its expiry all mean: ask again in {@link ErlangUsageCollector#ASK_AGAIN_DAYS}
   * days, at most {@link ErlangUsageCollector#MAX_ASKS} times in total. The ask is counted when it is shown.
   */
  @RequiresEdt
  private void ask(@NotNull Project project) {
    if (!myCollector.shouldAsk() || project.isDisposed()) return;
    boolean reminder = myCollector.isReminder();
    myCollector.markAsked();
    Notification notification = NotificationGroupManager.getInstance().getNotificationGroup(NOTIFICATION_GROUP)
      .createNotification(TITLE, notificationText(reminder), NotificationType.INFORMATION)
      .setSuggestionType(true);
    // the first action is the primary one; a balloon shows up to three actions before it folds them into "More"
    notification.addAction(NotificationAction.createSimpleExpiring(YES, () -> setEnabled(true)));
    notification.addAction(NotificationAction.createSimpleExpiring(LATER, () -> {}));
    notification.addAction(NotificationAction.createSimpleExpiring(NO, () -> setEnabled(false)));
    notification.setListener(new NotificationListener.Adapter() {
      @Override
      protected void hyperlinkActivated(@NotNull Notification notification, @NotNull HyperlinkEvent event) {
        ShowSettingsUtil.getInstance().showSettingsDialog(project, ErlangExternalToolsConfigurable.class);
      }
    });
    myNotification = notification;
    notification.notify(project);
  }

  @NotNull
  static String notificationText(boolean reminder) {
    return (reminder ? REMINDER_TEXT : TEXT) + " " + WHAT_IS_SENT_LINK;
  }

  private synchronized void startTimer() {
    if (myTimer != null) return;
    myTimer = AppExecutorUtil.getAppScheduledExecutorService().scheduleWithFixedDelay(this::tick, 1, TIMER_MINUTES, TimeUnit.MINUTES);
  }

  @RequiresBackgroundThread
  private void tick() {
    try {
      myCollector.flush();
      if (!myCollector.isCounting()) return;
      for (Project project : ProjectManager.getInstance().getOpenProjects()) {
        recordProjectLater(project);
      }
      if (isSendingAllowed()) myCollector.sendPending();
    }
    catch (Exception e) {
      // an exception would stop the timer
      LOG.debug("Erlang usage statistics", e);
    }
  }

  // the environment of the project for today, computed once a day off the EDT when the indices are ready
  private void recordProjectLater(@NotNull Project project) {
    if (project.isDisposed() || project.isDefault()) return;
    String today = LocalDate.now().toString();
    if (today.equals(project.getUserData(SNAPSHOT_DATE))) return;
    project.putUserData(SNAPSHOT_DATE, today);
    ReadAction.nonBlocking(() -> countErlangFiles(project))
      .inSmartMode(project)
      .expireWhen(project::isDisposed)
      .submit(AppExecutorUtil.getAppExecutorService())
      .onSuccess(count -> {
        // the SDK release check can start a process, so never on the EDT, even when the promise is already done
        if (count > 0) AppExecutorUtil.getAppExecutorService().execute(() -> {
          if (!project.isDisposed()) recordProject(project, count);
        });
      });
  }

  private static int countErlangFiles(@NotNull Project project) {
    int[] count = {0};
    GlobalSearchScope scope = GlobalSearchScope.projectScope(project);
    for (ErlangFileType type : List.of(ErlangFileType.MODULE, ErlangFileType.HEADER)) {
      FileTypeIndex.processFiles(type, file -> {
        ProgressManager.checkCanceled();
        return ++count[0] < FILE_COUNT_LIMIT;
      }, scope);
    }
    return count[0];
  }

  @RequiresBackgroundThread
  private void recordProject(@NotNull Project project, int erlangFiles) {
    myCollector.setProperty(ErlangUsageProperty.PROJECT_SIZE, sizeBucket(erlangFiles));
    boolean sdkConfigured = ErlangSdkType.getSdkPath(project) != null;
    myCollector.setProperty(ErlangUsageProperty.SDK_CONFIGURED, String.valueOf(sdkConfigured));
    ErlangSdkRelease release = sdkConfigured ? ErlangSdkType.getProjectSdkRelease(project) : null;
    myCollector.setProperty(ErlangUsageProperty.OTP, !sdkConfigured ? "none" : release == null ? "unknown" : otpMajor(release));
    myCollector.setProperty(ErlangUsageProperty.REBAR, ReadAction.computeBlocking(() -> rebar(project)));
    for (String inspection : ReadAction.computeBlocking(() -> changedInspections(project))) {
      myCollector.setProperty(ErlangUsageProperty.CHANGED_INSPECTIONS, inspection);
    }
  }

  @NotNull
  static String sizeBucket(int erlangFiles) {
    return erlangFiles <= 10 ? "up_to_10" : erlangFiles <= 100 ? "up_to_100" : erlangFiles <= 1000 ? "up_to_1000" : "over_1000";
  }

  // "R15" for R15B02, "27" for 27
  @NotNull
  static String otpMajor(@NotNull ErlangSdkRelease release) {
    String otp = release.getOtpRelease();
    int start = otp.startsWith("R") ? 1 : 0;
    int end = start;
    while (end < otp.length() && Character.isDigit(otp.charAt(end))) end++;
    return end > start ? otp.substring(0, end) : "unknown";
  }

  // the rebar of the project: by the configured path, else "unknown" when the project has a rebar.config
  @NotNull
  private static String rebar(@NotNull Project project) {
    RebarSettings settings = RebarSettings.getInstance(project);
    if (!StringUtil.isEmptyOrSpaces(settings.getRebarPath())) return settings.isRebar3() ? "rebar3" : "rebar2";
    VirtualFile baseDir = ProjectUtil.guessProjectDir(project);
    return baseDir != null && baseDir.findChild("rebar.config") != null ? "unknown" : "none";
  }

  // the short names of this plugin's inspections that are on or off unlike their default, in the current profile
  @NotNull
  static List<String> changedInspections(@NotNull Project project) {
    InspectionProfileImpl profile = InspectionProjectProfileManager.getInstance(project).getCurrentProfile();
    return LocalInspectionEP.LOCAL_INSPECTION.getExtensionList().stream()
      .filter(ep -> ep.implementationClass != null && ep.implementationClass.startsWith("org.intellij.erlang."))
      .filter(ep -> {
        HighlightDisplayKey key = HighlightDisplayKey.find(ep.getShortName());
        return key != null && profile.isToolEnabled(key, null) != ep.enabledByDefault;
      })
      .map(LocalInspectionEP::getShortName)
      .limit(ErlangUsageProperty.MAX_LIST_SIZE)
      .toList();
  }

  @NotNull
  private static Map<ErlangUsageProperty, String> appProperties() {
    Map<ErlangUsageProperty, String> properties = new EnumMap<>(ErlangUsageProperty.class);
    String version = pluginVersion();
    if (version != null) properties.put(ErlangUsageProperty.PLUGIN_VERSION, version);
    var build = ApplicationInfo.getInstance().getBuild();
    properties.put(ErlangUsageProperty.IDE_PRODUCT, build.getProductCode());
    properties.put(ErlangUsageProperty.IDE_BASELINE, String.valueOf(build.getBaselineVersion()));
    properties.put(ErlangUsageProperty.SMALL_IDE, String.valueOf(ErlangSystemUtil.isSmallIde()));
    properties.put(ErlangUsageProperty.OS, SystemInfo.isMac ? "mac" : SystemInfo.isWindows ? "windows" : SystemInfo.isLinux ? "linux" : "other");
    properties.put(ErlangUsageProperty.INLAY_PARAMETER_HINTS, String.valueOf(parameterHintsEnabled()));
    return properties;
  }

  private static boolean parameterHintsEnabled() {
    // null when the user never changed it; the provider is on by default
    Boolean enabled = DeclarativeInlayHintsSettings.Companion.getInstance().isProviderEnabled(ErlangParameterHintsProvider.PROVIDER_ID);
    return enabled == null || enabled;
  }

  @Nullable
  static String pluginVersion() {
    return ErlangUsageStatistics.class.getClassLoader() instanceof PluginAwareClassLoader loader
           ? loader.getPluginDescriptor().getVersion()
           : null;
  }

  @TestOnly
  public void resetForTests() {
    myCollector.loadState(new ErlangUsageStatisticsState());
  }
}
