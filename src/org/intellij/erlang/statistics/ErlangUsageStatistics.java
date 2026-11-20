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
  public static final String YES = "Yes, send statistics";
  public static final String LATER = "Later";
  public static final String NO = "No, thanks";
  public static final String WHAT_IS_SENT = "What is sent";
  public static final String SWITCH_PROPERTY = "erlang.usage.statistics";
  private static final Logger LOG = Logger.getInstance(ErlangUsageStatistics.class);
  private static final long TIMER_MINUTES = 45;
  private static final int FILE_COUNT_LIMIT = 1001;
  private static final Key<String> SNAPSHOT_DATE = Key.create("ErlangUsageStatistics.snapshotDate");
  // true when the project has .erl or .hrl files; only such projects count
  private static final Key<Boolean> ERLANG_PROJECT = Key.create("ErlangUsageStatistics.erlangProject");
  private static volatile Boolean ourAllowed;

  private final ErlangUsageCollector myCollector = new ErlangUsageCollector(
    LocalDate::now, ErlangUsageStatistics::appProperties, ErlangUsageCountry::current, ErlangUsageStatistics::isAllowed,
    new ErlangAmplitudeSender());
  private volatile ScheduledFuture<?> myTimer;
  // the notification that waits for an answer; it holds no project
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
   * Whether this IDE session may ask, count, store and send at all. It is decided once per session.
   */
  public static boolean isAllowed() {
    Boolean allowed = ourAllowed;
    if (allowed == null) {
      var application = ApplicationManager.getApplication();
      allowed = application != null && isAllowedEnvironment(pluginVersion(), application.isUnitTestMode(), application.isHeadlessEnvironment(),
                                                           System.getProperty(SWITCH_PROPERTY), System.getenv("DO_NOT_TRACK"));
      ourAllowed = allowed;
    }
    return allowed;
  }

  /**
   * False in tests, in headless mode, in local and CI builds (a SNAPSHOT version, or a version that cannot be read),
   * with -Derlang.usage.statistics=false and with DO_NOT_TRACK=1 (or true).
   */
  static boolean isAllowedEnvironment(@Nullable String pluginVersion,
                                      boolean unitTestMode,
                                      boolean headless,
                                      @Nullable String switchProperty,
                                      @Nullable String doNotTrack) {
    if (unitTestMode || headless) return false;
    if ("false".equalsIgnoreCase(switchProperty)) return false;
    if ("1".equals(doNotTrack) || "true".equalsIgnoreCase(doNotTrack)) return false;
    return pluginVersion != null && !pluginVersion.contains("SNAPSHOT");
  }

  @TestOnly
  public static void setAllowedForTests(@Nullable Boolean allowed) {
    ourAllowed = allowed;
  }

  // only projects with Erlang modules or headers count; a .config or .app file alone may belong to another tool
  public static boolean isErlangProject(@Nullable Project project) {
    return project != null && !project.isDisposed() && Boolean.TRUE.equals(project.getUserData(ERLANG_PROJECT));
  }

  @TestOnly
  public static void setErlangProjectForTests(@NotNull Project project, @Nullable Boolean erlang) {
    project.putUserData(ERLANG_PROJECT, erlang);
  }

  public void setEnabled(boolean enabled) {
    myCollector.setEnabled(enabled);
    Notification notification = myNotification;
    if (notification != null) notification.expire();
    if (enabled) {
      for (Project project : ProjectManager.getInstance().getOpenProjects()) {
        project.putUserData(SNAPSHOT_DATE, null);
        checkProject(project);
      }
    }
  }

  // called once for each opened project
  void projectOpened(@NotNull Project project) {
    if (!isAllowed()) return;
    startTimer();
    checkProject(project);
  }

  // finds out off the EDT, when the indices are ready, whether the project has Erlang files; then records its
  // environment and asks for the consent when it is time to
  private void checkProject(@NotNull Project project) {
    if (!isAllowed() || project.isDisposed() || project.isDefault()) return;
    ReadAction.nonBlocking(() -> countErlangFiles(project))
      .inSmartMode(project)
      .expireWhen(project::isDisposed)
      .submit(AppExecutorUtil.getAppExecutorService())
      .onSuccess(count -> {
        project.putUserData(ERLANG_PROJECT, count > 0);
        if (count == 0) return;
        AppExecutorUtil.getAppExecutorService().execute(() -> recordProject(project, count));
        if (myCollector.shouldAsk()) {
          ApplicationManager.getApplication().invokeLater(this::ask, ModalityState.nonModal(), project.getDisposed());
        }
      });
  }

  /**
   * Asks once. "Later", closing the notification and its expiry all mean: ask again in {@link ErlangUsageCollector#ASK_AGAIN_DAYS}
   * days, at most {@link ErlangUsageCollector#MAX_ASKS} times in total. The ask is counted when it is shown.
   * The notification shows in the project that is open; it keeps no reference to it.
   */
  @RequiresEdt
  private void ask() {
    if (!myCollector.shouldAsk()) return;
    showAsk(myCollector.isReminder());
  }

  @RequiresEdt
  private void showAsk(boolean reminder) {
    Project target = null;
    for (Project open : ProjectManager.getInstance().getOpenProjects()) {
      if (isErlangProject(open)) {
        target = open;
        break;
      }
    }
    if (target == null) return;
    Notification old = myNotification;
    if (old != null) old.expire();
    myCollector.markAsked();
    Notification notification = createAsk(reminder);
    myNotification = notification;
    notification.whenExpired(() -> {
      if (myNotification == notification) myNotification = null;
    });
    notification.notify(target);
  }

  @NotNull
  static Notification createAsk(boolean reminder) {
    Notification notification = NotificationGroupManager.getInstance().getNotificationGroup(NOTIFICATION_GROUP)
      .createNotification(TITLE, reminder ? REMINDER_TEXT : TEXT, NotificationType.INFORMATION)
      .setSuggestionType(true);
    // the first action is the primary one; the balloon may fold the last one into "More"
    notification.addAction(NotificationAction.createSimpleExpiring(YES, () -> getInstance().setEnabled(true)));
    notification.addAction(NotificationAction.createSimpleExpiring(LATER, () -> {}));
    notification.addAction(NotificationAction.createSimpleExpiring(NO, () -> getInstance().setEnabled(false)));
    // keeps the notification, so that the user can still answer after reading the settings
    notification.addAction(NotificationAction.create(WHAT_IS_SENT, (event, n) -> {
      Project project = event.getProject();
      if (project != null && !project.isDisposed()) {
        ShowSettingsUtil.getInstance().showSettingsDialog(project, ErlangExternalToolsConfigurable.class);
      }
    }));
    return notification;
  }

  @TestOnly
  @Nullable
  Notification getNotificationForTests() {
    return myNotification;
  }

  @TestOnly
  void showAskForTests(boolean reminder) {
    showAsk(reminder);
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
        if (!LocalDate.now().toString().equals(project.getUserData(SNAPSHOT_DATE))) checkProject(project);
      }
      myCollector.sendPending();
    }
    catch (Exception e) {
      // an exception would stop the timer
      LOG.debug("Erlang usage statistics", e);
    }
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

  // the environment of the project for today, once a day
  @RequiresBackgroundThread
  private void recordProject(@NotNull Project project, int erlangFiles) {
    if (!myCollector.isCounting() || project.isDisposed()) return;
    String today = LocalDate.now().toString();
    if (today.equals(project.getUserData(SNAPSHOT_DATE))) return;
    project.putUserData(SNAPSHOT_DATE, today);
    myCollector.setProperty(ErlangUsageProperty.PROJECT_SIZE, sizeBucket(erlangFiles));
    boolean sdkConfigured = ErlangSdkType.getSdkPath(project) != null;
    myCollector.setProperty(ErlangUsageProperty.SDK_CONFIGURED, String.valueOf(sdkConfigured));
    myCollector.setProperty(ErlangUsageProperty.OTP, otp(project, sdkConfigured));
    myCollector.setProperty(ErlangUsageProperty.REBAR, ReadAction.computeBlocking(() -> rebar(project)));
    for (String inspection : ReadAction.computeBlocking(() -> changedInspections(project))) {
      myCollector.setProperty(ErlangUsageProperty.CHANGED_INSPECTIONS, inspection);
    }
  }

  // only a release that is already known: the statistics never start erl, also not in a project that is not trusted
  @NotNull
  static String otp(@NotNull Project project, boolean sdkConfigured) {
    if (!sdkConfigured) return "none";
    ErlangSdkRelease release = ErlangSdkType.getKnownProjectSdkRelease(project);
    return release == null ? "unknown" : otpMajor(release);
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
      .filter(ep -> ep.getPluginDescriptor() != null && ErlangUsageCollector.PLUGIN_ID.equals(ep.getPluginDescriptor().getPluginId().getIdString()))
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
