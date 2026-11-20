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
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationInfo;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.components.*;
import com.intellij.openapi.diagnostic.ControlFlowException;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.project.ProjectUtil;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.UserDataHolderEx;
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
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.lang.ref.WeakReference;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Supplier;

/**
 * The opt-in usage statistics of the plugin: one summary of counts per day, sent to Amplitude.
 * See PRIVACY.md at the root of the repository for what is sent.
 */
@Service(Service.Level.APP)
@State(name = "ErlangUsageStatistics", storages = @Storage(value = "erlangUsageStatistics.xml", roamingType = RoamingType.DISABLED))
public final class ErlangUsageStatistics implements PersistentStateComponent<ErlangUsageStatisticsState>, Disposable {
  public static final String PRIVACY_URL = "https://github.com/ignatov/intellij-erlang/blob/master/PRIVACY.md";
  public static final String TITLE = "Help improve the Erlang plugin";
  public static final String QUESTION = "May the plugin send anonymous usage statistics?";
  // HTML: who asks on the first line, what for on the second
  public static final String TEXT = "Hi, I'm Sergey, the author of the Erlang plugin.<br>" +
                                    "Thank you for using it. To make it better, I would like to know which features people use.";
  public static final String REMINDER_TEXT = "Hi, it's Sergey again, the author of the Erlang plugin.<br>" +
                                             "Thank you for using it. It would help a lot to know which features people use.";
  public static final String SWITCH_PROPERTY = "erlang.usage.statistics";
  private static final Logger LOG = Logger.getInstance(ErlangUsageStatistics.class);
  private static final long TIMER_MINUTES = 45;
  private static final int FILE_COUNT_LIMIT = 1001;
  private static final Key<String> SNAPSHOT_DATE = Key.create("ErlangUsageStatistics.snapshotDate");
  // true when the project has .erl or .hrl files; only such projects count
  private static final Key<Boolean> ERLANG_PROJECT = Key.create("ErlangUsageStatistics.erlangProject");
  // when the project was first found to have Erlang files with the indices ready
  private static final Key<Long> READY_SINCE = Key.create("ErlangUsageStatistics.readySince");
  static final long ASK_DELAY_MS = 60_000;
  private static final long IDLE_MS = 5_000;
  private static final long RETRY_MS = 2_500;
  // 10 minutes of checks every 2.5 seconds
  static final int MAX_RETRIES = 240;
  private static volatile Boolean ourAllowed;

  private final ErlangUsageCollector myCollector = new ErlangUsageCollector(
    LocalDate::now, ErlangUsageStatistics::appProperties, ErlangUsageCountry::current, ErlangUsageStatistics::isAllowed,
    new ErlangAmplitudeSender());
  private volatile ScheduledFuture<?> myTimer;
  private volatile ErlangUsageAskUi myAskUi = ErlangUsageAskDialog.UI;
  // at most one question per IDE session, in whichever project
  private volatile boolean myAskedInSession;
  // the checks of the current ask chain
  private volatile int myRetries;
  private volatile int myScheduledForTests;
  private volatile boolean myDisposed;
  // true from askLater until the chain of checks it starts asks or gives up; one chain at a time
  private final AtomicBoolean myChainAlive = new AtomicBoolean();
  // the next check of the chain, cancelled when the plugin is unloaded
  private volatile ScheduledFuture<?> myPendingAsk;
  // null: the scheduled executor of the IDE
  private volatile AskScheduler myTestScheduler;
  // null: the open projects of the IDE
  private volatile Supplier<Project[]> myTestOpenProjects;

  // runs a check of the ask chain after a delay; tests drive the checks themselves
  interface AskScheduler {
    @NotNull
    ScheduledFuture<?> schedule(@NotNull Runnable task, long delayMillis);
  }

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
    myDisposed = true;
    ErlangUsageIdle.uninstall();
    ScheduledFuture<?> timer = myTimer;
    if (timer != null) timer.cancel(false);
    ScheduledFuture<?> ask = myPendingAsk;
    if (ask != null) ask.cancel(false);
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
    project.putUserData(READY_SINCE, Boolean.TRUE.equals(erlang) ? System.currentTimeMillis() - ASK_DELAY_MS - 1 : null);
  }

  @TestOnly
  static void setReadySinceForTests(@NotNull Project project, long time) {
    project.putUserData(READY_SINCE, time);
  }

  public void setEnabled(boolean enabled) {
    myCollector.setEnabled(enabled);
    // the answer is given, so no question is due any more
    ErlangUsageIdle.uninstall();
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
        ((UserDataHolderEx)project).putUserDataIfAbsent(READY_SINCE, System.currentTimeMillis());
        AppExecutorUtil.getAppExecutorService().execute(() -> recordProject(project, count));
        askLater(project);
      });
  }

  // starts a chain of checks that asks on the EDT, outside of any dialog, not earlier than a minute after the project is
  // ready; nothing when a chain runs already
  private void askLater(@NotNull Project project) {
    if (myAskedInSession || myDisposed || !myCollector.shouldAsk() || !isErlangProject(project)) return;
    if (!myChainAlive.compareAndSet(false, true)) return;
    myRetries = 0;
    // the idle time counts only from here, so a question that becomes due late in a session also waits for a quiet
    // moment, and the first check comes no earlier than the idle time
    ErlangUsageIdle.install(this);
    Long readySince = project.getUserData(READY_SINCE);
    long delay = readySince == null ? ASK_DELAY_MS : readySince + ASK_DELAY_MS - System.currentTimeMillis();
    schedule(project, Math.max(IDLE_MS, delay));
  }

  // no check is due any more, so the input listener stops
  private void endChain() {
    ErlangUsageIdle.uninstall();
    myChainAlive.set(false);
  }

  // the next check of the chain: a one-shot task that holds the project only weakly and is cancelled when the plugin is
  // unloaded
  private void schedule(@NotNull Project project, long delayMillis) {
    myScheduledForTests++;
    if (myDisposed) {
      endChain();
      return;
    }
    AskScheduler scheduler = myTestScheduler;
    if (scheduler == null && ApplicationManager.getApplication().isUnitTestMode()) return;
    if (scheduler == null) {
      scheduler = (task, delay) -> AppExecutorUtil.getAppScheduledExecutorService().schedule(task, delay, TimeUnit.MILLISECONDS);
    }
    WeakReference<Project> projectRef = new WeakReference<>(project);
    myPendingAsk = scheduler.schedule(() -> {
      if (myDisposed) {
        endChain();
        return;
      }
      // a closed project leaves the check to the other open Erlang projects
      ApplicationManager.getApplication().invokeLater(() -> ask(projectRef.get()), ModalityState.nonModal());
    }, delayMillis);
  }

  // a minute after the project opened and indexing finished, and a few seconds without key or mouse input; without
  // the input listener the user counts as busy, and the listener starts
  boolean isQuietTime(@NotNull Project project) {
    Long readySince = project.getUserData(READY_SINCE);
    if (readySince == null || System.currentTimeMillis() - readySince < ASK_DELAY_MS) return false;
    if (!ErlangUsageIdle.isInstalled()) {
      ErlangUsageIdle.install(this);
      return false;
    }
    return ErlangUsageIdle.idleMillis() >= IDLE_MS;
  }

  /**
   * Asks at most once per IDE session. "Ask me later", Esc and closing the dialog all mean: ask again in
   * {@link ErlangUsageCollector#ASK_AGAIN_DAYS} days, at most {@link ErlangUsageCollector#MAX_ASKS} times in total.
   * The ask is counted when the dialog is shown.
   */
  @RequiresEdt
  void ask(@Nullable Project project) {
    if (myDisposed || myAskedInSession || !myCollector.shouldAsk()) {
      endChain();
      return;
    }
    // the project of the chain first, then the other open Erlang projects: the user may work in the window of another one
    List<Project> candidates = new ArrayList<>();
    if (isErlangProject(project)) candidates.add(project);
    for (Project open : openProjects()) {
      if (open != project && isErlangProject(open)) candidates.add(open);
    }
    if (candidates.isEmpty()) {
      endChain();
      return;
    }
    Project target = null;
    for (Project candidate : candidates) {
      if (isQuietTime(candidate) && myAskUi.canAskNow(candidate)) {
        target = candidate;
        break;
      }
    }
    if (target == null) {
      // the user is busy: check again every few seconds for a while; after that, the timer starts a new chain later
      if (myRetries++ < MAX_RETRIES) schedule(candidates.get(0), RETRY_MS);
      else endChain();
      return;
    }
    myAskedInSession = true;
    endChain();
    boolean reminder = myCollector.isReminder();
    // the ask counts only once the dialog exists and is about to be shown
    answer(myAskUi.ask(target, reminder, myCollector::markAsked));
  }

  private Project @NotNull [] openProjects() {
    Supplier<Project[]> openProjects = myTestOpenProjects;
    return openProjects != null ? openProjects.get() : ProjectManager.getInstance().getOpenProjects();
  }

  void answer(@NotNull ErlangUsageAskUi.Choice choice) {
    switch (choice) {
      case YES -> setEnabled(true);
      case NO -> setEnabled(false);
      case LATER -> {}
    }
  }

  @TestOnly
  void setDisposedForTests(boolean disposed) {
    myDisposed = disposed;
  }

  @TestOnly
  int getScheduledForTests() {
    return myScheduledForTests;
  }

  @TestOnly
  void setSchedulerForTests(@Nullable AskScheduler scheduler) {
    myTestScheduler = scheduler;
  }

  @TestOnly
  void setOpenProjectsForTests(@Nullable Supplier<Project[]> openProjects) {
    myTestOpenProjects = openProjects;
  }

  @TestOnly
  void askLaterForTests(@NotNull Project project) {
    askLater(project);
  }

  @TestOnly
  void recordProjectForTests(@NotNull Project project, int erlangFiles) {
    recordProject(project, erlangFiles);
  }

  @TestOnly
  void tickForTests() {
    tick();
  }

  @TestOnly
  void setAskUiForTests(@NotNull ErlangUsageAskUi ui) {
    myAskUi = ui;
  }

  private synchronized void startTimer() {
    if (myTimer != null) return;
    myTimer = AppExecutorUtil.getAppScheduledExecutorService().scheduleWithFixedDelay(this::tick, 1, TIMER_MINUTES, TimeUnit.MINUTES);
  }

  @RequiresBackgroundThread
  private void tick() {
    try {
      myCollector.flush();
      for (Project project : ProjectManager.getInstance().getOpenProjects()) {
        if (myCollector.isCounting() && !LocalDate.now().toString().equals(project.getUserData(SNAPSHOT_DATE))) {
          checkProject(project);
        }
        else if (isErlangProject(project)) {
          askLater(project);
        }
        else if (!myCollector.isCounting() && !myAskedInSession && myCollector.shouldAsk()) {
          // the project may have got its first Erlang files since it was checked; the check asks when it finds them
          checkProject(project);
        }
      }
      if (myCollector.isCounting()) myCollector.sendPending();
    }
    catch (Throwable e) {
      // anything thrown, also an Error, would stop the timer for the rest of the session
      if (!(e instanceof ControlFlowException)) LOG.debug("Erlang usage statistics", e);
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
    // the SDK and the roots can change or the project close meanwhile, so both are read together in a read action
    record SdkState(boolean configured, @NotNull String otp) {
    }
    SdkState sdk = ReadAction.computeBlocking(() -> {
      if (project.isDisposed()) return null;
      boolean configured = ErlangSdkType.getSdkPath(project) != null;
      return new SdkState(configured, otp(project, configured));
    });
    if (sdk == null) return;
    myCollector.setProperty(ErlangUsageProperty.SDK_CONFIGURED, String.valueOf(sdk.configured()));
    myCollector.setProperty(ErlangUsageProperty.OTP, sdk.otp());
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
    myAskUi = ErlangUsageAskDialog.UI;
    myAskedInSession = false;
    myRetries = 0;
    myScheduledForTests = 0;
    myTestScheduler = null;
    myTestOpenProjects = null;
    myChainAlive.set(false);
  }
}
