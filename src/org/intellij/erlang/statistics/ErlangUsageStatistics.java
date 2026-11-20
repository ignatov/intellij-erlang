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

import com.intellij.ide.plugins.cl.PluginAwareClassLoader;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.time.LocalDate;

/**
 * The opt-in usage statistics of the plugin: one summary of counts per day, sent to Amplitude.
 */
@Service(Service.Level.APP)
@State(name = "ErlangUsageStatistics", storages = @Storage(value = "erlangUsageStatistics.xml", roamingType = RoamingType.DISABLED))
public final class ErlangUsageStatistics implements PersistentStateComponent<ErlangUsageStatisticsState> {
  public static final String SWITCH_PROPERTY = "erlang.usage.statistics";
  private static volatile Boolean ourAllowed;

  private final ErlangUsageCollector myCollector = new ErlangUsageCollector(LocalDate::now, ErlangUsageStatistics::isAllowed);

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
