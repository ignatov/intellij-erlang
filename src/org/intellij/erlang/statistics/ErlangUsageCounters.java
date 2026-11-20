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

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Counts one use of a feature for the usage statistics. A call costs almost nothing, and does nothing at all
 * unless the user allowed the statistics, and only in projects with Erlang modules or headers. The arguments are enum
 * constants only, so no text from the user's project can be counted.
 */
public final class ErlangUsageCounters {
  private ErlangUsageCounters() {
  }

  // one use in the project; it counts only when the project has Erlang modules or headers
  public static void count(@Nullable Project project, @NotNull ErlangUsageKey key) {
    ErlangUsageCollector collector = collector(project);
    if (collector != null) collector.count(key);
  }

  // false when nothing would be counted anyway, so that listeners can stop early
  public static boolean isCounting() {
    return collector() != null;
  }

  @Nullable
  private static ErlangUsageCollector collector(@Nullable Project project) {
    return ErlangUsageStatistics.isErlangProject(project) ? collector() : null;
  }

  @Nullable
  private static ErlangUsageCollector collector() {
    var application = ApplicationManager.getApplication();
    if (application == null || application.isDisposed()) return null;
    if (!ErlangUsageStatistics.isAllowed()) return null;
    ErlangUsageCollector collector = ErlangUsageStatistics.getInstance().getCollector();
    return collector.isCounting() ? collector : null;
  }
}
