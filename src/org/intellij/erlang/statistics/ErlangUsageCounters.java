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

import com.intellij.codeInsight.intention.preview.IntentionPreviewUtils;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Map;

import static java.util.Map.entry;
import static org.intellij.erlang.statistics.ErlangUsageKey.*;

/**
 * Counts one use of a feature for the usage statistics. A call costs almost nothing, and does nothing at all
 * unless the user allowed the statistics, and only in projects with Erlang modules or headers. The arguments are enum
 * constants and classes of this plugin only, so no text from the user's project can be counted.
 */
public final class ErlangUsageCounters {
  // the internal file templates of the plugin; any other template counts as NEW_FILE_OTHER
  private static final Map<String, ErlangUsageKey> NEW_FILE_TEMPLATES = Map.ofEntries(
    entry("Erlang Module", NEW_FILE_MODULE),
    entry("Erlang Header", NEW_FILE_HEADER),
    entry("Erlang EUnit Tests", NEW_FILE_EUNIT_TESTS),
    entry("Erlang Application", NEW_FILE_APPLICATION),
    entry("Erlang Application Minimal", NEW_FILE_APPLICATION),
    entry("Erlang Application Resource File", NEW_FILE_APP_RESOURCE),
    entry("Erlang Supervisor", NEW_FILE_SUPERVISOR),
    entry("Erlang Supervisor Minimal", NEW_FILE_SUPERVISOR),
    entry("Erlang Supervisor Legacy", NEW_FILE_SUPERVISOR),
    entry("Erlang Gen Server", NEW_FILE_GEN_SERVER),
    entry("Erlang Gen Server Minimal", NEW_FILE_GEN_SERVER),
    entry("Erlang Gen Statem", NEW_FILE_GEN_STATEM),
    entry("Erlang Gen FSM", NEW_FILE_GEN_FSM),
    entry("Erlang Gen FSM Minimal", NEW_FILE_GEN_FSM),
    entry("Erlang Gen Event", NEW_FILE_GEN_EVENT)
  );

  private ErlangUsageCounters() {
  }

  // one use in the project; it counts only when the project has Erlang modules or headers
  public static void count(@Nullable Project project, @NotNull ErlangUsageKey key) {
    ErlangUsageCollector collector = collector(project);
    if (collector != null) collector.count(key);
  }

  // a file was created from a template of the New Erlang File dialog; only the kind of the template counts
  public static void countNewFile(@Nullable Project project, @Nullable String templateName) {
    count(project, newFileKey(templateName));
  }

  @NotNull
  static ErlangUsageKey newFileKey(@Nullable String templateName) {
    ErlangUsageKey key = templateName != null ? NEW_FILE_TEMPLATES.get(templateName) : null;
    return key != null ? key : NEW_FILE_OTHER;
  }

  // a quick fix of this plugin was applied; the intention preview does not count
  public static void countQuickFix(@Nullable Project project, @NotNull Class<?> fixClass) {
    ErlangUsageCollector collector = collector(project);
    if (collector != null && !IntentionPreviewUtils.isIntentionPreviewActive()) collector.countQuickFix(fixClass);
  }

  // an intention of this plugin was applied; the intention preview does not count
  public static void countIntention(@Nullable Project project, @NotNull Class<?> intentionClass) {
    ErlangUsageCollector collector = collector(project);
    if (collector != null && !IntentionPreviewUtils.isIntentionPreviewActive()) collector.countIntention(intentionClass);
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
