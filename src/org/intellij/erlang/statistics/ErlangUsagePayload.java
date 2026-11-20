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

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Pattern;

/**
 * Writes the body of an Amplitude HTTP V2 request: one {@code daily_usage} event per day.
 * <p>
 * Only counters of {@link ErlangUsageKey}, properties of {@link ErlangUsageProperty} with the values their patterns
 * accept, and simple class names of this plugin's quick fixes and intentions get into the JSON. Everything else in the
 * stored state is skipped, so a name or a path cannot be sent even if it got into the state.
 */
public final class ErlangUsagePayload {
  public static final String EVENT_TYPE = "daily_usage";
  public static final int MAX_EVENTS_PER_REQUEST = 10;
  static final String QUICK_FIX_PREFIX = "quick_fix:";
  static final String INTENTION_PREFIX = "intention:";

  private static final Pattern DEVICE_ID = Pattern.compile("[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}");
  private static final Pattern DATE = Pattern.compile("\\d{4}-\\d{2}-\\d{2}");
  static final Pattern CLASS_NAME = Pattern.compile("[A-Z][A-Za-z0-9_]{0,80}");

  private ErlangUsagePayload() {
  }

  /**
   * An example daily summary for the consent dialog: fixed sample values written by {@link #build}, so its shape is that
   * of a real one. The key is hidden and the id is an obvious fake.
   */
  @NotNull
  public static String example() {
    return build(EXAMPLE_ID, List.of(exampleDay()), true);
  }

  // the event_properties object of the example, the part that shows what is counted
  @NotNull
  public static String exampleEventProperties() {
    ErlangUsageStatisticsState.Day day = exampleDay();
    return eventProperties(day, properties(day), true);
  }

  // the sample values of the example: every environment property and counters of every area
  @NotNull
  static ErlangUsageStatisticsState.Day exampleDay() {
    ErlangUsageStatisticsState.Day day = new ErlangUsageStatisticsState.Day();
    day.date = "2026-11-19";
    day.country = "Germany";
    String[][] properties = {
      {"PLUGIN_VERSION", "0.11.1304"}, {"IDE_PRODUCT", "IU"}, {"IDE_BASELINE", "263"}, {"SMALL_IDE", "false"}, {"OS", "mac"},
      {"OTP", "27"}, {"SDK_CONFIGURED", "true"}, {"REBAR", "rebar3"}, {"PROJECT_SIZE", "up_to_100"},
      {"INLAY_PARAMETER_HINTS", "true"}, {"CHANGED_INSPECTIONS", "ErlangSingleClauseCase"}};
    for (String[] property : properties) {
      day.properties.put(property[0], property[1]);
    }
    Object[][] counters = {
      {ErlangUsageKey.COMPLETION, 42}, {ErlangUsageKey.COMPLETION_FUNCTION, 30}, {ErlangUsageKey.COMPLETION_MODULE, 6},
      {ErlangUsageKey.COMPLETION_VARIABLE, 4}, {ErlangUsageKey.COMPLETION_RECORD, 2},
      {ErlangUsageKey.GO_TO_DECLARATION, 25}, {ErlangUsageKey.FIND_USAGES, 3}, {ErlangUsageKey.FILE_STRUCTURE_POPUP, 2},
      {ErlangUsageKey.QUICK_DOCUMENTATION, 5}, {ErlangUsageKey.PARAMETER_INFO, 3}, {ErlangUsageKey.OTP_DOCS_WEB_OK, 2},
      {ErlangUsageKey.REFORMAT, 2}, {ErlangUsageKey.RENAME, 1}, {ErlangUsageKey.LIVE_TEMPLATE, 3}, {ErlangUsageKey.NEW_FILE_MODULE, 1},
      {ErlangUsageKey.RUN_EUNIT, 4}, {ErlangUsageKey.RUN_REBAR, 2}, {ErlangUsageKey.DEBUG_APPLICATION, 1},
      {ErlangUsageKey.DEBUG_SESSION, 1}, {ErlangUsageKey.BREAKPOINT_ADDED, 3}, {ErlangUsageKey.DEBUG_STEP, 12},
      {ErlangUsageKey.BUILD_REBAR, 2}, {ErlangUsageKey.DIALYZER_OK, 1}, {ErlangUsageKey.CONSOLE_SEND_SELECTION, 1},
      {ErlangUsageKey.OPENED_ERL, 12}, {ErlangUsageKey.OPENED_HRL, 2}};
    for (Object[] counter : counters) {
      day.counters.put(((ErlangUsageKey)counter[0]).name(), ((Integer)counter[1]).longValue());
    }
    day.counters.put(QUICK_FIX_PREFIX + "ErlangExportFunctionFix", 1L);
    day.counters.put(INTENTION_PREFIX + "ErlangGenerateSpecIntention", 1L);
    return day;
  }

  public static final String EXAMPLE_ID = "00000000-0000-4000-8000-000000000000";
  // what the example shows first, in the lines that are visible without scrolling
  private static final List<String> EXAMPLE_FIRST = List.of(
    "completion", "completion_function", "go_to_declaration", "find_usages", "quick_documentation", "run_eunit",
    "debug_application", "reformat", "otp", "rebar", "project_size");

  @NotNull
  public static String build(@NotNull String deviceId, @NotNull List<ErlangUsageStatisticsState.Day> days) {
    return build(deviceId, days, false);
  }

  // for the example, the counters come first and the envelope last, so that the visible lines show what is counted
  @NotNull
  private static String build(@NotNull String deviceId, @NotNull List<ErlangUsageStatisticsState.Day> days, boolean example) {
    if (!DEVICE_ID.matcher(deviceId).matches()) throw new IllegalArgumentException("Not an installation id");
    if (days.size() > MAX_EVENTS_PER_REQUEST) throw new IllegalArgumentException("Too many events: " + days.size());
    StringBuilder events = new StringBuilder("[");
    boolean first = true;
    for (ErlangUsageStatisticsState.Day day : days) {
      if (day.date == null || !DATE.matcher(day.date).matches()) continue;
      if (!first) events.append(',');
      first = false;
      event(events, deviceId, day, day.country != null && ErlangUsageCountry.isKnown(day.country) ? day.country : null, example);
    }
    events.append(']');
    StringBuilder json = new StringBuilder("{");
    if (example) {
      json.append("\"events\":").append(events).append(",\"api_key\":");
      string(json, "…");
    }
    else {
      json.append("\"api_key\":");
      string(json, ErlangAmplitudeSender.API_KEY);
      json.append(",\"events\":").append(events);
    }
    return json.append('}').toString();
  }

  private static void event(@NotNull StringBuilder json,
                            @NotNull String deviceId,
                            @NotNull ErlangUsageStatisticsState.Day day,
                            @Nullable String country,
                            boolean example) {
    LocalDate date = LocalDate.parse(day.date);
    Map<ErlangUsageProperty, String> properties = properties(day);

    StringBuilder envelope = new StringBuilder();
    envelope.append("\"device_id\":");
    string(envelope, deviceId);
    envelope.append(",\"event_type\":");
    string(envelope, EVENT_TYPE);
    // noon UTC of the day, so that the day is the same in the charts of every time zone setting
    envelope.append(",\"time\":").append(date.atTime(LocalTime.NOON).toInstant(ZoneOffset.UTC).toEpochMilli());
    envelope.append(",\"insert_id\":");
    string(envelope, deviceId + "-" + day.date);
    optional(envelope, "app_version", properties.get(ErlangUsageProperty.PLUGIN_VERSION));
    optional(envelope, "platform", properties.get(ErlangUsageProperty.IDE_PRODUCT));
    optional(envelope, "os_name", properties.get(ErlangUsageProperty.OS));
    optional(envelope, "country", country);

    String eventProperties = "\"event_properties\":" + eventProperties(day, properties, example);

    json.append('{');
    if (example) json.append(eventProperties).append(',').append(envelope);
    else json.append(envelope).append(',').append(eventProperties);
    json.append('}');
  }

  // the event_properties object; for the example, the counters come first
  @NotNull
  private static String eventProperties(@NotNull ErlangUsageStatisticsState.Day day,
                                        @NotNull Map<ErlangUsageProperty, String> properties,
                                        boolean example) {
    // each value already written as JSON, by its property name
    Map<String, String> counters = new LinkedHashMap<>();
    // in the order of the enum, which groups them by area
    for (ErlangUsageKey counter : ErlangUsageKey.values()) {
      Long count = day.counters.get(counter.name());
      if (count != null && count > 0) counters.put(counter.jsonName(), String.valueOf(count.longValue()));
    }
    Map<String, Long> quickFixes = new TreeMap<>();
    Map<String, Long> intentions = new TreeMap<>();
    for (Map.Entry<String, Long> entry : day.counters.entrySet()) {
      String key = entry.getKey();
      Long count = entry.getValue();
      if (key == null || count == null || count <= 0) continue;
      if (key.startsWith(QUICK_FIX_PREFIX)) addClassCount(quickFixes, key.substring(QUICK_FIX_PREFIX.length()), count);
      else if (key.startsWith(INTENTION_PREFIX)) addClassCount(intentions, key.substring(INTENTION_PREFIX.length()), count);
    }
    if (!quickFixes.isEmpty()) counters.put("quick_fixes", classCounts(quickFixes));
    if (!intentions.isEmpty()) counters.put("intentions", classCounts(intentions));
    Map<String, String> environment = new LinkedHashMap<>();
    for (Map.Entry<ErlangUsageProperty, String> entry : properties.entrySet()) {
      StringBuilder value = new StringBuilder();
      value(value, entry.getKey(), entry.getValue());
      environment.put(entry.getKey().jsonName(), value.toString());
    }
    Map<String, String> all = new LinkedHashMap<>();
    if (example) {
      Map<String, String> rest = new LinkedHashMap<>(counters);
      rest.putAll(environment);
      for (String name : EXAMPLE_FIRST) {
        String value = rest.remove(name);
        if (value != null) all.put(name, value);
      }
      all.putAll(rest);
    }
    else {
      all.putAll(environment);
      all.putAll(counters);
    }
    StringBuilder eventProperties = new StringBuilder("{");
    boolean first = true;
    for (Map.Entry<String, String> entry : all.entrySet()) {
      first = comma(eventProperties, first);
      string(eventProperties, entry.getKey());
      eventProperties.append(':').append(entry.getValue());
    }
    eventProperties.append('}');
    return eventProperties.toString();
  }

  // the properties of the day that pass their checks, in a fixed order
  @NotNull
  static Map<ErlangUsageProperty, String> properties(@NotNull ErlangUsageStatisticsState.Day day) {
    Map<ErlangUsageProperty, String> result = new TreeMap<>();
    for (Map.Entry<String, String> entry : day.properties.entrySet()) {
      ErlangUsageProperty property = propertyKey(entry.getKey());
      String value = entry.getValue();
      if (property == null || value == null || value.isEmpty()) continue;
      if (property.kind() == ErlangUsageProperty.Kind.LIST) {
        List<String> values = ErlangUsageCollector.split(value).stream().filter(property::accepts)
          .limit(ErlangUsageProperty.MAX_LIST_SIZE).toList();
        if (!values.isEmpty()) result.put(property, String.join(",", values));
      }
      else if (property.accepts(value)) {
        result.put(property, value);
      }
    }
    return result;
  }

  private static void value(@NotNull StringBuilder json, @NotNull ErlangUsageProperty property, @NotNull String value) {
    switch (property.kind()) {
      case NUMBER, BOOLEAN -> json.append(value);
      case TEXT -> string(json, value);
      case LIST -> {
        json.append('[');
        boolean first = true;
        for (String item : ErlangUsageCollector.split(value)) {
          first = comma(json, first);
          string(json, item);
        }
        json.append(']');
      }
    }
  }

  private static void addClassCount(@NotNull Map<String, Long> counts, @NotNull String simpleName, long count) {
    if (CLASS_NAME.matcher(simpleName).matches()) counts.merge(simpleName, count, Long::sum);
  }

  @NotNull
  private static String classCounts(@NotNull Map<String, Long> counts) {
    StringBuilder json = new StringBuilder("{");
    boolean first = true;
    for (Map.Entry<String, Long> entry : counts.entrySet()) {
      first = comma(json, first);
      string(json, entry.getKey());
      json.append(':').append(entry.getValue().longValue());
    }
    return json.append('}').toString();
  }

  @Nullable
  static ErlangUsageKey counterKey(@NotNull String name) {
    try {
      return ErlangUsageKey.valueOf(name);
    }
    catch (IllegalArgumentException e) {
      return null;
    }
  }

  @Nullable
  static ErlangUsageProperty propertyKey(@Nullable String name) {
    if (name == null) return null;
    try {
      return ErlangUsageProperty.valueOf(name);
    }
    catch (IllegalArgumentException e) {
      return null;
    }
  }

  private static void optional(@NotNull StringBuilder json, @NotNull String name, @Nullable String value) {
    if (value == null) return;
    json.append(',');
    string(json, name);
    json.append(':');
    string(json, value);
  }

  private static boolean comma(@NotNull StringBuilder json, boolean first) {
    if (!first) json.append(',');
    return false;
  }

  // every value is checked against a pattern before it gets here; the escaping is a second guard
  private static void string(@NotNull StringBuilder json, @NotNull String value) {
    json.append('"');
    for (int i = 0; i < value.length(); i++) {
      char c = value.charAt(i);
      switch (c) {
        case '"' -> json.append("\\\"");
        case '\\' -> json.append("\\\\");
        default -> {
          if (c < 0x20 || c > 0x7e) json.append(String.format("\\u%04x", (int)c));
          else json.append(c);
        }
      }
    }
    json.append('"');
  }
}
