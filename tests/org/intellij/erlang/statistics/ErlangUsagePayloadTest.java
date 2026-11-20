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

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import junit.framework.TestCase;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public class ErlangUsagePayloadTest extends TestCase {
  private static final String ID = "8f1c2b0e-1111-4222-8333-444455556666";
  private static final Set<String> EVENT_FIELDS =
    Set.of("device_id", "event_type", "time", "insert_id", "app_version", "platform", "os_name", "country", "event_properties");

  public void testAmplitudeShape() {
    ErlangUsageStatisticsState.Day day = day("2026-11-19");
    day.counters.put("COMPLETION", 5L);
    day.counters.put("COMPLETION_FUNCTION", 3L);
    day.counters.put("quick_fix:ErlangExportFunctionFix", 2L);
    day.counters.put("intention:ErlangGenerateSpecIntention", 1L);
    day.properties.put("OTP", "27,R16");
    day.properties.put("SDK_CONFIGURED", "true");

    JsonObject body = JsonParser.parseString(ErlangUsagePayload.build(ID, List.of(day), "Germany")).getAsJsonObject();
    assertEquals(Set.of("api_key", "events"), body.keySet());
    assertEquals(ErlangUsagePayload.API_KEY, body.get("api_key").getAsString());
    JsonArray events = body.getAsJsonArray("events");
    assertEquals(1, events.size());
    JsonObject event = events.get(0).getAsJsonObject();
    assertEquals(EVENT_FIELDS, event.keySet());
    assertEquals(ID, event.get("device_id").getAsString());
    assertEquals("daily_usage", event.get("event_type").getAsString());
    assertEquals(ID + "-2026-11-19", event.get("insert_id").getAsString());
    assertEquals("0.11.1304", event.get("app_version").getAsString());
    assertEquals("IU", event.get("platform").getAsString());
    assertEquals("mac", event.get("os_name").getAsString());
    assertEquals("Germany", event.get("country").getAsString());
    assertFalse(event.has("ip"));

    JsonObject properties = event.getAsJsonObject("event_properties");
    assertEquals(5, properties.get("completion").getAsInt());
    assertEquals(3, properties.get("completion_function").getAsInt());
    assertEquals(263, properties.get("ide_baseline").getAsInt());
    assertFalse(properties.get("small_ide").getAsBoolean());
    assertTrue(properties.get("sdk_configured").getAsBoolean());
    assertEquals("[\"27\",\"R16\"]", properties.get("otp").toString());
    assertEquals(2, properties.getAsJsonObject("quick_fixes").get("ErlangExportFunctionFix").getAsInt());
    assertEquals(1, properties.getAsJsonObject("intentions").get("ErlangGenerateSpecIntention").getAsInt());
  }

  public void testOnlyAllowedKeysAndValues() {
    ErlangUsageStatisticsState.Day day = day("2026-11-19");
    day.counters.put("RENAME", 1L);
    day.counters.put("/Users/john/secret_project/src/secret_module.erl", 7L);
    day.counters.put("secret_function", 1L);
    day.counters.put("quick_fix:../secret_path", 1L);
    day.counters.put("quick_fix:secret_module:fix()", 1L);
    day.counters.put("intention:Hostile\"Name", 1L);
    day.properties.put("PROJECT_NAME", "secret_project");
    day.properties.put("OTP", "27,/usr/local/lib/erlang");
    day.properties.put("REBAR", "rebar3 compile");
    day.properties.put("PLUGIN_VERSION", "0.11.1304 /Users/john");
    day.properties.put("OS", "Darwin john-macbook");
    day.properties.put("IDE_BASELINE", "263; DROP");
    day.properties.put("CHANGED_INSPECTIONS", "ErlangUnusedFunction,secret_module,ErlangFoo/../bar");

    String json = ErlangUsagePayload.build(ID, List.of(day), "Atlantis");
    for (String hostile : List.of("secret", "/Users", "john", "compile", "DROP", "Darwin", "Atlantis", "../", "Hostile")) {
      assertFalse(hostile + " in " + json, json.contains(hostile));
    }
    JsonObject event = ErlangUsageCollectorTest.events(json).get(0).getAsJsonObject();
    assertFalse(event.has("app_version"));
    assertFalse(event.has("country"));
    JsonObject properties = event.getAsJsonObject("event_properties");
    assertAllowedKeys(properties);
    assertEquals("[\"27\"]", properties.get("otp").toString());
    assertEquals("[\"ErlangUnusedFunction\"]", properties.get("changed_inspections").toString());
    assertFalse(properties.has("rebar"));
    assertFalse(properties.has("quick_fixes"));
  }

  public void testAtMostTenEventsPerRequest() {
    List<ErlangUsageStatisticsState.Day> days = new ArrayList<>();
    for (int i = 1; i <= ErlangUsagePayload.MAX_EVENTS_PER_REQUEST + 1; i++) {
      ErlangUsageStatisticsState.Day day = day(String.format("2026-11-%02d", i));
      day.counters.put("RENAME", 1L);
      days.add(day);
    }
    assertEquals(10, ErlangUsageCollectorTest.events(ErlangUsagePayload.build(ID, days.subList(0, 10), null)).size());
    try {
      ErlangUsagePayload.build(ID, days, null);
      fail("11 events in one request");
    }
    catch (IllegalArgumentException expected) {
    }
  }

  public void testDeviceIdMustBeRandomId() {
    try {
      ErlangUsagePayload.build("john@john-macbook", List.of(), null);
      fail("not a UUID");
    }
    catch (IllegalArgumentException expected) {
    }
  }

  public void testCountryFromTimeZone() {
    assertEquals("Germany", ErlangUsageCountry.fromTimeZone(java.time.ZoneId.of("Europe/Berlin")));
    assertEquals("United States", ErlangUsageCountry.fromTimeZone(java.time.ZoneId.of("America/Los_Angeles")));
    assertNull(ErlangUsageCountry.fromTimeZone(java.time.ZoneId.of("UTC")));
    assertNull(ErlangUsageCountry.fromTimeZone(java.time.ZoneId.of("Antarctica/Troll")));
  }

  static void assertAllowedKeys(@NotNull JsonObject properties) {
    Set<String> allowed = new HashSet<>(Set.of("quick_fixes", "intentions"));
    for (ErlangUsageKey key : ErlangUsageKey.values()) allowed.add(key.jsonName());
    for (ErlangUsageProperty property : ErlangUsageProperty.values()) allowed.add(property.jsonName());
    for (Map.Entry<String, JsonElement> entry : properties.entrySet()) {
      assertTrue(entry.getKey(), allowed.contains(entry.getKey()));
      JsonElement value = entry.getValue();
      if (value.isJsonObject()) {
        for (String name : value.getAsJsonObject().keySet()) {
          assertTrue(name, ErlangUsagePayload.CLASS_NAME.matcher(name).matches());
        }
      }
    }
  }

  @NotNull
  private static ErlangUsageStatisticsState.Day day(@NotNull String date) {
    ErlangUsageStatisticsState.Day day = new ErlangUsageStatisticsState.Day();
    day.date = date;
    ErlangUsageCollectorTest.appProperties().forEach((property, value) -> day.properties.put(property.name(), value));
    return day;
  }
}
