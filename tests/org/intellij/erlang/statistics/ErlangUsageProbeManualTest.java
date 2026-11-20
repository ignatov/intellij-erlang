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
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.time.LocalDate;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * Sends one real {@code daily_usage} event with a fixed probe id and tiny fake counts to Amplitude, and prints the HTTP status.
 * It does nothing unless the environment variable ERLANG_USAGE_PROBE is 1, so the normal test runs never use the network:
 * <pre>ERLANG_USAGE_PROBE=1 ./gradlew test --tests 'org.intellij.erlang.statistics.ErlangUsageProbeManualTest' --rerun</pre>
 */
public class ErlangUsageProbeManualTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String FIXED_ID = "00000000-0000-4000-8000-000000000000";

  public void testSendOneProbeEvent() throws Exception {
    if (!"1".equals(System.getenv("ERLANG_USAGE_PROBE"))) return;
    ErlangUsageStatisticsState.Day day = new ErlangUsageStatisticsState.Day();
    day.date = LocalDate.now().minusDays(1).toString();
    ErlangUsageCollectorTest.appProperties().forEach((property, value) -> day.properties.put(property.name(), value));
    day.counters.put(ErlangUsageKey.COMPLETION.name(), 1L);
    day.counters.put(ErlangUsageKey.COMPLETION_FUNCTION.name(), 1L);
    day.counters.put(ErlangUsageKey.RENAME.name(), 1L);
    day.country = "Germany";
    // the payload takes only random ids; the probe id replaces a fixed one afterwards
    String json = ErlangUsagePayload.build(FIXED_ID, List.of(day)).replace(FIXED_ID, "dev-probe-000000");
    int status = ApplicationManager.getApplication()
      .executeOnPooledThread(() -> new ErlangAmplitudeSender().send(json))
      .get(30, TimeUnit.SECONDS);
    System.out.println("Amplitude answered HTTP " + status + " for " + json);
    assertEquals(200, status);
  }
}
