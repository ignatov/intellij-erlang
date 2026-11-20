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

import com.intellij.util.xmlb.annotations.Attribute;
import com.intellij.util.xmlb.annotations.Tag;
import com.intellij.util.xmlb.annotations.XCollection;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * The stored state of the usage statistics. It is application-level and is kept in a file that does not roam.
 */
public class ErlangUsageStatisticsState {
  // UNSET, YES or NO
  public String consent = ErlangUsageCollector.Consent.UNSET.name();
  // how many times the plugin asked
  public int askCount;
  // the local date of the last ask, yyyy-MM-dd
  public String lastAskDate;
  // a random UUID; it exists only while the user allows the statistics
  public String installationId;
  @XCollection(propertyElementName = "days")
  public List<Day> days = new ArrayList<>();

  @Tag("day")
  public static class Day {
    // the local date, yyyy-MM-dd
    @Attribute("date")
    public String date;
    // the country of the day, guessed from the time zone; null when the time zone is not in the table
    @Attribute("country")
    public String country;
    // the names of ErlangUsageKey constants, or "quick_fix:" or "intention:" and the simple name of a class of this plugin
    public Map<String, Long> counters = new TreeMap<>();
    // the names of ErlangUsageProperty constants; a LIST value is a sorted, comma-separated set
    public Map<String, String> properties = new TreeMap<>();
  }
}
