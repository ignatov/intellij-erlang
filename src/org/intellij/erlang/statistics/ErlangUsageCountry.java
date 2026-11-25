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

import java.time.ZoneId;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * The country of the user, guessed from the time zone of the system. Nothing else is used, and the IP address is
 * never sent. A time zone that is not in the table gives no country.
 */
public final class ErlangUsageCountry {
  private static final Map<String, String> BY_ZONE = new HashMap<>();
  private static final Set<String> COUNTRIES = new HashSet<>();

  static {
    add("Argentina", "America/Argentina/Buenos_Aires", "America/Buenos_Aires", "America/Argentina/Cordoba");
    add("Australia", "Australia/Sydney", "Australia/Melbourne", "Australia/Brisbane", "Australia/Perth", "Australia/Adelaide",
        "Australia/Hobart", "Australia/Darwin", "Australia/Canberra");
    add("Austria", "Europe/Vienna");
    add("Belarus", "Europe/Minsk");
    add("Belgium", "Europe/Brussels");
    add("Brazil", "America/Sao_Paulo", "America/Fortaleza", "America/Recife", "America/Manaus", "America/Bahia", "America/Belem");
    add("Bulgaria", "Europe/Sofia");
    add("Canada", "America/Toronto", "America/Vancouver", "America/Montreal", "America/Edmonton", "America/Winnipeg",
        "America/Halifax", "America/St_Johns", "America/Regina");
    add("Chile", "America/Santiago");
    add("China", "Asia/Shanghai", "Asia/Chongqing", "Asia/Harbin", "Asia/Urumqi", "PRC");
    add("Colombia", "America/Bogota");
    add("Croatia", "Europe/Zagreb");
    add("Czechia", "Europe/Prague");
    add("Denmark", "Europe/Copenhagen");
    add("Egypt", "Africa/Cairo");
    add("Estonia", "Europe/Tallinn");
    add("Finland", "Europe/Helsinki");
    add("France", "Europe/Paris");
    add("Georgia", "Asia/Tbilisi");
    add("Germany", "Europe/Berlin");
    add("Greece", "Europe/Athens");
    add("Hong Kong", "Asia/Hong_Kong");
    add("Hungary", "Europe/Budapest");
    add("India", "Asia/Kolkata", "Asia/Calcutta");
    add("Indonesia", "Asia/Jakarta");
    add("Ireland", "Europe/Dublin");
    add("Israel", "Asia/Jerusalem", "Asia/Tel_Aviv");
    add("Italy", "Europe/Rome");
    add("Japan", "Asia/Tokyo", "Japan");
    add("Kazakhstan", "Asia/Almaty", "Asia/Qostanay");
    add("Kenya", "Africa/Nairobi");
    add("Latvia", "Europe/Riga");
    add("Lithuania", "Europe/Vilnius");
    add("Malaysia", "Asia/Kuala_Lumpur");
    add("Mexico", "America/Mexico_City", "America/Monterrey", "America/Tijuana", "America/Cancun");
    add("Netherlands", "Europe/Amsterdam");
    add("New Zealand", "Pacific/Auckland");
    add("Nigeria", "Africa/Lagos");
    add("Norway", "Europe/Oslo");
    add("Pakistan", "Asia/Karachi");
    add("Philippines", "Asia/Manila");
    add("Poland", "Europe/Warsaw");
    add("Portugal", "Europe/Lisbon", "Atlantic/Azores", "Atlantic/Madeira");
    add("Romania", "Europe/Bucharest");
    add("Russia", "Europe/Moscow", "Europe/Samara", "Europe/Volgograd", "Asia/Yekaterinburg", "Asia/Omsk", "Asia/Novosibirsk",
        "Asia/Krasnoyarsk", "Asia/Irkutsk", "Asia/Yakutsk", "Asia/Vladivostok", "Asia/Magadan", "Asia/Kamchatka",
        "Europe/Kaliningrad");
    add("Serbia", "Europe/Belgrade");
    add("Singapore", "Asia/Singapore");
    add("Slovakia", "Europe/Bratislava");
    add("Slovenia", "Europe/Ljubljana");
    add("South Africa", "Africa/Johannesburg");
    add("South Korea", "Asia/Seoul");
    add("Spain", "Europe/Madrid", "Atlantic/Canary");
    add("Sweden", "Europe/Stockholm");
    add("Switzerland", "Europe/Zurich");
    add("Taiwan", "Asia/Taipei");
    add("Thailand", "Asia/Bangkok");
    add("Turkey", "Europe/Istanbul", "Asia/Istanbul");
    add("Ukraine", "Europe/Kyiv", "Europe/Kiev");
    add("United Arab Emirates", "Asia/Dubai");
    add("United Kingdom", "Europe/London");
    add("United States", "America/New_York", "America/Chicago", "America/Denver", "America/Los_Angeles", "America/Phoenix",
        "America/Anchorage", "America/Detroit", "America/Indiana/Indianapolis", "America/Boise", "Pacific/Honolulu",
        "US/Eastern", "US/Central", "US/Mountain", "US/Pacific");
    add("Uruguay", "America/Montevideo");
    add("Vietnam", "Asia/Ho_Chi_Minh", "Asia/Saigon");
  }

  private ErlangUsageCountry() {
  }

  private static void add(@NotNull String country, @NotNull String... zones) {
    COUNTRIES.add(country);
    for (String zone : zones) {
      BY_ZONE.put(zone, country);
    }
  }

  @Nullable
  public static String fromTimeZone(@NotNull ZoneId zone) {
    return BY_ZONE.get(zone.getId());
  }

  @Nullable
  public static String current() {
    return fromTimeZone(ZoneId.systemDefault());
  }

  static boolean isKnown(@NotNull String country) {
    return COUNTRIES.contains(country);
  }
}
