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

import java.util.Locale;
import java.util.regex.Pattern;

/**
 * The environment of a day: values that describe the IDE and the Erlang projects, not counts.
 * Each property accepts only values that match its pattern; the JSON writer checks them again.
 */
public enum ErlangUsageProperty {
  PLUGIN_VERSION(Kind.TEXT, "\\d{1,4}(\\.\\d{1,6}){0,3}"),
  IDE_PRODUCT(Kind.TEXT, "[A-Z]{2,4}"),
  // a JSON number: no leading zero
  IDE_BASELINE(Kind.NUMBER, "[1-9]\\d{2}"),
  SMALL_IDE(Kind.BOOLEAN, "true|false"),
  OS(Kind.TEXT, "mac|windows|linux|other"),
  // the OTP major release of the SDK of each Erlang project opened that day
  OTP(Kind.LIST, "R?\\d{1,2}|none|unknown"),
  SDK_CONFIGURED(Kind.BOOLEAN, "true|false"),
  REBAR(Kind.LIST, "rebar3|rebar2|unknown|none"),
  PROJECT_SIZE(Kind.LIST, "up_to_10|up_to_100|up_to_1000|over_1000"),
  INLAY_PARAMETER_HINTS(Kind.BOOLEAN, "true|false"),
  // the short names of this plugin's inspections whose enabled state differs from the default
  CHANGED_INSPECTIONS(Kind.LIST, "Erlang[A-Za-z0-9_]{1,80}");

  public static final int MAX_LIST_SIZE = 40;

  enum Kind {
    TEXT,
    NUMBER,
    // when several projects set it on one day, true wins
    BOOLEAN,
    // a set of values; each project adds its own
    LIST
  }

  private final Kind myKind;
  private final Pattern myPattern;

  ErlangUsageProperty(@NotNull Kind kind, @NotNull String pattern) {
    myKind = kind;
    myPattern = Pattern.compile(pattern);
  }

  @NotNull
  Kind kind() {
    return myKind;
  }

  boolean accepts(@NotNull String value) {
    return myPattern.matcher(value).matches();
  }

  @NotNull
  public String jsonName() {
    return name().toLowerCase(Locale.ROOT);
  }
}
