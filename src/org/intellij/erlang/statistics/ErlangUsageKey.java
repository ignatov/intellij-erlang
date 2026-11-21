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

/**
 * The daily counters of the usage statistics. The JSON name of a counter is its constant name in lower case,
 * so a counter can only be a constant of this enum and never a text from the user's project.
 */
public enum ErlangUsageKey {
  // completion items chosen in Erlang files
  COMPLETION,
  COMPLETION_FUNCTION,
  COMPLETION_MODULE,
  COMPLETION_RECORD,
  COMPLETION_MACRO,
  COMPLETION_VARIABLE,
  COMPLETION_TYPE,
  COMPLETION_KEYWORD,
  COMPLETION_OTHER,

  // navigation
  GO_TO_DECLARATION,
  FIND_USAGES,
  GO_TO_IMPLEMENTATION,
  GO_TO_SUPER,
  FILE_STRUCTURE_POPUP,
  STRUCTURE_VIEW,

  // documentation
  QUICK_DOCUMENTATION,
  OTP_DOCS_WEB_OK,
  OTP_DOCS_WEB_FAILED,
  PARAMETER_INFO,

  // editing
  REFORMAT,
  EMACS_FORMAT,
  LIVE_TEMPLATE,
  RENAME,
  INLINE,
  INTRODUCE_VARIABLE,
  EXTRACT_FUNCTION,
  SAFE_DELETE,
  NEW_FILE_MODULE,
  NEW_FILE_HEADER,
  NEW_FILE_EUNIT_TESTS,
  NEW_FILE_APPLICATION,
  NEW_FILE_APP_RESOURCE,
  NEW_FILE_SUPERVISOR,
  NEW_FILE_GEN_SERVER,
  NEW_FILE_GEN_STATEM,
  NEW_FILE_GEN_FSM,
  NEW_FILE_GEN_EVENT,
  NEW_FILE_OTHER,

  // run configurations started, by type and executor
  RUN_APPLICATION,
  DEBUG_APPLICATION,
  RUN_CONSOLE,
  DEBUG_CONSOLE,
  RUN_EUNIT,
  DEBUG_EUNIT,
  RUN_REBAR,
  DEBUG_REBAR,
  RUN_REBAR_EUNIT,
  DEBUG_REBAR_EUNIT,
  RUN_REMOTE_NODE,
  DEBUG_REMOTE_NODE,

  // debugger
  DEBUG_SESSION,
  BREAKPOINT_ADDED,
  EVALUATE_EXPRESSION,
  DEBUG_STEP,

  // build and tools
  BUILD_BUILT_IN,
  BUILD_REBAR,
  DIALYZER_OK,
  DIALYZER_FAILED,
  CONSOLE_SEND_SELECTION,
  EUNIT_GENERATE_TEST,

  // files opened in an editor, by kind
  OPENED_ERL,
  OPENED_HRL,
  OPENED_APP,
  OPENED_CONFIG,
  OPENED_ESCRIPT;

  @NotNull
  public String jsonName() {
    return name().toLowerCase(Locale.ROOT);
  }
}
