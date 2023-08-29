/*
 * Copyright 2012-2014 Sergey Ignatov
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

package org.intellij.erlang.formatter.settings;

import com.intellij.application.options.IndentOptionsEditor;
import com.intellij.application.options.SmartIndentOptionsEditor;
import com.intellij.lang.Language;
import com.intellij.psi.codeStyle.CodeStyleSettingsCustomizable;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.intellij.psi.codeStyle.LanguageCodeStyleSettingsProvider;
import org.intellij.erlang.ErlangLanguage;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangLanguageCodeStyleSettingsProvider extends LanguageCodeStyleSettingsProvider {
  private static final String AROUND_OPERATORS = "Around Operators";
  private static final String ALIGNMENT = "Alignment";

  @NotNull
  @Override
  public Language getLanguage() {
    return ErlangLanguage.INSTANCE;
  }

  @Override
  public String getCodeSample(@NotNull SettingsType settingsType) {
    if (settingsType == SettingsType.SPACING_SETTINGS || settingsType == SettingsType.WRAPPING_AND_BRACES_SETTINGS) {
      return DEFAULT_CODE_SAMPLE;
    }
    if (settingsType == SettingsType.INDENT_SETTINGS) {
      return INDENT_CODE_SAMPLE;
    }
    return BLANK_LINES_CODE_SAMPLE;
  }

  @Override
  public IndentOptionsEditor getIndentOptionsEditor() {
    return new SmartIndentOptionsEditor();
  }

  @NotNull
  @Override
  public CommonCodeStyleSettings getDefaultCommonSettings() {
    CommonCodeStyleSettings defaultSettings = new CommonCodeStyleSettings(getLanguage());
    CommonCodeStyleSettings.IndentOptions indentOptions = defaultSettings.initIndentOptions();
    indentOptions.INDENT_SIZE = 2;
    indentOptions.CONTINUATION_INDENT_SIZE = 4;
    indentOptions.TAB_SIZE = 2;

    return defaultSettings;
  }

  @Override
  public void customizeSettings(@NotNull CodeStyleSettingsCustomizable consumer, @NotNull SettingsType settingsType) {
    if (settingsType == SettingsType.SPACING_SETTINGS) {
      consumer.showStandardOptions(
//        "SPACE_BEFORE_METHOD_CALL_PARENTHESES", // todo
        "SPACE_AROUND_ASSIGNMENT_OPERATORS",
        "SPACE_AROUND_LOGICAL_OPERATORS",
        "SPACE_AROUND_EQUALITY_OPERATORS",
        "SPACE_AROUND_ADDITIVE_OPERATORS",
        "SPACE_AROUND_MULTIPLICATIVE_OPERATORS",
//        "SPACE_WITHIN_METHOD_CALL_PARENTHESES", // todo
//        "SPACE_WITHIN_METHOD_PARENTHESES", // todo
        "SPACE_AFTER_COMMA",
        "SPACE_BEFORE_COMMA"
      );
      consumer.renameStandardOption("SPACE_AROUND_ASSIGNMENT_OPERATORS", "Assignment operator");
      consumer.renameStandardOption("SPACE_AROUND_LOGICAL_OPERATORS", "Logical operators");
      consumer.renameStandardOption("SPACE_AROUND_EQUALITY_OPERATORS", "Equality operators");
      consumer.renameStandardOption("SPACE_AROUND_ADDITIVE_OPERATORS", "Additive operators");
      consumer.renameStandardOption("SPACE_AROUND_MULTIPLICATIVE_OPERATORS", "Multiplicative operators");

      consumer.showCustomOption(ErlangCodeStyleSettings.class, "SPACE_AROUND_EQ_IN_RECORDS", "= inside records", AROUND_OPERATORS);
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "SPACE_AROUND_SEND", "Send operator", AROUND_OPERATORS);
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "SPACE_AROUND_ARROW", "Right arrow", AROUND_OPERATORS);
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "SPACE_AROUND_LEFT_ARROW", "Left arrow", AROUND_OPERATORS);
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "SPACE_AROUND_OR_IN_LISTS", "| inside lists", AROUND_OPERATORS);
    }
    else if (settingsType == SettingsType.BLANK_LINES_SETTINGS) {
      consumer.showStandardOptions("KEEP_BLANK_LINES_IN_CODE");
    }
    else if (settingsType == SettingsType.WRAPPING_AND_BRACES_SETTINGS) {
      consumer.showStandardOptions(
//        "KEEP_LINE_BREAKS",
        "KEEP_FIRST_COLUMN_COMMENT",
        "CALL_PARAMETERS_WRAP",
//        "CALL_PARAMETERS_LPAREN_ON_NEXT_LINE",
//        "CALL_PARAMETERS_RPAREN_ON_NEXT_LINE",
        "METHOD_PARAMETERS_WRAP",
//        "METHOD_PARAMETERS_LPAREN_ON_NEXT_LINE",
//        "METHOD_PARAMETERS_RPAREN_ON_NEXT_LINE",
//        "ALIGN_MULTILINE_PARAMETERS",
//        "ALIGN_MULTILINE_PARAMETERS_IN_CALLS",
//        "ALIGN_MULTILINE_BINARY_OPERATION",
        "BINARY_OPERATION_WRAP"
//        "BINARY_OPERATION_SIGN_ON_NEXT_LINE",
//        "PARENTHESES_EXPRESSION_LPAREN_WRAP",
//        "PARENTHESES_EXPRESSION_RPAREN_WRAP"
      );
      
      consumer.renameStandardOption("CALL_PARAMETERS_WRAP", "Call arguments");
      consumer.renameStandardOption("METHOD_PARAMETERS_WRAP", "Function parameters");
      
      showCustomWrapOption(consumer, "EXPRESSION_IN_CLAUSE_WRAP", "Expression in clause", null);
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "ALIGN_MULTILINE_BLOCK", "Blocks (fun...end, etc)", ALIGNMENT);
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "ALIGN_FUNCTION_CLAUSES", "Function clauses", ALIGNMENT);
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "ALIGN_FUN_CLAUSES", "Fun expression clauses", ALIGNMENT);
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "ALIGN_GUARDS", "Guards", ALIGNMENT);
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "ALIGN_RECORD_FIELD_ASSIGNMENTS", "Record field assignments", ALIGNMENT);
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "INDENT_RELATIVE", "Honor relative", null);
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "NEW_LINE_BEFORE_COMMA", "Comma first style", null);
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "NEW_LINE_AFTER_ARROW", "Newline after '->'", null,
        ErlangCodeStyleSettings.NewLineAfterArrow.OPTIONS, ErlangCodeStyleSettings.NewLineAfterArrow.VALUES);
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "UNIFORM_BINARY_EXPRESSIONS", "Uniform binary expressions style", null);
    }
  }

  private static void showCustomWrapOption(CodeStyleSettingsCustomizable consumer, String name, String title, @Nullable String groupName) {
    consumer.showCustomOption(ErlangCodeStyleSettings.class, name, title, groupName, 
      CodeStyleSettingsCustomizable.WRAP_OPTIONS, CodeStyleSettingsCustomizable.WRAP_VALUES);
  }

  private static final String DEFAULT_CODE_SAMPLE =
    """
      %% ping comment
      ping(0, Pong_PID) ->
          Pong_PID ! finished,
          tut15:pong(),
          io:format("Ping finished~n", []);

      %% pong comment
      ping(N, Pong_PID)->
          Pong_PID ! {ping, self()},
          receive
              pong ->
                  io:format("Ping received pong~n", [])
          end,
          ping(N - 1, Pong_PID).

      pong() ->
          receive
              finished ->
                  io:format("Pong finished~n", []);
              {ping, Ping_PID} ->
                  io:format("Pong received ping~n", []),
                  Ping_PID ! pong,
                  pong()
          end.

      start() ->
          Pong_PID = spawn(tut15, pong, []),
          spawn(tut15, ping, [30 * 1000, Pong_PID]).

      -record(rec, {id, name="Default", binary}).

      f() ->
          Ok = true == false,
          Ok = true =:= false,
          #rec{id = 1, binary = <<"BINARY">>},
          X = case 1 of
              Z -> Z
          end.
         \s
      sort([Pivot|T]) ->
          sort([ X || X <- T, X < Pivot]) ++
          [Pivot] ++
          sort([ X || X <- T, X >= Pivot]);
      sort([]) -> [].""";

  private static final String BLANK_LINES_CODE_SAMPLE =
    """
      hello(Name) ->
          io:format("Hello, " ++ Name).

      hello() -> hello("IntelliJ").

      """;
  private static final String INDENT_CODE_SAMPLE =
    """
      foo() ->
          io:format("Hello"),
          List = [
            1,
            2
          ].""";
}
