/*
 * Copyright 2012-2013 Sergey Ignatov
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

/**
 * @author ignatov
 */
public class ErlangLanguageCodeStyleSettingsProvider extends LanguageCodeStyleSettingsProvider {
  @NotNull
  @Override
  public Language getLanguage() {
    return ErlangLanguage.INSTANCE;
  }

  @Override
  public String getCodeSample(@NotNull SettingsType settingsType) {
    if (settingsType == SettingsType.SPACING_SETTINGS) {
      return SPACING_CODE_SAMPLE;
    }
    if (settingsType == SettingsType.WRAPPING_AND_BRACES_SETTINGS) {
      return WRAPPING_CODE_SAMPLE;
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
      consumer.renameStandardOption("SPACE_AROUND_ASSIGNMENT_OPERATORS", "Assignment and receive operators");
      consumer.renameStandardOption("SPACE_AROUND_LOGICAL_OPERATORS", "Logical operators");
      consumer.renameStandardOption("SPACE_AROUND_EQUALITY_OPERATORS", "Equality operators");
      consumer.renameStandardOption("SPACE_AROUND_ADDITIVE_OPERATORS", "Additive operators");
      consumer.renameStandardOption("SPACE_AROUND_MULTIPLICATIVE_OPERATORS", "Multiplicative operators");
    }
    else if (settingsType == SettingsType.BLANK_LINES_SETTINGS) {
      consumer.showStandardOptions("KEEP_BLANK_LINES_IN_CODE");
    }
    else if (settingsType == SettingsType.WRAPPING_AND_BRACES_SETTINGS) {
      consumer.showStandardOptions(
//        "KEEP_LINE_BREAKS",
        "KEEP_FIRST_COLUMN_COMMENT"//,
//        "CALL_PARAMETERS_WRAP",
//        "CALL_PARAMETERS_LPAREN_ON_NEXT_LINE",
//        "CALL_PARAMETERS_RPAREN_ON_NEXT_LINE",
//        "METHOD_PARAMETERS_WRAP",
//        "METHOD_PARAMETERS_LPAREN_ON_NEXT_LINE",
//        "METHOD_PARAMETERS_RPAREN_ON_NEXT_LINE",
//        "ALIGN_MULTILINE_PARAMETERS",
//        "ALIGN_MULTILINE_PARAMETERS_IN_CALLS",
//        "ALIGN_MULTILINE_BINARY_OPERATION",
//        "BINARY_OPERATION_WRAP",
//        "BINARY_OPERATION_SIGN_ON_NEXT_LINE",
//        "PARENTHESES_EXPRESSION_LPAREN_WRAP",
//        "PARENTHESES_EXPRESSION_RPAREN_WRAP"
      );
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "ALIGN_MULTILINE_BLOCK", "Blocks (fun...end, etc)", "Alignment");
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "ALIGN_FUNCTION_CLAUSES", "Function clauses", "Alignment");
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "ALIGN_GUARDS", "Guards", "Alignment");
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "INDENT_RELATIVE", "Honor relative", null);
      consumer.showCustomOption(ErlangCodeStyleSettings.class, "NEW_LINE_BEFORE_COMMA", "Comma first style", null);
    }
  }

  private static final String SPACING_CODE_SAMPLE =
    "ping(0, Pong_PID) ->\n" +
      "    Pong_PID ! finished,\n" +
      "    tut15:pong(),\n" +
      "    io:format(\"Ping finished~n\", []);\n" +
      "\n" +
      "ping(N, Pong_PID)->\n" +
      "    Pong_PID ! {ping, self()},\n" +
      "    receive\n" +
      "        pong ->\n" +
      "            io:format(\"Ping received pong~n\", [])\n" +
      "    end,\n" +
      "    ping(N - 1, Pong_PID).\n" +
      "\n" +
      "pong() ->\n" +
      "    receive\n" +
      "        finished ->\n" +
      "            io:format(\"Pong finished~n\", []);\n" +
      "        {ping, Ping_PID} ->\n" +
      "            io:format(\"Pong received ping~n\", []),\n" +
      "            Ping_PID ! pong,\n" +
      "            pong()\n" +
      "    end.\n" +
      "\n" +
      "start() ->\n" +
      "    Pong_PID = spawn(tut15, pong, []),\n" +
      "    spawn(tut15, ping, [30, Pong_PID]).";

  private static final String WRAPPING_CODE_SAMPLE =
    "%% ping comment\n" +
      "ping(0, Pong_PID) ->\n" +
      "    Pong_PID ! finished,\n" +
      "    tut15:pong(),\n" +
      "    io:format(\"Ping finished~n\", []);\n" +
      "\n" +
      "" +
      "%% pong comment\n" +
      "ping(N, Pong_PID)->\n" +
      "    Pong_PID ! {ping, self()},\n" +
      "    receive\n" +
      "        pong ->\n" +
      "            io:format(\"Ping received pong~n\", [])\n" +
      "    end,\n" +
      "    ping(N - 1, Pong_PID).\n" +
      "\n" +
      "pong() ->\n" +
      "    receive\n" +
      "        finished ->\n" +
      "            io:format(\"Pong finished~n\", []);\n" +
      "        {ping, Ping_PID} ->\n" +
      "            io:format(\"Pong received ping~n\", []),\n" +
      "            Ping_PID ! pong,\n" +
      "            pong()\n" +
      "    end.\n" +
      "\n" +
      "start() ->\n" +
      "    Pong_PID = spawn(tut15, pong, []),\n" +
      "    spawn(tut15, ping, [30, Pong_PID]).\n" +
      "\n" +
      "f() ->\n" +
      "    X = case 1 of\n" +
      "        Z -> Z\n" +
      "    end.";

  private static final String BLANK_LINES_CODE_SAMPLE =
    "hello(Name) ->\n" +
      "    io:format(\"Hello, \" ++ Name).\n" +
      "\n" +
      "hello() -> hello(\"IntelliJ\").\n" +
      "\n";
  private static final String INDENT_CODE_SAMPLE =
    "foo() ->\n" +
      "    io:format(\"Hello\"),\n" +
      "    List = [\n" +
      "      1,\n" +
      "      2\n" +
      "    ].";
}
