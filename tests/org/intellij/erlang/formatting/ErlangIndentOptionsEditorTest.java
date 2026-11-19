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

package org.intellij.erlang.formatting;

import com.intellij.application.options.CodeStyle;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.intellij.psi.codeStyle.LanguageCodeStyleSettingsProvider;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.formatter.settings.ErlangCodeStyleSettings;
import org.intellij.erlang.formatter.settings.ErlangIndentOptionsEditor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangIndentOptionsEditorTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testGuardIndentDefault() {
    CodeStyleSettings settings = CodeStyle.createTestSettings();
    assertEquals(-1, guardIndent(settings));

    ErlangIndentOptionsEditor editor = createEditor(settings);
    assertFalse(editor.isModified(settings, indentOptions(settings)));
  }

  public void testGuardIndentRoundTrip() {
    CodeStyleSettings source = CodeStyle.createTestSettings();
    source.getCustomSettings(ErlangCodeStyleSettings.class).GUARD_INDENT_SIZE = 2;
    ErlangIndentOptionsEditor editor = createEditor(source);
    assertFalse(editor.isModified(source, indentOptions(source)));

    CodeStyleSettings target = CodeStyle.createTestSettings();
    assertTrue(editor.isModified(target, indentOptions(target)));
    editor.apply(target, indentOptions(target));
    assertEquals(2, guardIndent(target));
    assertFalse(editor.isModified(target, indentOptions(target)));

    editor.reset(CodeStyle.createTestSettings(), indentOptions(target));
    editor.apply(target, indentOptions(target));
    assertEquals(-1, guardIndent(target));
  }

  public void testDefaultIndentOptions() {
    CommonCodeStyleSettings.IndentOptions indentOptions = indentOptions(CodeStyle.createTestSettings());
    assertEquals(2, indentOptions.INDENT_SIZE);
    assertEquals(4, indentOptions.CONTINUATION_INDENT_SIZE);
    assertEquals(2, indentOptions.TAB_SIZE);
  }

  public void testSettingsPageUsesErlangEditor() {
    LanguageCodeStyleSettingsProvider provider = LanguageCodeStyleSettingsProvider.forLanguage(ErlangLanguage.INSTANCE);
    assertNotNull(provider);
    assertInstanceOf(provider.getIndentOptionsEditor(), ErlangIndentOptionsEditor.class);
  }

  public void testGuardIndentHintFollowsIndentField() {
    CodeStyleSettings settings = CodeStyle.createTestSettings();
    indentOptions(settings).INDENT_SIZE = 2;
    ErlangIndentOptionsEditor editor = createEditor(settings);
    assertEquals("2", editor.getGuardIndentField().getEmptyText().getText());

    editor.getIndentField().setText("6");
    assertEquals("6", editor.getGuardIndentField().getEmptyText().getText());

    editor.apply(settings, indentOptions(settings));
    assertEquals(6, indentOptions(settings).INDENT_SIZE);
    assertEquals(-1, guardIndent(settings));
  }

  private static ErlangIndentOptionsEditor createEditor(CodeStyleSettings settings) {
    ErlangIndentOptionsEditor editor = new ErlangIndentOptionsEditor();
    editor.createPanel();
    editor.reset(settings, indentOptions(settings));
    return editor;
  }

  private static CommonCodeStyleSettings.IndentOptions indentOptions(CodeStyleSettings settings) {
    CommonCodeStyleSettings.IndentOptions indentOptions = settings.getIndentOptions(ErlangFileType.MODULE);
    assertNotNull(indentOptions);
    return indentOptions;
  }

  private static int guardIndent(CodeStyleSettings settings) {
    return settings.getCustomSettings(ErlangCodeStyleSettings.class).GUARD_INDENT_SIZE;
  }
}
