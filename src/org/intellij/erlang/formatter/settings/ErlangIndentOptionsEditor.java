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

package org.intellij.erlang.formatter.settings;

import com.intellij.application.options.SmartIndentOptionsEditor;
import com.intellij.psi.codeStyle.CodeStyleConstraints;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.intellij.ui.components.fields.IntegerField;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class ErlangIndentOptionsEditor extends SmartIndentOptionsEditor {
  private static final String GUARD_INDENT_LABEL = "Guard indent:";

  private JLabel myGuardIndentLabel;
  private IntegerField myGuardIndentField;

  @Override
  protected void addComponents() {
    super.addComponents();
    myGuardIndentField = createIndentTextField(GUARD_INDENT_LABEL, CodeStyleConstraints.MIN_INDENT_SIZE, CodeStyleConstraints.MAX_INDENT_SIZE, -1);
    myGuardIndentLabel = new JLabel(GUARD_INDENT_LABEL);
    add(myGuardIndentLabel, myGuardIndentField);
  }

  @Override
  public boolean isModified(CodeStyleSettings settings, CommonCodeStyleSettings.IndentOptions options) {
    return super.isModified(settings, options) ||
           getUIGuardIndent() != settings.getCustomSettings(ErlangCodeStyleSettings.class).GUARD_INDENT_SIZE;
  }

  @Override
  public void apply(CodeStyleSettings settings, CommonCodeStyleSettings.IndentOptions options) {
    super.apply(settings, options);
    settings.getCustomSettings(ErlangCodeStyleSettings.class).GUARD_INDENT_SIZE = getUIGuardIndent();
  }

  @Override
  public void reset(@NotNull CodeStyleSettings settings, @NotNull CommonCodeStyleSettings.IndentOptions options) {
    super.reset(settings, options);
    myGuardIndentField.setValue(settings.getCustomSettings(ErlangCodeStyleSettings.class).GUARD_INDENT_SIZE);
    myGuardIndentField.setDefaultValueText(Integer.toString(options.INDENT_SIZE));
  }

  @Override
  public void setEnabled(boolean enabled) {
    super.setEnabled(enabled);
    if (myGuardIndentField != null) {
      myGuardIndentLabel.setEnabled(enabled);
      myGuardIndentField.setEnabled(enabled);
    }
  }

  @Override
  protected void setVisible(boolean visible) {
    super.setVisible(visible);
    if (myGuardIndentField != null) {
      myGuardIndentLabel.setVisible(visible);
      myGuardIndentField.setVisible(visible);
    }
  }

  private int getUIGuardIndent() {
    Integer value = myGuardIndentField.getValue();
    return value != null ? value : -1;
  }
}
