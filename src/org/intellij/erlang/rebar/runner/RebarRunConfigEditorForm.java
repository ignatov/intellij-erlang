/*
 * Copyright 2012 Sergey Ignatov
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

package org.intellij.erlang.rebar.runner;

import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

final class RebarRunConfigEditorForm extends SettingsEditor<RebarRunConfig> {
  private JPanel myComponent;
  private JTextField myCommandText;

  @Override
  protected void resetEditorFrom(@NotNull RebarRunConfig rebarRunConfig) {
    myCommandText.setText(rebarRunConfig.getCommand());
  }

  @Override
  protected void applyEditorTo(@NotNull RebarRunConfig rebarRunConfig) throws ConfigurationException {
    rebarRunConfig.setCommand(myCommandText.getText());
  }

  @NotNull
  @Override
  protected JComponent createEditor() {
    return myComponent;
  }

  @Override
  protected void disposeEditor() {
    myComponent.setVisible(false);
  }
}
