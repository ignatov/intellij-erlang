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

package org.intellij.erlang.elp;

import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.ui.ComboBox;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.ui.TitledSeparator;
import com.intellij.ui.components.JBLabel;
import com.intellij.util.ui.FormBuilder;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

import static org.intellij.erlang.utils.ErlangUiUtil.addBrowseFolderListener;

/**
 * The "ELP language server (experimental)" section of the Erlang External Tools page.
 */
public final class ElpSettingsPanel {
  private final TextFieldWithBrowseButton myPathField = new TextFieldWithBrowseButton();
  private final ComboBox<ElpMode> myModeBox = new ComboBox<>(ElpMode.values());
  private final JBLabel myOverrideLabel = new JBLabel();
  private final JPanel myPanel;

  public ElpSettingsPanel() {
    addBrowseFolderListener(myPathField, "Select ELP Executable", "", null, FileChooserDescriptorFactory.singleFile());
    myOverrideLabel.setComponentStyle(UIUtil.ComponentStyle.SMALL);
    myOverrideLabel.setFontColor(UIUtil.FontColor.BRIGHTER);
    myPanel = FormBuilder.createFormBuilder()
      .addComponent(new TitledSeparator("ELP Language Server (Experimental)"))
      .addLabeledComponent("Path to elp:", myPathField)
      .addComponentToRightColumn(hint("Empty path: no server is started. ELP needs erl and rebar3; the SDK bin and the rebar " +
                                      "directory are put first on its PATH."))
      .addLabeledComponent("Mode:", myModeBox)
      .addComponentToRightColumn(hint("DIAGNOSTICS: only diagnostics and their quick fixes. ALL: every LSP feature, " +
                                      "side by side with the plugin's own."))
      .addComponentToRightColumn(myOverrideLabel)
      .getPanel();
  }

  @NotNull
  private static JBLabel hint(@NotNull String text) {
    JBLabel label = new JBLabel(text);
    label.setComponentStyle(UIUtil.ComponentStyle.SMALL);
    label.setFontColor(UIUtil.FontColor.BRIGHTER);
    return label;
  }

  @NotNull
  public JComponent getComponent() {
    return myPanel;
  }

  public boolean isModified() {
    ElpSettings settings = ElpSettings.getInstance();
    return !settings.getStoredPath().equals(myPathField.getText().trim()) || settings.getStoredMode() != myModeBox.getItem();
  }

  public void apply() {
    ElpMode mode = myModeBox.getItem();
    ElpSettings.getInstance().update(myPathField.getText().trim(), mode != null ? mode : ElpMode.DIAGNOSTICS);
    updateOverrideLabel();
  }

  public void reset() {
    ElpSettings settings = ElpSettings.getInstance();
    myPathField.setText(settings.getStoredPath());
    myModeBox.setItem(settings.getStoredMode());
    updateOverrideLabel();
  }

  private void updateOverrideLabel() {
    boolean pathOverridden = System.getProperty(ElpConfiguration.PATH_PROPERTY) != null;
    boolean modeOverridden = ElpMode.parse(System.getProperty(ElpConfiguration.MODE_PROPERTY)) != null;
    if (!pathOverridden && !modeOverridden) {
      myOverrideLabel.setText("");
      return;
    }
    ElpConfiguration effective = ElpSettings.getInstance().getEffectiveConfiguration();
    myOverrideLabel.setText("Overridden by -D" + (pathOverridden ? ElpConfiguration.PATH_PROPERTY : "") +
                            (pathOverridden && modeOverridden ? " and -D" : "") +
                            (modeOverridden ? ElpConfiguration.MODE_PROPERTY : "") +
                            ": path '" + effective.path() + "', mode " + effective.mode());
  }
}
