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
import com.intellij.ui.SimpleListCellRenderer;
import com.intellij.ui.TitledSeparator;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBLabel;
import com.intellij.util.ui.FormBuilder;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;

import static org.intellij.erlang.utils.ErlangUiUtil.addBrowseFolderListener;

/**
 * The "ELP language server (experimental)" section of the Erlang External Tools page.
 */
public final class ElpSettingsPanel {
  private final TextFieldWithBrowseButton myPathField = new TextFieldWithBrowseButton();
  private final ComboBox<ElpMode> myModeBox = new ComboBox<>(ElpMode.values());
  private final JBCheckBox myEqwalizerBox = new JBCheckBox("Show eqWAlizer type errors");
  private final JBLabel myOverrideLabel = new JBLabel();
  private final JPanel myPanel;

  public ElpSettingsPanel() {
    addBrowseFolderListener(myPathField, "Select ELP Executable", "", null, FileChooserDescriptorFactory.singleFile());
    myModeBox.setRenderer(SimpleListCellRenderer.create("", ElpMode::getPresentableName));
    myOverrideLabel.setComponentStyle(UIUtil.ComponentStyle.SMALL);
    myOverrideLabel.setFontColor(UIUtil.FontColor.BRIGHTER);
    myPanel = FormBuilder.createFormBuilder()
      .addComponent(new TitledSeparator("ELP Language Server (Experimental)"))
      .addLabeledComponent("Path to elp:", myPathField)
      .addComponentToRightColumn(hint("Empty path: no server is started. ELP needs erl and rebar3; the SDK bin and the rebar " +
                                      "directory are put first on its PATH."))
      .addLabeledComponent("Engine:", myModeBox)
      .addComponentToRightColumn(hint("<html>Plugin: the plugin's own resolve serves everything; ELP adds its diagnostics.<br>" +
                                      "ELP: while ELP runs, it serves completion, parameter hints, documentation, function folding, " +
                                      "extend selection,<br>go to declaration and its intentions, and its diagnostics replace " +
                                      "the plugin's unresolved and unused checks.<br>Everything else stays with the plugin.</html>"))
      .addComponentToRightColumn(myEqwalizerBox)
      .addComponentToRightColumn(hint("eqWAlizer is ELP's gradual type checker. Code not written for it gets many type " +
                                      "errors that erlc does not report; they are shown as warnings."))
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
    return !settings.getStoredPath().equals(myPathField.getText().trim()) || settings.getStoredMode() != myModeBox.getItem() ||
           settings.getStoredEqwalizer() != myEqwalizerBox.isSelected();
  }

  public void apply() {
    ElpMode mode = myModeBox.getItem();
    ElpSettings.getInstance().update(myPathField.getText().trim(), mode != null ? mode : ElpMode.PLUGIN,
                                     myEqwalizerBox.isSelected());
    updateOverrideLabel();
  }

  public void reset() {
    ElpSettings settings = ElpSettings.getInstance();
    myPathField.setText(settings.getStoredPath());
    myModeBox.setItem(settings.getStoredMode());
    myEqwalizerBox.setSelected(settings.getStoredEqwalizer());
    updateOverrideLabel();
  }

  private void updateOverrideLabel() {
    boolean pathOverridden = System.getProperty(ElpConfiguration.PATH_PROPERTY) != null;
    boolean modeOverridden = ElpMode.parse(System.getProperty(ElpConfiguration.MODE_PROPERTY)) != null;
    boolean eqwalizerOverridden = ElpConfiguration.parseBoolean(System.getProperty(ElpConfiguration.EQWALIZER_PROPERTY)) != null;
    List<String> overridden = new ArrayList<>();
    if (pathOverridden) overridden.add("-D" + ElpConfiguration.PATH_PROPERTY);
    if (modeOverridden) overridden.add("-D" + ElpConfiguration.MODE_PROPERTY);
    if (eqwalizerOverridden) overridden.add("-D" + ElpConfiguration.EQWALIZER_PROPERTY);
    if (overridden.isEmpty()) {
      myOverrideLabel.setText("");
      return;
    }
    ElpConfiguration effective = ElpSettings.getInstance().getEffectiveConfiguration();
    myOverrideLabel.setText("Overridden by " + String.join(" and ", overridden) +
                            ": path '" + effective.path() + "', engine " + effective.mode().getPresentableName() +
                            ", eqWAlizer " + (effective.eqwalizer() ? "on" : "off"));
  }
}
