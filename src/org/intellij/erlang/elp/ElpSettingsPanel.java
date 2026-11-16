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

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.ComboBox;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.ui.TitledSeparator;
import com.intellij.ui.components.ActionLink;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBRadioButton;
import com.intellij.util.ui.FormBuilder;
import com.intellij.util.ui.UIUtil;
import org.intellij.erlang.elp.distribution.ElpAssetChoice;
import org.intellij.erlang.elp.distribution.ElpDistribution;
import org.intellij.erlang.elp.distribution.ElpPlatform;
import org.intellij.erlang.elp.distribution.ElpRelease;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import javax.swing.*;
import java.awt.*;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Future;

import static org.intellij.erlang.utils.ErlangUiUtil.addBrowseFolderListener;

/**
 * The "ELP language server (experimental)" section of the Erlang External Tools page. The server is the build the plugin
 * downloads for the project, after a click on Download, or a custom path.
 */
public final class ElpSettingsPanel {
  private final Project myProject;
  private final JBRadioButton myDownloadedButton = new JBRadioButton("Downloaded by the plugin");
  private final JBRadioButton myCustomButton = new JBRadioButton("Custom path");
  private final JBLabel myDownloadStateLabel = new JBLabel();
  private final JButton myDownloadButton = new JButton();
  private final TextFieldWithBrowseButton myPathField = new TextFieldWithBrowseButton();
  private final ComboBox<ElpMode> myModeBox = new ComboBox<>(ElpMode.values());
  private final JBCheckBox myEqwalizerBox = new JBCheckBox("Show eqWAlizer type errors");
  private final JBLabel myOverrideLabel = new JBLabel();
  private final JPanel myPanel;
  @Nullable private ElpRelease.Asset myDownloadableAsset;
  @Nullable private Future<?> myDownloadStateRefresh;

  public ElpSettingsPanel(@NotNull Project project) {
    myProject = project;
    addBrowseFolderListener(myPathField, "Select ELP Executable", "", null, FileChooserDescriptorFactory.singleFile());
    ButtonGroup sourceGroup = new ButtonGroup();
    sourceGroup.add(myDownloadedButton);
    sourceGroup.add(myCustomButton);
    myDownloadedButton.addActionListener(e -> updateEnabled());
    myCustomButton.addActionListener(e -> updateEnabled());
    myDownloadButton.addActionListener(e -> startDownload());
    myDownloadButton.setVisible(false);
    myDownloadStateLabel.setAllowAutoWrapping(true);
    myOverrideLabel.setComponentStyle(UIUtil.ComponentStyle.SMALL);
    myOverrideLabel.setFontColor(UIUtil.FontColor.BRIGHTER);
    ElpRelease release = ElpRelease.pinned();
    JPanel downloadRow = new JPanel(new BorderLayout(UIUtil.DEFAULT_HGAP, 0));
    downloadRow.add(myDownloadStateLabel, BorderLayout.CENTER);
    downloadRow.add(myDownloadButton, BorderLayout.EAST);
    myPanel = FormBuilder.createFormBuilder()
      .addComponent(new TitledSeparator("ELP Language Server (Experimental)"))
      .addLabeledComponent("Server:", myDownloadedButton)
      .addComponentToRightColumn(downloadRow)
      .addComponentToRightColumn(hint("From " + release.repository() + "/releases, " + release.license().replace(" OR ", " or ") +
                                      " license; runs locally."))
      .addComponentToRightColumn(myCustomButton)
      .addLabeledComponent("Path to elp:", myPathField)
      .addComponentToRightColumn(hint("Empty path: no server is started. ELP needs erl and rebar3; the SDK bin and the rebar " +
                                      "directory are put first on its PATH."))
      .addLabeledComponent("Engine:", myModeBox)
      .addComponentToRightColumn(hint("<html>Plugin: ELP is not started.<br>ELP: ELP answers; the plugin fills the gaps.</html>"))
      .addComponentToRightColumn(new ActionLink("Tell us how it went", (java.awt.event.ActionListener)e -> ElpFeedback.open(myProject)))
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

  @NotNull
  private ElpServerSource selectedSource() {
    return myCustomButton.isSelected() ? ElpServerSource.CUSTOM : ElpServerSource.DOWNLOADED;
  }

  public boolean isModified() {
    ElpSettings settings = ElpSettings.getInstance();
    return settings.getStoredSource() != selectedSource() || !settings.getStoredPath().equals(myPathField.getText().trim()) ||
           settings.getStoredMode() != myModeBox.getItem() || settings.getStoredEqwalizer() != myEqwalizerBox.isSelected();
  }

  public void apply() {
    ElpMode mode = myModeBox.getItem();
    ElpSettings.getInstance().update(selectedSource(), myPathField.getText().trim(), mode != null ? mode : ElpMode.PLUGIN,
                                     myEqwalizerBox.isSelected());
    updateOverrideLabel();
  }

  public void reset() {
    ElpSettings settings = ElpSettings.getInstance();
    (settings.getStoredSource() == ElpServerSource.CUSTOM ? myCustomButton : myDownloadedButton).setSelected(true);
    myPathField.setText(settings.getStoredPath());
    myModeBox.setItem(settings.getStoredMode());
    myEqwalizerBox.setSelected(settings.getStoredEqwalizer());
    updateEnabled();
    updateOverrideLabel();
    refreshDownloadState();
  }

  private void updateEnabled() {
    myPathField.setEnabled(myCustomButton.isSelected());
  }

  /** Finds the build for the project and whether it is installed, off the EDT, and shows it. */
  private void refreshDownloadState() {
    myDownloadableAsset = null;
    myDownloadButton.setVisible(false);
    myDownloadStateLabel.setText("Checking...");
    ElpDistribution distribution = ElpDistribution.getInstance();
    myDownloadStateRefresh = ApplicationManager.getApplication().executeOnPooledThread(() -> {
      ElpPlatform platform = distribution.getPlatform();
      ElpAssetChoice choice = distribution.chooseAsset(myProject);
      ElpRelease.Asset asset = choice.asset();
      Path installed = asset != null ? distribution.findInstalled(asset) : null;
      ApplicationManager.getApplication().invokeLater(() -> showDownloadState(platform, choice, installed), ModalityState.any());
    });
  }

  private void showDownloadState(@NotNull ElpPlatform platform, @NotNull ElpAssetChoice choice, @Nullable Path installed) {
    ElpRelease.Asset asset = choice.asset();
    ElpDistribution distribution = ElpDistribution.getInstance();
    String version = distribution.getRelease().version();
    if (asset == null) {
      setDownloadState("ELP " + version + ": no build fits. " + choice.message(), null);
    }
    else if (installed != null) {
      setDownloadState(buildName(version, platform, asset) + ": installed.", null);
    }
    else if (distribution.isDownloading(asset)) {
      setDownloadState("Downloading ELP " + version + "...", null);
      follow(asset);
    }
    else {
      setDownloadState(buildName(version, platform, asset) + ": not downloaded.", asset);
    }
  }

  @NotNull
  private static String buildName(@NotNull String version, @NotNull ElpPlatform platform, @NotNull ElpRelease.Asset asset) {
    return "ELP " + version + " for " + platform.presentableName() + ", OTP " + asset.otp() + " build";
  }

  private void setDownloadState(@NotNull String text, @Nullable ElpRelease.Asset downloadable) {
    myDownloadStateLabel.setText("<html>" + StringUtil.escapeXmlEntities(text) + "</html>");
    myDownloadableAsset = downloadable;
    myDownloadButton.setVisible(downloadable != null);
    myDownloadButton.setEnabled(downloadable != null);
    if (downloadable != null) myDownloadButton.setText("Download (" + downloadable.sizeInMegabytes() + ")");
  }

  private void startDownload() {
    ElpRelease.Asset asset = myDownloadableAsset;
    if (asset == null) return;
    myDownloadButton.setEnabled(false);
    myDownloadStateLabel.setText("Downloading ELP " + ElpDistribution.getInstance().getRelease().version() + "...");
    follow(asset);
  }

  /** Starts the download of {@code asset}, or joins the one that runs, and shows how it ends. */
  private void follow(@NotNull ElpRelease.Asset asset) {
    ElpDistribution.getInstance().download(myProject, asset, new ElpDistribution.DownloadCallback() {
      @Override
      public void installed(@NotNull Path elp) {
        refreshDownloadState();
      }

      @Override
      public void failed(@NotNull String message) {
        setDownloadState("Download failed. " + message, asset);
      }

      @Override
      public void cancelled() {
        refreshDownloadState();
      }
    });
  }

  /** The background part of the last refresh of the download state; its result is shown by a later EDT event. */
  @TestOnly
  @Nullable
  Future<?> getDownloadStateRefresh() {
    return myDownloadStateRefresh;
  }

  @TestOnly
  @NotNull
  JBLabel getDownloadStateLabel() {
    return myDownloadStateLabel;
  }

  @TestOnly
  @NotNull
  JButton getDownloadButton() {
    return myDownloadButton;
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
