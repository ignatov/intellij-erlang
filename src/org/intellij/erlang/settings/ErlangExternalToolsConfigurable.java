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

package org.intellij.erlang.settings;

import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.TitledSeparator;
import org.intellij.erlang.dialyzer.DialyzerSettings;
import org.intellij.erlang.emacs.EmacsSettings;
import org.intellij.erlang.rebar.settings.RebarConfigurationForm;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkForSmallIdes;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.sdk.ErlangSystemUtil;
import org.intellij.erlang.utils.ExtProcessUtil;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.File;

public class ErlangExternalToolsConfigurable implements SearchableConfigurable, Configurable.NoScroll {
  public static final String ERLANG_RELATED_TOOLS = "Erlang External Tools";

  private final Project myProject;
  private JPanel myPanel;
  private TextFieldWithBrowseButton myEmacsPathSelector;
  private JTextField myEmacsVersionText;
  private RebarConfigurationForm myRebarConfigurationForm;
  private TextFieldWithBrowseButton myPltPathSelector;
  private String myPrevEmacsPath;
  private EmacsSettings myEmacsSettings;
  private RebarSettings myRebarSettings;
  private DialyzerSettings myDialyzerSettings;
  private TextFieldWithBrowseButton mySdkPathSelector;
  private TitledSeparator mySdkTitledSeparator;
  private JLabel mySdkPathLabel;

  public ErlangExternalToolsConfigurable(@NotNull Project project) {
    myProject = project;
    myRebarSettings = RebarSettings.getInstance(project);
    myEmacsSettings = EmacsSettings.getInstance(project);
    myDialyzerSettings = DialyzerSettings.getInstance(project);
    myEmacsPathSelector.addBrowseFolderListener("Select Emacs executable", "", null, FileChooserDescriptorFactory.createSingleLocalFileDescriptor());
    myPltPathSelector.addBrowseFolderListener("Select dialyzer PLT", "", null, FileChooserDescriptorFactory.createSingleLocalFileDescriptor());
    mySdkPathSelector.addBrowseFolderListener("Select Erlang SDK path", "", null, FileChooserDescriptorFactory.getDirectoryChooserDescriptor("Erlang SDK root"));
    myPrevEmacsPath = myEmacsSettings.getEmacsPath();

    if (StringUtil.isEmpty(myRebarSettings.getRebarPath())) {
      VirtualFile baseDir = project.getBaseDir();
      if (baseDir != null) {
        VirtualFile rebar = baseDir.findChild("rebar");
        if (rebar != null) {
          String canonicalPath = rebar.getCanonicalPath();
          if (canonicalPath != null) {
            myRebarSettings.setRebarPath(canonicalPath);
          }
        }
      }
    }

    if (StringUtil.isEmpty(myEmacsSettings.getEmacsPath()) && (SystemInfo.isLinux || SystemInfo.isMac)) {
      String suggestedPath = "/usr/bin/emacs";
      File file = new File(suggestedPath);
      if (file.exists() && FileUtil.canExecute(file)) {
        myEmacsSettings.setEmacsPath(suggestedPath);
      }
    }
    
    if (!ErlangSystemUtil.isSmallIde()) {
      mySdkPathSelector.setVisible(false);
      mySdkTitledSeparator.setVisible(false);
      mySdkPathLabel.setVisible(false);
    }

    reset();
  }

  @NotNull
  @Override
  public String getId() {
    return ERLANG_RELATED_TOOLS;
  }

  @Nullable
  @Override
  public Runnable enableSearch(String option) {
    return null;
  }

  @NonNls
  @Override
  public String getDisplayName() {
    return ERLANG_RELATED_TOOLS;
  }

  @Nullable
  @Override
  public String getHelpTopic() {
    return null;
  }

  @Nullable
  @Override
  public JComponent createComponent() {
    myRebarConfigurationForm.createComponent();
    return myPanel;
  }

  @Override
  public boolean isModified() {
    String emacsSelectedPath = myEmacsPathSelector.getText();
    if (!myPrevEmacsPath.equals(emacsSelectedPath)) validateEmacsPath();

    return
         !myRebarSettings.getRebarPath().equals(myRebarConfigurationForm.getPath())
      || !myEmacsSettings.getEmacsPath().equals(emacsSelectedPath)
      || !myDialyzerSettings.getCurrentPltPath().equals(myPltPathSelector.getText())
      || !StringUtil.notNullize(ErlangSdkType.getSdkPath(myProject)).equals(mySdkPathSelector.getText());
  }

  @Override
  public void apply() throws ConfigurationException {
    myRebarSettings.setRebarPath(myRebarConfigurationForm.getPath());
    myEmacsSettings.setEmacsPath(myEmacsPathSelector.getText());
    myDialyzerSettings.setCurrentPltPath(myPltPathSelector.getText());
    if (ErlangSystemUtil.isSmallIde()) {
      ErlangSdkForSmallIdes.setUpOrUpdateSdk(myProject, mySdkPathSelector.getText());
    }
  }

  @Override
  public void reset() {
    myRebarConfigurationForm.setPath(myRebarSettings.getRebarPath());
    myEmacsPathSelector.setText(myEmacsSettings.getEmacsPath());
    myPltPathSelector.setText(myDialyzerSettings.getCurrentPltPath());
    mySdkPathSelector.setText(StringUtil.notNullize(ErlangSdkType.getSdkPath(myProject)));
    validateEmacsPath();
  }

  @Override
  public void disposeUIResources() {
  }

  private void validateEmacsPath() {
    String rawVersion = ExtProcessUtil.execAndGetFirstLine(3000, myEmacsPathSelector.getText(), "--version").getStdOut();
    myEmacsVersionText.setText(StringUtil.containsIgnoreCase(rawVersion, "emacs") ? rawVersion : "N/A");
  }
}
