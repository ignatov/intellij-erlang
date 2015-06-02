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

package org.intellij.erlang.configuration;

import com.intellij.compiler.options.CompilerConfigurable;
import com.intellij.ide.DataManager;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.options.newEditor.OptionsEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.settings.ErlangExternalToolsConfigurable;
import org.intellij.erlang.utils.AncestorAdapter;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.event.AncestorEvent;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class ErlangCompilerOptionsConfigurable extends CompilerConfigurable {

  private JPanel myRootPanel;
  private JCheckBox myUseRebarCompilerCheckBox;
  private JButton myConfigureRebarButton;
  private JCheckBox myAddDebugInfoCheckBox;
  private final ErlangCompilerSettings mySettings;
  private final Project myProject;

  public ErlangCompilerOptionsConfigurable(Project project) {
    super(project);
    myProject = project;
    mySettings = ErlangCompilerSettings.getInstance(project);
    setupUiListeners();
  }

  private void setupUiListeners() {
    myConfigureRebarButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        OptionsEditor optionsEditor = OptionsEditor.KEY.getData(DataManager.getInstance().getDataContext(myConfigureRebarButton));
        assert optionsEditor != null;
        SearchableConfigurable erlangExternalToolsConfigurable = optionsEditor.findConfigurableById(ErlangExternalToolsConfigurable.ERLANG_RELATED_TOOLS);
        optionsEditor.select(erlangExternalToolsConfigurable);
      }
    });
    myRootPanel.addAncestorListener(new AncestorAdapter() {
      @Override
      public void ancestorAdded(AncestorEvent event) {
        reset();
      }
    });
  }

  @NotNull
  @Override
  public String getId() {
    return "Erlang compiler";
  }

  @Override
  public String getDisplayName() {
    return "Erlang Compiler";
  }

  @Override
  public JComponent createComponent() {
    return myRootPanel;
  }

  @Override
  public void reset() {
    boolean rebarPathIsSet = StringUtil.isNotEmpty(RebarSettings.getInstance(myProject).getRebarPath());
    myUseRebarCompilerCheckBox.setEnabled(rebarPathIsSet);
    myConfigureRebarButton.setVisible(!rebarPathIsSet);
    myUseRebarCompilerCheckBox.setSelected(rebarPathIsSet && mySettings.isUseRebarCompilerEnabled());
    myAddDebugInfoCheckBox.setSelected(mySettings.isAddDebugInfoEnabled());
  }

  @Override
  public void apply() throws ConfigurationException {
    mySettings.setUseRebarCompilerEnabled(myUseRebarCompilerCheckBox.isSelected());
    mySettings.setAddDebugInfoEnabled(myAddDebugInfoCheckBox.isSelected());
  }

  @Override
  public boolean isModified() {
    return myUseRebarCompilerCheckBox.isSelected() != mySettings.isUseRebarCompilerEnabled() ||
      myAddDebugInfoCheckBox.isSelected() != mySettings.isAddDebugInfoEnabled();
  }
}
