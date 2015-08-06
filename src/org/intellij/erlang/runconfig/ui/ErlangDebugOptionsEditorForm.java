/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.runconfig.ui;

import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.ui.InputValidator;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.ui.*;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBList;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.runconfig.ErlangRunConfigurationBase;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collections;
import java.util.Set;

public class ErlangDebugOptionsEditorForm extends SettingsEditor<ErlangRunConfigurationBase.ErlangDebugOptions> {
  private JPanel myContent;
  private JPanel myModulesNotToInterpretPanel;
  private JBCheckBox myAutoUpdateModulesNotToInterpretCheckBox;

  private JBList myModulesNotToInterpretList;
  private CollectionListModel myModulesNotToInterpretListModel;

  public ErlangDebugOptionsEditorForm() {
    myAutoUpdateModulesNotToInterpretCheckBox.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(@NotNull ActionEvent e) {
        setAutoUpdateModulesNotToInterpret(myAutoUpdateModulesNotToInterpretCheckBox.isSelected());
      }
    });
  }

  @Override
  protected void resetEditorFrom(ErlangRunConfigurationBase.ErlangDebugOptions erlangDebugOptions) {
    myModulesNotToInterpretListModel.removeAll();
    for (String module : erlangDebugOptions.getModulesNotToInterpret()) {
      //noinspection unchecked
      myModulesNotToInterpretListModel.add(module);
    }
    setAutoUpdateModulesNotToInterpret(erlangDebugOptions.isAutoUpdateModulesNotToInterpret());
  }

  @Override
  protected void applyEditorTo(ErlangRunConfigurationBase.ErlangDebugOptions erlangDebugOptions) throws ConfigurationException {
    erlangDebugOptions.setAutoUpdateModulesNotToInterpret(myAutoUpdateModulesNotToInterpretCheckBox.isSelected());
    Set<String> modules = erlangDebugOptions.isAutoUpdateModulesNotToInterpret() ? Collections.<String>emptySet() :
      ContainerUtil.map2Set(myModulesNotToInterpretListModel.getItems(),
        new Function<Object, String>() {
          @Override
          public String fun(Object o) {
            return String.valueOf(o);
          }
        });
    erlangDebugOptions.setModulesNotToInterpret(modules);
  }

  @NotNull
  @Override
  protected JComponent createEditor() {
    return myContent;
  }

  private void setAutoUpdateModulesNotToInterpret(boolean autoUpdate) {
    myAutoUpdateModulesNotToInterpretCheckBox.setSelected(autoUpdate);
    myModulesNotToInterpretPanel.setEnabled(!autoUpdate);
    myModulesNotToInterpretList.setEnabled(!autoUpdate);
  }

  private void createUIComponents() {
    //noinspection unchecked
    myModulesNotToInterpretListModel = new CollectionListModel();
    myModulesNotToInterpretList = new JBList(myModulesNotToInterpretListModel);
    myModulesNotToInterpretList.setCellRenderer(new JBList.StripedListCellRenderer());
    myModulesNotToInterpretList.setEmptyText("Add non-debuggable modules here (e.g. NIF modules)");
    myModulesNotToInterpretPanel = ToolbarDecorator.createDecorator(myModulesNotToInterpretList, myModulesNotToInterpretListModel)
      .setAddAction(new AnActionButtonRunnable() {
        @Override
        public void run(AnActionButton anActionButton) {
          InputValidator inputValidator = new InputValidator() {
            @Override
            public boolean checkInput(String moduleName) {
              return !StringUtil.isEmptyOrSpaces(moduleName) &&
                !myModulesNotToInterpretListModel.getItems().contains(StringUtil.trim(moduleName));
            }

            @Override
            public boolean canClose(String s) {
              return true;
            }
          };
          String module = Messages.showInputDialog(myModulesNotToInterpretList,
            "Module name", "Add a Non-Debuggable Module", ErlangIcons.FILE, null, inputValidator);
          if (module != null) {
            //noinspection unchecked
            myModulesNotToInterpretListModel.add(StringUtil.trim(module));
          }
        }
      })
      .setRemoveAction(new AnActionButtonRunnable() {
        @Override
        public void run(AnActionButton anActionButton) {
          ListUtil.removeSelectedItems(myModulesNotToInterpretList);
        }
      })
      .setMoveUpAction(null)
      .setMoveDownAction(null)
      .createPanel();
    myModulesNotToInterpretPanel.setBorder(IdeBorderFactory.createTitledBorder("Modules not to interpret", true));
  }
}
