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

package org.intellij.erlang.context;

import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.UnnamedConfigurable;
import com.intellij.openapi.util.Comparing;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.ui.AnActionButton;
import com.intellij.ui.AnActionButtonRunnable;
import com.intellij.ui.ToolbarDecorator;
import com.intellij.ui.table.TableView;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.ContainerUtilRt;
import com.intellij.util.ui.ColumnInfo;
import com.intellij.util.ui.ElementProducer;
import com.intellij.util.ui.ListTableModel;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.event.KeyEvent;
import java.util.List;
import java.util.Map;

public class ErlangCompileContextConfigurable implements UnnamedConfigurable {
  private final ErlangCompileContext myCompileContext;

  private TableView<MyMacroDefinition> myMacrosTable;
  private TableView<String> myIncludesTable;

  private JPanel myRootPanel;
  private JPanel myMacrosPanel;
  private JPanel myIncludesPanel;

  public ErlangCompileContextConfigurable(ErlangCompileContext compileContext) {
    myCompileContext = compileContext;
  }

  @Nullable
  @Override
  public JComponent createComponent() {
    return myRootPanel;
  }

  @Override
  public boolean isModified() {
    return !Comparing.equal(getMacroDefinitionsFromUi(), myCompileContext.macroDefinitions) ||
      !Comparing.equal(getIncludePathsFromUi(), myCompileContext.includePaths);
  }

  @Override
  public void apply() throws ConfigurationException {
    myCompileContext.macroDefinitions = getMacroDefinitionsFromUi();
    myCompileContext.includePaths = getIncludePathsFromUi();
  }

  @Override
  public void reset() {
    myMacrosTable.getListTableModel().setItems(definitionsList(myCompileContext.macroDefinitions));
    myIncludesTable.getListTableModel().setItems(ContainerUtil.newArrayList(myCompileContext.includePaths));
  }

  @Override
  public void disposeUIResources() {
  }

  private void createUIComponents() {
    myMacrosTable = new TableView<MyMacroDefinition>(new MyMacrosTableModel(myCompileContext.macroDefinitions));
    myMacrosTable.getInputMap(JComponent.WHEN_FOCUSED).put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "startEditing");
    myMacrosTable.setMinRowHeight(new JTextField().getPreferredSize().height);
    myMacrosPanel = ToolbarDecorator.createDecorator(myMacrosTable, new ElementProducer<MyMacroDefinition>() {
      @Override
      public MyMacroDefinition createElement() {
        return new MyMacroDefinition("", "");
      }

      @Override
      public boolean canCreateElement() {
        return true;
      }
    }).setMoveDownAction(null)
      .setMoveUpAction(null)
      .createPanel();

    myIncludesTable = new TableView<String>(new ListTableModel<String>(new ColumnInfo("Path") {
      @Nullable
      @Override
      public Object valueOf(Object o) {
        return o;
      }
    }));
    myIncludesPanel = ToolbarDecorator.createDecorator(myIncludesTable)
      .setMoveUpAction(null)
      .setMoveUpAction(null)
      .setAddAction(new AnActionButtonRunnable() {
        @Override
        public void run(AnActionButton anActionButton) {
          //TODO show path chooser, update model
        }
      })
      .createPanel();
  }

  private Map<String, String> getMacroDefinitionsFromUi() {
    List<MyMacroDefinition> definitions = myMacrosTable.getItems();
    Map<String, String> definitionsMap = ContainerUtilRt.newHashMap(definitions.size());
    for (MyMacroDefinition definition : definitions) {
      String macroName = StringUtil.nullize(StringUtil.trim(definition.name), true);
      if (macroName != null) {
        definitionsMap.put(macroName, StringUtil.notNullize(StringUtil.nullize(definition.definition, true), "true"));
      }
    }
    return definitionsMap;
  }

  private List<String> getIncludePathsFromUi() {
    return ContainerUtil.newArrayList(myIncludesTable.getItems());
  }

  private static List<MyMacroDefinition> definitionsList(Map<String, String> definitionsMap) {
    List<MyMacroDefinition> definitionsList = ContainerUtil.newArrayListWithCapacity(definitionsMap.size());
    for (Map.Entry<String,String> nameDefinitionPair : definitionsMap.entrySet()) {
      definitionsList.add(new MyMacroDefinition(nameDefinitionPair.getKey(), nameDefinitionPair.getValue()));
    }
    return definitionsList;
  }


  private static class MyMacrosTableModel extends ListTableModel<MyMacroDefinition> {
    public MyMacrosTableModel(Map<String, String> definitions) {
      super(createColumnInfos(), definitionsList(definitions));
    }

    private static ColumnInfo[] createColumnInfos() {
      return new ColumnInfo[]{
        new ColumnInfo<MyMacroDefinition, String>("Name") {
          @Nullable
          @Override
          public String valueOf(MyMacroDefinition o) {
            return o.name;
          }

          @Override
          public void setValue(MyMacroDefinition myMacroDefinition, String value) {
            myMacroDefinition.name = value;
          }

          @Override
          public boolean isCellEditable(MyMacroDefinition myMacroDefinition) {
            return true;
          }
        },
        new ColumnInfo<MyMacroDefinition, String>("Definition") {
          @Nullable
          @Override
          public String valueOf(MyMacroDefinition o) {
            return o.definition;
          }

          @Override
          public void setValue(MyMacroDefinition myMacroDefinition, String value) {
            myMacroDefinition.definition = value;
          }

          @Override
          public boolean isCellEditable(MyMacroDefinition myMacroDefinition) {
            return true;
          }
        }
      };
    }
  }

  private static class MyMacroDefinition {
    public String name;
    public String definition;

    public MyMacroDefinition(String name, String definition) {
      this.name = name;
      this.definition = definition;
    }
  }
}
