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

package org.intellij.erlang.runconfig;

import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.util.Disposer;
import com.intellij.ui.TitledSeparator;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBList;
import com.intellij.util.ui.UIUtil;
import org.intellij.erlang.application.ui.ErlangRunConfigurationEditorForm;
import org.intellij.erlang.debugger.remote.ui.ErlangRemoteDebugConfigurationEditorForm;
import org.intellij.erlang.eunit.ErlangUnitRunConfiguration;
import org.intellij.erlang.eunit.ErlangUnitRunConfigurationType;
import org.intellij.erlang.eunit.ui.ErlangUnitRunConfigurationEditorForm;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.util.List;

public class ErlangRunConfigurationEditorUiTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testApplicationDebugOptionsAreCollapsed() {
    assertDebugOptions(new ErlangRunConfigurationEditorForm(getProject()), false);
  }

  public void testUnitDebugOptionsAreCollapsed() {
    assertDebugOptions(new ErlangUnitRunConfigurationEditorForm(getProject()), false);
  }

  public void testRemoteDebugOptionsAreExpanded() {
    assertDebugOptions(new ErlangRemoteDebugConfigurationEditorForm(), true);
  }

  public void testUnitTestKindRenderer() {
    ErlangUnitRunConfigurationEditorForm editor = new ErlangUnitRunConfigurationEditorForm(getProject());
    try {
      editor.resetFrom(new ErlangUnitRunConfiguration(getProject(), "eunit", ErlangUnitRunConfigurationType.getInstance()));
      JComboBox<?> testKindComboBox = findTestKindComboBox(editor.getComponent());
      @SuppressWarnings("unchecked")
      ListCellRenderer<Object> renderer = (ListCellRenderer<Object>) testKindComboBox.getRenderer();
      JBList<Object> list = new JBList<>();
      assertEquals("Module", getRenderedText(renderer, list, ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE));
      assertEquals("Function", getRenderedText(renderer, list, ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION));
    }
    finally {
      Disposer.dispose(editor);
    }
  }

  private static void assertDebugOptions(SettingsEditor<?> editor, boolean expanded) {
    try {
      JComponent root = editor.getComponent();
      TitledSeparator title = UIUtil.findComponentOfType(root, TitledSeparator.class);
      assertNotNull(title);
      assertEquals("&Debug Options", title.getText());

      JBCheckBox autoUpdate = findAutoUpdateCheckBox(root);
      assertEquals(expanded, isVisibleIn(autoUpdate, root));

      title.dispatchEvent(new MouseEvent(title, MouseEvent.MOUSE_RELEASED, System.currentTimeMillis(), 0, 1, 1, 1, false));
      assertEquals(!expanded, isVisibleIn(autoUpdate, root));
    }
    finally {
      Disposer.dispose(editor);
    }
  }

  private static JBCheckBox findAutoUpdateCheckBox(JComponent root) {
    for (JBCheckBox checkBox : UIUtil.findComponentsOfType(root, JBCheckBox.class)) {
      if ("Automatically detect modules not to interpret".equals(checkBox.getText())) return checkBox;
    }
    throw new AssertionError("The debug options content is not found");
  }

  private static boolean isVisibleIn(Component component, JComponent root) {
    for (Component c = component; c != null && c != root; c = c.getParent()) {
      if (!c.isVisible()) return false;
    }
    return true;
  }

  private static JComboBox<?> findTestKindComboBox(JComponent root) {
    @SuppressWarnings("rawtypes")
    List<JComboBox> comboBoxes = UIUtil.findComponentsOfType(root, JComboBox.class);
    for (JComboBox<?> comboBox : comboBoxes) {
      if (comboBox.getItemCount() > 0 && comboBox.getItemAt(0) instanceof ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind) {
        return comboBox;
      }
    }
    throw new AssertionError("The test kind combo box is not found");
  }

  private static String getRenderedText(ListCellRenderer<Object> renderer, JList<Object> list, Object value) {
    Component component = renderer.getListCellRendererComponent(list, value, 0, false, false);
    return ((JLabel) component).getText();
  }
}
