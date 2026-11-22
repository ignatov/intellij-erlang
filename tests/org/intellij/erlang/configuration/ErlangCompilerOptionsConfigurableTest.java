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

package org.intellij.erlang.configuration;

import com.intellij.ui.RawCommandLineEditor;
import com.intellij.util.ui.UIUtil;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import javax.swing.*;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;

public class ErlangCompilerOptionsConfigurableTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  // The Settings dialog hides and shows the component of a visited page; showing it again must not drop unsaved edits; #1405
  public void testRevisitKeepsUnsavedEdits() {
    ErlangCompilerOptionsConfigurable configurable = new ErlangCompilerOptionsConfigurable(getProject());
    try {
      JComponent component = configurable.createComponent();
      configurable.reset();
      RawCommandLineEditor editor = UIUtil.findComponentOfType(component, RawCommandLineEditor.class);
      assertNotNull(editor);
      editor.setText("+foo");
      assertTrue(configurable.isModified());

      AncestorEvent shown = new AncestorEvent(component, AncestorEvent.ANCESTOR_ADDED, component, component.getParent());
      for (AncestorListener listener : component.getAncestorListeners()) {
        listener.ancestorAdded(shown);
      }

      assertEquals("+foo", editor.getText());
      assertTrue(configurable.isModified());
    }
    finally {
      configurable.disposeUIResources();
    }
  }

  public void testRevisitKeepsUnsavedCheckBoxes() {
    ErlangCompilerOptionsConfigurable configurable = new ErlangCompilerOptionsConfigurable(getProject());
    try {
      JComponent component = configurable.createComponent();
      configurable.reset();
      JCheckBox debugInfo = findCheckBox(component, "debug info");
      boolean stored = debugInfo.isSelected();
      debugInfo.setSelected(!stored);
      assertTrue(configurable.isModified());

      fireShown(component);

      assertEquals(!stored, debugInfo.isSelected());
      assertTrue(configurable.isModified());
    }
    finally {
      configurable.disposeUIResources();
    }
  }

  public void testResetReloadsStoredValues() {
    ErlangCompilerOptionsConfigurable configurable = new ErlangCompilerOptionsConfigurable(getProject());
    try {
      JComponent component = configurable.createComponent();
      configurable.reset();
      RawCommandLineEditor editor = UIUtil.findComponentOfType(component, RawCommandLineEditor.class);
      assertNotNull(editor);
      editor.setText("+foo");
      JCheckBox debugInfo = findCheckBox(component, "debug info");
      boolean stored = debugInfo.isSelected();
      debugInfo.setSelected(!stored);

      configurable.reset();

      assertEquals("", editor.getText());
      assertEquals(stored, debugInfo.isSelected());
      assertFalse(configurable.isModified());
    }
    finally {
      configurable.disposeUIResources();
    }
  }

  public void testRevisitRefreshesRebarState() {
    RebarSettings rebarSettings = RebarSettings.getInstance(getProject());
    String oldRebarPath = rebarSettings.getRebarPath();
    ErlangCompilerOptionsConfigurable configurable = new ErlangCompilerOptionsConfigurable(getProject());
    try {
      rebarSettings.setRebarPath("");
      JComponent component = configurable.createComponent();
      configurable.reset();
      JCheckBox useRebar = findCheckBox(component, "rebar");
      JButton configureRebar = UIUtil.findComponentOfType(component, JButton.class);
      assertNotNull(configureRebar);
      RawCommandLineEditor editor = UIUtil.findComponentOfType(component, RawCommandLineEditor.class);
      assertNotNull(editor);
      assertFalse(useRebar.isEnabled());
      assertTrue(configureRebar.isVisible());
      editor.setText("+foo");

      rebarSettings.setRebarPath("/usr/bin/rebar3");
      fireShown(component);

      assertTrue(useRebar.isEnabled());
      assertFalse(configureRebar.isVisible());
      assertEquals("+foo", editor.getText());
      useRebar.setSelected(true);
      assertFalse(editor.isVisible());

      rebarSettings.setRebarPath("");
      fireShown(component);

      assertFalse(useRebar.isEnabled());
      assertFalse(useRebar.isSelected());
      assertTrue(configureRebar.isVisible());
      assertTrue(editor.isVisible());
      assertEquals("+foo", editor.getText());
    }
    finally {
      rebarSettings.setRebarPath(oldRebarPath);
      configurable.disposeUIResources();
    }
  }

  public void testRevisitAfterConfiguringRebarRestoresStoredRebarCompiler() {
    RebarSettings rebarSettings = RebarSettings.getInstance(getProject());
    ErlangCompilerSettings compilerSettings = ErlangCompilerSettings.getInstance(getProject());
    String oldRebarPath = rebarSettings.getRebarPath();
    boolean oldUseRebar = compilerSettings.isUseRebarCompilerEnabled();
    ErlangCompilerOptionsConfigurable configurable = new ErlangCompilerOptionsConfigurable(getProject());
    try {
      rebarSettings.setRebarPath("");
      compilerSettings.setUseRebarCompilerEnabled(true);
      JComponent component = configurable.createComponent();
      configurable.reset();
      fireShown(component);
      JCheckBox useRebar = findCheckBox(component, "rebar");
      assertFalse(useRebar.isSelected());
      assertFalse(useRebar.isEnabled());

      rebarSettings.setRebarPath("/usr/bin/rebar3");
      fireShown(component);

      assertTrue(useRebar.isEnabled());
      assertTrue(useRebar.isSelected());
      assertFalse(configurable.isModified());
      configurable.apply();
      assertTrue(compilerSettings.isUseRebarCompilerEnabled());
    }
    finally {
      rebarSettings.setRebarPath(oldRebarPath);
      compilerSettings.setUseRebarCompilerEnabled(oldUseRebar);
      configurable.disposeUIResources();
    }
  }

  // Opening the page with "compile with rebar" stored as on and no rebar path must not modify the page; #1470
  public void testOpeningWithoutRebarPathKeepsStoredRebarCompiler() {
    RebarSettings rebarSettings = RebarSettings.getInstance(getProject());
    ErlangCompilerSettings compilerSettings = ErlangCompilerSettings.getInstance(getProject());
    String oldRebarPath = rebarSettings.getRebarPath();
    boolean oldUseRebar = compilerSettings.isUseRebarCompilerEnabled();
    ErlangCompilerOptionsConfigurable configurable = new ErlangCompilerOptionsConfigurable(getProject());
    try {
      rebarSettings.setRebarPath("");
      compilerSettings.setUseRebarCompilerEnabled(true);
      JComponent component = configurable.createComponent();
      configurable.reset();
      fireShown(component);

      assertFalse(configurable.isModified());
      configurable.apply();
      assertTrue(compilerSettings.isUseRebarCompilerEnabled());
    }
    finally {
      rebarSettings.setRebarPath(oldRebarPath);
      compilerSettings.setUseRebarCompilerEnabled(oldUseRebar);
      configurable.disposeUIResources();
    }
  }

  private static void fireShown(JComponent component) {
    AncestorEvent shown = new AncestorEvent(component, AncestorEvent.ANCESTOR_ADDED, component, component.getParent());
    for (AncestorListener listener : component.getAncestorListeners()) {
      listener.ancestorAdded(shown);
    }
  }

  private static JCheckBox findCheckBox(JComponent component, String textPart) {
    for (JCheckBox checkBox : UIUtil.findComponentsOfType(component, JCheckBox.class)) {
      if (checkBox.getText().contains(textPart)) return checkBox;
    }
    throw new AssertionError("no check box with text containing " + textPart);
  }
}
