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
}
