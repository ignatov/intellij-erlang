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
import com.intellij.openapi.ui.ComboBox;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBLabel;
import com.intellij.util.ui.UIUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * The ELP section of the Erlang External Tools page: what it shows, what it stores, and that the running servers are told once.
 */
public class ElpSettingsPanelTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private ElpSettings.ElpSettingsState myOldState;
  private String myOldPathProperty;
  private String myOldModeProperty;
  private String myOldEqwalizerProperty;
  private final AtomicInteger myChanges = new AtomicInteger();

  private ElpSettingsPanel myPanel;
  private TextFieldWithBrowseButton myPath;
  private ComboBox<?> myMode;
  private JBCheckBox myEqwalizer;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    ElpSettings.ElpSettingsState old = ElpSettings.getInstance().getState();
    myOldState = new ElpSettings.ElpSettingsState();
    myOldState.path = old.path;
    myOldState.mode = old.mode;
    myOldState.eqwalizer = old.eqwalizer;
    myOldPathProperty = System.getProperty(ElpConfiguration.PATH_PROPERTY);
    myOldModeProperty = System.getProperty(ElpConfiguration.MODE_PROPERTY);
    myOldEqwalizerProperty = System.getProperty(ElpConfiguration.EQWALIZER_PROPERTY);
    System.clearProperty(ElpConfiguration.PATH_PROPERTY);
    System.clearProperty(ElpConfiguration.MODE_PROPERTY);
    System.clearProperty(ElpConfiguration.EQWALIZER_PROPERTY);
    store("/opt/elp/bin/elp", "ELP", true);
    ApplicationManager.getApplication().getMessageBus().connect(getTestRootDisposable())
      .subscribe(ElpSettings.TOPIC, (ElpSettings.Listener)myChanges::incrementAndGet);

    myPanel = new ElpSettingsPanel();
    myPath = UIUtil.findComponentOfType(myPanel.getComponent(), TextFieldWithBrowseButton.class);
    myMode = UIUtil.findComponentOfType(myPanel.getComponent(), ComboBox.class);
    myEqwalizer = UIUtil.findComponentOfType(myPanel.getComponent(), JBCheckBox.class);
    assertNotNull(myPath);
    assertNotNull(myMode);
    assertNotNull(myEqwalizer);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ElpSettings.getInstance().loadState(myOldState);
      restore(ElpConfiguration.PATH_PROPERTY, myOldPathProperty);
      restore(ElpConfiguration.MODE_PROPERTY, myOldModeProperty);
      restore(ElpConfiguration.EQWALIZER_PROPERTY, myOldEqwalizerProperty);
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  public void testResetShowsTheStoredSettings() {
    myPanel.reset();
    assertEquals("/opt/elp/bin/elp", myPath.getText());
    assertEquals(ElpMode.ELP, myMode.getSelectedItem());
    assertTrue(myEqwalizer.isSelected());
    assertFalse(myPanel.isModified());
  }

  /** A mode of the prototype is shown as the position it migrated to, and is not a modification. */
  public void testPrototypeModeIsShownAsItsPosition() {
    store("/opt/elp/bin/elp", "ALL", false);
    myPanel.reset();
    assertEquals(ElpMode.ELP, myMode.getSelectedItem());
    assertFalse(myPanel.isModified());
  }

  public void testApplyStoresTheChangesAndTellsTheServersOnce() {
    myPanel.reset();
    myPath.setText("  /usr/local/bin/elp  ");
    myMode.setSelectedItem(ElpMode.PLUGIN);
    myEqwalizer.setSelected(false);
    assertTrue(myPanel.isModified());

    myPanel.apply();
    ElpSettings settings = ElpSettings.getInstance();
    assertEquals("/usr/local/bin/elp", settings.getStoredPath());
    assertEquals(ElpMode.PLUGIN, settings.getStoredMode());
    assertFalse(settings.getStoredEqwalizer());
    assertEquals(1, myChanges.get());
    assertFalse("the trimmed path is what was stored", myPanel.isModified());

    myPanel.apply();
    assertEquals("nothing changed, nothing is restarted", 1, myChanges.get());
  }

  public void testEachFieldIsAModification() {
    for (Runnable change : List.<Runnable>of(() -> myPath.setText("/other/elp"), () -> myMode.setSelectedItem(ElpMode.PLUGIN),
                                             () -> myEqwalizer.setSelected(false))) {
      myPanel.reset();
      change.run();
      assertTrue(myPanel.isModified());
    }
  }

  /** The page says when a system property wins over the stored settings, and what is in effect. */
  public void testOverrideByASystemPropertyIsShown() {
    myPanel.reset();
    assertEquals("", overrideText());
    System.setProperty(ElpConfiguration.MODE_PROPERTY, "plugin");
    myPanel.reset();
    assertEquals("Overridden by -Derlang.elp.mode: path '/opt/elp/bin/elp', engine Plugin, eqWAlizer on", overrideText());
  }

  /** The text of the last label of the section, which reports the overrides. */
  @NotNull
  private String overrideText() {
    List<JBLabel> labels = UIUtil.findComponentsOfType(myPanel.getComponent(), JBLabel.class);
    assertNotEmpty(labels);
    return labels.getLast().getText();
  }

  private static void store(@NotNull String path, @NotNull String mode, boolean eqwalizer) {
    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    state.path = path;
    state.mode = mode;
    state.eqwalizer = eqwalizer;
    ElpSettings.getInstance().loadState(state);
  }

  private static void restore(@NotNull String key, String value) {
    if (value == null) System.clearProperty(key);
    else System.setProperty(key, value);
  }
}
