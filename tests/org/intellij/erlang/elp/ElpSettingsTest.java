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

import junit.framework.TestCase;

public class ElpSettingsTest extends TestCase {
  public void testPluginIsTheStoredDefault() {
    assertEquals(ElpMode.PLUGIN, new ElpSettings().getStoredMode());
    assertEquals("PLUGIN", new ElpSettings.ElpSettingsState().mode);
  }

  public void testStoredAllIsMigratedToTheElpPosition() {
    assertMigrated("ALL", "ELP", ElpMode.ELP);
    assertMigrated("all", "ELP", ElpMode.ELP);
  }

  public void testStoredDiagnosticsIsMigratedToThePluginPosition() {
    assertMigrated("DIAGNOSTICS", "PLUGIN", ElpMode.PLUGIN);
  }

  public void testCurrentNamesAreKept() {
    assertMigrated("ELP", "ELP", ElpMode.ELP);
    assertMigrated("PLUGIN", "PLUGIN", ElpMode.PLUGIN);
  }

  public void testUnknownModeIsKeptAndReadsAsThePluginPosition() {
    assertMigrated("everything", "everything", ElpMode.PLUGIN);
  }

  private static void assertMigrated(String stored, String expectedState, ElpMode expectedMode) {
    ElpSettings settings = new ElpSettings();
    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    state.path = "/opt/elp/bin/elp";
    state.mode = stored;
    settings.loadState(state);
    assertEquals(expectedState, settings.getState().mode);
    assertEquals(expectedMode, settings.getStoredMode());
  }
}
