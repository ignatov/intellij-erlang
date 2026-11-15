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

public class ElpConfigurationTest extends TestCase {
  public void testOffByDefault() {
    ElpConfiguration configuration = ElpConfiguration.resolve(new ElpSettings.ElpSettingsState().path,
                                                              new ElpSettings.ElpSettingsState().mode, null, null);
    assertFalse(configuration.isEnabled());
    assertEquals("", configuration.path());
    assertEquals(ElpMode.PLUGIN, configuration.mode());
    assertFalse(configuration.eqwalizer());
  }

  public void testEqwalizerIsOffByDefault() {
    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    assertFalse(ElpConfiguration.resolve("/opt/elp", state.mode, state.eqwalizer, null, null, null).eqwalizer());
    assertFalse(new ElpConfiguration("/opt/elp", ElpMode.ELP).eqwalizer());
  }

  public void testStoredEqwalizer() {
    assertTrue(ElpConfiguration.resolve("/opt/elp", "ELP", true, null, null, null).eqwalizer());
  }

  public void testEqwalizerPropertyOverridesStoredEqwalizer() {
    assertTrue(ElpConfiguration.resolve("/opt/elp", null, false, null, null, "true").eqwalizer());
    assertTrue(ElpConfiguration.resolve("/opt/elp", null, false, null, null, " TRUE ").eqwalizer());
    assertFalse(ElpConfiguration.resolve("/opt/elp", null, true, null, null, "false").eqwalizer());
  }

  public void testUnknownEqwalizerPropertyIsIgnored() {
    assertTrue(ElpConfiguration.resolve("/opt/elp", null, true, null, null, "").eqwalizer());
    assertTrue(ElpConfiguration.resolve("/opt/elp", null, true, null, null, "no").eqwalizer());
    assertFalse(ElpConfiguration.resolve("/opt/elp", null, false, null, null, "yes").eqwalizer());
  }

  public void testStoredSettings() {
    ElpConfiguration configuration = ElpConfiguration.resolve("/opt/elp/bin/elp", "ELP", null, null);
    assertTrue(configuration.isEnabled());
    assertEquals("/opt/elp/bin/elp", configuration.path());
    assertEquals(ElpMode.ELP, configuration.mode());
  }

  public void testPathIsTrimmed() {
    assertEquals("/opt/elp", ElpConfiguration.resolve("  /opt/elp ", null, null, null).path());
    assertFalse(ElpConfiguration.resolve("   ", null, null, null).isEnabled());
  }

  public void testPluginIsTheDefaultModeOnceAPathIsSet() {
    assertEquals(ElpMode.PLUGIN, ElpConfiguration.resolve("/opt/elp", null, null, null).mode());
    assertEquals(ElpMode.PLUGIN, ElpConfiguration.resolve("/opt/elp", "", null, null).mode());
    assertEquals(ElpMode.PLUGIN, ElpConfiguration.resolve("/opt/elp", "bogus", null, null).mode());
  }

  public void testPathPropertyOverridesStoredPath() {
    assertEquals("/sandbox/elp", ElpConfiguration.resolve("/opt/elp", "ELP", "/sandbox/elp", null).path());
    assertEquals("/sandbox/elp", ElpConfiguration.resolve("", null, "/sandbox/elp", null).path());
  }

  public void testEmptyPathPropertySwitchesTheServerOff() {
    assertFalse(ElpConfiguration.resolve("/opt/elp", "ELP", "", null).isEnabled());
  }

  public void testModePropertyOverridesStoredMode() {
    assertEquals(ElpMode.ELP, ElpConfiguration.resolve("/opt/elp", "PLUGIN", null, "ELP").mode());
    assertEquals(ElpMode.PLUGIN, ElpConfiguration.resolve("/opt/elp", "ELP", null, "plugin").mode());
  }

  public void testPrototypeModeNamesMapToTheEnginePositions() {
    assertEquals(ElpMode.ELP, ElpConfiguration.resolve("/opt/elp", "ALL", null, null).mode());
    assertEquals(ElpMode.PLUGIN, ElpConfiguration.resolve("/opt/elp", "DIAGNOSTICS", null, null).mode());
    assertEquals(ElpMode.ELP, ElpConfiguration.resolve("/opt/elp", "PLUGIN", null, "all").mode());
    assertEquals(ElpMode.PLUGIN, ElpConfiguration.resolve("/opt/elp", "ELP", null, "diagnostics").mode());
  }

  public void testUnknownModePropertyIsIgnored() {
    assertEquals(ElpMode.ELP, ElpConfiguration.resolve("/opt/elp", "ELP", null, "everything").mode());
  }

  public void testSystemProperties() {
    String oldPath = System.getProperty(ElpConfiguration.PATH_PROPERTY);
    String oldMode = System.getProperty(ElpConfiguration.MODE_PROPERTY);
    String oldEqwalizer = System.getProperty(ElpConfiguration.EQWALIZER_PROPERTY);
    try {
      System.clearProperty(ElpConfiguration.PATH_PROPERTY);
      System.clearProperty(ElpConfiguration.MODE_PROPERTY);
      System.clearProperty(ElpConfiguration.EQWALIZER_PROPERTY);
      assertEquals(new ElpConfiguration("/opt/elp", ElpMode.ELP), ElpConfiguration.resolve("/opt/elp", "ELP", false));
      assertEquals(new ElpConfiguration("/opt/elp", ElpMode.ELP, true), ElpConfiguration.resolve("/opt/elp", "ELP", true));

      System.setProperty(ElpConfiguration.PATH_PROPERTY, "/sandbox/elp");
      System.setProperty(ElpConfiguration.MODE_PROPERTY, "PLUGIN");
      System.setProperty(ElpConfiguration.EQWALIZER_PROPERTY, "true");
      assertEquals(new ElpConfiguration("/sandbox/elp", ElpMode.PLUGIN, true), ElpConfiguration.resolve("/opt/elp", "ELP", false));
      assertEquals(new ElpConfiguration("/sandbox/elp", ElpMode.PLUGIN, true), ElpConfiguration.resolve("", null, false));
    }
    finally {
      restore(ElpConfiguration.PATH_PROPERTY, oldPath);
      restore(ElpConfiguration.MODE_PROPERTY, oldMode);
      restore(ElpConfiguration.EQWALIZER_PROPERTY, oldEqwalizer);
    }
  }

  private static void restore(String key, String value) {
    if (value == null) System.clearProperty(key);
    else System.setProperty(key, value);
  }
}
