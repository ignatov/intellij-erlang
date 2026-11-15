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
    assertEquals(ElpMode.DIAGNOSTICS, configuration.mode());
  }

  public void testStoredSettings() {
    ElpConfiguration configuration = ElpConfiguration.resolve("/opt/elp/bin/elp", "ALL", null, null);
    assertTrue(configuration.isEnabled());
    assertEquals("/opt/elp/bin/elp", configuration.path());
    assertEquals(ElpMode.ALL, configuration.mode());
  }

  public void testPathIsTrimmed() {
    assertEquals("/opt/elp", ElpConfiguration.resolve("  /opt/elp ", null, null, null).path());
    assertFalse(ElpConfiguration.resolve("   ", null, null, null).isEnabled());
  }

  public void testDiagnosticsIsTheDefaultModeOnceAPathIsSet() {
    assertEquals(ElpMode.DIAGNOSTICS, ElpConfiguration.resolve("/opt/elp", null, null, null).mode());
    assertEquals(ElpMode.DIAGNOSTICS, ElpConfiguration.resolve("/opt/elp", "", null, null).mode());
    assertEquals(ElpMode.DIAGNOSTICS, ElpConfiguration.resolve("/opt/elp", "bogus", null, null).mode());
  }

  public void testPathPropertyOverridesStoredPath() {
    assertEquals("/sandbox/elp", ElpConfiguration.resolve("/opt/elp", "ALL", "/sandbox/elp", null).path());
    assertEquals("/sandbox/elp", ElpConfiguration.resolve("", null, "/sandbox/elp", null).path());
  }

  public void testEmptyPathPropertySwitchesTheServerOff() {
    assertFalse(ElpConfiguration.resolve("/opt/elp", "ALL", "", null).isEnabled());
  }

  public void testModePropertyOverridesStoredMode() {
    assertEquals(ElpMode.ALL, ElpConfiguration.resolve("/opt/elp", "DIAGNOSTICS", null, "ALL").mode());
    assertEquals(ElpMode.DIAGNOSTICS, ElpConfiguration.resolve("/opt/elp", "ALL", null, "diagnostics").mode());
  }

  public void testUnknownModePropertyIsIgnored() {
    assertEquals(ElpMode.ALL, ElpConfiguration.resolve("/opt/elp", "ALL", null, "everything").mode());
  }

  public void testSystemProperties() {
    String oldPath = System.getProperty(ElpConfiguration.PATH_PROPERTY);
    String oldMode = System.getProperty(ElpConfiguration.MODE_PROPERTY);
    try {
      System.clearProperty(ElpConfiguration.PATH_PROPERTY);
      System.clearProperty(ElpConfiguration.MODE_PROPERTY);
      assertEquals(new ElpConfiguration("/opt/elp", ElpMode.ALL), ElpConfiguration.resolve("/opt/elp", "ALL"));

      System.setProperty(ElpConfiguration.PATH_PROPERTY, "/sandbox/elp");
      System.setProperty(ElpConfiguration.MODE_PROPERTY, "DIAGNOSTICS");
      assertEquals(new ElpConfiguration("/sandbox/elp", ElpMode.DIAGNOSTICS), ElpConfiguration.resolve("/opt/elp", "ALL"));
      assertEquals(new ElpConfiguration("/sandbox/elp", ElpMode.DIAGNOSTICS), ElpConfiguration.resolve("", null));
    }
    finally {
      restore(ElpConfiguration.PATH_PROPERTY, oldPath);
      restore(ElpConfiguration.MODE_PROPERTY, oldMode);
    }
  }

  private static void restore(String key, String value) {
    if (value == null) System.clearProperty(key);
    else System.setProperty(key, value);
  }
}
