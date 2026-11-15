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

import com.intellij.psi.PsiFile;
import com.intellij.testFramework.ServiceContainerUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

/**
 * The engine switch with a stand-in for the running ELP server: {@link ElpEngine#ownsFeature}.
 */
public class ElpEngineTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private ElpSettings.ElpSettingsState myOldState;
  private String myOldPathProperty;
  private String myOldModeProperty;
  private boolean myServing;

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
    System.clearProperty(ElpConfiguration.PATH_PROPERTY);
    System.clearProperty(ElpConfiguration.MODE_PROPERTY);
    ServiceContainerUtil.registerOrReplaceServiceInstance(getProject(), ElpServerStatus.class, (file, feature) -> myServing,
                                                          getTestRootDisposable());
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ElpSettings.getInstance().loadState(myOldState);
      restore(ElpConfiguration.PATH_PROPERTY, myOldPathProperty);
      restore(ElpConfiguration.MODE_PROPERTY, myOldModeProperty);
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  public void testOwnsFeaturesOnlyInTheElpPositionWhileTheServerServesTheFile() {
    PsiFile module = myFixture.addFileToProject("src/a.erl", "-module(a).");
    PsiFile header = myFixture.addFileToProject("include/a.hrl", "");
    PsiFile appSrc = myFixture.addFileToProject("src/a.app.src", "{application, a, []}.");

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertTrue(ElpEngine.ownsFeature(module, ElpEngine.Feature.COMPLETION));
    assertTrue(ElpEngine.ownsFeature(header, ElpEngine.Feature.NAVIGATION));
    assertFalse("ELP does not get .app.src", ElpEngine.ownsFeature(appSrc, ElpEngine.Feature.COMPLETION));
    assertFalse(ElpEngine.ownsFeature(null, ElpEngine.Feature.COMPLETION));
    assertFalse(ElpEngine.ownsFeature(getProject(), null, ElpEngine.Feature.COMPLETION));

    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertFalse("no running server, the plugin answers", ElpEngine.ownsFeature(module, ElpEngine.Feature.COMPLETION));

    configure("/opt/elp/bin/elp", ElpMode.PLUGIN, true);
    assertFalse(ElpEngine.ownsFeature(module, ElpEngine.Feature.COMPLETION));

    configure("", ElpMode.ELP, true);
    assertFalse("no path, no ELP", ElpEngine.ownsFeature(module, ElpEngine.Feature.COMPLETION));
  }

  public void testModePropertyOverridesTheStoredPosition() {
    PsiFile module = myFixture.addFileToProject("src/a.erl", "-module(a).");
    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    System.setProperty(ElpConfiguration.MODE_PROPERTY, "plugin");
    assertFalse(ElpEngine.ownsFeature(module, ElpEngine.Feature.COMPLETION));
  }

  private void configure(String path, ElpMode mode, boolean serving) {
    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    state.path = path;
    state.mode = mode.name();
    ElpSettings.getInstance().loadState(state);
    myServing = serving;
  }

  private static void restore(String key, String value) {
    if (value == null) System.clearProperty(key);
    else System.setProperty(key, value);
  }
}
