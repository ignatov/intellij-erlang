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
import com.intellij.openapi.application.impl.LaterInvocator;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.ui.ComboBox;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.io.NioFiles;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBRadioButton;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.ui.UIUtil;
import org.intellij.erlang.elp.distribution.ElpDistribution;
import org.intellij.erlang.elp.distribution.ElpInstaller;
import org.intellij.erlang.elp.distribution.ElpPlatform;
import org.intellij.erlang.elp.distribution.ElpRelease;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * The ELP section of the Erlang External Tools page: what it shows, what it stores, and that the running servers are told once.
 * The project is on OTP 28 and the machine is taken for macOS arm64, with the downloads installed in a temporary directory.
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
  private JBRadioButton myDownloaded;
  private JBRadioButton myCustom;
  private Path myBaseDir;
  private ElpInstaller myInstaller;

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R28/", ErlangSdkRelease.V_28_0);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    myBaseDir = Files.createTempDirectory("erlang-elp-panel-test");
    myInstaller = new ElpInstaller(myBaseDir, ElpRelease.pinned(), "http://127.0.0.1:1/", (executable, release, indicator) -> {});
    ElpDistribution.getInstance().setUpForTests(myInstaller, new ElpPlatform("macos", "aarch64"), getTestRootDisposable());
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

    myPanel = new ElpSettingsPanel(getProject());
    myPath = UIUtil.findComponentOfType(myPanel.getComponent(), TextFieldWithBrowseButton.class);
    myMode = UIUtil.findComponentOfType(myPanel.getComponent(), ComboBox.class);
    myEqwalizer = UIUtil.findComponentOfType(myPanel.getComponent(), JBCheckBox.class);
    List<JBRadioButton> sources = UIUtil.findComponentsOfType(myPanel.getComponent(), JBRadioButton.class);
    assertNotNull(myPath);
    assertNotNull(myMode);
    assertNotNull(myEqwalizer);
    assertEquals(2, sources.size());
    myDownloaded = sources.get(0);
    myCustom = sources.get(1);
    assertEquals("Downloaded by the plugin", myDownloaded.getText());
    assertEquals("Custom path", myCustom.getText());
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      if (myPanel != null) waitForDownloadState();
      if (myBaseDir != null) NioFiles.deleteRecursively(myBaseDir);
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
    assertTrue("a stored path of an older version is a custom path", myCustom.isSelected());
    assertTrue(myPath.isEnabled());
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

  /** Whoever moves from ELP back to the plugin on this page is not offered ELP again. */
  public void testSwitchingFromElpToThePluginEndsTheOffer() {
    myPanel.reset();
    myEqwalizer.setSelected(false);
    myPanel.apply();
    assertFalse(ElpSettings.getInstance().isOfferNeverAsk());

    myMode.setSelectedItem(ElpMode.PLUGIN);
    myPanel.apply();
    assertTrue(ElpSettings.getInstance().isOfferNeverAsk());
  }

  /** Applying the page in the Plugin position, as most users do, does not end the offer. */
  public void testApplyingThePluginPositionKeepsTheOffer() {
    store("", "PLUGIN", false);
    myPanel.reset();
    myEqwalizer.setSelected(true);
    myPanel.apply();
    assertFalse(ElpSettings.getInstance().isOfferNeverAsk());
  }

  public void testEachFieldIsAModification() {
    for (Runnable change : List.<Runnable>of(() -> myPath.setText("/other/elp"), () -> myMode.setSelectedItem(ElpMode.PLUGIN),
                                             () -> myEqwalizer.setSelected(false), () -> myDownloaded.setSelected(true))) {
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

  /** Settings of an older version without a path: the downloaded server, and the path field is off. */
  public void testEmptyStoredPathShowsTheDownloadedServer() {
    store("", "PLUGIN", false);
    myPanel.reset();
    assertTrue(myDownloaded.isSelected());
    assertFalse(myPath.isEnabled());
    assertFalse(myPanel.isModified());
    myCustom.doClick();
    assertTrue(myPath.isEnabled());
    assertTrue(myPanel.isModified());
  }

  /** Choosing the downloaded server keeps the custom path for later, but it is no longer in effect. */
  public void testApplyStoresTheSource() {
    myPanel.reset();
    myDownloaded.doClick();
    myPanel.apply();
    ElpSettings settings = ElpSettings.getInstance();
    assertEquals(ElpServerSource.DOWNLOADED, settings.getStoredSource());
    assertEquals("/opt/elp/bin/elp", settings.getStoredPath());
    assertEquals("", settings.getEffectiveConfiguration().path());
    assertEquals(1, myChanges.get());
    assertFalse(myPanel.isModified());

    myCustom.doClick();
    myPath.setText("");
    myPanel.apply();
    assertEquals("a custom empty path stays custom", ElpServerSource.CUSTOM, settings.getStoredSource());
    assertEquals(2, myChanges.get());
  }

  public void testDownloadStateNotInstalled() throws Exception {
    myPanel.reset();
    assertEquals("Checking...", myPanel.getDownloadStateLabel().getText());
    waitForDownloadState();
    assertEquals("ELP 1.1.0 for macOS arm64, OTP 28.5 build: not downloaded.", downloadStateText());
    assertTrue(myPanel.getDownloadButton().isVisible());
    assertTrue(myPanel.getDownloadButton().isEnabled());
    assertEquals("Download (17.8 MB)", myPanel.getDownloadButton().getText());
    assertEquals("nothing is downloaded without the click", List.of(), NioFiles.list(myBaseDir));
  }

  /** The Settings dialog is modal: the state must still leave "Checking..." while it is open. */
  public void testDownloadStateIsShownInAModalDialog() throws Exception {
    Object dialog = new Object();
    LaterInvocator.enterModal(dialog);
    try {
      myPanel.reset();
      waitForDownloadState();
      assertEquals("ELP 1.1.0 for macOS arm64, OTP 28.5 build: not downloaded.", downloadStateText());
      assertTrue(myPanel.getDownloadButton().isVisible());
    }
    finally {
      LaterInvocator.leaveModal(dialog);
    }
  }

  public void testDownloadStateInstalled() throws Exception {
    ElpRelease.Asset asset = ElpRelease.pinned().assets().stream()
      .filter(a -> a.file().equals("elp-macos-aarch64-apple-darwin-otp-28.5.tar.gz")).findFirst().orElseThrow();
    Path elp = myInstaller.executable(asset);
    Files.createDirectories(elp.getParent());
    Files.writeString(elp, "installed");
    myPanel.reset();
    waitForDownloadState();
    assertEquals("ELP 1.1.0 for macOS arm64, OTP 28.5 build: installed.", downloadStateText());
    assertFalse(myPanel.getDownloadButton().isVisible());
  }

  /** The click starts the download; here nothing listens on the port, and the failure is shown with the button back. */
  public void testFailedDownloadIsShown() throws Exception {
    myPanel.reset();
    waitForDownloadState();
    myPanel.getDownloadButton().doClick();
    long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(60);
    while (!downloadStateText().startsWith("Download failed.")) {
      if (System.nanoTime() > deadline) fail(downloadStateText());
      PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
      Thread.sleep(10);
    }
    assertTrue(downloadStateText(), downloadStateText().startsWith(
      "Download failed. Cannot download ELP from http://127.0.0.1:1/elp-macos-aarch64-apple-darwin-otp-28.5.tar.gz: "));
    assertTrue(myPanel.getDownloadButton().isVisible());
    assertTrue(myPanel.getDownloadButton().isEnabled());
  }

  public void testDownloadStateWithoutSdk() throws Exception {
    ApplicationManager.getApplication().runWriteAction(() -> ProjectRootManager.getInstance(getProject()).setProjectSdk(null));
    myPanel.reset();
    waitForDownloadState();
    assertEquals("ELP 1.1.0: no build fits. The project has no Erlang SDK.", downloadStateText());
    assertFalse(myPanel.getDownloadButton().isVisible());
  }

  public void testWhereTheServerComesFrom() {
    List<String> texts = ContainerUtil.map(UIUtil.findComponentsOfType(myPanel.getComponent(), JBLabel.class), JBLabel::getText);
    assertContainsElements(texts, "From https://github.com/WhatsApp/erlang-language-platform/releases, MIT or Apache-2.0 license; runs locally.");
  }

  /**
   * Waits for the background part of the refresh while the EDT keeps dispatching events, as it does in the IDE. Blocking the EDT
   * here would hang on 2026.3: a write action is pending there while the test runs, and the refresh's read action waits for it.
   */
  private void waitForDownloadState() throws Exception {
    Future<?> refresh = myPanel.getDownloadStateRefresh();
    if (refresh != null) {
      long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(30);
      while (!refresh.isDone()) {
        if (System.nanoTime() > deadline) fail("the download state was not refreshed: " + downloadStateText());
        PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
        Thread.sleep(5);
      }
      refresh.get();
    }
    PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
  }

  @NotNull
  private String downloadStateText() {
    return StringUtil.removeHtmlTags(myPanel.getDownloadStateLabel().getText());
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
