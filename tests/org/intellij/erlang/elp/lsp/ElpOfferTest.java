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

package org.intellij.erlang.elp.lsp;

import com.intellij.notification.Notification;
import com.intellij.notification.Notifications;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.lsp.api.LspServerSupportProvider;
import com.intellij.testFramework.DumbModeTestUtils;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.ui.UiInterceptors;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpFeedback;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.elp.ElpPreflight;
import org.intellij.erlang.elp.ElpServerSource;
import org.intellij.erlang.elp.ElpSettings;
import org.intellij.erlang.elp.distribution.ElpAssetChoice;
import org.intellij.erlang.elp.distribution.ElpAvailability;
import org.intellij.erlang.elp.distribution.ElpDistribution;
import org.intellij.erlang.elp.distribution.ElpInstaller;
import org.intellij.erlang.elp.distribution.ElpPlatform;
import org.intellij.erlang.elp.distribution.ElpRelease;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;

/**
 * The offer to move to ELP: when it is shown, what its answers do, and the feedback link. The preflight facts and the
 * availability are made up where only the decision is tested; "Try ELP" runs against a fake installer on a project with OTP 28,
 * which pretends to run on macOS arm64 and downloads from a closed port. The dialog is the real one, intercepted before it
 * opens a window; {@link #myUser} answers it.
 */
public class ElpOfferTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String V1 = "0.11.1";
  private static final String V2 = "0.11.2";
  private static final String V3 = "0.11.3";

  private ElpSettings.ElpSettingsState myOldState;
  private final List<String> myOldProperties = new ArrayList<>();
  private final List<Notification> myNotifications = new ArrayList<>();
  /** The offer dialogs that were requested, closed by {@link #myUser}. */
  private final List<ElpOfferDialog> myDialogs = new ArrayList<>();
  private Consumer<ElpOfferDialog> myUser = d -> press(d, ElpOffer.NOT_NOW);
  private boolean myFrameActive = true;
  /** The time the offer reads, in milliseconds since the epoch. */
  private long myNow = System.currentTimeMillis();
  private ElpInstaller myInstaller;
  private ElpRelease myRelease;
  private ElpRelease.Asset myAsset;
  private Path myBaseDir;

  @NotNull
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
    ElpSettings.ElpSettingsState old = ElpSettings.getInstance().getState();
    myOldState = new ElpSettings.ElpSettingsState();
    myOldState.path = old.path;
    myOldState.source = old.source;
    myOldState.mode = old.mode;
    myOldState.eqwalizer = old.eqwalizer;
    myOldState.offerShownForVersion = old.offerShownForVersion;
    myOldState.offerNeverAsk = old.offerNeverAsk;
    myOldState.offerAnsweredTime = old.offerAnsweredTime;
    myOldState.offerShownCount = old.offerShownCount;
    for (String property : List.of(ElpConfiguration.PATH_PROPERTY, ElpConfiguration.MODE_PROPERTY, ElpOffer.PROPERTY)) {
      myOldProperties.add(System.getProperty(property));
      System.clearProperty(property);
    }
    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    state.source = ElpServerSource.DOWNLOADED.name();
    state.mode = ElpMode.PLUGIN.name();
    ElpSettings.getInstance().loadState(state);

    myAsset = new ElpRelease.Asset("elp-macos-aarch64-apple-darwin-otp-28.5.tar.gz", "macos", "aarch64", "aarch64-apple-darwin",
                                   "28.5", 17_800_000, "0".repeat(64), "elp");
    myRelease = new ElpRelease("test-tag", "1.1.0", "https://example.invalid/repo", "https://example.invalid/dl/",
                               "MIT OR Apache-2.0", List.of(myAsset));
    myBaseDir = FileUtil.createTempDirectory("elp-offer", null, true).toPath();
    myInstaller = new ElpInstaller(myBaseDir, myRelease, "http://127.0.0.1:1/dl/", (executable, r, indicator) -> {});
    ElpDistribution.getInstance().setUpForTests(myInstaller, new ElpPlatform("macos", "aarch64"), getTestRootDisposable());
    getProject().getMessageBus().connect(getTestRootDisposable()).subscribe(Notifications.TOPIC, new Notifications() {
      @Override
      public void notify(@NotNull Notification notification) {
        myNotifications.add(notification);
      }
    });
    ElpOffer.setUpForTests((project, content, reminder) -> {
      UiInterceptors.register(new UiInterceptors.UiInterceptor<>(ElpOfferDialog.class) {
        @Override
        protected void doIntercept(@NotNull ElpOfferDialog dialog) {
          myDialogs.add(dialog);
          myUser.accept(dialog);
        }
      });
      return ElpOffer.DIALOG.ask(project, content, reminder);
    }, () -> myFrameActive, getTestRootDisposable());
    // The offer is shown on macOS only; the tests decide it on every system.
    ElpOffer.macOsInTests(true, getTestRootDisposable());
    ElpOffer.clockInTests(() -> myNow, getTestRootDisposable());
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ElpSettings.getInstance().loadState(myOldState);
      List<String> properties = List.of(ElpConfiguration.PATH_PROPERTY, ElpConfiguration.MODE_PROPERTY, ElpOffer.PROPERTY);
      for (int i = 0; i < myOldProperties.size(); i++) {
        if (myOldProperties.get(i) == null) System.clearProperty(properties.get(i));
        else System.setProperty(properties.get(i), myOldProperties.get(i));
      }
      getProject().putUserData(ElpOffer.LAST_CHECK, null);
      getProject().putUserData(ElpOffer.PENDING, null);
      getProject().putUserData(ElpOffer.NOT_ELIGIBLE, null);
      UiInterceptors.clear();
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  public void testShownOncePerVersion() {
    assertTrue(offer(V1));
    ElpOfferDialog offer = assertOneElement(myDialogs);
    assertEquals(ElpOfferDialog.TITLE, offer.getTitle());
    assertEquals(List.of(ElpOffer.TRY, ElpOffer.NOT_NOW), offer.buttons().stream().map(a -> a.getValue(Action.NAME)).toList());
    assertNull("Try ELP is not the default button", offer.buttons().get(0).getValue(DialogWrapper.DEFAULT_ACTION));
    assertEquals(ElpOffer.NEVER, offer.dontAskCheckBox().getText());
    assertFalse(offer.dontAskCheckBox().isSelected());
    assertEquals(ElpOfferDialog.content(downloadable()), offer.content());
    assertFalse(ElpOfferDialog.text(offer.content()), ElpOfferDialog.text(offer.content()).contains(ElpOfferDialog.REMINDER));
    assertTrue(offer.isDisposed());
    assertEquals(V1, ElpSettings.getInstance().getOfferShownForVersion());
    assertEquals(1, ElpSettings.getInstance().getOfferShownCount());

    assertFalse(offer(V1));
    assertOneElement(myDialogs);
  }

  /**
   * The dialog opens by itself, maybe while the user types: no button is pressed by Enter, and the focus starts on "Not now", so
   * that a key typed for the editor does not download and start ELP.
   */
  public void testAKeyTypedForTheEditorDoesNotTryElp() {
    List<Object> seen = new ArrayList<>();
    myUser = d -> {
      for (Action button : d.buttons()) seen.add(button.getValue(DialogWrapper.DEFAULT_ACTION));
      seen.add(d.getPreferredFocusedComponent() instanceof JButton button ? button.getText() : d.getPreferredFocusedComponent());
      press(d, ElpOffer.NOT_NOW);
    };
    assertTrue(offer(V1));
    assertEquals(Arrays.asList(null, null, ElpOffer.NOT_NOW), seen);
  }

  public void testNotInTheElpPosition() {
    ElpSettings.getInstance().setMode(ElpMode.ELP);
    assertFalse(offer(V1));
    assertEmpty(myDialogs);
    assertEquals("", ElpSettings.getInstance().getOfferShownForVersion());
  }

  public void testNotWhenTheEngineIsForcedByTheProperty() {
    System.setProperty(ElpConfiguration.MODE_PROPERTY, "plugin");
    assertFalse(offer(V1));
  }

  public void testNotAfterDontAskAgainWasStored() {
    ElpSettings.getInstance().setOfferNeverAsk(true);
    assertFalse(offer(V1));
    assertEmpty(myDialogs);
  }

  public void testNotWhenSwitchedOffByTheProperty() {
    System.setProperty(ElpOffer.PROPERTY, "false");
    assertFalse(offer(V1));
    assertEmpty(myDialogs);
    System.setProperty(ElpOffer.PROPERTY, "true");
    assertTrue(offer(V1));
  }

  public void testNotWhereElpWouldNotWorkAndTheVersionIsNotMarked() {
    ElpPreflight.Result linux = ElpPreflight.evaluate(facts(false, null), layout(true));
    assertFalse(ElpOffer.offerIfEligible(getProject(), V1, linux, downloadable()));
    ElpPreflight.Result noRebarConfig = ElpPreflight.evaluate(facts(true, null), layout(false));
    assertFalse(ElpOffer.offerIfEligible(getProject(), V1, noRebarConfig, downloadable()));
    ElpPreflight.Result eligible = ElpPreflight.evaluate(facts(true, null), layout(true));
    ElpAvailability unavailable = new ElpAvailability(ElpAvailability.Kind.UNAVAILABLE, null, ElpAssetChoice.choose(
      myRelease, new ElpPlatform("freebsd", "x86_64"), true, 28), myRelease);
    assertFalse(ElpOffer.offerIfEligible(getProject(), V1, eligible, unavailable));
    ElpAvailability emptyCustomPath = new ElpAvailability(ElpAvailability.Kind.CONFIGURED, null, ElpAssetChoice.found(myAsset), myRelease);
    assertFalse(ElpOffer.offerIfEligible(getProject(), V1, eligible, emptyCustomPath));
    assertEmpty(myDialogs);
    assertEquals("another project may get the offer", "", ElpSettings.getInstance().getOfferShownForVersion());

    assertTrue("the missing download is not held against it", offer(V1));
  }

  /** "Not now" asks again in a newer plugin version, and not sooner than 30 days after the answer: releases come every few days. */
  public void testNotNowAsksAgainInANewerVersionAfterThirtyDays() {
    myUser = d -> press(d, ElpOffer.NOT_NOW);
    assertTrue(offer(V1));
    ElpOfferDialog offer = assertOneElement(myDialogs);
    assertTrue(offer.isDisposed());
    assertEquals(ElpOffer.Choice.NOT_NOW, offer.choice());
    assertEquals(V1, ElpSettings.getInstance().getOfferShownForVersion());
    assertEquals(myNow, ElpSettings.getInstance().getOfferAnsweredTime());
    assertFalse(ElpSettings.getInstance().isOfferNeverAsk());
    assertEquals(ElpMode.PLUGIN, ElpSettings.getInstance().getStoredMode());
    assertFalse(offer(V1));
    assertFalse("a newer version, but too soon", offer(V2));

    myNow += ElpOffer.ASK_AGAIN_MS - 1;
    assertFalse("a day short of 30 days", offer(V2));
    myNow += 1;
    assertFalse("30 days later, but the same version", offer(V1));
    assertFalse("30 days later, but an older version", offer("0.11.0"));
    assertTrue(offer(V2));
    assertEquals(2, myDialogs.size());
  }

  /** The second offer says sorry, reminds of the earlier "Not now", is the last one, and needs no "Don't ask again". */
  public void testTheSecondOfferRemindsAndIsTheLast() {
    myUser = d -> press(d, ElpOffer.NOT_NOW);
    assertTrue(offer(V1));
    myNow += ElpOffer.ASK_AGAIN_MS;
    assertTrue(offer(V2));
    ElpOfferDialog second = myDialogs.get(1);
    assertEquals(ElpOfferDialog.content(downloadable(), true), second.content());
    assertEquals(new ElpOfferDialog.Block(ElpOfferDialog.Kind.PARAGRAPH, "Sorry to ask again. You chose \"Not now\" earlier, so this " +
                                                                          "is a reminder, and it is the last one."),
                 second.content().get(0));
    assertEquals(ElpOfferDialog.content(downloadable()), second.content().subList(1, second.content().size()));
    assertNull("no Don't ask again", second.dontAskCheckBox());
    assertEquals(List.of(ElpOffer.TRY, ElpOffer.NOT_NOW), second.buttons().stream().map(a -> a.getValue(Action.NAME)).toList());
    assertEquals(2, ElpSettings.getInstance().getOfferShownCount());

    myNow += ElpOffer.ASK_AGAIN_MS;
    assertFalse("no third offer", offer(V3));
    assertEquals(2, myDialogs.size());
  }

  /** Whatever the answer to the second offer, there is no third. */
  public void testNoThirdOfferAfterAnyAnswer() {
    List<Consumer<ElpOfferDialog>> answers = List.of(d -> press(d, ElpOffer.NOT_NOW), DialogWrapper::doCancelAction,
                                                     d -> press(d, ElpOffer.TRY));
    for (Consumer<ElpOfferDialog> answer : answers) {
      ElpSettings.getInstance().loadState(new ElpSettings.ElpSettingsState());
      myDialogs.clear();
      myUser = d -> press(d, ElpOffer.NOT_NOW);
      assertTrue(offer(V1));
      myNow += ElpOffer.ASK_AGAIN_MS;
      myUser = answer;
      assertTrue(offer(V2));
      ElpSettings.getInstance().setMode(ElpMode.PLUGIN);
      myNow += ElpOffer.ASK_AGAIN_MS;
      assertFalse(myDialogs.get(1).choice().toString(), offer(V3));
      assertEquals(2, myDialogs.size());
    }
  }

  /** Esc and the close button of the window end where "Not now" does: in {@code doCancelAction}. */
  public void testEscIsNotNow() {
    myUser = DialogWrapper::doCancelAction;
    assertTrue(offer(V1));
    ElpOfferDialog offer = assertOneElement(myDialogs);
    assertTrue(offer.isDisposed());
    assertEquals(DialogWrapper.CANCEL_EXIT_CODE, offer.getExitCode());
    assertEquals(ElpOffer.Choice.NOT_NOW, offer.choice());
    assertEquals(V1, ElpSettings.getInstance().getOfferShownForVersion());
    assertFalse(ElpSettings.getInstance().isOfferNeverAsk());
    assertEquals(ElpMode.PLUGIN, ElpSettings.getInstance().getStoredMode());
    assertFalse(offer(V1));
    assertFalse(offer(V2));
    myNow += ElpOffer.ASK_AGAIN_MS;
    assertTrue(offer(V2));
  }

  public void testDontAskAgainStopsItForGood() {
    myUser = d -> {
      d.dontAskCheckBox().setSelected(true);
      press(d, ElpOffer.NOT_NOW);
    };
    assertTrue(offer(V1));
    assertEquals(ElpOffer.Choice.NEVER, assertOneElement(myDialogs).choice());
    assertTrue(ElpSettings.getInstance().isOfferNeverAsk());
    myNow += ElpOffer.ASK_AGAIN_MS;
    assertFalse(offer(V2));
    assertOneElement(myDialogs);
    assertEquals(ElpMode.PLUGIN, ElpSettings.getInstance().getStoredMode());
  }

  /** "Don't ask again" holds also when "Try ELP" is pressed with it: back on the plugin, the offer does not come again. */
  public void testTryElpWithDontAskAgainStopsItForGood() throws Exception {
    myUser = d -> {
      d.dontAskCheckBox().setSelected(true);
      press(d, ElpOffer.TRY);
    };
    assertTrue(offer(V1));
    assertEquals(ElpOffer.Choice.TRY_AND_NEVER, assertOneElement(myDialogs).choice());
    assertTrue(ElpSettings.getInstance().isOfferNeverAsk());
    await(ElpOffer.FAILED_TITLE);
    assertEquals("the download failed: still the plugin", ElpMode.PLUGIN, ElpSettings.getInstance().getStoredMode());
    myNow += ElpOffer.ASK_AGAIN_MS;
    assertFalse(offer(V2));
    assertOneElement(myDialogs);
  }

  public void testEscWithDontAskAgainStopsItForGood() {
    myUser = d -> {
      d.dontAskCheckBox().setSelected(true);
      d.doCancelAction();
    };
    assertTrue(offer(V1));
    assertEquals(ElpOffer.Choice.NEVER, assertOneElement(myDialogs).choice());
    assertTrue(ElpSettings.getInstance().isOfferNeverAsk());
    assertFalse(offer(V2));
  }

  public void testChoiceOfTheButtons() {
    assertEquals(ElpOffer.Choice.TRY, ElpOfferDialog.choice(DialogWrapper.OK_EXIT_CODE, false));
    assertEquals(ElpOffer.Choice.TRY_AND_NEVER, ElpOfferDialog.choice(DialogWrapper.OK_EXIT_CODE, true));
    assertEquals(ElpOffer.Choice.NOT_NOW, ElpOfferDialog.choice(DialogWrapper.CANCEL_EXIT_CODE, false));
    assertEquals(ElpOffer.Choice.NEVER, ElpOfferDialog.choice(DialogWrapper.CANCEL_EXIT_CODE, true));
  }

  /**
   * Not while the project is indexed: the dialog would come up during startup. The version stays unmarked, and the offer comes
   * by itself once the project is smart.
   */
  public void testNotInDumbModeAndTheVersionIsNotMarked() throws Throwable {
    ElpOffer.enableInTests(getTestRootDisposable());
    VirtualFile file = myFixture.addFileToProject("src/a.erl", "").getVirtualFile();
    DumbModeTestUtils.runInDumbModeSynchronously(getProject(), () -> {
      assertFalse(offer(V1));
      fileOpened(file);
      assertNull("a file opened while indexing does not use up the check", getProject().getUserData(ElpOffer.LAST_CHECK));
      assertEmpty(myDialogs);
      assertEquals("", ElpSettings.getInstance().getOfferShownForVersion());
    });
    waitFor(() -> !myDialogs.isEmpty(), "no offer after indexing");
    assertOneElement(myDialogs);
    assertEquals(V1, ElpSettings.getInstance().getOfferShownForVersion());
  }

  /**
   * The platform opens the restored editors while the project is indexed and nothing opens a file afterwards: the check of a
   * file opened then runs once the project is smart, once however many files were opened.
   */
  public void testAFileOpenedWhileIndexingIsCheckedWhenSmart() throws Throwable {
    ElpOffer.enableInTests(getTestRootDisposable());
    ElpOffer.preflightInTests(eligible(), getTestRootDisposable());
    VirtualFile a = myFixture.addFileToProject("src/a.erl", "").getVirtualFile();
    VirtualFile b = myFixture.addFileToProject("src/b.erl", "").getVirtualFile();
    DumbModeTestUtils.runInDumbModeSynchronously(getProject(), () -> {
      fileOpened(a);
      fileOpened(b);
      fileOpened(a);
    });
    waitFor(() -> !myDialogs.isEmpty(), "no offer after indexing");
    dispatchFor(500);
    assertOneElement(myDialogs);
    assertEquals(ElpFeedback.pluginVersion(), ElpSettings.getInstance().getOfferShownForVersion());
  }

  /** A check that waits for the end of indexing does nothing once the project is closed. */
  public void testNoCheckAfterTheProjectIsClosed() throws Throwable {
    ElpOffer.enableInTests(getTestRootDisposable());
    ElpOffer.preflightInTests(eligible(), getTestRootDisposable());
    Path dir = FileUtil.createTempDirectory("elp-offer-closed", null, true).toPath();
    Files.writeString(dir.resolve("a.erl"), "-module(a).\n");
    Project other = PlatformTestUtil.loadAndOpenProject(dir, getTestRootDisposable());
    VirtualFile file = LocalFileSystem.getInstance().refreshAndFindFileByNioFile(dir.resolve("a.erl"));
    assertNotNull(file);
    DumbModeTestUtils.EternalTaskShutdownToken dumb = DumbModeTestUtils.startEternalDumbModeTask(other);
    try {
      ReadAction.run(() -> ElpOffer.fileOpened(other, file));
      assertNotNull("the check waits for the end of indexing", other.getUserData(ElpOffer.PENDING));
    }
    finally {
      PlatformTestUtil.forceCloseProjectWithoutSaving(other);
    }
    try {
      DumbModeTestUtils.endEternalDumbModeTask(dumb);
    }
    catch (Throwable ignored) {
      // The dumb mode ended with the project.
    }
    dispatchFor(500);
    assertEmpty(myDialogs);
    assertEquals("", ElpSettings.getInstance().getOfferShownForVersion());
  }

  /** Not over another application or a window of another project; the offer comes once the frame is active again. */
  public void testWaitsUntilTheFrameIsActive() throws Exception {
    myFrameActive = false;
    getProject().putUserData(ElpOffer.LAST_CHECK, System.currentTimeMillis());
    assertFalse(offer(V1));
    assertEmpty(myDialogs);
    assertEquals("", ElpSettings.getInstance().getOfferShownForVersion());
    assertNull("an opened file checks again", getProject().getUserData(ElpOffer.LAST_CHECK));
    assertNotNull(ElpOffer.whyNotNow(getProject()));
    dispatchFor(200);
    assertEmpty("the frame is still not active", myDialogs);

    myFrameActive = true;
    assertNull(ElpOffer.whyNotNow(getProject()));
    waitFor(() -> !myDialogs.isEmpty(), "no offer after the frame became active");
    assertOneElement(myDialogs);
    assertEquals(V1, ElpSettings.getInstance().getOfferShownForVersion());
  }

  public void testContentSaysWhatTryElpDoes() {
    String downloadable = ElpOfferDialog.text(ElpOfferDialog.content(downloadable()));
    assertTrue(downloadable, downloadable.contains("WhatsApp develops"));
    assertTrue(downloadable, downloadable.contains("Downloads ELP 1.1.0 (17.8 MB) from GitHub and checks its checksum."));
    assertTrue(downloadable, downloadable.contains("runs rebar3 to fetch and build its dependencies and writes to _build"));
    assertTrue(downloadable, downloadable.contains("Settings | Other Settings | Erlang External Tools"));
    for (String heading : List.of("Why try it", "What stays with the plugin", "What Try ELP does")) {
      assertTrue(downloadable, downloadable.contains("\n" + heading + "\n"));
    }
    String installed = ElpOfferDialog.text(ElpOfferDialog.content(
      new ElpAvailability(ElpAvailability.Kind.INSTALLED, "/opt/elp", ElpAssetChoice.found(myAsset), myRelease)));
    assertFalse(installed, installed.contains("downloads"));
    assertFalse(installed, installed.contains("Downloads"));
    assertTrue(installed, installed.contains("runs rebar3"));
  }

  public void testTryElpWithAnInstalledServerSwitchesAndSwitchBackRestoresThePlugin() throws Exception {
    Path elp = myInstaller.executable(myAsset);
    Files.createDirectories(elp.getParent());
    Files.writeString(elp, "#!/bin/sh\nexit 0\n");
    assertTrue(elp.toFile().setExecutable(true));
    myUser = d -> press(d, ElpOffer.TRY);
    assertTrue(offer(V1));
    assertEquals(ElpOffer.Choice.TRY, assertOneElement(myDialogs).choice());

    Notification on = await(ElpOffer.ON_TITLE);
    assertEquals(ElpMode.ELP, ElpSettings.getInstance().getStoredMode());
    assertEquals(ElpServerSource.DOWNLOADED, ElpSettings.getInstance().getStoredSource());
    assertTrue(on.getContent(), on.getContent().contains("language services widget in the status bar"));

    fire(on, "Switch back to the plugin");
    assertEquals(ElpMode.PLUGIN, ElpSettings.getInstance().getStoredMode());
    assertTrue(on.isExpired());
    assertTrue("who switched back is not asked again", ElpSettings.getInstance().isOfferNeverAsk());
    myNow += ElpOffer.ASK_AGAIN_MS;
    assertFalse(offer(V2));
  }

  public void testFailedDownloadKeepsThePluginAndOffersRetry() throws Exception {
    myUser = d -> press(d, ElpOffer.TRY);
    assertTrue(offer(V1));
    assertOneElement(myDialogs);

    Notification failed = await(ElpOffer.FAILED_TITLE);
    assertEquals(ElpMode.PLUGIN, ElpSettings.getInstance().getStoredMode());
    assertEquals(List.of(ElpOffer.RETRY), failed.getActions().stream().map(a -> a.getTemplateText()).toList());
    assertNull(myInstaller.findInstalled(myAsset));
    assertEmpty(titled(ElpOffer.ON_TITLE));
  }

  public void testFeedbackUrlHasTheVersionsAndNoProjectPath() throws Exception {
    VirtualFile file = myFixture.addFileToProject("src/secret_module.erl", "-module(secret_module).").getVirtualFile();
    ElpFeedback.Info info = PlatformTestUtil.waitForFuture(
      ApplicationManager.getApplication().executeOnPooledThread(() -> ElpFeedback.collect(getProject())), 30_000);
    String url = ElpFeedback.url(info);

    assertTrue(url, url.startsWith(ElpFeedback.NEW_ISSUE_URL + "?labels=elp&title=ELP%20feedback&body="));
    String body = URLDecoder.decode(url.substring(url.indexOf("&body=") + "&body=".length()), StandardCharsets.UTF_8);
    assertEquals(ElpFeedback.body(info), body);
    assertTrue(body, body.contains("- Erlang plugin: " + ElpFeedback.pluginVersion() + "\n"));
    assertTrue(body, body.contains("- OTP release of the project SDK: 28"));
    assertTrue(body, body.contains("- ELP: 1.1.0, build elp-macos-aarch64-apple-darwin-otp-28.5 (not downloaded)"));
    assertTrue(body, body.contains("- IDE: "));
    assertTrue(body, body.contains("- OS and CPU: " + System.getProperty("os.name")));
    assertTrue(body, body.contains("### What worked?"));
    assertTrue(body, body.contains("### What did not work?"));
    assertTrue(body, body.contains("### Did you switch back to the plugin? Why?"));
    for (String secret : List.of(file.getPath(), file.getParent().getPath(), "secret_module", getProject().getName(),
                                 myBaseDir.toString(), "mockSdk")) {
      assertFalse(secret, url.contains(secret));
      assertFalse(secret, body.contains(secret));
    }
  }

  public void testCouldNotLoadOffersTheWayBackAndFeedback() {
    ElpNotifications.couldNotLoad(getProject(), new ElpLoadState((state, from, to) -> {}), "/p");
    Notification notification = assertOneElement(titled("ELP could not load the project"));
    List<String> actions = notification.getActions().stream().map(a -> a.getTemplateText()).toList();
    assertTrue(actions.toString(), actions.containsAll(List.of("Switch back to the plugin", "Tell us how it went")));
  }

  public void testOpeningAFileChecksTheProjectUnlessSwitchedOff() {
    ElpOffer.enableInTests(getTestRootDisposable());
    VirtualFile file = myFixture.addFileToProject("src/a.erl", "").getVirtualFile();
    System.setProperty(ElpOffer.PROPERTY, "false");
    fileOpened(file);
    assertNull(getProject().getUserData(ElpOffer.LAST_CHECK));

    System.clearProperty(ElpOffer.PROPERTY);
    ElpSettings.getInstance().setOfferShownForVersion(ElpFeedback.pluginVersion());
    fileOpened(file);
    assertNull("already shown for this version", getProject().getUserData(ElpOffer.LAST_CHECK));

    ElpSettings.getInstance().setOfferShownForVersion("");
    fileOpened(file);
    assertNotNull(getProject().getUserData(ElpOffer.LAST_CHECK));
  }

  /** Where the offer can never be shown, no file starts the check, which may run rebar3 for up to 30 seconds. */
  public void testNoCheckWhereTheOfferCannotBeShown() {
    ElpOffer.enableInTests(getTestRootDisposable());
    VirtualFile file = myFixture.addFileToProject("src/a.erl", "").getVirtualFile();
    ElpOffer.macOsInTests(false, getTestRootDisposable());
    assertNotNull(ElpOffer.whyNotAsk(V1));
    assertFalse(offer(V1));
    fileOpened(file);
    assertNull(getProject().getUserData(ElpOffer.LAST_CHECK));
    assertEmpty(myDialogs);

    ElpOffer.macOsInTests(true, getTestRootDisposable());
    fileOpened(file);
    assertNotNull(getProject().getUserData(ElpOffer.LAST_CHECK));
  }

  /** A project that ELP does not fit is checked once per IDE session, not again every minute. */
  public void testAProjectElpDoesNotFitIsNotCheckedAgain() throws Exception {
    ElpOffer.enableInTests(getTestRootDisposable());
    ElpOffer.preflightInTests(ElpPreflight.evaluate(facts(true, null), layout(false)), getTestRootDisposable());
    VirtualFile file = myFixture.addFileToProject("src/a.erl", "").getVirtualFile();
    fileOpened(file);
    assertNotNull(getProject().getUserData(ElpOffer.LAST_CHECK));
    waitFor(() -> getProject().getUserData(ElpOffer.NOT_ELIGIBLE) != null, "the project is not remembered as not eligible");
    assertEmpty(myDialogs);

    getProject().putUserData(ElpOffer.LAST_CHECK, null);
    fileOpened(file);
    assertNull("checked again", getProject().getUserData(ElpOffer.LAST_CHECK));
  }

  /** A project the IDE does not trust is not checked for the offer: the check would run the project's rebar3. */
  public void testNoCheckInAProjectThatIsNotTrusted() {
    ElpOffer.enableInTests(getTestRootDisposable());
    VirtualFile file = myFixture.addFileToProject("src/a.erl", "").getVirtualFile();
    ElpPreflight.trustInTests(false, getTestRootDisposable());
    fileOpened(file);
    assertNull(getProject().getUserData(ElpOffer.LAST_CHECK));

    ElpPreflight.trustInTests(true, getTestRootDisposable());
    fileOpened(file);
    assertNotNull(getProject().getUserData(ElpOffer.LAST_CHECK));
  }

  /** The offer's memory is written with the settings, and settings stored before it existed read as "never shown". */
  public void testOfferStateIsStoredAndOldSettingsRead() throws Exception {
    ElpSettings.getInstance().setOfferShownForVersion(V1);
    ElpSettings.getInstance().setOfferNeverAsk(true);
    ElpSettings.getInstance().setOfferAnsweredTime(1_700_000_000_000L);
    ElpSettings.getInstance().setOfferShownCount(1);
    String xml = JDOMUtil.write(XmlSerializer.serialize(ElpSettings.getInstance().getState()));
    assertTrue(xml, xml.contains("<option name=\"offerShownForVersion\" value=\"" + V1 + "\" />"));
    assertTrue(xml, xml.contains("<option name=\"offerNeverAsk\" value=\"true\" />"));
    assertTrue(xml, xml.contains("<option name=\"offerAnsweredTime\" value=\"1700000000000\" />"));
    assertTrue(xml, xml.contains("<option name=\"offerShownCount\" value=\"1\" />"));

    ElpSettings.ElpSettingsState old = XmlSerializer.deserialize(
      JDOMUtil.load("<state><option name=\"mode\" value=\"PLUGIN\" /></state>"), ElpSettings.ElpSettingsState.class);
    ElpSettings.getInstance().loadState(old);
    assertEquals("", ElpSettings.getInstance().getOfferShownForVersion());
    assertFalse(ElpSettings.getInstance().isOfferNeverAsk());
    assertTrue(offer(V1));
  }

  /** An offer stored before its time and count were: shown once, long ago, so a newer version shows the second and last offer. */
  public void testAnOfferStoredWithoutItsTimeAndCountWasShownOnceLongAgo() throws Exception {
    ElpSettings.ElpSettingsState old = XmlSerializer.deserialize(
      JDOMUtil.load("<state><option name=\"mode\" value=\"PLUGIN\" /><option name=\"offerShownForVersion\" value=\"" + V1 +
                    "\" /></state>"), ElpSettings.ElpSettingsState.class);
    ElpSettings.getInstance().loadState(old);
    assertEquals(0, ElpSettings.getInstance().getOfferAnsweredTime());
    assertEquals(1, ElpSettings.getInstance().getOfferShownCount());
    assertFalse(offer(V1));
    assertTrue(offer(V2));
    assertNull("the second offer", assertOneElement(myDialogs).dontAskCheckBox());
    myNow += ElpOffer.ASK_AGAIN_MS;
    assertFalse(offer(V3));
  }

  /**
   * The platform opens the files of the last session in a burst, and the first one may be outside the project, such as an SDK
   * module; it must not use up the check, or the project files of the burst are never checked.
   */
  public void testAFileOutsideTheProjectDoesNotUseUpTheCheck() throws Exception {
    ElpOffer.enableInTests(getTestRootDisposable());
    Path outside = FileUtil.createTempDirectory("elp-offer-sdk", null, true).toPath().resolve("eunit_autoexport.erl");
    Files.writeString(outside, "-module(eunit_autoexport).\n");
    VirtualFile sdkFile = LocalFileSystem.getInstance().refreshAndFindFileByNioFile(outside);
    assertNotNull(sdkFile);
    VirtualFile projectFile = myFixture.addFileToProject("src/a.erl", "").getVirtualFile();

    fileOpened(sdkFile);
    assertNull("no workspace root, no check", getProject().getUserData(ElpOffer.LAST_CHECK));
    fileOpened(projectFile);
    assertNotNull(getProject().getUserData(ElpOffer.LAST_CHECK));
  }

  /** The provider in the Plugin position; the starter is typed, because 2026.2 and later also have a client starter overload. */
  private void fileOpened(VirtualFile file) {
    LspServerSupportProvider.LspServerStarter starter = descriptor -> fail("ELP is started");
    ReadAction.run(() -> new ElpLspIntegrationProvider().fileOpened(getProject(), file, starter));
  }

  /** Offers on a project that ELP fits, with the server not downloaded yet. */
  private boolean offer(String version) {
    return ElpOffer.offerIfEligible(getProject(), version, eligible(), downloadable());
  }

  /** The preflight of a project that ELP fits, with the server not downloaded yet. */
  private static ElpPreflight.Result eligible() {
    return ElpPreflight.evaluate(facts(true, null), layout(true));
  }

  private ElpAvailability downloadable() {
    return new ElpAvailability(ElpAvailability.Kind.DOWNLOADABLE, null, ElpAssetChoice.found(myAsset), myRelease);
  }

  private static ElpPreflight.Facts facts(boolean macOs, ElpPreflight.Problem serverProblem) {
    ElpPreflight.Problem problem = serverProblem != null ? serverProblem
                                                         : new ElpPreflight.Problem(ElpPreflight.Reason.SERVER_NOT_DOWNLOADED, "not yet");
    return new ElpPreflight.Facts(true, "28", "/tools/rebar3", "3.28.0", null, new ElpConfiguration("", ElpMode.PLUGIN), problem,
                                  macOs, "");
  }

  private static ElpPreflight.Layout layout(boolean rebarConfig) {
    return new ElpPreflight.Layout("/p", rebarConfig, true, false, false);
  }

  /** Clicks the button {@code text} of the dialog. */
  private static void press(ElpOfferDialog dialog, String text) {
    Action button = dialog.buttons().stream().filter(a -> text.equals(a.getValue(Action.NAME))).findFirst().orElseThrow();
    button.actionPerformed(new ActionEvent(button, ActionEvent.ACTION_PERFORMED, text));
  }

  private List<Notification> titled(String title) {
    return myNotifications.stream().filter(n -> n.getTitle().equals(title)).toList();
  }

  private static void fire(Notification notification, String actionText) {
    AnAction action = notification.getActions().stream().filter(a -> actionText.equals(a.getTemplateText())).findFirst().orElseThrow();
    Notification.fire(notification, action, null);
  }

  private Notification await(String title) throws InterruptedException {
    waitFor(() -> !titled(title).isEmpty(), "no notification '" + title + "' in " + myNotifications.stream().map(Notification::getTitle).toList());
    return assertOneElement(titled(title));
  }

  /** Lets the scheduled checks and the events of {@code ms} milliseconds run. */
  private static void dispatchFor(long ms) throws InterruptedException {
    long deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(ms);
    while (System.nanoTime() < deadline) {
      PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
      Thread.sleep(10);
    }
  }

  private static void waitFor(BooleanSupplier condition, String message) throws InterruptedException {
    long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(60);
    while (!condition.getAsBoolean()) {
      if (System.nanoTime() > deadline) fail(message);
      PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
      Thread.sleep(10);
    }
  }
}
