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
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.lsp.api.LspServerSupportProvider;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
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

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.function.BooleanSupplier;

/**
 * The offer to move to ELP: when it is shown, what its answers do, and the feedback link. The preflight facts and the
 * availability are made up where only the decision is tested; "Try ELP" runs against a fake installer on a project with OTP 28,
 * which pretends to run on macOS arm64 and downloads from a closed port.
 */
public class ElpOfferTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String V1 = "0.11.1";
  private static final String V2 = "0.11.2";

  private ElpSettings.ElpSettingsState myOldState;
  private final List<String> myOldProperties = new ArrayList<>();
  private final List<Notification> myNotifications = new ArrayList<>();
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
    Notification offer = assertOneElement(offers());
    assertEquals(ElpOffer.TITLE, offer.getTitle());
    assertEquals(List.of(ElpOffer.TRY, ElpOffer.NOT_NOW, ElpOffer.NEVER), offer.getActions().stream().map(a -> a.getTemplateText()).toList());
    assertEquals(V1, ElpSettings.getInstance().getOfferShownForVersion());

    assertFalse(offer(V1));
    assertOneElement(offers());
  }

  public void testNotInTheElpPosition() {
    ElpSettings.getInstance().setMode(ElpMode.ELP);
    assertFalse(offer(V1));
    assertEmpty(offers());
    assertEquals("", ElpSettings.getInstance().getOfferShownForVersion());
  }

  public void testNotWhenTheEngineIsForcedByTheProperty() {
    System.setProperty(ElpConfiguration.MODE_PROPERTY, "plugin");
    assertFalse(offer(V1));
  }

  public void testNotAfterDontAskAgainWasStored() {
    ElpSettings.getInstance().setOfferNeverAsk(true);
    assertFalse(offer(V1));
    assertEmpty(offers());
  }

  public void testNotWhenSwitchedOffByTheProperty() {
    System.setProperty(ElpOffer.PROPERTY, "false");
    assertFalse(offer(V1));
    assertEmpty(offers());
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
    assertEmpty(offers());
    assertEquals("another project may get the offer", "", ElpSettings.getInstance().getOfferShownForVersion());

    assertTrue("the missing download is not held against it", offer(V1));
  }

  public void testNotNowAsksAgainAfterTheNextUpdate() {
    assertTrue(offer(V1));
    Notification offer = assertOneElement(offers());
    fire(offer, ElpOffer.NOT_NOW);
    assertTrue(offer.isExpired());
    assertEquals(V1, ElpSettings.getInstance().getOfferShownForVersion());
    assertEquals(ElpMode.PLUGIN, ElpSettings.getInstance().getStoredMode());
    assertFalse(offer(V1));

    assertTrue(offer(V2));
    assertEquals(2, offers().size());
  }

  public void testDontAskAgainStopsItForGood() {
    assertTrue(offer(V1));
    fire(assertOneElement(offers()), ElpOffer.NEVER);
    assertTrue(ElpSettings.getInstance().isOfferNeverAsk());
    assertFalse(offer(V2));
    assertEquals(ElpMode.PLUGIN, ElpSettings.getInstance().getStoredMode());
  }

  public void testContentSaysWhatTryElpDoes() {
    String downloadable = ElpOffer.content(downloadable());
    assertTrue(downloadable, downloadable.contains("from WhatsApp"));
    assertTrue(downloadable, downloadable.contains("Try ELP downloads ELP 1.1.0 (17.8 MB) from GitHub."));
    assertTrue(downloadable, downloadable.contains("runs rebar3 to fetch and build its dependencies and writes to _build"));
    assertTrue(downloadable, downloadable.contains("Settings | Other Settings | Erlang External Tools"));
    String installed = ElpOffer.content(new ElpAvailability(ElpAvailability.Kind.INSTALLED, "/opt/elp", ElpAssetChoice.found(myAsset),
                                                            myRelease));
    assertFalse(installed, installed.contains("downloads"));
    assertTrue(installed, installed.contains("runs rebar3"));
  }

  public void testTryElpWithAnInstalledServerSwitchesAndSwitchBackRestoresThePlugin() throws Exception {
    Path elp = myInstaller.executable(myAsset);
    Files.createDirectories(elp.getParent());
    Files.writeString(elp, "#!/bin/sh\nexit 0\n");
    assertTrue(elp.toFile().setExecutable(true));
    assertTrue(offer(V1));

    fire(assertOneElement(offers()), ElpOffer.TRY);
    Notification on = await(ElpOffer.ON_TITLE);
    assertEquals(ElpMode.ELP, ElpSettings.getInstance().getStoredMode());
    assertEquals(ElpServerSource.DOWNLOADED, ElpSettings.getInstance().getStoredSource());
    assertTrue(on.getContent(), on.getContent().contains("language services widget in the status bar"));

    fire(on, "Switch back to the plugin");
    assertEquals(ElpMode.PLUGIN, ElpSettings.getInstance().getStoredMode());
    assertTrue(on.isExpired());
  }

  public void testFailedDownloadKeepsThePluginAndOffersRetry() throws Exception {
    assertTrue(offer(V1));

    fire(assertOneElement(offers()), ElpOffer.TRY);
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

  /** The offer's memory is written with the settings, and settings stored before it existed read as "never shown". */
  public void testOfferStateIsStoredAndOldSettingsRead() throws Exception {
    ElpSettings.getInstance().setOfferShownForVersion(V1);
    ElpSettings.getInstance().setOfferNeverAsk(true);
    String xml = JDOMUtil.write(XmlSerializer.serialize(ElpSettings.getInstance().getState()));
    assertTrue(xml, xml.contains("<option name=\"offerShownForVersion\" value=\"" + V1 + "\" />"));
    assertTrue(xml, xml.contains("<option name=\"offerNeverAsk\" value=\"true\" />"));

    ElpSettings.ElpSettingsState old = XmlSerializer.deserialize(
      JDOMUtil.load("<state><option name=\"mode\" value=\"PLUGIN\" /></state>"), ElpSettings.ElpSettingsState.class);
    ElpSettings.getInstance().loadState(old);
    assertEquals("", ElpSettings.getInstance().getOfferShownForVersion());
    assertFalse(ElpSettings.getInstance().isOfferNeverAsk());
    assertTrue(offer(V1));
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
    return ElpOffer.offerIfEligible(getProject(), version, ElpPreflight.evaluate(facts(true, null), layout(true)), downloadable());
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

  private List<Notification> offers() {
    return myNotifications.stream().filter(n -> n.getGroupId().equals(ElpOffer.GROUP)).toList();
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

  private static void waitFor(BooleanSupplier condition, String message) throws InterruptedException {
    long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(60);
    while (!condition.getAsBoolean()) {
      if (System.nanoTime() > deadline) fail(message);
      PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
      Thread.sleep(10);
    }
  }
}
