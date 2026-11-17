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

import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.notification.Notification;
import com.intellij.notification.Notifications;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.lsp.api.LspServer;
import com.intellij.platform.lsp.api.LspServerDescriptor;
import com.intellij.platform.lsp.api.LspServerState;
import com.intellij.platform.lsp.api.LspServerSupportProvider;
import com.intellij.platform.lsp.api.customization.*;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.util.EnvironmentUtil;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpEnvironment;
import org.intellij.erlang.elp.ElpFiles;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.elp.ElpPreflight;
import org.intellij.erlang.elp.ElpServedRoots;
import org.intellij.erlang.elp.ElpServerEnvironment;
import org.intellij.erlang.elp.ElpServerSource;
import org.intellij.erlang.elp.distribution.ElpDistribution;
import org.intellij.erlang.elp.distribution.ElpInstaller;
import org.intellij.erlang.elp.distribution.ElpPlatform;
import org.intellij.erlang.elp.distribution.ElpRelease;
import org.intellij.erlang.elp.ElpSettings;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.lang.reflect.Proxy;
import java.util.List;

public class ElpLspClientDescriptorTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String SDK_HOME = new File("testData/mockSdk-R27").getAbsolutePath();
  private static final String REBAR = new File("/opt/rebar-tools/rebar3").getPath();
  /**
   * The client switches that a build against IDEA 2026.1 cannot set: the 2026.1 client has no such customizer, and later clients
   * add it enabled. Their overrides went in 3b2b951d ("Leave out the LSP client switches that IDEA 2026.1 does not have"). ELP
   * announces renameProvider and implementationProvider, so from 2026.2 on the client offers its own rename next to
   * ElpRenameHandler, and from 2026.3 on it asks ELP for implementations. They are expected enabled exactly where the running IDE
   * has them; any other enabled customizer still fails the test.
   */
  private static final List<String> NOT_SETTABLE_IN_A_2026_1_BUILD = List.of("getRenameCustomizer", "getGoToImplementationCustomizer");

  private ElpSettings.ElpSettingsState myOldElpState;
  private String myOldRebarPath;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    ElpSettings.ElpSettingsState old = ElpSettings.getInstance().getState();
    myOldElpState = new ElpSettings.ElpSettingsState();
    myOldElpState.path = old.path;
    myOldElpState.source = old.source;
    myOldElpState.mode = old.mode;
    myOldElpState.eqwalizer = old.eqwalizer;
    myOldRebarPath = RebarSettings.getInstance(getProject()).getRebarPath();
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ElpSettings.getInstance().loadState(myOldElpState);
      RebarSettings.getInstance(getProject()).setRebarPath(myOldRebarPath);
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  @NotNull
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ApplicationManager.getApplication().runWriteAction(
          (Computable<Sdk>)() -> ErlangSdkType.createMockSdk(SDK_HOME, ErlangSdkRelease.V_27_0));
      }
    };
  }

  public void testCommandLineAndPath() throws Exception {
    setUpProjectSdk();
    RebarSettings.getInstance(getProject()).setRebarPath(REBAR);
    VirtualFile root = file("src/a.erl").getParent().getParent();
    String elp = executable();
    ElpLspClientDescriptor descriptor = new ElpLspClientDescriptor(getProject(), root, new ElpConfiguration(elp, ElpMode.PLUGIN));

    // On the EDT: waiting there for a pooled thread can deadlock with a scanning write action queued by the files just added.
    GeneralCommandLine commandLine = descriptor.createCommandLine();

    assertEquals(elp, commandLine.getExePath());
    assertEquals(List.of("server"), commandLine.getParametersList().getList());
    assertEquals(root.getPath(), commandLine.getWorkDirectory().getPath());
    String expectedHead = new File(SDK_HOME, "bin").getPath() + File.pathSeparator + new File(REBAR).getParent();
    String inherited = EnvironmentUtil.getValue("PATH");
    assertEquals(inherited == null ? expectedHead : expectedHead + File.pathSeparator + inherited,
                 commandLine.getEnvironment().get("PATH"));
    assertEquals(REBAR, commandLine.getEnvironment().get("ELP_REBAR3"));
    Path tmpDir = descriptor.getTmpDir();
    assertNotNull(tmpDir);
    assertEquals(tmpDir.toString(), commandLine.getEnvironment().get("TMPDIR"));
    assertTrue(Files.isDirectory(tmpDir));
    ElpServerEnvironment.deleteSessionDirectory(tmpDir);
  }

  /**
   * A project opened through a symlink: ELP gets the real path of the files under the root, and the real paths it answers with map
   * back to the files the project knows. Files outside the root keep their path.
   */
  public void testFilesUnderASymlinkedRootGoToElpByTheirRealPath() throws Exception {
    Path temp = FileUtil.createTempDirectory("elp-real-path", null, true).toPath().toRealPath();
    Path real = Files.createDirectories(temp.resolve("real dir/project/src"));
    Files.writeString(real.resolve("a.erl"), "-module(a).");
    Path link = Files.createSymbolicLink(temp.resolve("link"), temp.resolve("real dir"));
    Path outside = Files.writeString(temp.resolve("outside.erl"), "-module(outside).");
    VirtualFile root = LocalFileSystem.getInstance().refreshAndFindFileByNioFile(link.resolve("project"));
    assertNotNull(root);
    VirtualFile module = root.findFileByRelativePath("src/a.erl");
    assertNotNull(module);
    VirtualFile outsideFile = LocalFileSystem.getInstance().refreshAndFindFileByNioFile(outside);
    assertNotNull(outsideFile);
    ElpLspClientDescriptor descriptor = new ElpLspClientDescriptor(getProject(), root, new ElpConfiguration("/opt/elp/bin/elp", ElpMode.ELP));
    String linkUri = descriptor.getFileUri(module);
    assertEquals(link.resolve("project/src/a.erl").toString(), java.net.URI.create(linkUri).getPath());

    descriptor.resolveRealRoot();
    String realUri = descriptor.getFileUri(module);
    assertEquals(real.resolve("a.erl").toString(), java.net.URI.create(realUri).getPath());
    assertTrue(realUri, realUri.contains("real%20dir"));
    assertEquals(module, descriptor.findFileByUri(realUri));
    assertEquals("ELP's own encoding of the same path", module, descriptor.findFileByUri(real.resolve("a.erl").toUri().toString()));
    assertEquals("the link still works", module, descriptor.findFileByUri(linkUri));
    assertEquals(outsideFile, descriptor.findFileByUri(descriptor.getFileUri(outsideFile)));
    assertEquals(outside.toString(), java.net.URI.create(descriptor.getFileUri(outsideFile)).getPath());
  }

  public void testPathIsInheritedWithoutSdkAndRebar() throws Exception {
    RebarSettings.getInstance(getProject()).setRebarPath("");
    assertEquals(EnvironmentUtil.getValue("PATH"), ElpEnvironment.serverPath(getProject()));
  }

  public void testOnlyErlangModulesAndHeadersAreSupported() {
    ElpLspClientDescriptor descriptor = descriptor(ElpMode.PLUGIN);
    assertTrue(descriptor.isSupportedFile(file("src/a.erl")));
    assertTrue(descriptor.isSupportedFile(file("include/a.hrl")));
    assertFalse(descriptor.isSupportedFile(file("rebar.config")));
    assertFalse(descriptor.isSupportedFile(file("src/x.app.src")));
    assertFalse(descriptor.isSupportedFile(file("x.escript")));
    assertEquals("erlang", descriptor.getLanguageId(file("src/b.erl")));
  }

  public void testElpModeServesExactlyTheFeaturesItOwns() throws Exception {
    ElpLspClientDescriptor descriptor = descriptor(ElpMode.ELP);
    LspCustomization customization = descriptor.getLspCustomization();
    List<String> expected = new ArrayList<>(List.of("getCodeActionsCustomizer", "getCommandsCustomizer", "getCompletionCustomizer",
                                                    "getDiagnosticsCustomizer",
                                                    "getDocumentHighlightsCustomizer", "getFoldingRangeCustomizer",
                                                    "getGoToDefinitionCustomizer", "getGoToTypeDefinitionCustomizer", "getHoverCustomizer",
                                                    "getInlayHintCustomizer", "getSelectionRangeCustomizer", "getSignatureHelpCustomizer",
                                                    "getSymbolKindCustomizer", "getWorkspaceSymbolCustomizer"));
    for (Method method : LspCustomization.class.getMethods()) {
      if (NOT_SETTABLE_IN_A_2026_1_BUILD.contains(method.getName())) expected.add(method.getName());
    }
    expected.sort(null);
    assertEquals(expected, notDisabledCustomizers(customization));

    assertFalse(assertInstanceOf(customization.getDiagnosticsCustomizer(), ElpDiagnosticsSupport.class).isShowEqwalizer());
    LspCodeActionsSupport codeActions = assertInstanceOf(customization.getCodeActionsCustomizer(), LspCodeActionsSupport.class);
    assertTrue(codeActions.getQuickFixesSupport());
    assertTrue(codeActions.getIntentionActionsSupport());
    assertInstanceOf(customization.getFindReferencesCustomizer(), LspFindReferencesDisabled.class);
    assertInstanceOf(customization.getSemanticTokensCustomizer(), LspSemanticTokensDisabled.class);
    assertTrue(assertInstanceOf(customization.getDocumentHighlightsCustomizer(), LspDocumentHighlightsSupport.class)
                 .shouldAskServerForDocumentHighlights(myFixture.addFileToProject("src/highlights.erl", "").getContainingFile()));
    assertInstanceOf(customization.getWorkspaceSymbolCustomizer(), LspWorkspaceSymbolSupport.class);
    assertNotNull(descriptor.getLspServerListener());
  }

  /** Completion comes from ELP in the ELP position, only where the plugin is silent; see ElpLiveServerTest for the files. */
  public void testCompletionGoesToElpInTheElpPosition() {
    LspCompletionCustomizer completion = descriptor(ElpMode.ELP).getLspCustomization().getCompletionCustomizer();
    assertInstanceOf(completion, LspCompletionSupport.class);
    assertNotSame(LspCompletionSupport.class, completion.getClass());
  }

  /** A root that ELP served and that a later preflight refuses, here because the project is not trusted, goes back to the plugin. */
  public void testARefusedPreflightTakesTheRootOutOfTheRegistry() throws Exception {
    setUpProjectSdk();
    VirtualFile module = file("src/a.erl");
    configure(executable(), ElpMode.ELP);
    VirtualFile root = ReadAction.compute(() -> ElpFiles.findWorkspaceRoot(getProject(), module));
    assertNotNull(root);
    ElpServedRoots.getInstance().setRootsForTests(List.of(root.getPath(), "/elsewhere"), getTestRootDisposable());
    ElpPreflight.trustInTests(false, getTestRootDisposable());
    List<LspServerState> stops = new ArrayList<>();
    LspServerState[] state = {LspServerState.Running};
    ElpLspClientDescriptor running = new ElpLspClientDescriptor(getProject(), root, new ElpConfiguration(executable(), ElpMode.ELP));
    LspServer client = (LspServer)Proxy.newProxyInstance(getClass().getClassLoader(), new Class<?>[]{LspServer.class},
                                                         (proxy, method, args) -> switch (method.getName()) {
      case "getDescriptor" -> running;
      case "getState" -> state[0];
      case "hashCode" -> System.identityHashCode(proxy);
      case "equals" -> proxy == args[0];
      default -> null;
    });
    ElpLspServerStatus.setClientsForTests(project -> List.of(client), getTestRootDisposable());
    ElpLspIntegrationProvider.onServerStopInTests(project -> stops.add(state[0]), getTestRootDisposable());
    assertEmpty(fileOpened(module));
    assertEquals(List.of("/elsewhere"), ElpServedRoots.getInstance().getRoots());
    PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
    assertEquals("the running server of the refused root stops", List.of(LspServerState.Running), stops);

    state[0] = LspServerState.ShutdownNormally;
    assertEmpty(fileOpened(module));
    PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
    assertEquals("nothing runs there now", 1, stops.size());
  }

  private static List<String> notDisabledCustomizers(LspCustomization customization) throws Exception {
    List<String> notDisabled = new ArrayList<>();
    for (Method method : LspCustomization.class.getMethods()) {
      if (method.getParameterCount() != 0 || !method.getName().endsWith("Customizer")) continue;
      Object customizer = method.invoke(customization);
      if (!customizer.getClass().getSimpleName().endsWith("Disabled")) notDisabled.add(method.getName());
    }
    notDisabled.sort(null);
    return notDisabled;
  }

  public void testClientCapabilities() {
    // The platform derives only the semantic tokens capability from the customization; it still advertises the other
    // features, but sends no requests for the switched off ones.
    var textDocument = descriptor(ElpMode.ELP).getClientCapabilities().getTextDocument();
    assertNotNull(textDocument.getPublishDiagnostics());
    assertNotNull(textDocument.getCodeAction());
    assertNull(textDocument.getSemanticTokens());
  }

  public void testEqwalizerSwitchReachesTheDiagnosticsCustomizerInBothModes() {
    for (ElpMode mode : ElpMode.values()) {
      for (boolean eqwalizer : new boolean[]{false, true}) {
        VirtualFile root = file("src/" + mode + eqwalizer + ".erl").getParent();
        ElpLspClientDescriptor descriptor =
          new ElpLspClientDescriptor(getProject(), root, new ElpConfiguration("/opt/elp/bin/elp", mode, eqwalizer));
        ElpDiagnosticsSupport diagnostics =
          assertInstanceOf(descriptor.getLspCustomization().getDiagnosticsCustomizer(), ElpDiagnosticsSupport.class);
        assertEquals(mode + " " + eqwalizer, eqwalizer, diagnostics.isShowEqwalizer());
      }
    }
  }

  public void testProviderPassesTheStoredEqwalizerSwitch() throws Exception {
    String oldProperty = System.getProperty(ElpConfiguration.EQWALIZER_PROPERTY);
    System.clearProperty(ElpConfiguration.EQWALIZER_PROPERTY);
    try {
      setUpProjectSdk();
      ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
      state.path = executable();
      state.mode = ElpMode.ELP.name();
      state.eqwalizer = true;
      ElpSettings.getInstance().loadState(state);
      List<LspServerDescriptor> started = fileOpened(file("src/a.erl"));
      assertTrue(assertInstanceOf(started.get(0), ElpLspClientDescriptor.class).getConfiguration().eqwalizer());
    }
    finally {
      if (oldProperty != null) System.setProperty(ElpConfiguration.EQWALIZER_PROPERTY, oldProperty);
    }
  }

  public void testProviderStartsElpOnlyForErlangFilesOnceAPathIsSet() throws Exception {
    setUpProjectSdk();
    VirtualFile module = file("src/a.erl");
    String elp = executable();
    configure("", ElpMode.PLUGIN);
    assertEmpty(fileOpened(module));

    configure(elp, ElpMode.PLUGIN);
    assertEmpty("the Plugin position starts no ELP", fileOpened(module));

    configure(elp, ElpMode.ELP);
    List<LspServerDescriptor> started = fileOpened(module);
    assertSize(1, started);
    ElpLspClientDescriptor descriptor = assertInstanceOf(started.get(0), ElpLspClientDescriptor.class);
    assertEquals(new ElpConfiguration(elp, ElpMode.ELP), descriptor.getConfiguration());
    assertSize(1, descriptor.getRoots());

    assertSize(1, fileOpened(file("include/a.hrl")));
    assertEmpty(fileOpened(file("rebar.config")));
    assertEmpty(fileOpened(file("src/x.app.src")));
    assertEmpty(fileOpened(file("x.escript")));
  }

  private String executable() throws Exception {
    File elp = FileUtil.createTempFile("elp", null, true);
    assertTrue(elp.setExecutable(true));
    return elp.getPath();
  }

  private void configure(String path, ElpMode mode) {
    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    state.path = path;
    state.mode = mode.name();
    ElpSettings.getInstance().loadState(state);
  }

  /**
   * Without an SDK ELP is not started: the plugin answers, and the user is told why, once.
   */
  public void testProviderDoesNotStartElpThatCannotLoadTheProject() throws Exception {
    List<Notification> notifications = new ArrayList<>();
    getProject().getMessageBus().connect(getTestRootDisposable()).subscribe(Notifications.TOPIC, new Notifications() {
      @Override
      public void notify(@NotNull Notification notification) {
        notifications.add(notification);
      }
    });
    VirtualFile module = file("src/a.erl");
    configure(executable(), ElpMode.ELP);
    assertEmpty(fileOpened(module));
    assertEmpty(fileOpened(file("src/b.erl")));
    PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
    Notification notification = assertOneElement(notifications);
    assertEquals("ELP is not started", notification.getTitle());
    assertTrue(notification.getContent(), notification.getContent().contains("No Erlang SDK is configured for the project"));

    setUpProjectSdk();
    assertSize(1, fileOpened(module));
  }

  /** ELP runs rebar3 and the project's build: it is not started in a project the IDE does not trust, and the user is told why. */
  public void testProviderDoesNotStartElpInAProjectThatIsNotTrusted() throws Exception {
    List<Notification> notifications = new ArrayList<>();
    getProject().getMessageBus().connect(getTestRootDisposable()).subscribe(Notifications.TOPIC, new Notifications() {
      @Override
      public void notify(@NotNull Notification notification) {
        notifications.add(notification);
      }
    });
    setUpProjectSdk();
    VirtualFile module = file("src/a.erl");
    configure(executable(), ElpMode.ELP);
    ElpPreflight.trustInTests(false, getTestRootDisposable());
    assertEmpty(fileOpened(module));
    PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
    Notification notification = assertOneElement(notifications);
    assertEquals("ELP is not started", notification.getTitle());
    assertTrue(notification.getContent(), notification.getContent().contains("not trusted"));

    ElpPreflight.trustInTests(true, getTestRootDisposable());
    assertSize(1, fileOpened(module));
  }

  /**
   * The downloaded server: the provider starts the build installed for the project's OTP release, and while it is not installed
   * starts nothing and says that ELP can be downloaded.
   */
  public void testProviderStartsTheDownloadedServerOnlyWhenItIsInstalled() throws Exception {
    setUpProjectSdk();
    List<Notification> notifications = new ArrayList<>();
    getProject().getMessageBus().connect(getTestRootDisposable()).subscribe(Notifications.TOPIC, new Notifications() {
      @Override
      public void notify(@NotNull Notification notification) {
        notifications.add(notification);
      }
    });
    ElpRelease.Asset asset = new ElpRelease.Asset("elp-macos-aarch64-apple-darwin-otp-27.3.tar.gz", "macos", "aarch64",
                                                  "aarch64-apple-darwin", "27.3", 1, "0".repeat(64), "elp");
    ElpRelease release = new ElpRelease("test-tag", "1.1.0", "https://example.invalid/repo", "https://example.invalid/dl/",
                                        "MIT OR Apache-2.0", List.of(asset));
    ElpInstaller installer = new ElpInstaller(FileUtil.createTempDirectory("elp-installed", null, true).toPath(), release,
                                              "http://127.0.0.1:1/dl/", (executable, r, indicator) -> {});
    ElpDistribution.getInstance().setUpForTests(installer, new ElpPlatform("macos", "aarch64"), getTestRootDisposable());
    String oldProperty = System.getProperty(ElpConfiguration.PATH_PROPERTY);
    System.clearProperty(ElpConfiguration.PATH_PROPERTY);
    try {
      ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
      state.source = ElpServerSource.DOWNLOADED.name();
      state.mode = ElpMode.ELP.name();
      ElpSettings.getInstance().loadState(state);
      ElpPreflight.getInstance(getProject()).invalidate();
      VirtualFile module = file("src/a.erl");

      assertEmpty("nothing is installed", fileOpened(module));
      PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
      Notification notification = assertOneElement(notifications);
      assertTrue(notification.getContent(), notification.getContent().contains("ELP is not downloaded yet: download it on the Erlang " +
                                                                               "External Tools settings page."));

      Path elp = installer.executable(asset);
      Files.createDirectories(elp.getParent());
      Files.writeString(elp, "#!/bin/sh\nexit 0\n");
      assertTrue(elp.toFile().setExecutable(true));
      ElpPreflight.getInstance(getProject()).invalidate();
      ElpLspClientDescriptor descriptor = assertInstanceOf(assertOneElement(fileOpened(module)), ElpLspClientDescriptor.class);
      assertEquals(elp.toString(), descriptor.getConfiguration().path());
      assertEquals(ElpMode.ELP, descriptor.getConfiguration().mode());
    }
    finally {
      if (oldProperty != null) System.setProperty(ElpConfiguration.PATH_PROPERTY, oldProperty);
    }
  }

  /**
   * As the platform: the facts of the check are gathered off the EDT first, then the provider is asked.
   */
  private List<LspServerDescriptor> fileOpened(VirtualFile file) {
    ElpPreflight.Facts facts = ElpPreflight.getInstance(getProject()).cachedFacts();
    if (facts == null || !facts.trusted() || !facts.settingsKey().equals(ElpPreflight.settingsKey())) {
      PlatformTestUtil.waitForFuture(ApplicationManager.getApplication().executeOnPooledThread(
        () -> ElpPreflight.getInstance(getProject()).gatherFacts()), 30_000);
    }
    List<LspServerDescriptor> started = new ArrayList<>();
    LspServerSupportProvider.LspServerStarter starter = started::add;
    ReadAction.run(() -> new ElpLspIntegrationProvider().fileOpened(getProject(), file, starter));
    return started;
  }

  /** A descriptor for the content root, as the provider starts it for the files of the project. */
  private ElpLspClientDescriptor descriptor(ElpMode mode) {
    VirtualFile root = file("src/" + mode + ".erl").getParent().getParent();
    return new ElpLspClientDescriptor(getProject(), root, new ElpConfiguration("/opt/elp/bin/elp", mode));
  }

  private VirtualFile file(String path) {
    return myFixture.addFileToProject(path, "").getVirtualFile();
  }
}
