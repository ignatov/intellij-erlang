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

import com.intellij.codeInsight.CodeInsightSettings;
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementPresentation;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.PathEnvironmentVariableUtil;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.platform.lsp.api.LspServer;
import org.intellij.erlang.elp.ElpPreflight;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import com.intellij.notification.Notification;
import com.intellij.notification.Notifications;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.newvfs.impl.VfsRootAccess;
import com.intellij.platform.lsp.api.LspServerManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.IndexingTestUtil;
import com.intellij.testFramework.JUnit38AssumeSupportRunner;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.util.containers.ContainerUtil;
import org.eclipse.lsp4j.InitializeParams;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpEngine;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.elp.ElpServedRoots;
import org.intellij.erlang.elp.ElpSettings;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.junit.Assume;
import org.junit.runner.RunWith;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicReference;

/**
 * A smoke test with the real ELP through the platform's LSP client on {@code testData/elp/project}: the server starts, publishes
 * a diagnostic, answers a definition and a rename. Skipped unless the environment variable {@code ERLANG_ELP_PATH} names the
 * {@code elp} binary; ELP needs {@code erl} on the PATH. Run it with
 * {@code ERLANG_ELP_PATH=/path/to/elp ./gradlew test --rerun --tests 'org.intellij.erlang.elp.lsp.ElpLiveServerTest'}; without
 * {@code --rerun} Gradle may take the results of a run without the variable, which is not an input of the task.
 */
@RunWith(JUnit38AssumeSupportRunner.class)
public class ElpLiveServerTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final int TIMEOUT_SECONDS = 120;

  private ElpSettings.ElpSettingsState myOldState;
  private VirtualFile myRoot;
  private Sdk myOldSdk;
  private boolean mySdkChanged;
  private String myOldRebarPath;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    ElpSettings.ElpSettingsState old = ElpSettings.getInstance().getState();
    myOldState = new ElpSettings.ElpSettingsState();
    myOldState.path = old.path;
    myOldState.source = old.source;
    myOldState.mode = old.mode;
    myOldState.eqwalizer = old.eqwalizer;
    ElpPreflight.trustInTests(true, getTestRootDisposable());
    ElpServedRoots.getInstance().setRootsForTests(List.of(), getTestRootDisposable());
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      LspServerManager.getInstance(getProject()).stopServers(ElpLspIntegrationProvider.class);
      ElpSettings.getInstance().loadState(myOldState);
      ElpRestarts.resetForTests(getProject());
      if (mySdkChanged) WriteAction.run(() -> ProjectRootManager.getInstance(getProject()).setProjectSdk(myOldSdk));
      if (myOldRebarPath != null) RebarSettings.getInstance(getProject()).setRebarPath(myOldRebarPath);
      if (myRoot != null) PsiTestUtil.removeContentEntry(getModule(), myRoot);
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  public void testDiagnosticDefinitionAndRename() throws Exception {
    String elp = elpPath();
    // ELP breaks under a symlinked path, such as the temporary directory of macOS, so it gets the real path.
    Path project = FileUtil.createTempDirectory("elp-live", null, true).toPath().toRealPath().resolve("project");
    FileUtil.copyDir(new File("testData/elp/project"), project.toFile());
    ElpLspClientDescriptor descriptor = startElp(elp, project);
    VirtualFile shop = file("src/shop.erl");
    // ELP serves only once it has said Running in elp/status, which it sends because the client asks for it.
    assertEquals(List.of(ElpLoadState.State.STARTING, ElpLoadState.State.LOADING, ElpLoadState.State.RUNNING),
                 descriptor.getLoadState().history());

    // Definition: the remote call in price/1 goes to lookup/2 in shop_backend.erl; ELP answers once it has loaded the project.
    Editor editor = myFixture.getEditor();
    int call = editor.getDocument().getText().indexOf("lookup(shop_memory") + 2;
    PsiFile shopPsi = myFixture.getFile();
    AtomicReference<PsiElement[]> targets = new AtomicReference<>();
    PlatformTestUtil.waitWithEventsDispatching("ELP did not answer the definition", () -> {
      targets.set(definition(shopPsi, editor, call));
      return isFunction(targets.get());
    }, TIMEOUT_SECONDS);
    assertSize(1, targets.get());
    ErlangFunction lookup = assertInstanceOf(targets.get()[0], ErlangFunction.class);
    assertEquals("lookup", lookup.getName());
    assertEquals("shop_backend.erl", lookup.getContainingFile().getName());

    // A diagnostic of ELP in the module with the planted defects.
    myFixture.openFileInEditor(file("src/shop_defects.erl"));
    PlatformTestUtil.waitWithEventsDispatching("ELP published no diagnostics", () -> {
      List<HighlightInfo> infos = myFixture.doHighlighting();
      return ContainerUtil.exists(infos, info -> "function unused_function/0 is unused".equals(info.getDescription()));
    }, TIMEOUT_SECONDS);

    // Rename of the local function sum/2.
    myFixture.openFileInEditor(shop);
    Editor shopEditor = myFixture.getEditor();
    shopEditor.getCaretModel().moveToOffset(shopEditor.getDocument().getText().indexOf("\nsum([], Acc)") + 2);
    assertNull(ElpRenameHandler.renameAtCaret(getProject(), shopEditor, myFixture.getFile(), "add_up"));
    String text = shopEditor.getDocument().getText();
    assertTrue(text, text.contains("add_up([price(Name) || Name <- Names], 0)."));
    assertTrue(text, text.contains("add_up([Price | Rest], Acc) -> add_up(Rest, Acc + Price)."));

    // ELP extracts its eqWAlizer binary into the plugin's temporary directory of the session, which goes when the server stops.
    Path tmpDir = descriptor.getTmpDir();
    assertNotNull(tmpDir);
    PlatformTestUtil.waitWithEventsDispatching("ELP wrote nothing into " + tmpDir, () -> eqwalizerIn(tmpDir) != null, TIMEOUT_SECONDS);
    LspServerManager.getInstance(getProject()).stopServers(ElpLspIntegrationProvider.class);
    PlatformTestUtil.waitWithEventsDispatching(tmpDir + " is still there", () -> !Files.exists(tmpDir), TIMEOUT_SECONDS);
  }

  /**
   * Two renames in a row of an exported function, with the module that calls it not open in an editor. ELP reads that module from
   * the disk: without a save before the second rename, it saw the old call there and left it with the name of the first rename.
   */
  public void testSecondRenameSeesTheFirstInAFileThatIsNotOpen() throws Exception {
    String elp = elpPath();
    Path project = FileUtil.createTempDirectory("elp-live", null, true).toPath().toRealPath().resolve("project");
    FileUtil.copyDir(new File("testData/elp/project"), project.toFile());
    startElp(elp, project);
    Editor shopEditor = myFixture.getEditor();
    PsiFile shopPsi = myFixture.getFile();
    int call = shopEditor.getDocument().getText().indexOf("lookup(shop_memory") + 2;
    PlatformTestUtil.waitWithEventsDispatching("ELP did not answer the definition", () -> isFunction(definition(shopPsi, shopEditor, call)),
                                               TIMEOUT_SECONDS);
    VirtualFile shop = file("src/shop.erl");
    VirtualFile backend = file("src/shop_backend.erl");
    myFixture.openFileInEditor(backend);
    FileEditorManager editors = FileEditorManager.getInstance(getProject());
    editors.closeFile(shop);
    PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
    assertFalse(editors.isFileOpen(shop));
    Document shopDocument = FileDocumentManager.getInstance().getDocument(shop);
    assertNotNull(shopDocument);
    Editor editor = myFixture.getEditor();
    PsiFile backendPsi = myFixture.getFile();
    assertEquals(backend, backendPsi.getVirtualFile());

    editor.getCaretModel().moveToOffset(editor.getDocument().getText().indexOf("\nlookup(Backend, Key)") + 3);
    assertNull(ElpRenameHandler.renameAtCaret(getProject(), editor, backendPsi, "find"));
    assertTrue(editor.getDocument().getText(), editor.getDocument().getText().contains("\nfind(Backend, Key) ->"));
    assertTrue(shopDocument.getText(), shopDocument.getText().contains("shop_backend:find(shop_memory"));
    assertFalse("the edit is not saved", Files.readString(project.resolve("src/shop.erl")).contains("shop_backend:find("));

    editor.getCaretModel().moveToOffset(editor.getDocument().getText().indexOf("\nfind(Backend, Key)") + 3);
    assertNull(ElpRenameHandler.renameAtCaret(getProject(), editor, backendPsi, "fetch_item"));
    assertTrue(editor.getDocument().getText(), editor.getDocument().getText().contains("\nfetch_item(Backend, Key) ->"));
    assertTrue(shopDocument.getText(), shopDocument.getText().contains("shop_backend:fetch_item(shop_memory"));
  }

  /**
   * ELP reports {@code L0003 Unknown application} on every file whose URI goes through a symlink, and resolves nothing there; the
   * plugin sends it the real path and maps its answers back to the path the project was opened by.
   */
  public void testProjectOpenedThroughASymlink() throws Exception {
    String elp = elpPath();
    Path temp = FileUtil.createTempDirectory("elp-live", null, true).toPath().toRealPath();
    Path real = temp.resolve("real");
    FileUtil.copyDir(new File("testData/elp/project"), real.resolve("project").toFile());
    Path link = Files.createSymbolicLink(temp.resolve("link"), real);
    VfsRootAccess.allowRootAccess(getTestRootDisposable(), real.toString());
    ElpLspClientDescriptor descriptor = startElp(elp, link.resolve("project"));
    assertEquals(link.resolve("project").toString(), myRoot.getPath());
    VirtualFile shop = file("src/shop.erl");

    Editor editor = myFixture.getEditor();
    int call = editor.getDocument().getText().indexOf("lookup(shop_memory") + 2;
    PsiFile shopPsi = myFixture.getFile();
    AtomicReference<PsiElement[]> targets = new AtomicReference<>();
    PlatformTestUtil.waitWithEventsDispatching("ELP did not answer the definition", () -> {
      targets.set(definition(shopPsi, editor, call));
      return targets.get() != null;
    }, TIMEOUT_SECONDS);
    assertSize(1, targets.get());
    VirtualFile target = assertInstanceOf(targets.get()[0], ErlangFunction.class).getContainingFile().getVirtualFile();
    assertEquals(file("src/shop_backend.erl"), target);

    myFixture.openFileInEditor(file("src/shop_defects.erl"));
    AtomicReference<List<HighlightInfo>> infos = new AtomicReference<>();
    PlatformTestUtil.waitWithEventsDispatching("ELP published no diagnostics", () -> {
      infos.set(myFixture.doHighlighting());
      return ContainerUtil.exists(infos.get(), info -> "function unused_function/0 is unused".equals(info.getDescription()));
    }, TIMEOUT_SECONDS);
    assertFalse(ContainerUtil.exists(infos.get(), info -> "Unknown application".equals(info.getDescription())));
    // The URI that ELP gets is the real path.
    assertEquals(real.resolve("project/src/shop.erl").toUri().getPath(), java.net.URI.create(descriptor.getFileUri(shop)).getPath());
    // So are rootUri and the workspace folders of initialize.
    InitializeParams initialize = descriptor.createInitializeParams();
    assertEquals(real.resolve("project").toString(), java.net.URI.create(initialize.getRootUri()).getPath());
    assertEquals(initialize.getRootUri(), assertOneElement(initialize.getWorkspaceFolders()).getUri());
  }

  /**
   * Without {@code erl} on its PATH ELP never loads: it shows "Could not find OTP" and errors, ends its progress and sends no
   * {@code elp/status}. The plugin keeps answering, and the user is told that ELP cannot find Erlang/OTP.
   */
  public void testLoadWithoutErlFailsAndThePluginAnswers() throws Exception {
    String elp = elpPath();
    List<Notification> notifications = new CopyOnWriteArrayList<>();
    getProject().getMessageBus().connect(getTestRootDisposable()).subscribe(Notifications.TOPIC, new Notifications() {
      @Override
      public void notify(@NotNull Notification notification) {
        notifications.add(notification);
      }
    });
    Path project = FileUtil.createTempDirectory("elp-live", null, true).toPath().toRealPath().resolve("project");
    FileUtil.copyDir(new File("testData/elp/project"), project.toFile());
    long start = System.currentTimeMillis();
    ElpLspClientDescriptor descriptor = startElp(elp, project, Map.of("PATH", "/usr/bin:/bin:/usr/sbin:/sbin"));
    PlatformTestUtil.waitWithEventsDispatching("ELP's load did not fail", () -> descriptor.getLoadState().state() == ElpLoadState.State.FAILED,
                                               TIMEOUT_SECONDS);
    long failedAfter = System.currentTimeMillis() - start;
    assertFalse(ElpEngine.ownsFeature(getProject(), file("src/shop.erl"), ElpEngine.Feature.NAVIGATION));
    assertFalse("ELP never ran, the root is not served", ElpServedRoots.getInstance().isUnderServedRoot(myRoot));
    assertTrue(descriptor.getLoadState().isOtpNotFound());
    assertFalse(descriptor.getLoadState().history().contains(ElpLoadState.State.RUNNING));
    PlatformTestUtil.waitWithEventsDispatching("no notification", () -> ContainerUtil.exists(
      notifications, n -> "ELP could not load the project".equals(n.getTitle())), TIMEOUT_SECONDS);
    Notification failure = ContainerUtil.find(notifications, n -> "ELP could not load the project".equals(n.getTitle()));
    assertTrue(failure.getContent(), failure.getContent().contains("ELP cannot find Erlang/OTP"));
    assertFalse("the Info balloon of every start goes to the log",
                ContainerUtil.exists(notifications, n -> !ElpNotifications.GROUP.equals(n.getGroupId()) && n.getContent().contains("ELP version")));
    System.out.println("ELP without erl: FAILED after " + failedAfter + " ms; history " + descriptor.getLoadState().history() +
                       "; last lines " + descriptor.getLoadState().lastLogLines());
  }

  /**
   * Measured in the IDE: after {@code kill -9} of ELP the plugin's features came back, but nothing told the user and ELP was not
   * started again in 100 s. Now: a balloon and one automatic restart; a second crash within five minutes leaves the served root
   * without code insight, says so, and offers Restart and the way back to the plugin. No process of the servers is left after the
   * stop.
   */
  public void testCrashIsToldAndRestartedOnce() throws Exception {
    String elp = elpPath();
    String otp = otpRoot();
    File rebar3 = PathEnvironmentVariableUtil.findInPath("rebar3");
    Assume.assumeTrue("rebar3 is not on the PATH", rebar3 != null);
    Sdk sdk = ErlangSdkType.createMockSdk(otp, ErlangSdkRelease.V_29_0);
    myOldSdk = ProjectRootManager.getInstance(getProject()).getProjectSdk();
    mySdkChanged = true;
    WriteAction.run(() -> {
      ProjectJdkTable.getInstance().addJdk(sdk, getTestRootDisposable());
      ProjectRootManager.getInstance(getProject()).setProjectSdk(sdk);
    });
    myOldRebarPath = RebarSettings.getInstance(getProject()).getRebarPath();
    RebarSettings.getInstance(getProject()).setRebarPath(rebar3.getPath());
    ElpRestarts.resetForTests(getProject());
    List<Notification> notifications = new CopyOnWriteArrayList<>();
    getProject().getMessageBus().connect(getTestRootDisposable()).subscribe(Notifications.TOPIC, new Notifications() {
      @Override
      public void notify(@NotNull Notification notification) {
        notifications.add(notification);
      }
    });

    // Started as the IDE starts it: the support provider, after the preflight.
    Path project = FileUtil.createTempDirectory("elp-live", null, true).toPath().toRealPath().resolve("project");
    FileUtil.copyDir(new File("testData/elp/project"), project.toFile());
    VfsRootAccess.allowRootAccess(getTestRootDisposable(), project.toString());
    myRoot = LocalFileSystem.getInstance().refreshAndFindFileByNioFile(project);
    assertNotNull(myRoot);
    PsiTestUtil.addSourceContentToRoots(getModule(), myRoot);
    IndexingTestUtil.waitUntilIndexesAreReady(getProject());
    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    state.path = elp;
    state.mode = ElpMode.ELP.name();
    ElpSettings.getInstance().loadState(state);
    ElpPreflight.getInstance(getProject()).invalidate();
    VirtualFile shop = file("src/shop.erl");
    myFixture.openFileInEditor(shop);
    ElpLspIntegrationProvider.startServersIfNeeded(getProject());
    Set<Long> pids = new HashSet<>();
    // The served-root registry: the root enters when ELP first says Running, not before.
    List<Boolean> servedBeforeRunning = new CopyOnWriteArrayList<>();
    PlatformTestUtil.waitWithEventsDispatching("ELP does not run", () -> {
      if (ElpLspServerStatus.findClient(getProject(), shop, ElpEngine.Feature.NAVIGATION) != null) return true;
      servedBeforeRunning.add(ElpServedRoots.getInstance().isUnderServedRoot(myRoot));
      return false;
    }, TIMEOUT_SECONDS);
    assertFalse("asked " + servedBeforeRunning.size() + " times before Running", servedBeforeRunning.contains(true));
    ElpLspClientDescriptor first = waitForServing(shop, null, pids);
    assertEquals(List.of(myRoot.getPath()), ElpServedRoots.getInstance().getRoots());

    long killed = System.currentTimeMillis();
    assertTrue(ProcessHandle.of(first.getProcessPid()).orElseThrow().destroyForcibly());
    // A crash and the restart do not give the root back to the plugin.
    List<Boolean> servedWhileDown = new CopyOnWriteArrayList<>();
    PlatformTestUtil.waitWithEventsDispatching("ELP was not restarted", () -> {
      LspServer client = ElpLspServerStatus.findClient(getProject(), shop, ElpEngine.Feature.NAVIGATION);
      if (client != null && client.getDescriptor() != first) return true;
      servedWhileDown.add(ElpEngine.ownsFeature(getProject(), shop, ElpEngine.Feature.COMPLETION));
      return false;
    }, TIMEOUT_SECONDS);
    assertFalse("asked " + servedWhileDown.size() + " times while ELP was down", servedWhileDown.contains(false));
    ElpLspClientDescriptor second = waitForServing(shop, first, pids);
    long restartedAfter = System.currentTimeMillis() - killed;
    Notification restarted = waitForNotification(notifications, "ELP stopped unexpectedly", 1);
    assertTrue(restarted.getContent(), restarted.getContent().contains("with exit code 137; the plugin started it again."));
    assertEmpty(restarted.getActions().stream().filter(a -> "Restart ELP".equals(a.getTemplateText())).toList());

    assertTrue(ProcessHandle.of(second.getProcessPid()).orElseThrow().destroyForcibly());
    Notification stays = waitForNotification(notifications, "ELP stopped unexpectedly", 2);
    assertTrue(stays.getContent(), stays.getContent().contains("again within 5 minutes." + ElpNotifications.STAYS_OFF));
    AnAction restart = ContainerUtil.find(stays.getActions(), a -> "Restart ELP".equals(a.getTemplateText()));
    assertNotNull(restart);
    assertNotNull(ContainerUtil.find(stays.getActions(), a -> "Switch back to the plugin".equals(a.getTemplateText())));
    long quietUntil = System.currentTimeMillis() + 5_000;
    PlatformTestUtil.waitWithEventsDispatching("", () -> System.currentTimeMillis() > quietUntil, 10);
    assertNull("not restarted a second time", ElpLspServerStatus.findClient(getProject(), shop, ElpEngine.Feature.NAVIGATION));
    assertTrue("the root stays ELP's, the plugin does not answer", ElpEngine.ownsFeature(getProject(), shop, ElpEngine.Feature.NAVIGATION));

    ElpRestarts.restart(getProject());
    waitForServing(shop, second, pids);
    LspServerManager.getInstance(getProject()).stopServers(ElpLspIntegrationProvider.class);
    PlatformTestUtil.waitWithEventsDispatching("a process of ELP is left: " + pids, () -> !ContainerUtil.exists(
      pids, pid -> ProcessHandle.of(pid).map(ProcessHandle::isAlive).orElse(false)), TIMEOUT_SECONDS);
    System.out.println("ELP crash: restarted and serving again " + restartedAfter + " ms after kill -9; processes seen " + pids.size());
  }

  /**
   * Completion comes from ELP with a typed prefix, and the plugin's contributors add nothing: the items are ELP's
   * ({@code lookup/2}), none is the plugin's function element.
   */
  public void testCompletionFromElpWithAPrefix() throws Exception {
    String elp = elpPath();
    Path project = FileUtil.createTempDirectory("elp-live", null, true).toPath().toRealPath().resolve("project");
    FileUtil.copyDir(new File("testData/elp/project"), project.toFile());
    startElp(elp, project);
    Editor editor = myFixture.getEditor();
    PsiFile shopPsi = myFixture.getFile();
    int call = editor.getDocument().getText().indexOf("lookup(shop_memory") + 2;
    PlatformTestUtil.waitWithEventsDispatching("ELP did not answer the definition", () -> isFunction(definition(shopPsi, editor, call)),
                                               TIMEOUT_SECONDS);
    // "shop_backend:look|up(": the prefix "look" typed.
    editor.getCaretModel().moveToOffset(editor.getDocument().getText().indexOf("lookup(shop_memory") + "look".length());
    CodeInsightSettings settings = CodeInsightSettings.getInstance();
    boolean autocomplete = settings.AUTOCOMPLETE_ON_CODE_COMPLETION;
    settings.AUTOCOMPLETE_ON_CODE_COMPLETION = false;
    try {
      LookupElement[] items = myFixture.completeBasic();
      assertNotNull(items);
      List<String> texts = ContainerUtil.map(items, item -> {
        LookupElementPresentation presentation = new LookupElementPresentation();
        item.renderElement(presentation);
        return presentation.getItemText();
      });
      System.out.println("ELP completion of shop_backend:look: " + texts);
      assertContainsElements(texts, "lookup/2");
      assertFalse("an item of the plugin's contributors: " + texts,
                  ContainerUtil.exists(items, item -> item.getPsiElement() instanceof ErlangFunction));
    }
    finally {
      settings.AUTOCOMPLETE_ON_CODE_COMPLETION = autocomplete;
    }
  }

  /**
   * A module is renamed by ELP: the file, {@code -module} and the qualified calls. ELP 1.1.0 leaves {@code -behaviour} and
   * {@code -import} alone.
   */
  public void testModuleRenameThroughElp() throws Exception {
    String elp = elpPath();
    Path project = FileUtil.createTempDirectory("elp-live", null, true).toPath().toRealPath().resolve("project");
    FileUtil.copyDir(new File("testData/elp/project"), project.toFile());
    startElp(elp, project);
    Editor shopEditor = myFixture.getEditor();
    PsiFile shopPsi = myFixture.getFile();
    int call = shopEditor.getDocument().getText().indexOf("lookup(shop_memory") + 2;
    PlatformTestUtil.waitWithEventsDispatching("ELP did not answer the definition", () -> isFunction(definition(shopPsi, shopEditor, call)),
                                               TIMEOUT_SECONDS);
    VirtualFile backend = file("src/shop_backend.erl");
    myFixture.openFileInEditor(backend);
    Editor editor = myFixture.getEditor();
    editor.getCaretModel().moveToOffset(editor.getDocument().getText().indexOf("-module(shop_back") + "-module(shop_back".length());
    assertNull(ElpRenameHandler.renameAtCaret(getProject(), editor, myFixture.getFile(), "shop_store"));
    assertEquals("shop_store.erl", backend.getName());
    assertTrue(editor.getDocument().getText(), editor.getDocument().getText().startsWith("-module(shop_store)."));
    String shopText = shopEditor.getDocument().getText();
    assertTrue(shopText, shopText.contains("shop_store:lookup(shop_memory, Name)"));
    Document memory = FileDocumentManager.getInstance().getDocument(file("src/shop_memory.erl"));
    assertNotNull(memory);
    System.out.println("ELP module rename, shop_memory.erl starts with: " + memory.getText().lines().limit(3).toList());
  }

  private static boolean isFunction(@Nullable PsiElement[] targets) {
    return targets != null && targets.length == 1 && targets[0] instanceof ErlangFunction;
  }

  /** Waits until a server other than {@code previous} serves {@code file}, and collects the ids of its process and children. */
  @NotNull
  private ElpLspClientDescriptor waitForServing(@NotNull VirtualFile file, @Nullable ElpLspClientDescriptor previous, @NotNull Set<Long> pids) {
    AtomicReference<ElpLspClientDescriptor> current = new AtomicReference<>();
    PlatformTestUtil.waitWithEventsDispatching("ELP does not serve " + file, () -> {
      LspServer client = ElpLspServerStatus.findClient(getProject(), file, ElpEngine.Feature.NAVIGATION);
      if (client == null || client.getDescriptor() == previous) return false;
      current.set((ElpLspClientDescriptor)client.getDescriptor());
      return true;
    }, TIMEOUT_SECONDS);
    long pid = current.get().getProcessPid();
    pids.add(pid);
    ProcessHandle.of(pid).ifPresent(process -> process.descendants().forEach(child -> pids.add(child.pid())));
    return current.get();
  }

  @NotNull
  private static Notification waitForNotification(@NotNull List<Notification> notifications, @NotNull String title, int count) {
    PlatformTestUtil.waitWithEventsDispatching("no notification " + title + " #" + count, () -> ContainerUtil.filter(
      notifications, n -> title.equals(n.getTitle())).size() >= count, TIMEOUT_SECONDS);
    return ContainerUtil.filter(notifications, n -> title.equals(n.getTitle())).get(count - 1);
  }

  @NotNull
  private static String otpRoot() throws Exception {
    ProcessOutput output = new CapturingProcessHandler(
      new GeneralCommandLine("erl", "-noshell", "-eval", "io:put_chars(code:root_dir()), halt().")).runProcess(60_000);
    Assume.assumeTrue("erl is not on the PATH", output.getExitCode() == 0 && !output.getStdout().isBlank());
    return output.getStdout().trim();
  }

  @NotNull
  private static String elpPath() {
    String elp = System.getenv("ERLANG_ELP_PATH");
    Assume.assumeTrue("Set ERLANG_ELP_PATH to the elp binary to run this test", elp != null && !elp.isBlank());
    assertTrue("ERLANG_ELP_PATH is " + elp + ", which is not a file", new File(elp).isFile());
    return elp;
  }

  /**
   * Makes {@code project} a content root, opens {@code src/shop.erl} and starts ELP in the ELP position for it.
   */
  @NotNull
  private ElpLspClientDescriptor startElp(@NotNull String elp, @NotNull Path project) {
    ElpLspClientDescriptor descriptor = startElp(elp, project, null);
    VirtualFile shop = file("src/shop.erl");
    PlatformTestUtil.waitWithEventsDispatching("ELP did not start", () -> ElpEngine.ownsFeature(getProject(), shop, ElpEngine.Feature.NAVIGATION),
                                               TIMEOUT_SECONDS);
    return descriptor;
  }

  @NotNull
  private ElpLspClientDescriptor startElp(@NotNull String elp, @NotNull Path project, @Nullable Map<String, String> environment) {
    VfsRootAccess.allowRootAccess(getTestRootDisposable(), project.toString());
    myRoot = LocalFileSystem.getInstance().refreshAndFindFileByNioFile(project);
    assertNotNull(myRoot);
    PsiTestUtil.addSourceContentToRoots(getModule(), myRoot);
    IndexingTestUtil.waitUntilIndexesAreReady(getProject());

    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    state.path = elp;
    state.mode = ElpMode.ELP.name();
    ElpSettings.getInstance().loadState(state);

    VirtualFile shop = file("src/shop.erl");
    myFixture.openFileInEditor(shop);
    ElpLspClientDescriptor descriptor = new ElpLspClientDescriptor(getProject(), myRoot, new ElpConfiguration(elp, ElpMode.ELP));
    descriptor.setEnvironmentForTests(environment);
    LspServerManager.getInstance(getProject()).ensureServerStarted(ElpLspIntegrationProvider.class, descriptor);
    return descriptor;
  }

  @Nullable
  private static String eqwalizerIn(@NotNull Path dir) {
    String[] names = dir.toFile().list();
    if (names == null) return null;
    for (String name : names) {
      if (name.startsWith("eqwalizer")) return name;
    }
    return null;
  }

  @Nullable
  private static PsiElement[] definition(@NotNull PsiFile file, @NotNull Editor editor, int offset) {
    return PlatformTestUtil.waitForFuture(ApplicationManager.getApplication().executeOnPooledThread(() -> ReadAction.compute(
      () -> new ElpGotoDeclarationHandler().getGotoDeclarationTargets(file.findElementAt(offset), offset, editor))), 30_000);
  }

  @NotNull
  private VirtualFile file(@NotNull String relativePath) {
    VirtualFile file = myRoot.findFileByRelativePath(relativePath);
    assertNotNull(relativePath, file);
    return file;
  }
}
