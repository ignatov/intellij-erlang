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

import com.intellij.codeInsight.TargetElementUtil;
import com.intellij.codeInsight.daemon.impl.DaemonProgressIndicator;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInspection.InspectionEngine;
import com.intellij.codeInspection.ex.LocalInspectionToolWrapper;
import com.intellij.lang.folding.FoldingDescriptor;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.impl.SimpleDataContext;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.newvfs.impl.VfsRootAccess;
import com.intellij.platform.lsp.api.LspServerState;
import com.intellij.platform.lsp.api.customization.LspCustomization;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.refactoring.rename.RenameHandlerRegistry;
import com.intellij.testFramework.IndexingTestUtil;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.testFramework.utils.parameterInfo.MockCreateParameterInfoContext;
import com.intellij.util.containers.ContainerUtil;
import org.eclipse.lsp4j.InitializeResult;
import org.eclipse.lsp4j.ServerCapabilities;
import org.intellij.erlang.ErlangParameterInfoHandler;
import org.intellij.erlang.documentation.ErlangDocumentationProvider;
import org.intellij.erlang.editor.ErlangFoldingBuilder;
import org.intellij.erlang.editor.ErlangWordSelectioner;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpEngine;
import org.intellij.erlang.elp.ElpEngine.Feature;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.elp.ElpServedRoots;
import org.intellij.erlang.elp.ElpSettings;
import org.intellij.erlang.go.ErlangSymbolContributor;
import org.intellij.erlang.hierarchy.ErlangCallHierarchyProvider;
import org.intellij.erlang.hints.ErlangParameterHintsProvider;
import org.intellij.erlang.inspection.ErlangUnusedFunctionInspection;
import org.intellij.erlang.intention.ErlangExportFunctionIntention;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

/**
 * The engine switch for every {@link Feature}, with the real status of the LSP part, the root of the test project in the served-root
 * registry and a client that announced the capabilities the real ELP announced. A new feature fails
 * {@link #testEveryFeatureHasAnOwner} until it gets an owner here, and does not compile until it gets a probe of the plugin's
 * counterpart.
 */
public class ElpEngineSwitchTest extends ElpRecordedProjectTestBase {
  private enum Owner {PLUGIN, ELP}

  /** Who answers a feature in the {@code ELP} position under a served root; the documented decision of {@link ElpEngine}. */
  private static final Map<Feature, Owner> OWNER = new EnumMap<>(Feature.class);

  static {
    OWNER.put(Feature.COMPLETION, Owner.ELP);
    OWNER.put(Feature.PARAMETER_HINTS, Owner.ELP);
    OWNER.put(Feature.DOCUMENTATION, Owner.ELP);
    OWNER.put(Feature.FOLDING, Owner.ELP);
    OWNER.put(Feature.INTENTIONS, Owner.ELP);
    OWNER.put(Feature.SELECTION, Owner.ELP);
    OWNER.put(Feature.NAVIGATION, Owner.ELP);
    OWNER.put(Feature.DIAGNOSTICS, Owner.ELP);
    OWNER.put(Feature.FIND_USAGES, Owner.ELP);
    OWNER.put(Feature.RENAME, Owner.ELP);
    OWNER.put(Feature.SIGNATURE_HELP, Owner.ELP);
    OWNER.put(Feature.DOCUMENT_HIGHLIGHTS, Owner.ELP);
    OWNER.put(Feature.WORKSPACE_SYMBOLS, Owner.ELP);
    OWNER.put(Feature.CALL_HIERARCHY, Owner.ELP);
  }

  private static final String PROBE = """
    -module(probe).
    -export([f/0]).

    %% @doc Says hello.
    hello(Name) ->
      Name.

    f() -> hello(1).

    unused() -> ok.
    """;

  public void testEveryFeatureHasAnOwner() {
    assertEquals(EnumSet.allOf(Feature.class), OWNER.keySet());
    for (Feature feature : Feature.values()) assertEquals("ELP or nothing: " + feature, Owner.ELP, OWNER.get(feature));
  }

  /** The Plugin position: the plugin owns every feature, and the switch does not even look for a server, although one runs. */
  public void testPluginPositionAsksNoServer() {
    position(ElpMode.PLUGIN);
    PsiFile shop = psiFile("src/shop.erl");
    myClientLookups.set(0);
    for (Feature feature : Feature.values()) {
      assertFalse(feature.name(), ElpEngine.ownsFeature(shop, feature));
    }
    assertFalse(ElpEngine.servesWholeProject(getProject()));
    assertEquals(0, myClientLookups.get());
    assertEmpty(myServer.requests());
  }

  /** The ELP position with the server serving and the real capabilities: the documented owner, for a module and for a header. */
  public void testElpPositionWhileTheServerServes() {
    for (String file : List.of("src/shop.erl", "src/shop.hrl")) {
      PsiFile psiFile = psiFile(file);
      for (Feature feature : Feature.values()) {
        boolean elp = OWNER.get(feature) == Owner.ELP;
        assertEquals(file + " " + feature, elp, ElpEngine.ownsFeature(psiFile, feature));
      }
    }
  }

  /** A served root stays ELP's while its server starts, stops or crashed: the plugin does not answer in between. */
  public void testServedRootStaysElpsWhileTheServerIsNotRunning() {
    PsiFile shop = psiFile("src/shop.erl");
    myClientLookups.set(0);
    for (LspServerState state : List.of(LspServerState.Initializing, LspServerState.ShutdownNormally,
                                        LspServerState.ShutdownUnexpectedly)) {
      myClient.setState(state);
      for (Feature feature : Feature.values()) assertTrue(state + " " + feature, ElpEngine.ownsFeature(shop, feature));
    }
    assertEquals("the switch does not look at the server", 0, myClientLookups.get());
  }

  /** A served root stays ELP's while ELP reloads the project, or after a reload failed. */
  public void testServedRootStaysElpsWhileElpLoads() {
    PsiFile shop = psiFile("src/shop.erl");
    for (ElpLoadState.State state : List.of(ElpLoadState.State.STARTING, ElpLoadState.State.LOADING, ElpLoadState.State.FAILED,
                                            ElpLoadState.State.STOPPED)) {
      myDescriptor.getLoadState().setStateForTests(state);
      for (Feature feature : Feature.values()) assertTrue(state + " " + feature, ElpEngine.ownsFeature(shop, feature));
    }
  }

  /**
   * The client is Running about 0.01 s after the start, but ELP answers only "elp is still loading" until it has loaded the
   * project, and never when it cannot: a root enters the registry when ELP first says Running, and the plugin answers until then.
   * From then on a crash does not give it back; switching back to the Plugin position does.
   */
  public void testRootEntersTheRegistryOnTheFirstRunningAndSurvivesACrash() {
    ElpServedRoots.getInstance().setRootsForTests(List.of(), getTestRootDisposable());
    PsiFile shop = psiFile("src/shop.erl");
    ElpLoadState loadState = new ElpLspClientDescriptor(getProject(), myRoot, new ElpConfiguration(ELP_PATH, ElpMode.ELP)).getLoadState();
    loadState.started(System.currentTimeMillis());
    loadState.initialized();
    assertEquals(ElpLoadState.State.LOADING, loadState.state());
    assertFalse("the plugin answers while ELP loads for the first time", ElpEngine.ownsFeature(shop, Feature.NAVIGATION));
    loadState.status("Running", System.currentTimeMillis());
    assertEquals(List.of(myRoot.getPath()), ElpServedRoots.getInstance().getRoots());
    assertTrue(ElpEngine.ownsFeature(shop, Feature.NAVIGATION));
    loadState.stopped();
    assertTrue("a crash does not give the root back", ElpEngine.ownsFeature(shop, Feature.COMPLETION));

    ElpSettings.getInstance().setMode(ElpMode.PLUGIN);
    assertTrue(ElpServedRoots.getInstance().isEmpty());
    ElpSettings.getInstance().setMode(ElpMode.ELP);
    assertFalse("the root enters again only when ELP runs again", ElpEngine.ownsFeature(shop, Feature.NAVIGATION));
  }

  /** A client of the Plugin position never enters the registry. */
  public void testClientOfThePluginPositionDoesNotEnterTheRegistry() {
    ElpServedRoots.getInstance().setRootsForTests(List.of(), getTestRootDisposable());
    ElpLoadState loadState = new ElpLspClientDescriptor(getProject(), myRoot, new ElpConfiguration(ELP_PATH, ElpMode.PLUGIN)).getLoadState();
    loadState.started(System.currentTimeMillis());
    loadState.initialized();
    loadState.status("Running", System.currentTimeMillis());
    loadState.stopped();
    assertTrue(ElpServedRoots.getInstance().isEmpty());
  }

  /**
   * Until ELP has loaded the project the client asks it for nothing of its own accord, so that ELP does not answer "elp is still
   * loading", which IDEA 2026.2 logs as an IDE error; the plugin stays silent meanwhile. Diagnostics, which ELP pushes, stay on.
   */
  public void testElpIsAskedForNothingWhileItLoads() {
    PsiFile shop = psiFile("src/shop.erl");
    LspCustomization customization = myDescriptor.getLspCustomization();
    List<Supplier<Object>> asked = List.of(
      customization::getFoldingRangeCustomizer, customization::getInlayHintCustomizer, customization::getCodeActionsCustomizer,
      customization::getHoverCustomizer, customization::getSelectionRangeCustomizer, customization::getSignatureHelpCustomizer,
      customization::getDocumentHighlightsCustomizer, customization::getGoToDefinitionCustomizer,
      customization::getGoToTypeDefinitionCustomizer, customization::getWorkspaceSymbolCustomizer);
    for (ElpLoadState.State state : List.of(ElpLoadState.State.STARTING, ElpLoadState.State.LOADING, ElpLoadState.State.FAILED,
                                            ElpLoadState.State.STOPPED)) {
      myDescriptor.getLoadState().setStateForTests(state);
      for (Supplier<Object> customizer : asked) {
        String name = customizer.get().getClass().getSimpleName();
        assertTrue(state + ": " + name, name.endsWith("Disabled"));
      }
      assertInstanceOf(customization.getDiagnosticsCustomizer(), ElpDiagnosticsSupport.class);
      for (Feature feature : Feature.values()) assertTrue(state + " " + feature, ElpEngine.ownsFeature(shop, feature));
    }
    myDescriptor.getLoadState().setStateForTests(ElpLoadState.State.RUNNING);
    for (Supplier<Object> customizer : asked) {
      String name = customizer.get().getClass().getSimpleName();
      assertFalse(name, name.endsWith("Disabled"));
    }
  }

  /** The platform's language services widget says whether ELP has loaded the project. */
  public void testWidgetShowsTheLoadState() {
    myDescriptor.getLoadState().setStateForTests(ElpLoadState.State.LOADING);
    ElpLspIntegrationProvider.ElpWidgetItem loading = new ElpLspIntegrationProvider.ElpWidgetItem(myClient, null);
    assertTrue(loading.getServerLabel(), loading.getServerLabel().endsWith(" (loading the project)"));
    assertFalse(loading.isError());
    myDescriptor.getLoadState().setStateForTests(ElpLoadState.State.FAILED);
    ElpLspIntegrationProvider.ElpWidgetItem failed = new ElpLspIntegrationProvider.ElpWidgetItem(myClient, null);
    assertTrue(failed.getServerLabel(), failed.getServerLabel().endsWith(" (could not load the project)"));
    assertTrue(failed.isError());
    myDescriptor.getLoadState().setStateForTests(ElpLoadState.State.RUNNING);
    ElpLspIntegrationProvider.ElpWidgetItem running = new ElpLspIntegrationProvider.ElpWidgetItem(myClient, null);
    assertFalse(running.getServerLabel(), running.getServerLabel().contains("("));
    assertFalse(running.isError());
  }

  /**
   * A server that did not announce the capability of a feature is not asked for it, but the feature does not go back to the plugin
   * under a served root.
   */
  public void testServedRootStaysElpsWithoutTheCapability() {
    PsiFile shop = psiFile("src/shop.erl");
    for (Feature withdrawn : Feature.values()) {
      InitializeResult result = myAnswers.initializeResult();
      boolean announced = withdraw(result.getCapabilities(), withdrawn);
      myClient.setInitializeResult(result);
      for (Feature feature : Feature.values()) {
        assertTrue("without " + withdrawn + ": " + feature, ElpEngine.ownsFeature(shop, feature));
        assertEquals("without " + withdrawn + ": is ELP asked for " + feature, feature != withdrawn || !announced,
                     ElpLspServerStatus.findClient(getProject(), shop.getVirtualFile(), feature) != null);
      }
    }
  }

  /** The files ELP must not get stay with the plugin, while the modules and headers next to them are served. */
  public void testFilesElpDoesNotGet() {
    for (String file : List.of("src/shop.app.src", "rebar.config")) {
      PsiFile psiFile = psiFile(file);
      for (Feature feature : Feature.values()) assertFalse(file + " " + feature, ElpEngine.ownsFeature(psiFile, feature));
    }
    assertTrue(ElpEngine.ownsFeature(psiFile("src/shop_backend.erl"), Feature.NAVIGATION));
  }

  /**
   * A file under another content root, which this server does not load, stays with the plugin, so its inspections are not
   * silenced. A file outside the project's content, such as an OTP source, goes to the client, so it is ELP's.
   */
  public void testAFileUnderAnotherContentRootStaysWithThePlugin() throws Exception {
    Path otherDir = FileUtil.createTempDirectory("elp-other-root", null, true).toPath();
    Files.writeString(otherDir.resolve("other.erl"), "-module(other).\n");
    Path outsideDir = FileUtil.createTempDirectory("elp-outside", null, true).toPath();
    Files.writeString(outsideDir.resolve("lists.erl"), "-module(lists).\n");
    VfsRootAccess.allowRootAccess(getTestRootDisposable(), otherDir.toString(), outsideDir.toString());
    VirtualFile otherRoot = LocalFileSystem.getInstance().refreshAndFindFileByNioFile(otherDir);
    VirtualFile outside = LocalFileSystem.getInstance().refreshAndFindFileByNioFile(outsideDir.resolve("lists.erl"));
    assertNotNull(otherRoot);
    assertNotNull(outside);
    PsiTestUtil.addSourceContentToRoots(getModule(), otherRoot);
    try {
      IndexingTestUtil.waitUntilIndexesAreReady(getProject());
      VirtualFile other = otherRoot.findChild("other.erl");
      assertNotNull(other);
      assertFalse(myDescriptor.isSupportedFile(other));
      assertFalse(ElpEngine.ownsFeature(getProject(), other, Feature.DIAGNOSTICS));
      assertFalse(ElpEngine.ownsFeature(getProject(), other, Feature.NAVIGATION));
      assertTrue(myDescriptor.isSupportedFile(outside));
      assertTrue(ElpEngine.ownsFeature(getProject(), outside, Feature.DIAGNOSTICS));
      assertTrue(ElpEngine.ownsFeature(getProject(), file("src/shop.erl"), Feature.DIAGNOSTICS));
    }
    finally {
      PsiTestUtil.removeContentEntry(getModule(), otherRoot);
    }
  }

  /**
   * One engine per file: the files that the server of a served root gets are exactly those where the plugin is silent. An OTP
   * source of the project's SDK goes to ELP, so hover, inlay hints, folding and the diagnostics there are ELP's alone; so are the
   * files of a symlinked root by their real path.
   */
  public void testElpGetsExactlyTheFilesWhereThePluginIsSilent() throws Exception {
    VirtualFile lists = LocalFileSystem.getInstance().refreshAndFindFileByPath(
      new java.io.File("testData/mockSdk-R15B02/lib/stdlib-1.18.2/src/lists.erl").getAbsolutePath());
    assertNotNull(lists);
    Path temp = FileUtil.createTempDirectory("elp-real-root", null, true).toPath().toRealPath();
    Path real = Files.createDirectories(temp.resolve("real/linked/src"));
    Files.writeString(real.resolve("linked.erl"), "-module(linked).\n");
    Path link = Files.createSymbolicLink(temp.resolve("link"), temp.resolve("real"));
    VfsRootAccess.allowRootAccess(getTestRootDisposable(), temp.toString());
    VirtualFile linkedRoot = LocalFileSystem.getInstance().refreshAndFindFileByNioFile(link.resolve("linked"));
    VirtualFile byRealPath = LocalFileSystem.getInstance().refreshAndFindFileByNioFile(real.resolve("linked.erl"));
    assertNotNull(linkedRoot);
    assertNotNull(byRealPath);
    PsiTestUtil.addSourceContentToRoots(getModule(), linkedRoot);
    try {
      IndexingTestUtil.waitUntilIndexesAreReady(getProject());
      VirtualFile byLink = linkedRoot.findFileByRelativePath("src/linked.erl");
      assertNotNull(byLink);
      ElpLspClientDescriptor linked = new ElpLspClientDescriptor(getProject(), linkedRoot, new ElpConfiguration(ELP_PATH, ElpMode.ELP));
      linked.resolveRealRoot();
      ElpServedRoots.getInstance().setRootsForTests(List.of(myRoot.getPath(), linkedRoot.getPath()), getTestRootDisposable());

      Map<VirtualFile, Boolean> expected = new java.util.LinkedHashMap<>();
      expected.put(file("src/shop.erl"), true);
      expected.put(file("src/shop.hrl"), true);
      expected.put(file("rebar.config"), false);
      expected.put(lists, true);
      for (Map.Entry<VirtualFile, Boolean> entry : expected.entrySet()) {
        VirtualFile file = entry.getKey();
        assertEquals(file.getPath(), entry.getValue().booleanValue(), myDescriptor.isSupportedFile(file));
        assertEquals(file.getPath(), entry.getValue().booleanValue(), ElpEngine.ownsFeature(getProject(), file, Feature.DOCUMENTATION));
      }
      for (VirtualFile file : List.of(byLink, byRealPath)) {
        assertTrue(file.getPath(), linked.isSupportedFile(file));
        assertTrue(file.getPath(), ElpEngine.ownsFeature(getProject(), file, Feature.FOLDING));
      }
      assertFalse("the file of another root", myDescriptor.isSupportedFile(byLink));
    }
    finally {
      ElpServedRoots.getInstance().setRealPath(linkedRoot.getPath(), null);
      PsiTestUtil.removeContentEntry(getModule(), linkedRoot);
    }
  }

  /**
   * A project is served only through its own workspace root: opened on a directory under a root that another project's ELP serves,
   * for example an application of a monorepo, it stays with the plugin until its own server runs.
   */
  public void testAProjectUnderAnotherProjectsServedRootIsNotServed() {
    ElpServedRoots.getInstance().setRootsForTests(List.of(myRoot.getParent().getPath()), getTestRootDisposable());
    assertTrue(ElpServedRoots.getInstance().isUnderServedRoot(myRoot));
    for (Feature feature : Feature.values()) {
      assertFalse(feature.name(), ElpEngine.ownsFeature(getProject(), file("src/shop.erl"), feature));
    }
    assertFalse(ElpEngine.servesWholeProject(getProject()));
  }

  /** A client started for the Plugin position, as before a switch of the position, is never asked. */
  public void testClientOfThePluginPositionIsNeverAsked() {
    FakeElpClient pluginClient = new FakeElpClient(
      new ElpLspClientDescriptor(getProject(), myRoot, new ElpConfiguration(ELP_PATH, ElpMode.PLUGIN)), myServer,
      myAnswers.initializeResult());
    ElpLspServerStatus.setClientsForTests(project -> List.of(pluginClient), getTestRootDisposable());
    VirtualFile shop = file("src/shop.erl");
    for (Feature feature : Feature.values()) assertNull(feature.name(), ElpLspServerStatus.findClient(getProject(), shop, feature));
  }

  /** The stored modes of the prototype: {@code ALL} is the ELP position, {@code DIAGNOSTICS} the Plugin position. */
  public void testStoredPrototypeModes() {
    PsiFile shop = psiFile("src/shop.erl");
    store("ALL");
    assertEquals(ElpMode.ELP, ElpSettings.getInstance().getStoredMode());
    assertTrue(ElpEngine.ownsFeature(shop, Feature.NAVIGATION));
    store("DIAGNOSTICS");
    assertEquals(ElpMode.PLUGIN, ElpSettings.getInstance().getStoredMode());
    assertFalse(ElpEngine.ownsFeature(shop, Feature.NAVIGATION));
  }

  /**
   * The plugin's counterpart of every feature at its entry point, in the three states: the Plugin position, the ELP position with
   * the server serving, and the ELP position with the server stopped. The plugin answers only in the Plugin position.
   */
  public void testThePluginsCounterpartAtItsEntryPoint() {
    for (Feature feature : Feature.values()) {
      for (int state = 0; state < 3; state++) {
        position(state == 0 ? ElpMode.PLUGIN : ElpMode.ELP);
        myClient.setState(state == 2 ? LspServerState.ShutdownUnexpectedly : LspServerState.Running);
        Boolean plugin = pluginAnswers(feature);
        if (plugin == null) continue;
        boolean expected = state == 0 || OWNER.get(feature) != Owner.ELP;
        assertEquals(feature + " in state " + state, expected, plugin.booleanValue());
      }
    }
  }

  /**
   * Whether the plugin's counterpart of {@code feature} answers, or {@code null} when the plugin has no switch for it: the platform
   * prefers ELP's document highlights over the plugin's references while a server runs.
   */
  @Nullable
  private Boolean pluginAnswers(@NotNull Feature feature) {
    return switch (feature) {
      case COMPLETION -> {
        myFixture.configureByText("completion.erl", "-module(completion).\nbar1() -> ok.\nbar2() -> ok.\nfoo() -> bar<caret>");
        LookupElement[] items = myFixture.completeBasic();
        yield items != null && ContainerUtil.exists(items, item -> item.getLookupString().equals("bar1"));
      }
      case PARAMETER_HINTS -> {
        probe("f() -> hel<caret>lo(1).");
        yield new ErlangParameterHintsProvider().createCollector(myFixture.getFile(), myFixture.getEditor()) != null;
      }
      case DOCUMENTATION -> {
        probe("f() -> hel<caret>lo(1).");
        PsiElement original = myFixture.getFile().findElementAt(myFixture.getCaretOffset());
        yield new ErlangDocumentationProvider().generateDoc(((ErlangFile)myFixture.getFile()).getFunction("hello", 1), original) != null;
      }
      case FOLDING -> {
        probe("f() -> hel<caret>lo(1).");
        FoldingDescriptor[] regions = new ErlangFoldingBuilder().buildFoldRegions(myFixture.getFile(), myFixture.getEditor().getDocument(), false);
        yield ContainerUtil.exists(regions, region -> !(region.getElement().getPsi() instanceof PsiComment));
      }
      case INTENTIONS -> {
        probe("unu<caret>sed() -> ok.");
        yield new ErlangExportFunctionIntention().isAvailable(getProject(), myFixture.getEditor(), myFixture.getFile());
      }
      case SELECTION -> {
        probe("f() -> hel<caret>lo(1).");
        yield new ErlangWordSelectioner().canSelect(myFixture.getFile().findElementAt(myFixture.getCaretOffset()));
      }
      case NAVIGATION -> {
        Editor editor = openAt(myAnswers.get("textDocument/definition", "remote_function"));
        int offset = editor.getCaretModel().getOffset();
        PsiFile file = myFixture.getFile();
        yield onPooledThread(() -> ReadAction.compute(
          () -> new ElpGotoDeclarationHandler().getGotoDeclarationTargets(file.findElementAt(offset), offset, editor))) == null;
      }
      case DIAGNOSTICS -> {
        probe("unu<caret>sed() -> ok.");
        yield !unusedFunctionProblemsOnTheFly(myFixture.getFile()).isEmpty();
      }
      case FIND_USAGES -> {
        probe("f() -> hel<caret>lo(1).");
        PsiElement target = TargetElementUtil.findTargetElement(myFixture.getEditor(), TargetElementUtil.getInstance().getAllAccepted());
        yield !new ElpFindUsagesHandlerFactory().canFindUsages(target);
      }
      case RENAME -> {
        probe("f() -> hel<caret>lo(1).");
        yield !ContainerUtil.exists(RenameHandlerRegistry.getInstance().getRenameHandlers(((EditorEx)myFixture.getEditor()).getDataContext()),
                                    handler -> handler instanceof ElpRenameHandler);
      }
      case SIGNATURE_HELP -> {
        probe("f() -> hello(<caret>1).");
        yield new ErlangParameterInfoHandler().findElementForParameterInfo(
          new MockCreateParameterInfoContext(myFixture.getEditor(), myFixture.getFile())) != null;
      }
      case WORKSPACE_SYMBOLS -> {
        probe("f() -> hel<caret>lo(1).");
        yield new ErlangSymbolContributor().getItemsByName("hello", "hello", getProject(), false).length > 0;
      }
      case CALL_HIERARCHY -> {
        probe("f() -> hel<caret>lo(1).");
        ErlangFunction hello = ((ErlangFile)myFixture.getFile()).getFunction("hello", 1);
        yield new ErlangCallHierarchyProvider().getTarget(SimpleDataContext.builder().add(CommonDataKeys.PSI_ELEMENT, hello)
                                                            .add(CommonDataKeys.PSI_FILE, myFixture.getFile()).build()) != null;
      }
      case DOCUMENT_HIGHLIGHTS -> null;
    };
  }

  private void probe(@NotNull String lineWithCaret) {
    String text = PROBE.replace(lineWithCaret.replace("<caret>", ""), lineWithCaret);
    assertTrue(lineWithCaret, text.contains("<caret>"));
    myFixture.configureByText("probe.erl", text);
  }

  @NotNull
  private static List<?> unusedFunctionProblemsOnTheFly(@NotNull PsiFile file) {
    LocalInspectionToolWrapper wrapper = new LocalInspectionToolWrapper(new ErlangUnusedFunctionInspection());
    DaemonProgressIndicator indicator = new DaemonProgressIndicator();
    return ProgressManager.getInstance().runProcess(
      () -> InspectionEngine.inspectEx(List.of(wrapper), file, file.getTextRange(), file.getTextRange(), true, false, true, indicator,
                                       (tool, descriptor) -> true), indicator).values().stream().flatMap(List::stream).toList();
  }

  /**
   * Takes back the capability that {@code feature} needs.
   *
   * @return whether there was one to take back; diagnostics are pushed and need none
   */
  private static boolean withdraw(@NotNull ServerCapabilities capabilities, @NotNull Feature feature) {
    return switch (feature) {
      case COMPLETION -> {
        capabilities.setCompletionProvider(null);
        yield true;
      }
      case PARAMETER_HINTS -> {
        capabilities.setInlayHintProvider((Boolean)null);
        yield true;
      }
      case DOCUMENTATION -> {
        capabilities.setHoverProvider((Boolean)null);
        yield true;
      }
      case FOLDING -> {
        capabilities.setFoldingRangeProvider((Boolean)null);
        yield true;
      }
      case INTENTIONS -> {
        capabilities.setCodeActionProvider((Boolean)null);
        yield true;
      }
      case SELECTION -> {
        capabilities.setSelectionRangeProvider((Boolean)null);
        yield true;
      }
      case NAVIGATION -> {
        capabilities.setDefinitionProvider((Boolean)null);
        yield true;
      }
      case FIND_USAGES -> {
        capabilities.setReferencesProvider((Boolean)null);
        yield true;
      }
      case RENAME -> {
        capabilities.setRenameProvider((Boolean)null);
        yield true;
      }
      case SIGNATURE_HELP -> {
        capabilities.setSignatureHelpProvider(null);
        yield true;
      }
      case DOCUMENT_HIGHLIGHTS -> {
        capabilities.setDocumentHighlightProvider((Boolean)null);
        yield true;
      }
      case WORKSPACE_SYMBOLS -> {
        capabilities.setWorkspaceSymbolProvider((Boolean)null);
        yield true;
      }
      case CALL_HIERARCHY -> {
        capabilities.setCallHierarchyProvider((Boolean)null);
        yield true;
      }
      case DIAGNOSTICS -> false;
    };
  }

  private static void store(@NotNull String mode) {
    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    state.path = ELP_PATH;
    state.mode = mode;
    ElpSettings.getInstance().loadState(state);
  }
}
