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
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.platform.lsp.api.LspServerState;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.refactoring.rename.RenameHandlerRegistry;
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
import org.intellij.erlang.elp.ElpSettings;
import org.intellij.erlang.hints.ErlangParameterHintsProvider;
import org.intellij.erlang.inspection.ErlangUnusedFunctionInspection;
import org.intellij.erlang.intention.ErlangExportFunctionIntention;
import org.intellij.erlang.psi.ErlangFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.EnumMap;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;

/**
 * The engine switch for every {@link Feature}, with the real status of the LSP part and a client that announced the capabilities
 * the real ELP announced. A new feature fails {@link #testEveryFeatureHasAnOwner} until it gets an owner here, and does not compile
 * until it gets a probe of the plugin's counterpart.
 */
public class ElpEngineSwitchTest extends ElpRecordedProjectTestBase {
  private enum Owner {PLUGIN, ELP}

  /** Who answers a feature in the {@code ELP} position while ELP serves the file; the documented decision of {@link ElpEngine}. */
  private static final Map<Feature, Owner> OWNER = new EnumMap<>(Feature.class);

  static {
    OWNER.put(Feature.COMPLETION, Owner.PLUGIN);
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
    for (Feature feature : Feature.values()) {
      assertEquals(feature.name(), OWNER.get(feature) == Owner.ELP, ElpEngine.elpMayOwn(feature));
    }
  }

  /** The Plugin position: the plugin owns every feature, and the switch does not even look for a server, although one runs. */
  public void testPluginPositionAsksNoServer() {
    position(ElpMode.PLUGIN);
    PsiFile shop = psiFile("src/shop.erl");
    myClientLookups.set(0);
    for (Feature feature : Feature.values()) {
      assertFalse(feature.name(), ElpEngine.ownsFeature(shop, feature));
      assertFalse(feature.name(), ElpEngine.ownsProjectFeature(getProject(), feature));
    }
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
        assertEquals(feature.name(), elp, ElpEngine.ownsProjectFeature(getProject(), feature));
      }
    }
  }

  public void testPluginAnswersWhileTheServerIsNotRunning() {
    PsiFile shop = psiFile("src/shop.erl");
    for (LspServerState state : List.of(LspServerState.Initializing, LspServerState.ShutdownNormally,
                                        LspServerState.ShutdownUnexpectedly)) {
      myClient.setState(state);
      for (Feature feature : Feature.values()) {
        assertFalse(state + " " + feature, ElpEngine.ownsFeature(shop, feature));
        assertFalse(state + " " + feature, ElpEngine.ownsProjectFeature(getProject(), feature));
      }
    }
  }

  /** A server that did not announce the capability of a feature leaves that feature, and only that one, with the plugin. */
  public void testPluginAnswersWithoutTheCapability() {
    PsiFile shop = psiFile("src/shop.erl");
    for (Feature withdrawn : Feature.values()) {
      InitializeResult result = myAnswers.initializeResult();
      boolean announced = withdraw(result.getCapabilities(), withdrawn);
      myClient.setInitializeResult(result);
      for (Feature feature : Feature.values()) {
        boolean elp = OWNER.get(feature) == Owner.ELP && (feature != withdrawn || !announced);
        assertEquals("without " + withdrawn + ": " + feature, elp, ElpEngine.ownsFeature(shop, feature));
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

  /** A client started for the Plugin position, as before a switch of the position, serves nothing. */
  public void testClientOfThePluginPositionServesNothing() {
    FakeElpClient pluginClient = new FakeElpClient(
      new ElpLspClientDescriptor(getProject(), myRoot, new ElpConfiguration(ELP_PATH, ElpMode.PLUGIN)), myServer,
      myAnswers.initializeResult());
    ElpLspServerStatus.setClientsForTests(project -> List.of(pluginClient), getTestRootDisposable());
    PsiFile shop = psiFile("src/shop.erl");
    for (Feature feature : Feature.values()) assertFalse(feature.name(), ElpEngine.ownsFeature(shop, feature));
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
   * the server serving, and the ELP position with the server stopped. The plugin answers unless ELP serves and owns the feature.
   */
  public void testThePluginsCounterpartAtItsEntryPoint() {
    for (Feature feature : Feature.values()) {
      for (int state = 0; state < 3; state++) {
        position(state == 0 ? ElpMode.PLUGIN : ElpMode.ELP);
        myClient.setState(state == 2 ? LspServerState.ShutdownUnexpectedly : LspServerState.Running);
        Boolean plugin = pluginAnswers(feature);
        if (plugin == null) continue;
        boolean expected = !(state == 1 && OWNER.get(feature) == Owner.ELP);
        assertEquals(feature + " in state " + state, expected, plugin.booleanValue());
      }
    }
  }

  /**
   * Whether the plugin's counterpart of {@code feature} answers, or {@code null} when the plugin has no switch for it: the platform
   * prefers ELP's document highlights over the plugin's references while a server runs, and ELP's workspace symbols stay switched
   * off in the LSP customization, so Go to Symbol keeps the plugin's contributors.
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
      case DOCUMENT_HIGHLIGHTS, WORKSPACE_SYMBOLS -> null;
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
