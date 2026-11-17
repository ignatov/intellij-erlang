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

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.codeInsight.daemon.LineMarkerInfo;
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupManager;
import com.intellij.codeInsight.navigation.actions.GotoDeclarationAction;
import com.intellij.codeInspection.LocalInspectionEP;
import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.lang.folding.FoldingDescriptor;
import com.intellij.navigation.ChooseByNameContributor;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.actionSystem.impl.SimpleDataContext;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.newvfs.impl.VfsRootAccess;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.SyntaxTraverser;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.refactoring.rename.RenameHandler;
import com.intellij.refactoring.rename.RenameHandlerRegistry;
import com.intellij.testFramework.IndexingTestUtil;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.testFramework.ServiceContainerUtil;
import com.intellij.testFramework.utils.parameterInfo.MockCreateParameterInfoContext;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangParameterInfoHandler;
import org.intellij.erlang.completion.ErlangCompletionContributor;
import org.intellij.erlang.documentation.ErlangDocumentationProvider;
import org.intellij.erlang.editor.ErlangFoldingBuilder;
import org.intellij.erlang.editor.ErlangWordSelectioner;
import org.intellij.erlang.elp.lsp.ElpFindUsagesHandlerFactory;
import org.intellij.erlang.elp.lsp.ElpLspIntegrationProvider;
import org.intellij.erlang.elp.lsp.ElpRenameHandler;
import org.intellij.erlang.go.ErlangModuleContributor;
import org.intellij.erlang.go.ErlangSymbolContributor;
import org.intellij.erlang.hierarchy.ErlangCallHierarchyProvider;
import org.intellij.erlang.hints.ErlangParameterHintsProvider;
import org.intellij.erlang.inspection.ErlangHeadMismatchInspection;
import org.intellij.erlang.inspection.ErlangUndefinedBehaviourInspection;
import org.intellij.erlang.inspection.ErlangUnresolvedFunctionInspection;
import org.intellij.erlang.inspection.ErlangUnusedFunctionInspection;
import org.intellij.erlang.intention.ErlangExportFunctionIntention;
import org.intellij.erlang.marker.ErlangRecursiveCallLineMarkerProvider;
import org.intellij.erlang.navigation.ErlangGotoSuperHandler;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * The engine switch with the served-root registry and a stand-in for the LSP part: {@link ElpEngine#ownsFeature} and the plugin
 * features it gates. No ELP server runs here, as while ELP loads or after it crashed: under a served root the plugin is silent all
 * the same.
 */
public class ElpEngineTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private ElpSettings.ElpSettingsState myOldState;
  private String myOldPathProperty;
  private String myOldModeProperty;

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
    // The LSP part is there, but no server runs.
    ServiceContainerUtil.registerOrReplaceServiceInstance(getProject(), ElpServerStatus.class, new ElpServerStatus() {
      @Override
      public boolean isServing(@NotNull VirtualFile file, @NotNull ElpEngine.Feature feature) {
        return false;
      }

      @Override
      public boolean isServingProject(@NotNull ElpEngine.Feature feature) {
        return false;
      }
    }, getTestRootDisposable());
    ElpPreflight.trustInTests(true, getTestRootDisposable());
    ElpServedRoots.getInstance().setRootsForTests(List.of(), getTestRootDisposable());
    // No server is started here: the real provider would run the preflight on the opened files and change the registry.
    ElpLspIntegrationProvider.ignoreOpenedFilesInTests(getTestRootDisposable());
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

  public void testOwnsFeaturesOnlyInTheElpPositionUnderAServedRoot() {
    PsiFile module = myFixture.addFileToProject("src/a.erl", "-module(a).");
    PsiFile header = myFixture.addFileToProject("include/a.hrl", "");
    PsiFile appSrc = myFixture.addFileToProject("src/a.app.src", "{application, a, []}.");

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertTrue(ElpEngine.ownsFeature(module, ElpEngine.Feature.DOCUMENTATION));
    assertTrue(ElpEngine.ownsFeature(header, ElpEngine.Feature.NAVIGATION));
    assertFalse("ELP does not get .app.src", ElpEngine.ownsFeature(appSrc, ElpEngine.Feature.DOCUMENTATION));
    assertFalse(ElpEngine.ownsFeature(null, ElpEngine.Feature.DOCUMENTATION));
    assertFalse(ElpEngine.ownsFeature(getProject(), null, ElpEngine.Feature.DOCUMENTATION));

    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertFalse("a root that ELP does not serve, the plugin answers", ElpEngine.ownsFeature(module, ElpEngine.Feature.DOCUMENTATION));

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    ElpPreflight.trustInTests(false, getTestRootDisposable());
    assertFalse("an untrusted project, the plugin answers", ElpEngine.ownsFeature(module, ElpEngine.Feature.DOCUMENTATION));
    ElpPreflight.trustInTests(true, getTestRootDisposable());

    configure("/opt/elp/bin/elp", ElpMode.PLUGIN, true);
    assertFalse(ElpEngine.ownsFeature(module, ElpEngine.Feature.DOCUMENTATION));

    configure("", ElpMode.ELP, true);
    assertTrue("no path is the downloaded server, which serves here", ElpEngine.ownsFeature(module, ElpEngine.Feature.DOCUMENTATION));

    ElpSettings.getInstance().getState().source = ElpServerSource.CUSTOM.name();
    assertFalse("an empty custom path, no ELP", ElpEngine.ownsFeature(module, ElpEngine.Feature.DOCUMENTATION));
  }

  public void testModePropertyOverridesTheStoredPosition() {
    PsiFile module = myFixture.addFileToProject("src/a.erl", "-module(a).");
    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    System.setProperty(ElpConfiguration.MODE_PROPERTY, "plugin");
    assertFalse(ElpEngine.ownsFeature(module, ElpEngine.Feature.DOCUMENTATION));
  }

  public void testEveryFeatureIsElpsUnderAServedRoot() {
    PsiFile module = myFixture.addFileToProject("src/a.erl", "-module(a).");
    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    for (ElpEngine.Feature feature : ElpEngine.Feature.values()) {
      assertTrue(feature.name(), ElpEngine.ownsFeature(module, feature));
    }
  }

  public void testCompletion() {
    myFixture.configureByText("a.erl", "-module(a).\nbar1() -> ok.\nbar2() -> ok.\nfoo() -> bar<caret>");
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "bar1", "bar2");

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertDoesntContain(variants(), "bar1", "bar2");
  }

  /** Under a served root where ELP cannot complete, the empty lookup says why instead of "No suggestions". */
  public void testEmptyCompletionSaysWhyElpDoesNotComplete() {
    myFixture.configureByText("a.erl", "-module(a).\nfoo() -> ok.\nbar() -> fo<caret>");
    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertEmpty(myFixture.completeBasic());
    assertEquals(ElpServerStatus.NOT_RUNNING, emptyLookupMessage());

    configure("/opt/elp/bin/elp", ElpMode.PLUGIN, true);
    assertNull(emptyLookupMessage());
  }

  private String emptyLookupMessage() {
    PsiFile file = myFixture.getFile();
    int offset = myFixture.getCaretOffset();
    CompletionParameters parameters = new CompletionParameters(file.findElementAt(offset - 1), file, CompletionType.BASIC, offset, 1,
                                                               myFixture.getEditor(), () -> false);
    return new ErlangCompletionContributor().handleEmptyLookup(parameters, myFixture.getEditor());
  }

  public void testCompletionOfReferenceVariants() {
    // The platform also completes the variants of the reference at the caret, outside the plugin's contributors.
    myFixture.configureByText("a.erl", "-module(a).\n-define(MACRO1, 1).\n-define(MACRO2, 2).\n-record(rec1, {}).\n-record(rec2, {}).\n" +
                                       "bar1() -> ok.\nbar2() -> ok.\nfoo() -> bar<caret>().");
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "bar1", "bar2");

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertDoesntContain(variants(), "bar1", "bar2");
  }

  public void testCompletionOfMacroAndRecordVariants() {
    myFixture.configureByText("a.erl", "-module(a).\n-define(MACRO1, 1).\n-define(MACRO2, 2).\n-record(rec1, {}).\n-record(rec2, {}).\n" +
                                       "foo() -> {?MAC<caret>, #rec}.");
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "MACRO1", "MACRO2");
    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertDoesntContain(variants(), "MACRO1", "MACRO2");

    myFixture.getEditor().getCaretModel().moveToOffset(myFixture.getFile().getText().indexOf("#rec") + 4);
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "rec1", "rec2");
    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertDoesntContain(variants(), "rec1", "rec2");
  }

  public void testVariableCompletion() {
    myFixture.configureByText("a.erl", "-module(a).\nfoo(Alpha1, Alpha2) -> Al<caret>");
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "Alpha1", "Alpha2");

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertDoesntContain(variants(), "Alpha1", "Alpha2");
  }

  public void testParameterHints() {
    myFixture.configureByText("a.erl", "-module(a).\nf(Name) -> Name.\ng() -> f(1).");
    ErlangParameterHintsProvider provider = new ErlangParameterHintsProvider();
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertNotNull(provider.createCollector(myFixture.getFile(), myFixture.getEditor()));

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertNull(provider.createCollector(myFixture.getFile(), myFixture.getEditor()));
  }

  public void testRefreshRecomputesTheParameterHintsOfOpenEditors() {
    myFixture.configureByText("a.erl", "-module(a).\nf(Name) -> Name.\ng() -> f(1).");
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    myFixture.doHighlighting();
    assertEquals(1, inlineInlays());

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    ElpEngine.refreshProject(getProject());
    PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
    myFixture.doHighlighting();
    assertEquals("ELP owns the hints now", 0, inlineInlays());

    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    ElpEngine.refreshProject(getProject());
    PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
    myFixture.doHighlighting();
    assertEquals("the root is not served any more, the plugin's hints are back", 1, inlineInlays());
  }

  private int inlineInlays() {
    return myFixture.getEditor().getInlayModel().getInlineElementsInRange(0, myFixture.getEditor().getDocument().getTextLength()).size();
  }

  public void testFunctionFoldingButNotCommentFolding() {
    myFixture.configureByText("a.erl", "-module(a).\n%% a comment\nf() ->\n  ok.\n");
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertEquals(2, foldingRegions().length);

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    FoldingDescriptor[] regions = foldingRegions();
    assertEquals(1, regions.length);
    assertEquals("%% a comment", regions[0].getElement().getText());
  }

  public void testExportFunctionIntention() {
    myFixture.configureByText("a.erl", "-module(a).\nf<caret>oo() -> ok.");
    ErlangExportFunctionIntention intention = new ErlangExportFunctionIntention();
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertTrue(intention.isAvailable(getProject(), myFixture.getEditor(), myFixture.getFile()));

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertFalse(intention.isAvailable(getProject(), myFixture.getEditor(), myFixture.getFile()));
  }

  public void testDocumentation() {
    myFixture.configureByText("a.erl", "-module(a).\n%% @doc Says hello.\nhello() -> ok.\nf() -> hel<caret>lo().");
    ErlangFunction function = ((ErlangFile)myFixture.getFile()).getFunction("hello", 0);
    PsiElement original = myFixture.getFile().findElementAt(myFixture.getCaretOffset());
    ErlangDocumentationProvider provider = new ErlangDocumentationProvider();
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertNotNull(provider.generateDoc(function, original));

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertNull(provider.generateDoc(function, original));
    assertNull("without the element in the editor the target's file decides", provider.generateDoc(function, null));
  }

  public void testThePluginDocumentsNoCompletionItemUnderAServedRoot() {
    myFixture.configureByText("a.erl", "-module(a).\n%% @doc Says hello.\nhello1() -> ok.\nhello2() -> ok.\nf() -> hel<caret>");
    ErlangFunction function = ((ErlangFile)myFixture.getFile()).getFunction("hello1", 0);
    ErlangDocumentationProvider provider = new ErlangDocumentationProvider();
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "hello1", "hello2");
    // The platform passes the element at the caret as the original element of a lookup item too.
    PsiElement original = myFixture.getFile().findElementAt(myFixture.getCaretOffset() - 1);
    assertNotNull(provider.generateDoc(function, original));

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertNotNull(LookupManager.getInstance(getProject()).getActiveLookup());
    assertNull("ELP's items carry their own documentation", provider.generateDoc(function, original));
  }

  public void testExtendSelection() {
    myFixture.configureByText("a.erl", "-module(a).\nf() -> o<caret>k.");
    PsiElement element = myFixture.getFile().findElementAt(myFixture.getCaretOffset());
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertTrue(new ErlangWordSelectioner().canSelect(element));

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertFalse(new ErlangWordSelectioner().canSelect(element));
  }

  public void testEveryInspectionIsSilentUnderAServedRoot() {
    myFixture.enableInspections(ErlangUnresolvedFunctionInspection.class, ErlangUnusedFunctionInspection.class,
                                ErlangHeadMismatchInspection.class, ErlangUndefinedBehaviourInspection.class);
    myFixture.configureByText("a.erl", "-module(a).\n-behaviour(no_such_behaviour).\n-export([f/0]).\nf() -> undefined_fun().\n" +
                                       "unused() -> ok.\ng(1) -> ok;\nh(2) -> ok.\n");
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertEquals(Set.of("Unresolved function undefined_fun/0", "Unused function 'unused/0'", "Unused function 'g/1'",
                        "Head mismatch: should be 'g/1'", "Unresolved behaviour no_such_behaviour"),
                 descriptions(myFixture.doHighlighting()));

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    DaemonCodeAnalyzer.getInstance(getProject()).restart(myFixture.getFile(), "ELP serves the file now");
    assertEquals("also the behaviour inspection, which has no ELP counterpart", Set.of(), descriptions(myFixture.doHighlighting()));
  }

  public void testCounterpartTableCoversOnlyRegisteredInspections() {
    Set<Class<?>> registered = ContainerUtil.map2Set(
      LocalInspectionEP.LOCAL_INSPECTION.getExtensionList(),
      ep -> ep.getInstance().getClass());
    for (var entry : ElpDiagnosticCounterparts.TABLE.entrySet()) {
      assertTrue(entry.getKey().getSimpleName(), registered.contains(entry.getKey()));
      assertNotEmpty(entry.getValue().elpCodes());
      for (String code : entry.getValue().elpCodes()) assertTrue(code, code.matches("[LEPW]\\d{4}"));
    }
    assertFalse(ElpDiagnosticCounterparts.hasElpCounterpart(ErlangUndefinedBehaviourInspection.class));
  }

  public void testRunWithPluginSuspendsOwnershipOnThisThread() {
    PsiFile module = myFixture.addFileToProject("src/a.erl", "-module(a).");
    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    ElpEngine.runWithPlugin(ElpEngine.Feature.RENAME, () -> {
      assertFalse(ElpEngine.ownsFeature(module, ElpEngine.Feature.RENAME));
      assertTrue(ElpEngine.ownsFeature(module, ElpEngine.Feature.DOCUMENTATION));
    });
    assertTrue(ElpEngine.ownsFeature(module, ElpEngine.Feature.RENAME));
  }

  public void testRenameHasOneHandlerInEachPosition() {
    myFixture.configureByText("a.erl", "-module(a).\nf<caret>oo() -> ok.\nbar() -> foo().");
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    List<? extends RenameHandler> pluginHandlers = RenameHandlerRegistry.getInstance().getRenameHandlers(dataContext());
    assertNotEmpty(pluginHandlers);
    assertFalse(ContainerUtil.exists(pluginHandlers, handler -> handler instanceof ElpRenameHandler));

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    List<? extends RenameHandler> elpHandlers = RenameHandlerRegistry.getInstance().getRenameHandlers(dataContext());
    assertSize(1, elpHandlers);
    assertInstanceOf(elpHandlers.getFirst(), ElpRenameHandler.class);

    ElpEngine.runWithPlugin(ElpEngine.Feature.RENAME, () -> assertEquals(
      ContainerUtil.map(pluginHandlers, Object::getClass),
      ContainerUtil.map(RenameHandlerRegistry.getInstance().getRenameHandlers(dataContext()), Object::getClass)));
  }

  public void testFindUsagesDoesNotFallBackToThePluginWhenElpDoesNotAnswer() {
    myFixture.configureByText("a.erl", "-module(a).\nf<caret>oo() -> ok.\nbar() -> foo(), foo().");
    PsiElement target = ((ErlangFile)myFixture.getFile()).getFunction("foo", 0);
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertSize(2, myFixture.findUsages(target));

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    ElpFindUsagesHandlerFactory factory = new ElpFindUsagesHandlerFactory();
    assertTrue(factory.canFindUsages(target));
    assertEmpty("no server answers, and the plugin does not search", myFixture.findUsages(target));
    FindUsagesHandler highlighting = factory.createFindUsagesHandler(target, true);
    assertNotNull(highlighting);
    assertEmpty("ELP's document highlights, not the plugin's references",
                highlighting.findReferencesToHighlight(target, GlobalSearchScope.fileScope(myFixture.getFile())));
  }

  /** Go to declaration never falls back to the plugin's references: with no answer from ELP it goes nowhere. */
  public void testNavigationDoesNotFallBackToThePluginsReferences() {
    myFixture.configureByText("a.erl", "-module(a).\nfoo() -> ok.\nbar() -> f<caret>oo().");
    int offset = myFixture.getCaretOffset();
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    PsiElement[] plugin = GotoDeclarationAction.findAllTargetElements(getProject(), myFixture.getEditor(), offset);
    assertInstanceOf(assertOneElement(plugin), ErlangFunction.class);

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    PsiElement[] elp = GotoDeclarationAction.findAllTargetElements(getProject(), myFixture.getEditor(), offset);
    assertEquals("the element at the caret", myFixture.getFile().findElementAt(offset), assertOneElement(elp));
  }

  /** Call hierarchy, Go to Super and the gutter markers are computed from the plugin's resolve: off under a served root. */
  public void testResolveBasedNavigationIsOffUnderAServedRoot() {
    myFixture.configureByText("a.erl", "-module(a).\nf<caret>oo() -> foo().");
    ErlangFunction function = ((ErlangFile)myFixture.getFile()).getFunction("foo", 0);
    DataContext context = SimpleDataContext.builder().add(CommonDataKeys.PSI_ELEMENT, function)
      .add(CommonDataKeys.PSI_FILE, myFixture.getFile()).build();
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertNotNull(new ErlangCallHierarchyProvider().getTarget(context));
    assertTrue(new ErlangGotoSuperHandler().isValidFor(myFixture.getEditor(), myFixture.getFile()));
    assertNotEmpty(recursionMarkers());

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertNull(new ErlangCallHierarchyProvider().getTarget(context));
    assertFalse(new ErlangGotoSuperHandler().isValidFor(myFixture.getEditor(), myFixture.getFile()));
    assertEmpty(recursionMarkers());
  }

  private List<LineMarkerInfo<?>> recursionMarkers() {
    List<PsiElement> elements = SyntaxTraverser.psiTraverser(myFixture.getFile()).toList();
    List<LineMarkerInfo<?>> markers = new ArrayList<>();
    new ErlangRecursiveCallLineMarkerProvider().collectSlowLineMarkers(elements, markers);
    return markers;
  }

  /**
   * Go to Symbol: the plugin's contributor is silent in a project whose roots ELP serves all, and in a project with roots that ELP
   * does not serve it leaves out only the files under served roots. A served root is a workspace root: here the content root of the
   * test project and another content root. Go to Class: the plugin's modules everywhere, because the platform does not take ELP's
   * modules for Go to Class; Go to Symbol does not ask for them, so ELP's module is not listed twice there.
   */
  public void testSymbolContributorsLeaveServedFilesToElp() throws Exception {
    myFixture.addFileToProject("served/served_mod.erl", "-module(served_mod).\nserved_fun() -> ok.");
    Path otherDir = FileUtil.createTempDirectory("elp-other-root", null, true).toPath();
    Files.writeString(otherDir.resolve("other_mod.erl"), "-module(other_mod).\nother_fun() -> ok.");
    VfsRootAccess.allowRootAccess(getTestRootDisposable(), otherDir.toString());
    VirtualFile otherRoot = LocalFileSystem.getInstance().refreshAndFindFileByNioFile(otherDir);
    assertNotNull(otherRoot);
    PsiTestUtil.addSourceContentToRoots(getModule(), otherRoot);
    try {
      IndexingTestUtil.waitUntilIndexesAreReady(getProject());
      String root = contentRoot();
      ErlangSymbolContributor symbols = new ErlangSymbolContributor();
      ErlangModuleContributor modules = new ErlangModuleContributor();
      configure("/opt/elp/bin/elp", ElpMode.ELP, false);
      assertContainsElements(List.of(symbols.getNames(getProject(), false)), "served_fun", "other_fun");
      assertSize(1, modules.getItemsByName("served_mod", "served_mod", getProject(), false));

      ElpServedRoots.getInstance().setRootsForTests(List.of(root), getTestRootDisposable());
      assertFalse(ElpEngine.servesWholeProject(getProject()));
      assertSize(1, modules.getItemsByName("served_mod", "served_mod", getProject(), false));
      assertSize(1, modules.getItemsByName("other_mod", "other_mod", getProject(), false));
      assertEmpty(symbols.getItemsByName("served_fun", "served_fun", getProject(), false));
      assertSize(1, symbols.getItemsByName("other_fun", "other_fun", getProject(), false));

      ElpServedRoots.getInstance().setRootsForTests(List.of(root, otherRoot.getPath()), getTestRootDisposable());
      assertTrue(ElpEngine.servesWholeProject(getProject()));
      assertEmpty(symbols.getNames(getProject(), true));
      assertContainsElements(List.of(modules.getNames(getProject(), true)), "served_mod", "other_mod");
      assertSize(1, modules.getItemsByName("other_mod", "other_mod", getProject(), true));
      // No ELP server runs in this test.
      assertSize(1, modules.getItemsByName("served_mod", "served_mod", getProject(), true));
      assertFalse("Go to Symbol does not ask for the modules",
                  ContainerUtil.exists(ChooseByNameContributor.SYMBOL_EP_NAME.getExtensionList(), c -> c instanceof ErlangModuleContributor));
      assertTrue(ContainerUtil.exists(ChooseByNameContributor.CLASS_EP_NAME.getExtensionList(), c -> c instanceof ErlangModuleContributor));
    }
    finally {
      PsiTestUtil.removeContentEntry(getModule(), otherRoot);
    }
  }

  /**
   * The guard of the roots that ELP does not serve: in the ELP position, with the root left out of the registry as after a refused
   * preflight, completion, go to declaration and the inspections answer exactly as in the Plugin position.
   */
  public void testARootThatElpDoesNotServeBehavesAsThePluginPosition() {
    myFixture.enableInspections(ErlangUnresolvedFunctionInspection.class, ErlangUnusedFunctionInspection.class,
                                ErlangUndefinedBehaviourInspection.class);
    String text = "-module(a).\n-behaviour(no_such_behaviour).\nbar1() -> ok.\nbar2() -> undefined_fun().\nfoo() -> bar1(), bar<caret>";
    myFixture.configureByText("a.erl", text);
    int call = text.indexOf("bar1(), ") + 1;
    configure("/opt/elp/bin/elp", ElpMode.PLUGIN, false);
    List<String> pluginCompletion = variants();
    Set<String> pluginProblems = descriptions(myFixture.doHighlighting());
    PsiElement[] pluginTargets = GotoDeclarationAction.findAllTargetElements(getProject(), myFixture.getEditor(), call);
    assertContainsElements(pluginCompletion, "bar1", "bar2");
    assertNotEmpty(pluginProblems);
    assertInstanceOf(assertOneElement(pluginTargets), ErlangFunction.class);

    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    LookupManager.getInstance(getProject()).hideActiveLookup();
    DaemonCodeAnalyzer.getInstance(getProject()).restart(myFixture.getFile(), "the ELP position");
    assertEquals(pluginCompletion, variants());
    assertEquals(pluginProblems, descriptions(myFixture.doHighlighting()));
    assertEquals(List.of(pluginTargets), List.of(GotoDeclarationAction.findAllTargetElements(getProject(), myFixture.getEditor(), call)));
  }

  public void testParameterInfo() {
    myFixture.configureByText("a.erl", "-module(a).\nf(Name) -> Name.\ng() -> f(1).");
    int argument = myFixture.getFile().getText().indexOf("f(1)") + 2;
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertNotNull(parameterInfoAt(argument));

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertNull(parameterInfoAt(argument));
  }

  private Object parameterInfoAt(int offset) {
    myFixture.getEditor().getCaretModel().moveToOffset(offset);
    return new ErlangParameterInfoHandler().findElementForParameterInfo(
      new MockCreateParameterInfoContext(myFixture.getEditor(), myFixture.getFile()));
  }

  private DataContext dataContext() {
    return ((EditorEx)myFixture.getEditor()).getDataContext();
  }

  private FoldingDescriptor[] foldingRegions() {
    return new ErlangFoldingBuilder().buildFoldRegions(myFixture.getFile(), myFixture.getEditor().getDocument(), false);
  }

  /** The completion variants at the caret; a single variant is inserted, which shows as a marker. */
  private List<String> variants() {
    LookupElement[] elements = myFixture.completeBasic();
    if (elements == null) return List.of("<a single variant was inserted>");
    List<String> strings = ContainerUtil.map(elements, LookupElement::getLookupString);
    LookupManager.getInstance(getProject()).hideActiveLookup();
    return strings;
  }

  private String contentRoot() {
    return ModuleRootManager.getInstance(getModule()).getContentRoots()[0].getPath();
  }

  private static List<String> lookupStrings(LookupElement[] elements) {
    assertNotNull("one variant was inserted", elements);
    return ContainerUtil.map(elements, LookupElement::getLookupString);
  }

  private static Set<String> descriptions(List<HighlightInfo> infos) {
    return ContainerUtil.map2Set(ContainerUtil.filter(infos, info -> info.getDescription() != null), HighlightInfo::getDescription);
  }

  /** Chooses the position; {@code served} puts the project's content root into the served-root registry, or takes it out. */
  private void configure(String path, ElpMode mode, boolean served) {
    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    state.path = path;
    state.mode = mode.name();
    ElpSettings.getInstance().loadState(state);
    ElpServedRoots.getInstance().setRootsForTests(served ? List.of(contentRoot()) : List.of(), getTestRootDisposable());
  }

  private static void restore(String key, String value) {
    if (value == null) System.clearProperty(key);
    else System.setProperty(key, value);
  }
}
