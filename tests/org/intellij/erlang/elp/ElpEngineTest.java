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
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInspection.LocalInspectionEP;
import com.intellij.lang.folding.FoldingDescriptor;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.refactoring.rename.RenameHandler;
import com.intellij.refactoring.rename.RenameHandlerRegistry;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.ServiceContainerUtil;
import com.intellij.testFramework.utils.parameterInfo.MockCreateParameterInfoContext;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangParameterInfoHandler;
import org.intellij.erlang.documentation.ErlangDocumentationProvider;
import org.intellij.erlang.editor.ErlangFoldingBuilder;
import org.intellij.erlang.editor.ErlangWordSelectioner;
import org.intellij.erlang.elp.lsp.ElpFindUsagesHandlerFactory;
import org.intellij.erlang.elp.lsp.ElpRenameHandler;
import org.intellij.erlang.hints.ErlangParameterHintsProvider;
import org.intellij.erlang.inspection.ErlangHeadMismatchInspection;
import org.intellij.erlang.inspection.ErlangUndefinedBehaviourInspection;
import org.intellij.erlang.inspection.ErlangUnresolvedFunctionInspection;
import org.intellij.erlang.inspection.ErlangUnusedFunctionInspection;
import org.intellij.erlang.intention.ErlangExportFunctionIntention;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Set;

/**
 * The engine switch with a stand-in for the running ELP server: {@link ElpEngine#ownsFeature} and the plugin features it gates.
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
    ServiceContainerUtil.registerOrReplaceServiceInstance(getProject(), ElpServerStatus.class, new ElpServerStatus() {
      @Override
      public boolean isServing(@NotNull VirtualFile file, @NotNull ElpEngine.Feature feature) {
        return myServing;
      }

      @Override
      public boolean isServingProject(@NotNull ElpEngine.Feature feature) {
        return myServing;
      }
    }, getTestRootDisposable());
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
    assertTrue(ElpEngine.ownsFeature(module, ElpEngine.Feature.DOCUMENTATION));
    assertTrue(ElpEngine.ownsFeature(header, ElpEngine.Feature.NAVIGATION));
    assertFalse("ELP does not get .app.src", ElpEngine.ownsFeature(appSrc, ElpEngine.Feature.DOCUMENTATION));
    assertFalse(ElpEngine.ownsFeature(null, ElpEngine.Feature.DOCUMENTATION));
    assertFalse(ElpEngine.ownsFeature(getProject(), null, ElpEngine.Feature.DOCUMENTATION));

    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertFalse("no running server, the plugin answers", ElpEngine.ownsFeature(module, ElpEngine.Feature.DOCUMENTATION));

    configure("/opt/elp/bin/elp", ElpMode.PLUGIN, true);
    assertFalse(ElpEngine.ownsFeature(module, ElpEngine.Feature.DOCUMENTATION));

    configure("", ElpMode.ELP, true);
    assertFalse("no path, no ELP", ElpEngine.ownsFeature(module, ElpEngine.Feature.DOCUMENTATION));
  }

  public void testModePropertyOverridesTheStoredPosition() {
    PsiFile module = myFixture.addFileToProject("src/a.erl", "-module(a).");
    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    System.setProperty(ElpConfiguration.MODE_PROPERTY, "plugin");
    assertFalse(ElpEngine.ownsFeature(module, ElpEngine.Feature.DOCUMENTATION));
  }

  public void testCompletionStaysWithThePlugin() {
    PsiFile module = myFixture.addFileToProject("src/a.erl", "-module(a).");
    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertFalse(ElpEngine.elpMayOwn(ElpEngine.Feature.COMPLETION));
    assertFalse(ElpEngine.ownsFeature(module, ElpEngine.Feature.COMPLETION));
    for (ElpEngine.Feature feature : ElpEngine.Feature.values()) {
      if (feature != ElpEngine.Feature.COMPLETION) assertTrue(feature.name(), ElpEngine.elpMayOwn(feature));
    }
  }

  public void testCompletion() {
    myFixture.configureByText("a.erl", "-module(a).\nbar1() -> ok.\nbar2() -> ok.\nfoo() -> bar<caret>");
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "bar1", "bar2");

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "bar1", "bar2");
  }

  public void testCompletionOfReferenceVariants() {
    // The platform also completes the variants of the reference at the caret, outside the plugin's contributors.
    myFixture.configureByText("a.erl", "-module(a).\n-define(MACRO1, 1).\n-define(MACRO2, 2).\n-record(rec1, {}).\n-record(rec2, {}).\n" +
                                       "bar1() -> ok.\nbar2() -> ok.\nfoo() -> bar<caret>().");
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "bar1", "bar2");

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "bar1", "bar2");
  }

  public void testCompletionOfMacroAndRecordVariants() {
    myFixture.configureByText("a.erl", "-module(a).\n-define(MACRO1, 1).\n-define(MACRO2, 2).\n-record(rec1, {}).\n-record(rec2, {}).\n" +
                                       "foo() -> {?MAC<caret>, #rec}.");
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "MACRO1", "MACRO2");
    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "MACRO1", "MACRO2");

    myFixture.getEditor().getCaretModel().moveToOffset(myFixture.getFile().getText().indexOf("#rec") + 4);
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "rec1", "rec2");
    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "rec1", "rec2");
  }

  public void testVariableCompletion() {
    myFixture.configureByText("a.erl", "-module(a).\nfoo(Alpha1, Alpha2) -> Al<caret>");
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "Alpha1", "Alpha2");

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "Alpha1", "Alpha2");
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
    assertEquals("the server stopped, the plugin's hints are back", 1, inlineInlays());
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
    assertNotNull("without the element in the editor the plugin answers", provider.generateDoc(function, null));
  }

  public void testExtendSelection() {
    myFixture.configureByText("a.erl", "-module(a).\nf() -> o<caret>k.");
    PsiElement element = myFixture.getFile().findElementAt(myFixture.getCaretOffset());
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertTrue(new ErlangWordSelectioner().canSelect(element));

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertFalse(new ErlangWordSelectioner().canSelect(element));
  }

  public void testInspectionsWithAnElpCounterpartAreSilentButOthersStay() {
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
    assertEquals("ELP reports nothing for behaviours, so the plugin keeps that class",
                 Set.of("Unresolved behaviour no_such_behaviour"), descriptions(myFixture.doHighlighting()));
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

  public void testFindUsagesFallsBackToThePluginWhenElpDoesNotAnswer() {
    myFixture.configureByText("a.erl", "-module(a).\nf<caret>oo() -> ok.\nbar() -> foo(), foo().");
    PsiElement target = ((ErlangFile)myFixture.getFile()).getFunction("foo", 0);
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertSize(2, myFixture.findUsages(target));

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertTrue(new ElpFindUsagesHandlerFactory().canFindUsages(target));
    assertSize(2, myFixture.findUsages(target));
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

  private static List<String> lookupStrings(LookupElement[] elements) {
    assertNotNull("one variant was inserted", elements);
    return ContainerUtil.map(elements, LookupElement::getLookupString);
  }

  private static Set<String> descriptions(List<HighlightInfo> infos) {
    return ContainerUtil.map2Set(ContainerUtil.filter(infos, info -> info.getDescription() != null), HighlightInfo::getDescription);
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
