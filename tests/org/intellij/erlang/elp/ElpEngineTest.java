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
import com.intellij.lang.folding.FoldingDescriptor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.ServiceContainerUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.documentation.ErlangDocumentationProvider;
import org.intellij.erlang.editor.ErlangFoldingBuilder;
import org.intellij.erlang.editor.ErlangWordSelectioner;
import org.intellij.erlang.hints.ErlangParameterHintsProvider;
import org.intellij.erlang.inspection.ErlangHeadMismatchInspection;
import org.intellij.erlang.inspection.ErlangUnresolvedFunctionInspection;
import org.intellij.erlang.inspection.ErlangUnusedFunctionInspection;
import org.intellij.erlang.intention.ErlangExportFunctionIntention;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

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

  public void testCompletion() {
    myFixture.configureByText("a.erl", "-module(a).\nbar1() -> ok.\nbar2() -> ok.\nfoo() -> bar<caret>");
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "bar1", "bar2");

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertEmpty(lookupStrings(myFixture.completeBasic()));
  }

  public void testVariableCompletion() {
    myFixture.configureByText("a.erl", "-module(a).\nfoo(Alpha1, Alpha2) -> Al<caret>");
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertContainsElements(lookupStrings(myFixture.completeBasic()), "Alpha1", "Alpha2");

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertEmpty(lookupStrings(myFixture.completeBasic()));
  }

  public void testParameterHints() {
    myFixture.configureByText("a.erl", "-module(a).\nf(Name) -> Name.\ng() -> f(1).");
    ErlangParameterHintsProvider provider = new ErlangParameterHintsProvider();
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertNotNull(provider.createCollector(myFixture.getFile(), myFixture.getEditor()));

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    assertNull(provider.createCollector(myFixture.getFile(), myFixture.getEditor()));
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

  public void testResolveInspectionsAreSilentButOthersStay() {
    myFixture.enableInspections(ErlangUnresolvedFunctionInspection.class, ErlangUnusedFunctionInspection.class,
                                ErlangHeadMismatchInspection.class);
    myFixture.configureByText("a.erl", "-module(a).\n-export([f/0]).\nf() -> undefined_fun().\nunused() -> ok.\n" +
                                       "g(1) -> ok;\nh(2) -> ok.\n");
    configure("/opt/elp/bin/elp", ElpMode.ELP, false);
    assertEquals(Set.of("Unresolved function undefined_fun/0", "Unused function 'unused/0'", "Unused function 'g/1'",
                        "Head mismatch: should be 'g/1'"),
                 descriptions(myFixture.doHighlighting()));

    configure("/opt/elp/bin/elp", ElpMode.ELP, true);
    DaemonCodeAnalyzer.getInstance(getProject()).restart(myFixture.getFile(), "ELP serves the file now");
    assertEquals(Set.of("Head mismatch: should be 'g/1'"), descriptions(myFixture.doHighlighting()));
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
