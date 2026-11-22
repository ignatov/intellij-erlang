package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * D2: module-prefix fragments, which erlc compiles: {@code lists?COLON reverse(L)} with {@code -define(COLON, :).} and
 * {@code ?LISTS reverse(L)} with {@code -define(LISTS, lists:).} (the owner's samples gaps/gap_module_prefix.erl and
 * one_file/m_fragments.erl, here with a project module mylists). The main tree's recovery split the clause, so the
 * piece {@code reverse(L).} kept a false error and its call resolved to nothing. No false error; the function name
 * and the module name resolve to what the expanded code means.
 */
public class ErlangMacroLayerModulePrefixTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    myFixture.addFileToProject("p/mylists.erl", "-module(mylists).\n-export([reverse/1]).\nreverse(L) -> L.\n");
  }

  private PsiFile open(String name, String text) {
    PsiFile f = myFixture.addFileToProject("p/" + name, text);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    List<String> errors = new ArrayList<>();
    for (HighlightInfo h : myFixture.doHighlighting()) {
      if (h.getSeverity() == HighlightSeverity.ERROR) errors.add(h.getDescription());
    }
    assertEquals(name + ": no false error", List.of(), errors);
    return f;
  }

  private static String target(PsiFile f, String marker) throws Exception {
    int at = f.getText().indexOf(marker.replace("|", "")) + marker.indexOf('|');
    return inBackgroundWithIndicator(() -> {
      PsiReference ref = f.findReferenceAt(at);
      PsiElement t = ref == null ? null : ref.resolve();
      if (t instanceof ErlangFunction fn) return fn.getContainingFile().getName() + ":" + fn.getName() + "/" + fn.getArity();
      if (t instanceof PsiFile file) return file.getName();
      if (t != null && t.getContainingFile() != null) return t.getContainingFile().getName() + ":" + t.getText();
      return String.valueOf(t);
    });
  }

  public void testColonInAMacro() throws Exception {
    PsiFile f = open("gmp.erl", "-module(gmp).\n-export([rev/1]).\n-define(COLON, :).\nrev(L) ->\n  mylists?COLON reverse(L).\n");
    assertEquals("mylists.erl:reverse/1", target(f, "rev|erse(L)"));
    assertTrue(target(f, "myl|ists?COLON"), target(f, "myl|ists?COLON").startsWith("mylists.erl"));
    ErlangFunction reverse = inBackgroundWithIndicator(
      () -> ((ErlangFile)myFixture.getPsiManager().findFile(myFixture.findFileInTempDir("p/mylists.erl"))).getFunction("reverse", 1));
    int uses = inBackgroundWithIndicator(() -> (int)ReferencesSearch.search(reverse).findAll().stream()
      .filter(r -> r.getElement().getContainingFile() == f).count());
    assertEquals("find usages of mylists:reverse/1 lists the use", 1, uses);
  }

  public void testModuleAndColonInAMacro() throws Exception {
    PsiFile f = open("gml.erl", "-module(gml).\n-export([prefix/1]).\n-define(LISTS, mylists:).\nprefix(L) ->\n  ?LISTS reverse(L).\n");
    assertEquals("mylists.erl:reverse/1", target(f, "rev|erse(L)"));
  }

  public void testCompletionAfterAModulePrefixMacro() {
    open("gmc.erl", "-module(gmc).\n-export([prefix/1]).\n-define(LISTS, mylists:).\nprefix(L) ->\n  ?LISTS reverse(L).\n");
    myFixture.getEditor().getCaretModel().moveToOffset(myFixture.getFile().getText().indexOf("reverse(L)") + 3);
    LookupElement[] items = myFixture.complete(com.intellij.codeInsight.completion.CompletionType.BASIC);
    List<String> names = new ArrayList<>();
    if (items != null) for (LookupElement e : items) names.add(e.getLookupString());
    else names.add(myFixture.getEditor().getDocument().getText().contains("reverse") ? "reverse" : "");
    assertTrue(names.toString(), names.stream().anyMatch(n -> n.startsWith("reverse")));
  }
}
