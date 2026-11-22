package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.descriptor;
import static org.intellij.erlang.macrolayer.LayerTestSupport.enableAll;
import static org.intellij.erlang.macrolayer.LayerTestSupport.read;

/**
 * The highlighting after an edit equals the highlighting of a fresh project with the same texts (fuzz finding F8):
 * an edit that changes what the layer sees (an include line, a header's macro) changes its answers anywhere in the
 * file, not only in the edited form, so no result of an earlier pass may be kept for another form.
 */
public class ErlangMacroLayerTypedStateTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String HEADER = "-define(HH2(X), when is_integer(X)).\n";
  private static final String BEFORE = "-module(m).\n-include(\"h2.hrl\").\n-export([guarded3/1]).\nguarded3(X) ?HH2(X) -> X.\n";
  private static final String AFTER = "-module(m).\n%%-include(\"h2.hrl\").\n-export([guarded3/1]).\nguarded3(X) ?HH2(X) -> X.\n";

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    ErlangMacroLayer.setQuietMillisForTests(0, getTestRootDisposable());
    enableAll(myFixture);
  }

  private List<String> highlights(PsiFile f) {
    List<String> r = new ArrayList<>();
    for (HighlightInfo h : myFixture.doHighlighting()) {
      if (h.getSeverity().compareTo(HighlightSeverity.WEAK_WARNING) >= 0 && h.getDescription() != null) {
        r.add(h.getStartOffset() + "-" + h.getEndOffset() + " " + h.getDescription());
      }
    }
    Collections.sort(r);
    return r;
  }

  private List<String> fresh(String dir, String text) {
    myFixture.addFileToProject(dir + "/h2.hrl", HEADER);
    PsiFile f = myFixture.addFileToProject(dir + "/m.erl", text);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    return highlights(f);
  }

  public void testAnIncludeLineBrokenByTyping() {
    myFixture.addFileToProject("typed/h2.hrl", HEADER);
    PsiFile f = myFixture.addFileToProject("typed/m.erl", BEFORE);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    highlights(f);
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(f);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getText().indexOf("-include"), "%%"));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    assertEquals(AFTER, d.getText());
    List<String> typed = highlights(f);
    assertEquals("the typed state equals a fresh project", fresh("fresh", AFTER), typed);
  }

  public void testAnIncludeLineRepairedByTyping() {
    myFixture.addFileToProject("typed2/h2.hrl", HEADER);
    PsiFile f = myFixture.addFileToProject("typed2/m.erl", AFTER);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    highlights(f);
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(f);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> {
      int at = d.getText().indexOf("%%-include");
      d.deleteString(at, at + 2);
    });
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    assertEquals(BEFORE, d.getText());
    List<String> typed = highlights(f);
    assertEquals("the typed state equals a fresh project", fresh("fresh2", BEFORE), typed);
  }

  /**
   * F8: a "function" whose name is a macro use ({@code guarded3(X) ?HH2(X) -> X.} parses as a function named ?HH2) is
   * not reported as unused: it is the parser's recovery, not a function of the source. Its warning sat on the same
   * range as "Unresolved macros '?HH2'" once the include broke, so which one stayed depended on the order of passes.
   */
  public void testNoUnusedFunctionNamedByAMacroUse() {
    myFixture.addFileToProject("named/h2.hrl", HEADER);
    PsiFile f = myFixture.addFileToProject("named/m.erl", BEFORE);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    for (String h : highlights(f)) assertFalse(h, h.contains("Unused function '?HH2/1'"));
  }

  /**
   * #1486: after {@code setup?PAR,} the recovery makes {@code hello().} a function; the export goes to the one that
   * {@code ?DEF(hello).} makes, and the call is not reported as an unused function.
   */
  public void testNoUnusedFunctionForTheRestOfASplitForm() throws Exception {
    PsiFile f = myFixture.addFileToProject("greet/greet.erl", read(Paths.get("testData/macroLayer/nav", "greet.erl")));
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    for (String h : highlights(f)) assertFalse(h, h.contains("Unused function"));
  }
}
