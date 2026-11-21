package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.CodeInsightSettings;
import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.*;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 6 (next), item 2: completion of ordinary names after a fragment use in the same clause. The main tree's error
 * recovery after {@code foo?PAR} puts the rest of the clause into dummy blocks, so the providers find nothing there;
 * the layer runs them in the expanded copy. Each case is compared with its control: the same text with every macro use
 * replaced by its body (the text erlc compiles). The case must offer every item that the control offers for the prefix.
 */
public class ErlangMacroLayerCompletionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private boolean myAutocomplete;
  private int myN;

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    ErlangMacroLayerLog.enableForTests(true);
    ErlangMacroLayerLog.reset();
    myAutocomplete = CodeInsightSettings.getInstance().AUTOCOMPLETE_ON_CODE_COMPLETION;
    CodeInsightSettings.getInstance().AUTOCOMPLETE_ON_CODE_COMPLETION = false;
    myFixture.addFileToProject("c/mylists.erl", "-module(mylists).\n-export([reverse/1, reverse/2, map/2]).\n" +
                                                "reverse(L) -> L.\nreverse(L, T) -> L ++ T.\nmap(F, L) -> [F(X) || X <- L].\n");
    myFixture.addFileToProject("c/h.hrl", "-define(HPAR, ()).\n");
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      CodeInsightSettings.getInstance().AUTOCOMPLETE_ON_CODE_COMPLETION = myAutocomplete;
      ErlangMacroLayerLog.enableForTests(false);
    }
    finally {
      super.tearDown();
    }
  }

  /** The lookup strings offered at {@code <caret>}, after the file is highlighted (in tests completion runs on the UI thread). */
  private List<String> complete(String text) {
    int caret = text.indexOf("<caret>");
    assertTrue(caret >= 0);
    String body = text.replace("<caret>", "");
    String module = "m" + (myN++);
    body = body.replace("-module(m).", "-module(" + module + ").");
    PsiFile f = myFixture.addFileToProject("c/" + module + ".erl", body);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    myFixture.getEditor().getCaretModel().moveToOffset(caret + ("-module(" + module + ").").length() - "-module(m).".length());
    LookupElement[] items = myFixture.complete(CompletionType.BASIC);
    List<String> r = new ArrayList<>();
    if (items != null) {
      for (LookupElement e : items) {
        r.add(e.getLookupString());
        PsiElement psi = e.getPsiElement();
        if (psi != null) {
          PsiFile pf = psi.getContainingFile();
          assertNull("a lookup element's target is in an expanded copy: " + e + " " + psi, pf == null ? null : pf.getUserData(ErlangMacroLayer.COPY_OF));
        }
      }
    }
    myFixture.getLookup();
    if (myFixture.getLookup() != null) myFixture.getLookup().hideLookup(true);
    return r;
  }

  private void check(String what, String withMacro, String control, String... expected) {
    int taken0 = takenOver();
    List<String> got = complete(withMacro);
    int taken = takenOver() - taken0;
    List<String> want = complete(control);
    System.out.println("LAYER-COMPLETION " + what + ": with the macro " + got + " (through the copy: " + taken + "); control " + want + " events " + ErlangMacroLayerLog.events().stream().filter(e -> e.kind().equals("COMPLETION")).map(e -> e.reason()).toList());
    for (String e : expected) assertTrue(what + ": the control offers " + e + ": " + want, want.contains(e));
    List<String> missing = new ArrayList<>(want);
    missing.removeAll(got);
    assertTrue(what + ": not offered with the macro: " + missing + "; offered " + got, missing.isEmpty());
  }

  /** Completions answered through the copy (by reflection, so that the test also runs on a build without the contributor). */
  private static int takenOver() {
    try {
      java.lang.reflect.Field f = Class.forName("org.intellij.erlang.macrolayer.ErlangMacroLayerCompletionContributor").getDeclaredField("TAKEN_OVER");
      f.setAccessible(true);
      return ((Number)f.get(null)).intValue();
    }
    catch (ReflectiveOperationException e) {
      return 0;
    }
  }

  private static final String HEAD = "-module(m).\n-export([f/2]).\n-define(PAR, ()).\n-define(ID(X), X).\n-define(SEP, ;).\n" +
                                     "-record(rec_one, {field_a, field_b}).\n-include(\"h.hrl\").\nfoo() -> ok.\nhello_world() -> ok.\n";

  public void testVariable() {
    check("variable", HEAD + "f(Abc1, Abc2) ->\n  foo?PAR,\n  X1 = Ab<caret>,\n  X1.\n",
          HEAD + "f(Abc1, Abc2) ->\n  foo(),\n  X1 = Ab<caret>,\n  X1.\n", "Abc1", "Abc2");
  }

  public void testLocalFunction() {
    check("local function", HEAD + "f(A, _) ->\n  foo?PAR,\n  X1 = hel<caret>,\n  {A, X1}.\n",
          HEAD + "f(A, _) ->\n  foo(),\n  X1 = hel<caret>,\n  {A, X1}.\n", "hello_world");
  }

  public void testRecordName() {
    check("record name", HEAD + "f(A, _) ->\n  foo?PAR,\n  X1 = #rec_<caret>,\n  {A, X1}.\n",
          HEAD + "f(A, _) ->\n  foo(),\n  X1 = #rec_<caret>,\n  {A, X1}.\n", "rec_one");
  }

  public void testRecordField() {
    check("record field", HEAD + "f(R, _) ->\n  foo?PAR,\n  X1 = R#rec_one{fie<caret>},\n  X1.\n",
          HEAD + "f(R, _) ->\n  foo(),\n  X1 = R#rec_one{fie<caret>},\n  X1.\n", "field_a", "field_b");
  }

  /** 3e (from the real IDE): a field access after a fragment use. */
  public void testRecordFieldAccess() {
    check("record field access", HEAD + "f(R, _) ->\n  foo?PAR,\n  X1 = R#rec_one.fie<caret>,\n  X1.\n",
          HEAD + "f(R, _) ->\n  foo(),\n  X1 = R#rec_one.fie<caret>,\n  X1.\n", "field_a", "field_b");
  }

  /** 3e: a record being typed, its brace not closed yet. */
  public void testRecordFieldInAnUnclosedRecord() {
    check("record field, brace not closed", HEAD + "f(_, _) ->\n  foo?PAR,\n  X1 = #rec_one{fie<caret>\n  X1.\n",
          HEAD + "f(_, _) ->\n  foo(),\n  X1 = #rec_one{fie<caret>\n  X1.\n", "field_a", "field_b");
    check("record update, brace not closed", HEAD + "f(R, _) ->\n  foo?PAR,\n  X1 = R#rec_one{fie<caret>\n  X1.\n",
          HEAD + "f(R, _) ->\n  foo(),\n  X1 = R#rec_one{fie<caret>\n  X1.\n", "field_a", "field_b");
  }

  public void testRemoteFunction() {
    check("module:function", HEAD + "f(L, _) ->\n  foo?PAR,\n  X1 = mylists:rev<caret>,\n  X1.\n",
          HEAD + "f(L, _) ->\n  foo(),\n  X1 = mylists:rev<caret>,\n  X1.\n", "reverse");
  }

  public void testAfterASeparatorMacroBetweenClauses() {
    check("after a separator macro", HEAD + "f(1, _) -> a ?SEP\nf(Abc1, Abc2) -> Ab<caret>.\n",
          HEAD + "f(1, _) -> a ;\nf(Abc1, Abc2) -> Ab<caret>.\n", "Abc1", "Abc2");
  }

  public void testInsideTheArgumentsOfAMacroUse() {
    check("inside the arguments of a macro use", HEAD + "f(Abc1, Abc2) ->\n  foo?PAR,\n  ?ID(Ab<caret>).\n",
          HEAD + "f(Abc1, Abc2) ->\n  foo(),\n  (Ab<caret>).\n", "Abc1", "Abc2");
  }

  public void testInAFormWithAHeaderMacro() {
    check("header macro", HEAD + "f(Abc1, Abc2) ->\n  foo?HPAR,\n  X1 = Ab<caret>,\n  X1.\n",
          HEAD + "f(Abc1, Abc2) ->\n  foo(),\n  X1 = Ab<caret>,\n  X1.\n", "Abc1", "Abc2");
  }

  /**
   * The auto-popup asks for the copy as a passive query (the same mark the contributor puts on it): right after a key
   * press it builds nothing; it waits for the copy, and the next key press makes it give up. The explicit completion is
   * not passive (tested above, through the platform's completion on the UI thread with the copy there).
   */
  public void testAutoPopupBuildsNothingWhileTyping() throws Exception {
    PsiFile f = myFixture.addFileToProject("c/auto.erl", HEAD.replace("-module(m).", "-module(auto).") +
                                                          "f(Abc1, Abc2) ->\n  foo?PAR,\n  X1 = Ab,\n  X1.\n");
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    int builds0 = builds().size();
    com.intellij.openapi.editor.Document d = com.intellij.psi.PsiDocumentManager.getInstance(getProject()).getDocument(f);
    for (int i = 0; i < 3; i++) {
      com.intellij.openapi.command.WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getTextLength(), "%"));
      com.intellij.psi.PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
      java.util.concurrent.Future<Object> popup = com.intellij.openapi.application.ApplicationManager.getApplication().executeOnPooledThread(
        () -> inBackgroundWithIndicator(() -> ErlangMacroLayer.asPassiveQuery(() -> ErlangMacroLayer.getCopy(f, "completion"))));
      Thread.sleep(100);
      assertFalse("the auto-popup waits", popup.isDone());
      com.intellij.openapi.command.WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getTextLength(), "%"));
      assertNull("the next key press: master's answer", popup.get(30, java.util.concurrent.TimeUnit.SECONDS));
      com.intellij.psi.PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    }
    assertEquals("no build while typing", builds0, builds().size());
    assertEquals(0, count("CANCELLED"));
  }

  /** The cost of the extra parse in a large file (about 200 KB), printed; the result must still be right. */
  public void testCostInALargeFile() {
    StringBuilder big = new StringBuilder(HEAD);
    for (int i = 0; i < 2000; i++) big.append("g").append(i).append("(X) ->\n  case X of\n    {a, Y} -> Y + ").append(i).append(";\n    _ -> [X, X]\n  end.\n");
    String withMacro = big + "f(Abc1, Abc2) ->\n  foo?PAR,\n  X1 = Ab<caret>,\n  X1.\n";
    long t0 = System.nanoTime();
    List<String> got = complete(withMacro);
    long ms = (System.nanoTime() - t0) / 1_000_000;
    System.out.println("LAYER-COMPLETION large file: " + withMacro.length() + " chars, parse of the copy's text with the dummy " +
                       ErlangMacroLayerCompletionContributor.ourLastParseNanos / 1_000_000 + " ms, highlighting and completion " + ms + " ms");
    assertTrue(got.toString(), got.containsAll(List.of("Abc1", "Abc2")));
  }

  // ------------------------------------------------------------------------------------------------ item 3

  private static final String MADE = "-module(m).\n-export([f/1, g/1]).\n-define(DEF(N), N() -> ok).\n-define(BIND(V), V = 1).\n" +
                                     "-define(HX, Xh = 5).\n-define(R, -record(mr, {mf1, mf2})).\n?DEF(zqhello).\n?R.\n";
  /** The same names written out, as erlc sees them. */
  private static final String WRITTEN = "-module(m).\n-export([f/1, g/1]).\nzqhello() -> ok.\n-record(mr, {mf1, mf2}).\n";

  public void testFunctionMadeByAMacro() {
    check("function made by ?DEF", MADE + "g(_) -> zqh<caret>.\nf(_) -> ok.\n", WRITTEN + "g(_) -> zqh<caret>.\nf(_) -> ok.\n", "zqhello");
  }

  public void testExportOfAFunctionMadeByAMacro() {
    check("export of a function made by ?DEF", MADE + "-export([zqh<caret>]).\ng(_) -> ok.\nf(_) -> ok.\n",
          WRITTEN + "-export([zqh<caret>]).\ng(_) -> ok.\nf(_) -> ok.\n", "zqhello");
  }

  public void testVariableBoundThroughAMacroArgument() {
    check("variable bound by ?BIND(Xyz)", MADE + "f(_) -> ?BIND(Xyz), Xy<caret>.\ng(_) -> ok.\n",
          WRITTEN + "f(_) -> Xyz = 1, Xy<caret>.\ng(_) -> ok.\n", "Xyz");
  }

  public void testFreeVariableOfAMacroBody() {
    check("free variable of ?HX", MADE + "f(_) -> ?HX, X<caret>.\ng(_) -> ok.\n",
          WRITTEN + "f(_) -> Xh = 5, X<caret>.\ng(_) -> ok.\n", "Xh");
  }

  public void testRecordMadeByAMacro() {
    check("record made by ?R", MADE + "f(_) -> #m<caret>.\ng(_) -> ok.\n", WRITTEN + "f(_) -> #m<caret>.\ng(_) -> ok.\n", "mr");
  }

  public void testFieldOfARecordMadeByAMacro() {
    check("field of a record made by ?R", MADE + "f(X) -> X#mr{m<caret>}.\ng(_) -> ok.\n",
          WRITTEN + "f(X) -> X#mr{m<caret>}.\ng(_) -> ok.\n", "mf1", "mf2");
  }

  /** The names are kept on the file (strings only) for the auto-popup, which uses them when the copy is one edit old. */
  public void testMadeNamesAreKeptOnTheFile() {
    complete(MADE + "g(_) -> zqh<caret>.\nf(_) -> ok.\n");
    PsiFile f = myFixture.getFile();
    ErlangMacroLayerCompletionContributor.Made made = f.getUserData(ErlangMacroLayerCompletionContributor.MADE_NAMES);
    assertNotNull(made);
    assertEquals(Set.of("zqhello/0"), made.functions().keySet());
    assertEquals(Set.of("mr"), made.records().keySet());
    assertEquals(List.of("mf1", "mf2"), made.fields().get("mr"));
    assertTrue("no target is kept", made.functions().values().stream().allMatch(Objects::isNull));
  }

  /** A form that the main tree parses: the layer does not take part, and completion is master's. */
  public void testAFormTheMainTreeParsesIsLeftToMaster() {
    int taken0 = takenOver();
    List<String> got = complete(HEAD + "f(Abc1, Abc2) ->\n  X1 = ?ID(Abc1),\n  Ab<caret>.\n");
    assertTrue(got.toString(), got.containsAll(List.of("Abc1", "Abc2")));
    assertEquals(taken0, takenOver());
  }
}
