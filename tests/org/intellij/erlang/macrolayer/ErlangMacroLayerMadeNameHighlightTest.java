package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.IdentifierHighlightingComputer;
import com.intellij.codeInsight.daemon.impl.IdentifierHighlightingResult;
import com.intellij.codeInsight.daemon.impl.IdentifierOccurrence;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.ProperTextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 6 (next), item 3a (from the real IDE): the identifier highlighting of a name that only a macro makes (a
 * function made by {@code ?HGEN(hfoo).} with a header macro or by {@code ?DEF(hello).} with a same-file one, a record
 * and a field made by a macro form). The layer resolves the uses to the atom leaf in the macro use; the highlighting
 * must paint the uses and that leaf, with the caret on a use or on the leaf, with no typing.
 */
public class ErlangMacroLayerMadeNameHighlightTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private PsiFile myFile;

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
  }

  private void open(String name, String text) {
    myFile = myFixture.addFileToProject("h/" + name, text);
    myFixture.configureFromExistingVirtualFile(myFile.getVirtualFile());
    myFixture.doHighlighting();
  }

  private int offset(String marker) {
    int bar = marker.indexOf('|');
    String m = marker.substring(0, bar) + marker.substring(bar + 1);
    int at = myFile.getText().indexOf(m);
    assertTrue("no " + m, at >= 0);
    return at + bar;
  }

  /** The occurrences that the platform's identifier highlighting paints at the marker (line:text). */
  private String paint(String marker) throws Exception {
    Editor editor = myFixture.getEditor();
    int caret = offset(marker);
    PsiFile f = myFile;
    return inBackgroundWithIndicator(() -> {
      IdentifierHighlightingResult r = new IdentifierHighlightingComputer(f, editor, ProperTextRange.create(0, f.getTextLength()), caret).computeRanges();
      List<String> o = new ArrayList<>();
      for (IdentifierOccurrence x : r.occurrences()) {
        o.add((StringUtil.offsetToLineNumber(f.getText(), x.range().getStartOffset()) + 1) + ":" +
              f.getText().substring(x.range().getStartOffset(), x.range().getEndOffset()));
      }
      Collections.sort(o);
      return o.toString();
    });
  }

  private void check(String what, String expected, String... markers) throws Exception {
    for (String m : markers) {
      String got = paint(m);
      System.out.println("MADE-HIGHLIGHT " + what + " at " + m + ": " + got);
      assertEquals(what + " at " + m, expected, got);
    }
  }

  public void testFunctionMadeByAHeaderMacro() throws Exception {
    myFixture.addFileToProject("h/v5h.hrl", "-define(HGEN(N), N() -> gen_ok).\n");
    open("v5a.erl", "-module(v5a).\n-include(\"v5h.hrl\").\n-export([k/0]).\n?HGEN(hfoo).\nk() -> hfoo().\n");
    check("function made by a header macro", "[4:hfoo, 5:hfoo]", "k() -> h|foo", "?HGEN(h|foo)");
  }

  public void testFunctionMadeByASameFileMacro() throws Exception {
    open("d.erl", "-module(d).\n-export([k/0]).\n-define(DEF(N), N() -> ok).\n?DEF(hello).\nk() -> hello().\n");
    check("function made by a same-file macro", "[4:hello, 5:hello]", "k() -> he|llo", "?DEF(he|llo)");
  }

  public void testRecordAndFieldMadeByAMacroForm() throws Exception {
    open("r.erl", "-module(r).\n-export([k/0]).\n-define(R, -record(mr, {mf})).\n?R.\nk() -> #mr{mf = 1}.\n");
    check("record made by a macro form", "[3:mr, 5:mr]", "#m|r{", "-record(m|r,");
    check("field made by a macro form", "[3:mf, 5:mf]", "{m|f = 1}", "{m|f})");
  }
}
