package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.IdentifierHighlightingComputer;
import com.intellij.codeInsight.daemon.impl.IdentifierOccurrence;
import com.intellij.openapi.util.ProperTextRange;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 6 (next), items 3b and 3c (from the real IDE): find usages of a macro lists each use once (the use element
 * {@code ?PAR} and its name element both had a reference over the same name), and a macro with definitions of two
 * arities is two macros for find usages, the identifier highlighting and rename, as epp chooses the definition by the
 * number of arguments (rename started on one arity renames that definition and its uses, like a function).
 */
public class ErlangMacroLayerMacroUsagesTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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
    myFile = myFixture.addFileToProject("u/" + name, text);
    myFixture.configureFromExistingVirtualFile(myFile.getVirtualFile());
    myFixture.doHighlighting();
  }

  private String at(TextRange r) {
    return (StringUtil.offsetToLineNumber(myFile.getText(), r.getStartOffset()) + 1) + ":" + myFile.getText().substring(r.getStartOffset(), r.getEndOffset());
  }

  private ErlangMacrosDefinition define(int index) {
    return ((ErlangFile)myFile).getMacroses().get(index);
  }

  private String usages(PsiElement target) throws Exception {
    return inBackgroundWithIndicator(() -> {
      List<String> r = new ArrayList<>();
      for (PsiReference ref : ReferencesSearch.search(target).findAll()) {
        r.add(at(ref.getRangeInElement().shiftRight(ref.getElement().getTextRange().getStartOffset())));
      }
      Collections.sort(r);
      return r.toString();
    });
  }

  private String paint(String marker) throws Exception {
    int bar = marker.indexOf('|');
    int caret = myFile.getText().indexOf(marker.replace("|", "")) + bar;
    PsiFile f = myFile;
    return inBackgroundWithIndicator(() -> {
      List<String> o = new ArrayList<>();
      for (IdentifierOccurrence x : new IdentifierHighlightingComputer(f, myFixture.getEditor(), ProperTextRange.create(0, f.getTextLength()), caret)
        .computeRanges().occurrences()) {
        o.add(at(TextRange.create(x.range())));
      }
      Collections.sort(o);
      return o.toString();
    });
  }

  /** 3c: each use once. */
  public void testEachUseIsFoundOnce() throws Exception {
    open("one.erl", "-module(one).\n-export([f/0]).\n-define(ONE, 1).\nf() -> ?ONE + ?ONE.\n");
    assertEquals("[4:ONE, 4:ONE]", usages(define(0)));
  }

  /** 3c: find usages (the platform's handler, as the IDE runs it) of the owner's macro lists its use once; one reference at the use. */
  public void testFindUsagesListsTheUseOnce() {
    open("red.erl", "-module(red).\n-export([main/0]).\n-define(PAR, ()).\nmain() ->\n  foo(),\n  ?PAR,\n  io:format(\"hello\").\nfoo() -> ok.\n");
    List<String> r = new ArrayList<>();
    for (com.intellij.usageView.UsageInfo u : myFixture.findUsages(define(0))) {
      com.intellij.openapi.util.Segment s = u.getSegment();
      r.add(s == null ? "null" : at(TextRange.create(s)));
    }
    Collections.sort(r);
    assertEquals("[6:PAR]", r.toString());
    PsiReference ref = myFile.findReferenceAt(myFile.getText().indexOf("?PAR,") + 2);
    assertFalse("one reference at the use: " + ref, ref instanceof com.intellij.psi.impl.source.resolve.reference.impl.PsiMultiReference);
  }

  /** 3b: find usages of each arity. */
  public void testFindUsagesTellsTheArities() throws Exception {
    open("ar.erl", "-module(ar).\n-export([f/0]).\n-define(M(), zero).\n-define(M(X), {one, X}).\nf() ->\n  {?M(), ?M(1),\n   ?M(2)}.\n");
    assertEquals("[6:M]", usages(define(0)));
    assertEquals("[6:M, 7:M]", usages(define(1)));
  }

  /** 3b: the identifier highlighting at a use of M/1 paints the definition of M/1 and its uses only. */
  public void testIdentifierHighlightingTellsTheArities() throws Exception {
    open("ari.erl", "-module(ari).\n-export([f/0]).\n-define(M(), zero).\n-define(M(X), {one, X}).\nf() ->\n  {?M(), ?M(1),\n   ?M(2)}.\n");
    assertEquals("[4:M, 6:M, 7:M]", paint("?|M(1)"));
    assertEquals("[3:M, 6:M]", paint("?|M()"));
  }

  /** 3b: rename of M/1 renames its definition and its uses; M/0 stays. */
  public void testRenameOfOneArity() {
    open("arr.erl", "-module(arr).\n-export([f/0]).\n-define(M(), zero).\n-define(M(X), {one, X}).\nf() ->\n  {?M(), ?M(1),\n   ?M(2)}.\n");
    myFixture.renameElement(define(1), "N");
    assertEquals("-module(arr).\n-export([f/0]).\n-define(M(), zero).\n-define(N(X), {one, X}).\nf() ->\n  {?M(), ?N(1),\n   ?N(2)}.\n",
                 myFile.getText());
  }

  /** 3b: a use that the parser left as bare tokens ({@code foo?P(1)}) is told apart by its arity too. */
  public void testBareUseTellsTheArities() throws Exception {
    open("arb.erl", "-module(arb).\n-export([f/0, foo/1]).\n-define(P, ()).\n-define(P(X), (X)).\nfoo(X) -> X.\nf() ->\n  foo?P(1),\n  ok.\n");
    assertEquals("[]", usages(define(0)));
    assertEquals("[7:P]", usages(define(1)));
  }
}
