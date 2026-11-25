package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.Set;
import java.util.TreeSet;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * A {@code -define} whose body is broken at every use. The plugin shows no error inside a well-formed body (it may be a
 * fragment that is valid where it is used), so the error must come at the use, where erlc 29.1 reports it. The five
 * cases are mutants of the mutant run (m0146, m0148, m0255, m0312, m0341).
 */
public class ErlangMacroLayerBrokenBodyTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    ErlangMacroLayer.setEdtBuildForTests(true, getTestRootDisposable());
  }

  private int myCount;

  private PsiFile open(String text) {
    PsiFile f = myFixture.addFileToProject("bb" + myCount++ + "/m.erl", text);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    return f;
  }

  private void assertErrorOnLine(String text, int line) {
    PsiFile f = open(text);
    Set<Integer> lines = new TreeSet<>();
    for (HighlightInfo h : myFixture.doHighlighting()) {
      if (h.getSeverity() == HighlightSeverity.ERROR) lines.add(StringUtil.offsetToLineNumber(f.getText(), h.getStartOffset()) + 1);
    }
    assertTrue("an error on line " + line + " (erlc's), errors on " + lines, lines.contains(line));
  }

  private static final String HEAD = "-module(m).\n-export([f/1]).\n-compile([nowarn_unused_function]).\n";

  public void testEmptyListElementThroughAnotherMacro() {
    // m0146.erl:6:21: syntax error before: ','
    assertErrorOnLine(HEAD + "-define(QOS, [0, , 2]).\n-define(ID(Y), Y).\nf(X) -> _ = X, ?ID(?QOS).\n", 6);
  }

  public void testEmptyListElement() {
    // m0148.erl:5:17: syntax error before: ','
    assertErrorOnLine(HEAD + "-define(QOS, [0, , 2]).\nf(X) -> _ = X, ?QOS, ok.\n", 5);
  }

  public void testUnclosedParenthesisInATopLevelUse() {
    // m0255.erl:5:2: syntax error before: '('
    assertErrorOnLine(HEAD + "-define(i16(Int), (Int bsr 8 band 255, Int band 255).\n?i16(a).\nf(_) -> ok.\n", 5);
  }

  public void testUnclosedParenthesisInALongerBody() {
    // m0312.erl:5:2: syntax error before: '('
    assertErrorOnLine(HEAD + "-define(i32(Int), (Int bsr 24) band 255, (Int bsr 16) band 255, (Int bsr 8 band 255, Int band 255).\n" +
                      "?i32(a).\nf(_) -> ok.\n", 5);
  }

  public void testAFragmentValidAtItsUseStaysClean() {
    // erlc compiles it: [1 , X]
    open(HEAD + "-define(TAIL(X), , X]).\nf(X) -> [1 ?TAIL(X).\n");
    for (HighlightInfo h : myFixture.doHighlighting()) {
      assertFalse(h.getDescription(), h.getSeverity() == HighlightSeverity.ERROR);
    }
  }

  public void testMissingArgumentAfterAComma() {
    // review probe d1_used.erl: syntax error before: ')' at the use
    assertErrorOnLine("-module(m).\n-export([f/0]).\n-define(M, foo(1,)).\nf() -> ?M.\nfoo(X) -> X.\n", 4);
  }

  public void testAnUnusedBrokenBodyShowsNothing() {
    // review probe d1_unused.erl: erlc accepts it
    open("-module(m).\n-export([f/0]).\n-define(M, foo(1,)).\nf() -> ok.\n");
    for (HighlightInfo h : myFixture.doHighlighting()) {
      assertFalse(h.getDescription(), h.getSeverity() == HighlightSeverity.ERROR);
    }
  }
}
