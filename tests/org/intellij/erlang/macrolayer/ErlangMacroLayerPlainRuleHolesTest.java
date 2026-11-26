package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.*;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 5, last fix round: every '=' of a macro body that can bind makes the macro not plain. A record brace may hold a
 * match chain ({@code #r{a = V = W}}); such a use binds a variable of the caller, so the form must ask the copy.
 */
public class ErlangMacroLayerPlainRuleHolesTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  /** Macros that can bind: none of them is plain. "new" are the ones the old rule called plain. */
  static final String NON_PLAIN = """
    -define(REC2(W, V), #r{a = V = W}).
    -define(NESTR(W, V), #r{a = #s{b = V = W}}).
    -define(UPD(R, W, V), R#r{a = V = W}).
    -define(QREC(W, V), #r{'a' = V = W}).
    -define(MREC(W, V), #r{?FA = V = W}).
    -define(UREC(W, V), #r{_ = V = W}).
    -define(SREC(W, V), #r{a = _ = V = W}).
    -define(BREC(W, V), case W of _ -> #r{a = V = W} end).
    -define(PFIELD(F, V), #r{F = V}).
    -define(MAPA(W, V), #{k := V = W}).
    -define(MAPB(W, V), #{k => V = W}).
    -define(MAPU(M, W, V), M#{k := V = W}).
    -define(BIN(W, V), <<(V = W):8>>).
    -define(FUNH(V), fun(V) -> ok end).
    -define(FUNB(W, V), fun() -> V = W end).
    -define(NFUN(N), fun N() -> ok end).
    -define(CASEH(W, V), case W of V -> ok end).
    -define(CASEB(W, V), case W of _ -> V = W end).
    -define(RECV(V), receive V -> ok end).
    -define(TRYC(W, V), try W catch V -> ok end).
    -define(TRYOF(W, V), try W of V -> ok after ok end).
    -define(MAYBE(W, V), maybe V ?= W end).
    -define(BLOCK(W, V), begin V = W end).
    -define(LC(W, V), [V || V <- W]).
    -define(BC(W, V), << <<V>> || <<V>> <= W >>).
    -define(MC(W, K, V), #{K => V || K := V <- W}).
    -define(PAT(W, V), case W of #r{a = V} -> ok end).
    """;
  static final List<String> NEW_HOLES = List.of("REC2", "NESTR", "UPD", "QREC", "MREC", "UREC", "SREC", "BREC", "PFIELD");
  /** Record and map braces that only build a value stay plain. */
  static final String PLAIN = """
    -define(FA, a).
    -define(R1(V), #r{a = V}).
    -define(R2(V, W), #r{a = V, b = W}).
    -define(R3(V), #r{'a' = V}).
    -define(R4(V), #r{_ = V}).
    -define(R5(R, V), R#r{a = V}).
    -define(R6(V), #r{a = #s{b = V}}).
    -define(R7(V), #r{?FA = V}).
    -define(M1(V), #{k => V}).
    -define(M2(M, V), M#{k := V}).
    """;

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableAll(myFixture);
  }

  public void testMacrosThatCanBindAreNotPlain() throws Exception {
    PsiFile f = myFixture.addFileToProject("ph.erl", "-module(ph).\n-record(r, {a, b}).\n-record(s, {b}).\n" + PLAIN + NON_PLAIN);
    ErlangMacroLayer.Table t = inBackground(() -> ErlangMacroLayer.table((ErlangFile)f));
    Set<String> expected = new TreeSet<>();
    NON_PLAIN.lines().forEach(l -> expected.add(l.substring("-define(".length(), l.indexOf('(', "-define(".length()))));
    assertEquals(expected, new TreeSet<>(t.nonPlain));
    assertTrue(expected.containsAll(NEW_HOLES));
  }

  /** p04 of the verifier: the use binds Y; with the gate on, Y is not reported unbound (as with the gate off). */
  public void testRecordMatchChainBindsTheCallersVariable() throws Exception {
    myFixture.addFileToProject("hd/p19.hrl", "-record(hr, {a}).\n-define(HREC2(W, V), #hr{a = V = W}).\n");
    PsiFile f = myFixture.addFileToProject("hd/p04.erl", """
      -module(p04).
      -export([f/1, g/1, h/1, k/1]).
      -include("p19.hrl").
      -record(r, {a}).
      -record(s, {b}).
      -define(REC2(W, V), #r{a = V = W}).
      -define(NEST(W, V), #r{a = #s{b = V = W}}).
      -define(UPD(R, W, V), R#r{a = V = W}).
      f(X) -> _ = ?REC2(X, Y), Y.
      g(X) -> _ = ?NEST(X, Y), Y.
      h(R) -> _ = ?UPD(R, 1, Z), Z.
      k(X) -> _ = ?HREC2(X, Hy), Hy.
      """);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    List<String> errors = new ArrayList<>();
    for (HighlightInfo i : myFixture.doHighlighting()) {
      if (i.getSeverity().compareTo(HighlightSeverity.ERROR) >= 0) errors.add(i.getDescription());
    }
    assertEquals(List.of(), errors);
    // the last variable of each function resolves to its binding place in the macro argument
    String text = f.getText();
    String[][] cases = {{"f(X) -> _ = ?REC2(X, Y), Y.", "Y"}, {"g(X) -> _ = ?NEST(X, Y), Y.", "Y"},
                        {"h(R) -> _ = ?UPD(R, 1, Z), Z.", "Z"}, {"k(X) -> _ = ?HREC2(X, Hy), Hy.", "Hy"}};
    for (String[] c : cases) {
      int start = text.indexOf(c[0]);
      int use = start + c[0].lastIndexOf(c[1]);
      int bind = start + c[0].indexOf(c[1] + ")");
      Integer target = inBackground(() -> {
        com.intellij.psi.PsiReference ref = f.findReferenceAt(use);
        com.intellij.psi.PsiElement r = ref == null ? null : ref.resolve();
        return r == null ? null : r.getTextOffset();
      });
      assertEquals(c[0], Integer.valueOf(bind), target);
    }
  }
}
