package org.intellij.erlang.macrolayer;

import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.Map;
import java.util.TreeMap;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 5, fix 4: in a form that needs the copy, a variable asks only when it is a free variable of a macro body (not a
 * parameter) or lies in the arguments of a use that needs substitution.
 */
public class ErlangMacroLayerVariableGateTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ErlangMacroLayerLog.enableForTests(false);
    }
    finally {
      super.tearDown();
    }
  }

  public void testOnlyFreeBodyVariablesAndArgumentsOfSubstitutedUsesAsk() throws Exception {
    PsiFile f = myFixture.addFileToProject("v.erl", """
      -module(v).
      -define(B(X), X = 1).
      -define(P(A), {A}).
      -define(FREE, W = 2).
      -export([f/1]).
      f(T) -> ?B(Z), ?P(Q), ?FREE, ?P(?B(N)), {Z, Q, T, X, A, W, N}.
      """);
    int tuple = f.getText().indexOf("{Z, Q");
    Map<String, String> outcomes = new TreeMap<>();
    for (String v : new String[]{"Z", "Q", "T", "X", "A", "W", "N"}) {
      int at = f.getText().indexOf(v, tuple);
      ErlangMacroLayerStats.reset();
      inBackgroundWithIndicator(() -> {
        PsiReference ref = f.findReferenceAt(at);
        return ((PsiPolyVariantReference)ref).multiResolve(false).length;
      });
      ErlangMacroLayerStats.snapshot().forEach((k, c) -> {
        String[] p = k.split("\t");
        if (p[1].equals("variable(multiResolve)") && !p[2].startsWith("nested:")) outcomes.put(v, p[2].replaceFirst(":.*", ""));
      });
    }
    assertEquals("{A=gate, N=asked(arg-var), Q=gate, T=gate, W=asked(body-var), X=gate, Z=asked(arg-var)}", outcomes.toString());
  }

  /**
   * Round 6: a free variable of a macro body asks only in a form that uses that macro, directly or through the macros
   * of a body it uses. S is free in OTHER, W in FREE: in f/1, which uses only B, neither asks.
   */
  public void testFreeBodyVariablesAskOnlyInFormsThatUseTheirMacro() throws Exception {
    PsiFile f = myFixture.addFileToProject("v7.erl", """
      -module(v7).
      -define(B(X), X = 1).
      -define(FREE, W = 2).
      -define(OTHER, S + 1).
      -define(NEST, ?FREE).
      -export([f/1, g/1, h/0]).
      f(S) -> ?B(Z), {Z, S, W}.
      g(S) -> ?OTHER, ?B(Y), {S, Y}.
      h() -> ?NEST, W.
      """);
    Map<String, String> outcomes = new TreeMap<>();
    for (String[] c : new String[][]{{"f:S", "S, W}"}, {"f:W", "W}."}, {"g:S", "S, Y}"}, {"h:W", "W.\n"}}) {
      int at = f.getText().indexOf(c[1]);
      ErlangMacroLayerStats.reset();
      inBackgroundWithIndicator(() -> {
        PsiReference ref = f.findReferenceAt(at);
        return ((PsiPolyVariantReference)ref).multiResolve(false).length;
      });
      ErlangMacroLayerStats.snapshot().forEach((k, v) -> {
        String[] p = k.split("\t");
        if (p[1].equals("variable(multiResolve)") && !p[2].startsWith("nested:")) outcomes.put(c[0], p[2].replaceFirst(":.*", ""));
      });
    }
    assertEquals("{f:S=gate, f:W=gate, g:S=asked(body-var), h:W=asked(body-var)}", outcomes.toString());
  }
}
