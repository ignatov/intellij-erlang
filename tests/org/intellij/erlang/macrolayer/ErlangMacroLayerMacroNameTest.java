package org.intellij.erlang.macrolayer;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFunctionReference;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/** Round 5, fix 3: the name of a macro call ({@code ?T(...)}) is never sent to the copy: no copy can answer for it. */
public class ErlangMacroLayerMacroNameTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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
    ErlangMacroLayerStats.reset();
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

  public void testMacroCallNameAsksNothing() throws Exception {
    // the form needs the copy (?B binds Z), so before round 5 every unresolved reference in it asked the copy
    PsiFile f = myFixture.addFileToProject("m.erl", "-module(m).\n-define(B(X), X = 1).\n-define(T(A), {A}).\n-export([f/0]).\n" +
                                                    "f() -> ?B(Z), ?T(Z), fun ?T/1.\n");
    String result = inBackgroundWithIndicator(() -> {
      List<String> r = new ArrayList<>();
      for (String marker : List.of("?T(Z)", "?T/1")) {
        PsiReference ref = null;
        for (PsiElement e = f.findElementAt(f.getText().indexOf(marker) + 1); e != null && ref == null; e = e.getParent()) {
          for (PsiReference x : e.getReferences()) if (x instanceof ErlangFunctionReference) ref = x;
        }
        assertNotNull(marker, ref);
        r.add(marker + "=" + ref.resolve());
      }
      return r.toString();
    });
    assertEquals("[?T(Z)=null, ?T/1=null]", result);
    assertEquals("no copy build for the name of a macro call", 0, builds().size());
    List<String> outcomes = new ArrayList<>();
    ErlangMacroLayerStats.snapshot().forEach((k, v) -> {
      String[] p = k.split("\t");
      if (p[1].startsWith("resolve:")) outcomes.add(p[2] + " x" + v[0]);
    });
    assertEquals(List.of("gate:macro-name x2"), outcomes);
  }
}
