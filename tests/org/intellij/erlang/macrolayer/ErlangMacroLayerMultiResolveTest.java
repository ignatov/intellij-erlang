package org.intellij.erlang.macrolayer;

import com.intellij.psi.*;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Paths;
import java.util.*;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 6: a reference with both paths gives the layer's answer through {@code multiResolve} where {@code resolve()}
 * gives it. Calls of functions made by macros, in the file and through a header: {@code multiResolve(false)} is the
 * layer's answer, and {@code multiResolve(true)} has it first. A call that master resolves keeps master's results.
 */
public class ErlangMacroLayerMultiResolveTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private final Map<String, PsiFile> myFiles = new LinkedHashMap<>();

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
    for (String n : List.of("h.hrl", "i.erl", "i2.erl", "s.erl")) {
      myFiles.put(n, myFixture.addFileToProject("nav/" + n, read(Paths.get("testData/macroLayer/nav", n))));
    }
  }

  /** resolve(), multiResolve(false) and multiResolve(true) of the reference at the marker, as "offset" lists. */
  private String[] answers(String file, String marker) throws Exception {
    PsiFile f = myFiles.get(file);
    int at = f.getText().indexOf(marker);
    assertTrue(marker, at >= 0);
    return inBackground(() -> {
      PsiReference ref = f.findReferenceAt(at);
      assertTrue(String.valueOf(ref), ref instanceof PsiPolyVariantReference);
      PsiElement r = ref.resolve();
      return new String[]{r == null ? "null" : where(r), list(((PsiPolyVariantReference)ref).multiResolve(false)),
        list(((PsiPolyVariantReference)ref).multiResolve(true))};
    });
  }

  private static String where(PsiElement e) {
    return e.getContainingFile().getName() + "@" + e.getTextRange().getStartOffset();
  }

  private static String list(ResolveResult[] results) {
    List<String> r = new ArrayList<>();
    for (ResolveResult rr : results) r.add(rr.getElement() == null ? "null" : where(rr.getElement()));
    return String.join(",", r);
  }

  private void assertLayerAnswer(String file, String marker) throws Exception {
    String[] a = answers(file, marker);
    System.out.println("LAYER-MR " + file + " " + marker + ": resolve=" + a[0] + " multi(false)=" + a[1] + " multi(true)=" + a[2]);
    assertFalse("the layer resolves " + marker, a[0].equals("null"));
    assertEquals("multiResolve(false) is resolve()", a[0], a[1]);
    assertTrue("multiResolve(true) starts with resolve(): " + a[2], (a[2] + ",").startsWith(a[0] + ","));
  }

  public void testCallOfAFunctionMadeByASameFileMacro() throws Exception {
    assertLayerAnswer("s.erl", "made(), fixed()");
    assertLayerAnswer("s.erl", "fixed()}");
  }

  public void testCallOfAFunctionMadeByAHeaderMacro() throws Exception {
    assertLayerAnswer("i.erl", "hmade(), hfixed()");
    assertLayerAnswer("i.erl", "hfixed()}");
  }

  public void testExportOfAFunctionMadeByAMacro() throws Exception {
    assertLayerAnswer("s.erl", "made/0");
  }

  /** A call that master resolves: multiResolve(false) stays empty, as on master; multiResolve(true) has master's candidates. */
  public void testCallThatMasterResolvesKeepsMastersResults() throws Exception {
    String[] b = answers("s.erl", "a/1");
    System.out.println("LAYER-MR master: " + Arrays.toString(b));
    assertEquals("", b[1]);
    assertFalse(b[0].equals("null"));
    assertTrue(b[2], b[2].contains(b[0]));
  }
}
