package org.intellij.erlang.macrolayer;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangRecordDefinition;
import org.intellij.erlang.psi.impl.ErlangBareMacroReference;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import static org.intellij.erlang.macrolayer.LayerTestSupport.descriptor;
import static org.intellij.erlang.macrolayer.LayerTestSupport.inBackgroundWithIndicator;

/**
 * The cost of the layer's hosted references, without a clock (review C10): the platform asks every ancestor of a word
 * occurrence for its references, and each asked element used to scan its children for a bare macro use or a token
 * that only the copy gives a role. In a file with nothing to host no element scans its children; in a file with
 * fragments the same references are found as before, and the work of a search grows with the uses, not with their
 * square.
 */
public class ErlangMacroLayerHostsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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

  private static String module(String name, int uses, String extra) {
    return module(name, uses, extra, "r", "");
  }

  private static String module(String name, int uses, String extra, String record, String head) {
    StringBuilder sb = new StringBuilder("-module(" + name + ").\n-export([t/0]).\n-record(" + record + ", {a}).\n" + extra +
                                         "t() ->\n" + head + "  [");
    for (int i = 0; i < uses; i++) sb.append(i == 0 ? "" : ",\n   ").append("#").append(record).append("{}");
    return sb.append("].\n").toString();
  }

  private int usagesOfR(PsiFile f) throws Exception {
    return usagesOf(f, "r");
  }

  private int usagesOf(PsiFile f, String record) throws Exception {
    ErlangRecordDefinition rec = inBackgroundWithIndicator(() -> ((ErlangFile)f).getRecord(record));
    return inBackgroundWithIndicator(() -> ReferencesSearch.search(rec).findAll().size());
  }

  public void testAFileWithNothingToHostScansNoChildren() throws Exception {
    PsiFile f = myFixture.addFileToProject("h/c10.erl", module("c10", 2_000, ""));
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    long scans = ErlangMacroLayerHosts.CHILD_SCANS.get();
    assertEquals(2_000, usagesOfR(f));
    assertEquals("child scans by the hosted references", 0, ErlangMacroLayerHosts.CHILD_SCANS.get() - scans);
  }

  public void testAFileWithFragmentsKeepsItsReferences() throws Exception {
    PsiFile f = myFixture.addFileToProject("h/c10f.erl", module("c10f", 200, "-define(PAR, ()).\nf() ->\n  foo?PAR,\n  ok.\nfoo() -> ok.\n"));
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    assertEquals(200, usagesOfR(f));
    int bare = inBackgroundWithIndicator(() -> {
      int n = 0;
      for (PsiElement e : PsiTreeUtil.collectElements(f, x -> true)) {
        for (PsiReference r : e.getReferences()) if (r instanceof ErlangBareMacroReference) n++;
      }
      return n;
    });
    assertTrue("the bare use of ?PAR is still hosted: " + bare, bare > 0);
  }

  /** A bare use in the function itself: error recovery puts the list into top-level error elements, which the file hosts. */
  public void testAFragmentInsideTheFunctionCostsLinearWork() throws Exception {
    String extra = "-define(PAR, ()).\nfoo() -> ok.\n";
    PsiFile small = myFixture.addFileToProject("h/c10s.erl", module("c10s", 200, extra, "rs", "  foo?PAR,\n"));
    PsiFile large = myFixture.addFileToProject("h/c10l.erl", module("c10l", 2_000, extra, "rl", "  foo?PAR,\n"));
    myFixture.configureFromExistingVirtualFile(small.getVirtualFile());
    long v0 = ErlangMacroLayerHosts.LEAF_VISITS.get();
    int smallUsages = usagesOf(small, "rs");
    long v1 = ErlangMacroLayerHosts.LEAF_VISITS.get();
    myFixture.configureFromExistingVirtualFile(large.getVirtualFile());
    long v2 = ErlangMacroLayerHosts.LEAF_VISITS.get();
    int largeUsages = usagesOf(large, "rl");
    long v3 = ErlangMacroLayerHosts.LEAF_VISITS.get();
    assertEquals("the same usages in both files (by the size)", smallUsages * 10, largeUsages);
    long smallWork = Math.max(1, v1 - v0), largeWork = v3 - v2;
    // ten times the uses: about ten times the work when linear, a hundred times when quadratic
    assertTrue("leaves walked: " + smallWork + " for 200 uses, " + largeWork + " for 2,000", largeWork <= 20 * smallWork);
  }
}
