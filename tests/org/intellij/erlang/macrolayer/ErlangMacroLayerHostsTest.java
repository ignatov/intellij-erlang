package org.intellij.erlang.macrolayer;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.PsiReferenceService;
import com.intellij.psi.ReferenceRange;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangRecordDefinition;
import org.intellij.erlang.psi.impl.ErlangBareMacroReference;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

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
    int uses = 4_000;
    PsiFile f = myFixture.addFileToProject("h/c10i.erl", module("c10i", uses, "-define(PAR, ()).\nfoo() -> ok.\n", "r", "  foo?PAR,\n"));
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    long visits = ErlangMacroLayerHosts.LEAF_VISITS.get();
    usagesOfR(f);
    long work = ErlangMacroLayerHosts.LEAF_VISITS.get() - visits;
    // the file is asked once per occurrence: walking its leaves each time is uses * uses (16,000,000); once per search
    // is a few leaves per use (more when parallel search threads compute the same cached answer at the same time)
    assertTrue("leaves walked for " + uses + " uses: " + work, work <= 250L * uses);
  }

  /**
   * Find usages asks the file once per occurrence, with the offset: the file gives only the references at the offset
   * (at most two: one ends there, the next starts there), the same ones, in the same order, as its full list filtered by
   * the offset.
   */
  public void testTheFileGivesFindUsagesOnlyTheReferencesAtTheOffset() throws Exception {
    PsiFile f = myFixture.addFileToProject("h/c10o.erl", module("c10o", 2_000, "-define(PAR, ()).\nfoo() -> ok.\n", "r", "  foo?PAR,\n"));
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    ErlangRecordDefinition rec = inBackgroundWithIndicator(() -> ((ErlangFile)f).getRecord("r"));
    String result = inBackgroundWithIndicator(() -> {
      PsiReference[] all = f.getReferences();
      int most = 0;
      for (int offset = 0; offset <= f.getTextLength(); offset++) {
        List<PsiReference> expected = new ArrayList<>();
        for (PsiReference r : all) if (ReferenceRange.containsOffsetInElement(r, offset)) expected.add(r);
        List<PsiReference> hinted = PsiReferenceService.getService().getReferences(f, new PsiReferenceService.Hints(rec, offset));
        if (!describe(expected).equals(describe(hinted))) return "at " + offset + ": " + describe(hinted) + " instead of " + describe(expected);
        most = Math.max(most, hinted.size());
      }
      return all.length + " references, at most " + most + " at one offset";
    });
    assertTrue(result, result.endsWith("at most 1 at one offset") || result.endsWith("at most 2 at one offset"));
    assertTrue("the file hosts the token references of the fragment: " + result, Integer.parseInt(result.split(" ")[0]) >= 2_000);
  }

  private static String describe(List<PsiReference> refs) {
    StringBuilder sb = new StringBuilder();
    for (PsiReference r : refs) sb.append(r.getClass().getSimpleName()).append(r.getRangeInElement()).append(' ');
    return sb.toString();
  }
}
