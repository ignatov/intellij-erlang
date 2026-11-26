package org.intellij.erlang.macrolayer;

import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.psi.*;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * M1 (architecture review): an answer kept in the copy must not outlive a change of another file. The copy of a.erl
 * stays when b.erl or a non-directive form of a header changes (an unrelated edit rebuilds no copy), but the kept
 * answers are computed again: a deleted target gives no answer (not an invalid element), a new target is found.
 */
public class ErlangMacroLayerStaleAnswerTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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

  /** The targets of the references at the marker (all results), each checked valid; "name@file" strings. */
  private static List<String> targets(PsiFile f, String marker) throws Exception {
    return inBackgroundWithIndicator(() -> {
      int at = f.getText().indexOf(marker.replace("|", "")) + marker.indexOf('|');
      PsiReference ref = f.findReferenceAt(at);
      List<String> r = new ArrayList<>();
      if (ref == null) return r;
      List<PsiReference> refs = new ArrayList<>();
      if (ref instanceof com.intellij.psi.impl.source.resolve.reference.impl.PsiMultiReference m) refs.addAll(List.of(m.getReferences()));
      else refs.add(ref);
      for (PsiReference one : refs) {
        List<PsiElement> found = new ArrayList<>();
        if (one instanceof PsiPolyVariantReference p) for (ResolveResult rr : p.multiResolve(false)) found.add(rr.getElement());
        else if (one.resolve() != null) found.add(one.resolve());
        for (PsiElement e : found) {
          if (e == null) continue;
          assertTrue("a returned element is valid: " + one, e.isValid());
          r.add(e.getText().split("[(\n]")[0] + "@" + e.getContainingFile().getName());
        }
      }
      return r;
    });
  }

  private void edit(PsiFile f, String from, String to) {
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(f);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> {
      int at = d.getText().indexOf(from);
      assertTrue(from, at >= 0);
      d.replaceString(at, at + from.length(), to);
    });
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
  }

  private PsiFile open(String a) {
    PsiFile f = myFixture.addFileToProject("s/a.erl", a);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    return f;
  }

  private static final String A = "-module(a).\n-export([f/0]).\n-define(CALL(M, F), M:F(), ok).\nf() -> ?CALL(b, g).\n";

  public void testDeletedTargetInAnotherFile() throws Exception {
    PsiFile b = myFixture.addFileToProject("s/b.erl", "-module(b).\n-export([g/0]).\ng() -> ok.\n");
    PsiFile a = open(A);
    assertTrue(targets(a, "?CALL(b, |g)").toString(), targets(a, "?CALL(b, |g)").contains("g@b.erl"));
    int builds = builds().size();
    edit(b, "g() -> ok.\n", "");
    assertFalse("no answer for a deleted target", targets(a, "?CALL(b, |g)").contains("g@b.erl"));
    assertEquals("the edit of b.erl rebuilt no copy of a.erl", builds, builds().size());
  }

  public void testNewTargetInAnotherFile() throws Exception {
    PsiFile b = myFixture.addFileToProject("s/b.erl", "-module(b).\n-export([g/0]).\nh() -> ok.\n");
    PsiFile a = open(A);
    assertFalse(targets(a, "?CALL(b, |g)").contains("g@b.erl"));
    edit(b, "h() -> ok.\n", "h() -> ok.\ng() -> ok.\n");
    assertTrue("the new target is found: " + targets(a, "?CALL(b, |g)"), targets(a, "?CALL(b, |g)").contains("g@b.erl"));
  }

  public void testDeletedRecordInAHeader() throws Exception {
    PsiFile h = myFixture.addFileToProject("s/h.hrl", "-record(rec, {a}).\n");
    PsiFile a = open("-module(a).\n-include(\"h.hrl\").\n-export([f/0]).\n-define(NEW(R), X = #R{}).\nf() -> ?NEW(rec), X.\n");
    assertTrue(targets(a, "?NEW(r|ec)").toString(), targets(a, "?NEW(r|ec)").stream().anyMatch(s -> s.endsWith("@h.hrl")));
    edit(h, "-record(rec, {a}).\n", "");
    assertFalse("no answer for a deleted record", targets(a, "?NEW(r|ec)").stream().anyMatch(s -> s.endsWith("@h.hrl")));
  }
}
