package org.intellij.erlang.macrolayer;

import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/** Round 5: the defects of the macro layer found by the fourth verifier and the real-IDE check. */
public class ErlangMacroLayerDefects5Test extends ErlangLightPlatformCodeInsightFixtureTestCase {
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
      assertEquals("a copy was built on the UI thread", List.of(), uiThreadBuilds());
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      ErlangMacroLayerLog.enableForTests(false);
      super.tearDown();
    }
  }

  static void out(String s) {
    System.out.println("LAYER-D5 " + s);
  }

  private void highlight(PsiFile... files) {
    for (PsiFile f : files) {
      myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
      myFixture.doHighlighting();
    }
  }

  private static int buildsOf(String fileName) {
    return (int)builds().stream().filter(e -> e.file().equals(fileName)).count();
  }

  private static String reasons() {
    return builds().stream().map(e -> e.file() + ":" + e.reason() + ":" + e.trigger()).toList().toString();
  }

  private static PsiReference refAt(PsiFile f, String marker) {
    int at = f.getText().indexOf(marker);
    assertTrue(marker, at >= 0);
    return f.findReferenceAt(at);
  }

  private static String resolved(PsiFile f, String marker) throws Exception {
    return inBackground(() -> {
      PsiElement t = refAt(f, marker).resolve();
      return t == null ? "null" : t.getClass().getSimpleName() + "@" + (t.getContainingFile() == null ? "?" : t.getContainingFile().getVirtualFile().getPath());
    });
  }

  // ------------------------------------------------------------------------------------------------ defect 1

  /**
   * P5: creating, renaming or deleting a file that no include resolves to rebuilds no copy. A file event that changes
   * what an include resolves to (a header appears, disappears, or an application directory starts to shadow it)
   * rebuilds only the copies of the files whose includes change.
   */
  public void testFileEventsRebuildOnlyTheCopiesWhoseIncludesChange() throws Exception {
    myFixture.addFileToProject("d1/h.hrl", "-define(H_BIND(V), V = 42).\n");
    PsiFile a = myFixture.addFileToProject("d1/a.erl", "-module(a).\n-include(\"h.hrl\").\n-export([f/0]).\nf() -> ?H_BIND(Z), Z.\n");
    PsiFile late = myFixture.addFileToProject("d1/late.erl", "-module(late).\n-include(\"late.hrl\").\n-export([f/0]).\nf() -> ?L_BIND(Q), Q.\n");
    myFixture.addFileToProject("d1/lib1/include/s.hrl", "-define(S_BIND(V), V = 1).\n");
    PsiFile c = myFixture.addFileToProject("d1/c.erl", "-module(c).\n-include_lib(\"lib1/include/s.hrl\").\n-export([f/0]).\nf() -> ?S_BIND(Y), {Y, W}.\n");
    PsiFile b = myFixture.addFileToProject("d1/b.erl", "-module(b).\n-define(B(X), X = 1).\n-export([g/0]).\ng() -> ?B(Q), Q.\n");
    highlight(a, late, c, b);
    assertEquals("ErlangQVarImpl@/src/d1/a.erl", resolved(a, "Z.\n"));
    assertEquals("null", resolved(late, "Q.\n"));
    assertEquals("null", resolved(c, "W}"));
    int b0 = builds().size();
    out("D1 initial " + reasons());

    // 1. files that no include resolves to: created, renamed, moved, deleted; also a header with another name
    VirtualFile junk = myFixture.addFileToProject("junk/unrelated.erl", "-module(unrelated).\n").getVirtualFile();
    myFixture.addFileToProject("junk/other.hrl", "-define(OTHER, 1).\n");
    myFixture.addFileToProject("d1/sub/x.erl", "-module(x).\n");
    VirtualFile gone = myFixture.addFileToProject("junk/gone/y.erl", "-module(y).\n").getVirtualFile().getParent();
    WriteAction.run(() -> {
      junk.rename(this, "unrelated2.erl");
      VirtualFile dir = junk.getParent().createChildDirectory(this, "moved");
      junk.move(this, dir); // the platform clears the caches of every PSI file: their modification stamps move
      junk.delete(this);
      gone.delete(this); // a directory with a file: the same
    });
    highlight(a, late, c, b);
    int b1 = builds().size();
    out("D1 unrelated file events: rebuilds=" + (b1 - b0) + " " + reasons());
    assertEquals("unrelated file events rebuilt copies: " + reasons(), 0, b1 - b0);

    // 2. a file with the name of an included header appears where no include looks: nothing changes
    myFixture.addFileToProject("junk/h.hrl", "-define(H_BIND(V), V = 0, Extra = 1).\n");
    myFixture.addFileToProject("apps/lib1/include/s.hrl", "-define(S_BIND(V), V = 2, W = 3).\n");
    highlight(a, late, c, b);
    int b2 = builds().size();
    out("D1 same names elsewhere: rebuilds=" + (b2 - b1) + " " + reasons());
    assertEquals(0, b2 - b1);

    // 3. the missing header of late.erl appears: only late.erl gets a (first) copy
    myFixture.addFileToProject("d1/late.hrl", "-define(L_BIND(V), V = 1).\n");
    highlight(a, late, c, b);
    int b3 = builds().size();
    out("D1 late.hrl appears: builds=" + (b3 - b2) + " " + reasons());
    assertEquals(1, b3 - b2);
    assertEquals(1, buildsOf("late.erl"));
    assertEquals("ErlangQVarImpl@/src/d1/late.erl", resolved(late, "Q.\n"));

    // 4. an application directory lib1 appears: c.erl's include_lib now resolves to apps/lib1/include/s.hrl
    myFixture.addFileToProject("apps/lib1/ebin/lib1.app", "{application, lib1, [{vsn, \"1\"}]}.\n");
    highlight(a, late, c, b);
    int b4 = builds().size();
    out("D1 lib1 application appears: builds=" + (b4 - b3) + " " + reasons());
    assertEquals(1, b4 - b3);
    assertEquals(2, buildsOf("c.erl"));
    assertEquals("ErlangMacroLayerHeaderTarget@/src/apps/lib1/include/s.hrl", resolved(c, "W}"));

    // 5. the header of a.erl is renamed away: a.erl falls back to master (no copy); then it comes back
    VirtualFile h = myFixture.findFileInTempDir("d1/h.hrl");
    WriteAction.run(() -> h.rename(this, "h_old.hrl"));
    highlight(a, late, c, b);
    int b5 = builds().size();
    out("D1 h.hrl renamed away: builds=" + (b5 - b4) + " " + reasons());
    assertEquals(0, b5 - b4);
    assertEquals("null", resolved(a, "Z.\n"));
    WriteAction.run(() -> h.rename(this, "h.hrl"));
    highlight(a, late, c, b);
    int b6 = builds().size();
    out("D1 h.hrl renamed back: builds=" + (b6 - b5) + " " + reasons());
    assertEquals(1, b6 - b5);
    assertEquals("ErlangQVarImpl@/src/d1/a.erl", resolved(a, "Z.\n"));
    assertEquals(1, buildsOf("b.erl"));
  }
}
