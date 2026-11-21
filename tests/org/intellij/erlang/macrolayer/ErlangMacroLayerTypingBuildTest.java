package org.intellij.erlang.macrolayer;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.util.Disposer;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 5, fix 2: while a file changes, a caller that is not its highlighting (the target-element search, the
 * identifier highlighting's isReferenceTo, ...) starts no copy build: it uses the copy that is there, or gets
 * master's answer. Highlighting builds; after the quiet time any caller builds again.
 */
public class ErlangMacroLayerTypingBuildTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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

  private void edit(PsiFile f, String text) {
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(f);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getTextLength(), text));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
  }

  /** The target of the last Z of f/0, from a pooled thread under a progress indicator that is not the daemon's. */
  private String resolveLastZ(PsiFile f) throws Exception {
    return inBackgroundWithIndicator(() -> {
      int at = f.getText().lastIndexOf("Z.");
      PsiReference ref = f.findReferenceAt(at);
      PsiElement t = ref == null ? null : ref.resolve();
      return t == null ? "null" : t.getTextOffset() + "";
    });
  }

  public void testOtherCallersBuildNoCopyWhileTheFileChanges() throws Exception {
    Disposable quiet = Disposer.newDisposable();
    ErlangMacroLayer.setQuietMillisForTests(600_000, quiet);
    PsiFile f = myFixture.addFileToProject("t.erl", "-module(t).\n-define(B(X), X = 1).\n-export([f/0]).\nf() -> ?B(Z), Z.\n");
    String binding = String.valueOf(f.getText().indexOf("Z)"));
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    assertEquals(1, builds().size());
    assertEquals(binding, resolveLastZ(f));

    edit(f, "%% typed\n");
    assertEquals("master's answer while the file changes", "null", resolveLastZ(f));
    assertEquals("no build by the other caller", 1, builds().size());
    assertEquals(1, count("SKIPPED"));

    myFixture.doHighlighting();
    assertEquals("highlighting builds", 2, builds().size());
    assertEquals("the copy of the highlighting serves the other caller", binding, resolveLastZ(f));
    assertEquals(2, builds().size());

    edit(f, "%% typed again\n");
    Disposer.dispose(quiet); // the user paused: the quiet time is over
    ErlangMacroLayer.setQuietMillisForTests(0, getTestRootDisposable());
    assertEquals(binding, resolveLastZ(f));
    assertEquals("after the pause any caller builds", 3, builds().size());
  }
}
