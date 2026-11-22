package org.intellij.erlang.macrolayer.fuzz.regress;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.Disposer;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.editor.ErlangAnnotator;
import org.intellij.erlang.macrolayer.fuzz.EditorActionProbe;
import org.intellij.erlang.macrolayer.fuzz.LayerHooks;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

/**
 * The two one-off signatures of #1461, seen once each in the fuzz runs of #1442: the platform assertion
 * "assignRangeHighlighters returned unsorted list" on two {@link ErlangAnnotator} infos at one range, and a highlighting
 * pass that changed the document with the layer flipped to off. Each method states the correct behaviour for the
 * reported shape.
 */
public class MacroFuzzOneOffFindingsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String MAIN = "-module(m).\n-include(\"h1.hrl\").\n-export([f11/1]).\n-define(F(A), A).\n" +
                                     "f11(X) ->\n    Y = ?F(X),\n    [?H0 | Y].\n" +
                                     "f12() ->\n    f11(1),\n    b:g().\n";

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/", ErlangSdkRelease.V_29_0);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    EditorActionProbe.enableInspections(myFixture);
  }

  private static int annotatorInfosAt(List<HighlightInfo> infos, int start, int end) {
    int n = 0;
    for (HighlightInfo h : infos) {
      if (h.getStartOffset() == start && h.getEndOffset() == end && ErlangAnnotator.class.equals(h.getToolId())) n++;
    }
    return n;
  }

  /**
   * Item 1: the annotator gives two infos at the range of the function name {@code f11} (an {@code ERASE_MARKER} one and
   * one with the scheme's attributes). Highlighting again after each typed text before the name (the trail
   * {@code type , @103} and {@code highlight}) must keep them at that range without a platform assertion.
   */
  public void testTwoAnnotatorInfosAtOneRangeSurviveTypingAndHighlighting() {
    myFixture.addFileToProject("h1.hrl", "-define(H0, 1 + 1).\n");
    myFixture.addFileToProject("b.erl", "-module(b).\n-export([g/0]).\ng() -> ok.\n");
    for (boolean on : new boolean[]{true, false}) {
      Disposable d = Disposer.newDisposable();
      try {
        LayerHooks.setEnabled(on, d);
        myFixture.configureByText("m" + on + ".erl", MAIN.replace("-module(m)", "-module(m" + on + ")"));
        for (String typed : new String[]{",", " ", "?", ")", ".", "\n", "foo", ","}) {
          String text = myFixture.getEditor().getDocument().getText();
          int at = text.indexOf("Y = ?F(X)");
          myFixture.getEditor().getCaretModel().moveToOffset(at);
          myFixture.type(typed);
          PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
          List<HighlightInfo> infos = myFixture.doHighlighting();
          text = myFixture.getEditor().getDocument().getText();
          int name = text.lastIndexOf("\nf11(") + 1;
          if (name > 0) {
            assertEquals("layer " + on + ", after typing '" + typed + "': ErlangAnnotator infos at f11",
                         2, annotatorInfosAt(infos, name, name + 3));
          }
        }
      }
      finally {
        Disposer.dispose(d);
      }
    }
  }

  /**
   * Item 2: the last actions of the trail with the layer off (flip to off, highlight, references, folding, an edit of
   * {@code h1.hrl}, EditorEnter, highlight). The highlighting must not change the document. The fixture checks this
   * itself, since this test does not allow highlighting to change documents.
   */
  public void testHighlightingAfterFlipToOffDoesNotChangeDocument() {
    Disposable on = Disposer.newDisposable();
    PsiFile h1 = myFixture.addFileToProject("h1.hrl", "-define(H0, 1 + 1).\n");
    myFixture.addFileToProject("b.erl", "-module(b).\n-export([g/0]).\ng() -> ok.\n");
    myFixture.configureByText("m.erl", MAIN);
    LayerHooks.setEnabled(true, on);
    myFixture.doHighlighting();
    Disposer.dispose(on);
    Disposable off = Disposer.newDisposable();
    try {
      LayerHooks.setEnabled(false, off);
      myFixture.doHighlighting();
      myFixture.getFile().findReferenceAt(myFixture.getFile().getText().indexOf("f11(1)"));
      com.intellij.codeInsight.folding.CodeFoldingManager.getInstance(getProject()).buildInitialFoldings(myFixture.getEditor());
      Document hd = PsiDocumentManager.getInstance(getProject()).getDocument(h1);
      assertNotNull(hd);
      WriteCommandAction.runWriteCommandAction(getProject(), () -> hd.setText(hd.getText() + "\n-define(ADDED1, 1).\n"));
      myFixture.getEditor().getCaretModel().moveToOffset(myFixture.getEditor().getDocument().getText().indexOf("    [?H0"));
      myFixture.performEditorAction("EditorEnter");
      Document doc = myFixture.getEditor().getDocument();
      String before = doc.getText();
      myFixture.doHighlighting();
      assertEquals("the highlighting with the layer off changed the document", before, doc.getText());
    }
    finally {
      Disposer.dispose(off);
    }
  }
}
