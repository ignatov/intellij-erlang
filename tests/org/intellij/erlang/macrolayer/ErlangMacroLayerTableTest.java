package org.intellij.erlang.macrolayer;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Future;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 5, fix 5: the table of a file is checked against its header trackers once per change, not on every question,
 * and computed once per edit however many threads ask at the same time.
 */
public class ErlangMacroLayerTableTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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

  private PsiFile includer() {
    myFixture.addFileToProject("tt/h1.hrl", "-define(H1(X), X = 1).\n-include(\"h2.hrl\").\n");
    myFixture.addFileToProject("tt/h2.hrl", "-define(H2, ok).\n");
    myFixture.addFileToProject("tt/h3.hrl", "-define(H3(A), {A}).\n");
    return myFixture.addFileToProject("tt/a.erl", "-module(a).\n-include(\"h1.hrl\").\n-include(\"h3.hrl\").\n-export([f/0]).\n" +
                                                  "f() -> ?H1(Z), {Z, ?H2, ?H3(Z)}.\n");
  }

  public void testNoHeaderCheckWhileNothingChanges() throws Exception {
    PsiFile f = includer();
    inBackground(() -> ErlangMacroLayer.table((ErlangFile)f));
    long checks0 = ErlangMacroLayerRetention.DIRECTIVE_CHECKS.get();
    inBackground(() -> {
      for (int i = 0; i < 1000; i++) assertNotNull(ErlangMacroLayer.table((ErlangFile)f));
      return null;
    });
    assertEquals("header tracker checks for 1,000 questions without a change", 0, ErlangMacroLayerRetention.DIRECTIVE_CHECKS.get() - checks0);
    // a change elsewhere: the trackers are asked once, and the table stays
    int tables0 = ErlangMacroLayer.TABLES_BUILT.get();
    PsiFile other = myFixture.addFileToProject("tt/other.erl", "-module(other).\n");
    edit(other, "%% x\n");
    long checks1 = ErlangMacroLayerRetention.DIRECTIVE_CHECKS.get();
    inBackground(() -> {
      for (int i = 0; i < 1000; i++) assertNotNull(ErlangMacroLayer.table((ErlangFile)f));
      return null;
    });
    assertEquals("one check per header after a change elsewhere", 3, ErlangMacroLayerRetention.DIRECTIVE_CHECKS.get() - checks1);
    assertEquals("no new table after a change elsewhere", tables0, ErlangMacroLayer.TABLES_BUILT.get());
  }

  public void testOneTableBuildPerEditForConcurrentAskers() throws Exception {
    ErlangMacroLayer.setQuietMillisForTests(0, getTestRootDisposable()); // the askers come after the user paused
    PsiFile f = includer();
    inBackground(() -> ErlangMacroLayer.table((ErlangFile)f));
    for (int round = 0; round < 5; round++) {
      edit(f, "%% edit " + round + "\n");
      int tables0 = ErlangMacroLayer.TABLES_BUILT.get();
      CountDownLatch start = new CountDownLatch(1);
      List<Future<?>> futures = new ArrayList<>();
      for (int i = 0; i < 8; i++) {
        futures.add(ApplicationManager.getApplication().executeOnPooledThread(() -> {
          start.await();
          return ProgressManager.getInstance().runProcess(() -> ReadAction.compute(() -> ErlangMacroLayer.table((ErlangFile)f)),
                                                          new EmptyProgressIndicator());
        }));
      }
      start.countDown();
      for (Future<?> x : futures) assertNotNull(x.get());
      assertEquals("table builds for 8 threads after edit " + round, 1, ErlangMacroLayer.TABLES_BUILT.get() - tables0);
    }
  }

  private void edit(PsiFile f, String text) {
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(f);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getTextLength(), text));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
  }
}
