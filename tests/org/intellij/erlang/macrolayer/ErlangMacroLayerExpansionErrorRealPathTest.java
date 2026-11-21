package org.intellij.erlang.macrolayer;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.util.ProgressIndicatorBase;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static org.intellij.erlang.macrolayer.LayerTestSupport.descriptor;

/**
 * The expansion-error pass on the path the daemon takes: collected on one thread, computed on a pooled thread under an
 * indicator (a non-blocking read action, without the switch that lets tests build on the UI thread), applied only to
 * the text it was computed for.
 */
public class ErlangMacroLayerExpansionErrorRealPathTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String TEXT = "-module(c5).\n-export([f/0]).\n-define(A, ?B).\n-define(B, ?A).\nf() -> ?A.\n";

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    // not typing: the pass may build at once
    ErlangMacroLayer.setQuietMillisForTests(0, getTestRootDisposable());
  }

  private ErlangMacroLayerExpansionErrorAnnotator.Result annotateOnAPooledThread(ErlangMacroLayerExpansionErrorAnnotator a,
                                                                               ErlangMacroLayerExpansionErrorAnnotator.Info info) throws Exception {
    return ApplicationManager.getApplication().executeOnPooledThread(
      () -> ProgressManager.getInstance().runProcess(() -> a.doAnnotate(info), new ProgressIndicatorBase())).get(60, TimeUnit.SECONDS);
  }

  private static List<String> apply(ErlangMacroLayerExpansionErrorAnnotator a, PsiFile f, ErlangMacroLayerExpansionErrorAnnotator.Result r) {
    List<String> calls = new ArrayList<>();
    AnnotationHolder holder = (AnnotationHolder)Proxy.newProxyInstance(AnnotationHolder.class.getClassLoader(), new Class[]{AnnotationHolder.class},
      (proxy, method, args) -> {
        calls.add(method.getName());
        throw new AssertionError("an annotation was applied: " + method.getName());
      });
    try {
      a.apply(f, r, holder);
    }
    catch (AssertionError e) {
      return calls;
    }
    return calls;
  }

  public void testComputedOnAPooledThreadUnderAnIndicator() throws Exception {
    PsiFile f = myFixture.addFileToProject("rp/c5.erl", TEXT);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    ErlangMacroLayerExpansionErrorAnnotator a = new ErlangMacroLayerExpansionErrorAnnotator();
    ErlangMacroLayerExpansionErrorAnnotator.Info info = ApplicationManager.getApplication().runReadAction(
      (com.intellij.openapi.util.Computable<ErlangMacroLayerExpansionErrorAnnotator.Info>)() -> a.collectInformation(f));
    assertNotNull("collected", info);
    ErlangMacroLayerExpansionErrorAnnotator.Result r = annotateOnAPooledThread(a, info);
    assertNotNull("computed", r);
    assertEquals(1, r.problems().size());
    assertTrue(r.problems().get(0).message(), r.problems().get(0).message().contains("circular macro 'A'"));
  }

  public void testNothingAppliedAfterTheDocumentChanged() throws Exception {
    PsiFile f = myFixture.addFileToProject("rp/c6.erl", TEXT.replace("c5", "c6"));
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    ErlangMacroLayerExpansionErrorAnnotator a = new ErlangMacroLayerExpansionErrorAnnotator();
    ErlangMacroLayerExpansionErrorAnnotator.Info info = ApplicationManager.getApplication().runReadAction(
      (com.intellij.openapi.util.Computable<ErlangMacroLayerExpansionErrorAnnotator.Info>)() -> a.collectInformation(f));
    assertNotNull(info);
    ErlangMacroLayerExpansionErrorAnnotator.Result r = annotateOnAPooledThread(a, info);
    assertNotNull(r);
    assertFalse("there is something to apply", r.problems().isEmpty());
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(f);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(0, "%% changed\n"));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    assertEquals("nothing applied to a text the problems were not computed for", List.of(), apply(a, f, r));
  }
}
