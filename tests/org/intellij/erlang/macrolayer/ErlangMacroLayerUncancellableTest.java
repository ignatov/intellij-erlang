package org.intellij.erlang.macrolayer;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 5, fix 6: a caller that cannot be cancelled (a plain read action with no progress indicator, or a
 * non-cancellable section) neither builds a copy nor waits for another thread's build: it gets the copy that is there,
 * or master's answer. A key press waits for every running read action, so such a build would hold the key press.
 */
public class ErlangMacroLayerUncancellableTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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

  private static String lastZ(PsiFile f) {
    PsiReference ref = f.findReferenceAt(f.getText().lastIndexOf("Z."));
    PsiElement t = ref == null ? null : ref.resolve();
    return t == null ? "null" : String.valueOf(t.getTextOffset());
  }

  public void testUncancellableCallersNeitherBuildNorWait() throws Exception {
    PsiFile f = myFixture.addFileToProject("u.erl", "-module(u).\n-define(B(X), X = 1).\n-export([f/0]).\nf() -> ?B(Z), Z.\n");
    String binding = String.valueOf(f.getText().indexOf("Z)"));

    assertEquals("no indicator: master's answer", "null", inBackgroundWithoutIndicator(() -> lastZ(f)));
    assertEquals("non-cancellable section: master's answer", "null", inBackgroundWithIndicator(
      () -> ProgressManager.getInstance().computeInNonCancelableSection(() -> lastZ(f))));
    assertEquals("no build for them", 0, builds().size());
    assertEquals(2, count("SKIPPED"));

    // a cancellable caller builds; while its build runs, an uncancellable caller does not wait for it
    CountDownLatch inBuild = new CountDownLatch(1), release = new CountDownLatch(1);
    ErlangMacroLayer.setBuildHookForTests(() -> {
      inBuild.countDown();
      try {
        release.await(30, TimeUnit.SECONDS);
      }
      catch (InterruptedException e) {
        Thread.currentThread().interrupt();
      }
    }, getTestRootDisposable());
    Future<String> builder = ApplicationManager.getApplication().executeOnPooledThread(
      () -> ProgressManager.getInstance().runProcess(() -> ReadAction.compute(() -> lastZ(f)), new EmptyProgressIndicator()));
    assertTrue(inBuild.await(30, TimeUnit.SECONDS));
    long t0 = System.nanoTime();
    assertEquals("during another thread's build: master's answer at once", "null", inBackgroundWithoutIndicator(() -> lastZ(f)));
    long waitedMs = (System.nanoTime() - t0) / 1_000_000;
    assertTrue("the uncancellable caller waited " + waitedMs + " ms", waitedMs < 5_000);
    release.countDown();
    assertEquals(binding, builder.get(30, TimeUnit.SECONDS));
    assertEquals(1, builds().size());

    assertEquals("the copy that is there serves an uncancellable caller", binding, inBackgroundWithoutIndicator(() -> lastZ(f)));
    assertEquals(1, builds().size());
  }
}
