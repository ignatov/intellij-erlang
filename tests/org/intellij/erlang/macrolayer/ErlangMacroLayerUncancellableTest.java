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
 * Round 5, fix 6: a caller that cannot be cancelled (a non-cancellable section) neither builds a copy nor waits for
 * another thread's build: it gets the copy that is there, or master's answer. A key press waits for every running read
 * action, so such a build would hold the key press. Round 6: a caller with no progress indicator (a plain read action
 * on a pooled thread, or a coroutine read action such as the update of the rename action) is no longer such a caller:
 * it builds under an indicator of the layer's own that a write action cancels (ErlangMacroLayerNoIndicatorTest tests
 * that a key press does not wait for it).
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

    // round 6: the caller with no indicator moved to testACallerWithoutAnIndicatorBuilds
    assertEquals("non-cancellable section: master's answer", "null", inBackgroundWithIndicator(
      () -> ProgressManager.getInstance().computeInNonCancelableSection(() -> lastZ(f))));
    assertEquals("no build for it", 0, builds().size());
    assertEquals(1, count("SKIPPED"));

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
    assertEquals("during another thread's build: master's answer at once", "null", inBackgroundWithIndicator(
      () -> ProgressManager.getInstance().computeInNonCancelableSection(() -> lastZ(f))));
    long waitedMs = (System.nanoTime() - t0) / 1_000_000;
    assertTrue("the uncancellable caller waited " + waitedMs + " ms", waitedMs < 5_000);
    release.countDown();
    assertEquals(binding, builder.get(30, TimeUnit.SECONDS));
    assertEquals(1, builds().size());

    assertEquals("the copy that is there serves an uncancellable caller", binding, inBackgroundWithIndicator(
      () -> ProgressManager.getInstance().computeInNonCancelableSection(() -> lastZ(f))));
    assertEquals("and a caller with no indicator", binding, inBackgroundWithoutIndicator(() -> lastZ(f)));
    assertEquals(1, builds().size());
  }

  /** Round 6: a caller with no indicator gets the layer's answer; it builds under the layer's own indicator. */
  public void testACallerWithoutAnIndicatorBuilds() throws Exception {
    PsiFile f = myFixture.addFileToProject("w.erl", "-module(w).\n-define(B(X), X = 1).\n-export([f/0]).\nf() -> ?B(Z), Z.\n");
    String binding = String.valueOf(f.getText().indexOf("Z)"));
    assertEquals(binding, inBackgroundWithoutIndicator(() -> lastZ(f)));
    assertEquals(1, builds().size());
  }
}
