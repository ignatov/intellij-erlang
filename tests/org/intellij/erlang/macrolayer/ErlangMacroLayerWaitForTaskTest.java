package org.intellij.erlang.macrolayer;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

/**
 * {@link LayerTestSupport#waitForTask} keeps the UI thread serving events while it waits. A plain {@code Future.get()}
 * on the UI thread does not, and deadlocked the tests on 2026.3 when a background write action waited for the UI thread.
 */
public class ErlangMacroLayerWaitForTaskTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testTaskThatNeedsTheUiThreadEnds() throws Exception {
    assertTrue(ApplicationManager.getApplication().isDispatchThread());
    Boolean served = LayerTestSupport.waitForTask(ApplicationManager.getApplication().executeOnPooledThread(() -> {
      CountDownLatch ran = new CountDownLatch(1);
      ApplicationManager.getApplication().invokeLater(ran::countDown, ModalityState.any());
      // bounded, so a helper that blocks the UI thread fails this test instead of hanging it
      return ran.await(10, TimeUnit.SECONDS);
    }));
    assertTrue("the UI thread did not run the event of the task while it waited", served);
  }

  public void testTaskExceptionReachesTheCaller() {
    try {
      Callable<Object> failing = () -> {
        throw new IllegalStateException("from the task");
      };
      LayerTestSupport.waitForTask(ApplicationManager.getApplication().executeOnPooledThread(failing));
      fail("no exception");
    }
    catch (Exception e) {
      assertInstanceOf(e, ExecutionException.class);
      assertEquals("from the task", e.getCause().getMessage());
    }
  }
}
