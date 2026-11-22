package org.intellij.erlang.macrolayer;

import com.intellij.testFramework.EdtTestUtil;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import com.intellij.testFramework.fixtures.CompletionAutoPopupTester;
import com.intellij.util.ThrowableRunnable;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * Round 6 (next), N1 (from the real IDE): the auto-popup after a fragment use, through the platform's real auto-popup
 * path (typed key, completion in the background). It must get the copy's items at once (building the copy under the
 * completion's own indicator, which the next key cancels), not wait for the daemon's copy.
 */
public class ErlangMacroLayerAutoPopupTest extends BasePlatformTestCase {
  private CompletionAutoPopupTester myTester;

  @Override
  protected boolean runInDispatchThread() {
    return false;
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myTester = new CompletionAutoPopupTester(myFixture);
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

  @Override
  protected void runTestRunnable(@NotNull ThrowableRunnable<Throwable> testRunnable) throws Throwable {
    myTester.runWithAutoPopupEnabled(testRunnable);
  }

  public void testAutoPopupAfterAFragmentUse() {
    EdtTestUtil.runInEdtAndWait(() -> {
      myFixture.configureByText("ap.erl", "-module(ap).\n-export([f/2, foo/0]).\n-define(PAR, ()).\nfoo() -> ok.\n" +
                                          "f(Abc1, Abc2) ->\n  foo?PAR,\n  X2 = A<caret>\n");
      myFixture.doHighlighting();
    });
    long t0 = System.nanoTime();
    myTester.typeWithPauses("b");
    myTester.joinAutopopup();
    myTester.joinCompletion();
    long ms = (System.nanoTime() - t0) / 1_000_000;
    List<String> items = EdtTestUtil.runInEdtAndGet(() -> myFixture.getLookupElementStrings());
    long waits = ErlangMacroLayerLog.events().stream().filter(e -> e.kind().equals("WAITED")).count();
    System.out.println("LAYER-AUTOPOPUP items " + items + " after " + ms + " ms, waits " + waits + ", builds " +
                       ErlangMacroLayerLog.events().stream().filter(e -> e.kind().equals("BUILD")).map(e -> e.trigger()).toList());
    assertNotNull("the popup is shown", items);
    assertTrue(String.valueOf(items), items.containsAll(List.of("Abc1", "Abc2")));
    assertEquals("the auto-popup did not wait for the daemon's copy", 0, waits);
  }
}
