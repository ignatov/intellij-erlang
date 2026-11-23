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

  /**
   * #1537: the auto-popup offers a function that a macro makes ({@code small_order/0} from {@code ?TEST(small_order, ok)}),
   * as explicit completion does, also when the copy of the typed text is not built yet. Another module matches the prefix,
   * so the popup is shown either way.
   */
  public void testAutoPopupOffersAFunctionMadeByAMacro() {
    EdtTestUtil.runInEdtAndWait(() -> {
      myFixture.addFileToProject("ssl_manager.erl", "-module(ssl_manager).\n-export([start_link/1]).\nstart_link(_) -> ok.\n");
      myFixture.configureByText("shop_tests.erl", "-module(shop_tests).\n-export([all/0, small_order/0, big_order/0]).\n\n" +
                                                  "-define(TEST(Name, Expr), Name() -> Expr).\n\n" +
                                                  "?TEST(small_order, ok).\n?TEST(big_order, too_big).\n\n" +
                                                  "all() ->\n  small_order(),\n  big_order()<caret>.\n");
      myFixture.doHighlighting();
    });
    myTester.typeWithPauses(",\n  sma");
    myTester.joinAutopopup();
    myTester.joinCompletion();
    List<String> items = EdtTestUtil.runInEdtAndGet(() -> myFixture.getLookupElementStrings());
    assertNotNull("the popup is shown", items);
    assertTrue(String.valueOf(items), items.contains("small_order"));
  }

  /** #1537: the auto-popup after {@code ssl_manager:} offers its functions, not the small_order/0 that a macro makes here. */
  public void testAutoPopupAfterAnotherModuleOffersNoFunctionMadeByAMacro() {
    EdtTestUtil.runInEdtAndWait(() -> {
      myFixture.addFileToProject("ssl_manager.erl", "-module(ssl_manager).\n-export([start_link/1]).\nstart_link(_) -> ok.\n");
      myFixture.configureByText("shop_tests.erl", "-module(shop_tests).\n-export([all/0, small_order/0, big_order/0]).\n\n" +
                                                  "-define(TEST(Name, Expr), Name() -> Expr).\n\n" +
                                                  "?TEST(small_order, ok).\n?TEST(big_order, too_big).\n\n" +
                                                  "all() ->\n  small_order(),\n  big_order()<caret>.\n");
      myFixture.doHighlighting();
    });
    myTester.typeWithPauses(",\n  ssl_manager:s");
    myTester.joinAutopopup();
    myTester.joinCompletion();
    List<String> items = EdtTestUtil.runInEdtAndGet(() -> myFixture.getLookupElementStrings());
    assertNotNull("the popup is shown", items);
    assertTrue(String.valueOf(items), items.contains("start_link"));
    assertFalse(String.valueOf(items), items.contains("small_order"));
  }
}
