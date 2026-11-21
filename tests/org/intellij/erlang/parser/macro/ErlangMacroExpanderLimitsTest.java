package org.intellij.erlang.parser.macro;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.util.ProgressIndicatorBase;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Expansion;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.IncludeResolver;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Outcome;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.Nullable;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * The expander stops a macro expansion that would run for minutes or fill the memory, with a limit outcome, and stops
 * at once when its indicator is cancelled (it runs inside a read action).
 */
public class ErlangMacroExpanderLimitsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final long BOUND_MS = 10_000;

  /** {@code -define(E, ). -define(L1, ?E ?E). ... -define(Ln, ?Ln-1 ?Ln-1).}: 2^n expansions with no output token. */
  static String emptyChain(int n, int uses) {
    StringBuilder sb = new StringBuilder("-module(m).\n-define(E, ).\n-define(L1, ?E ?E).\n");
    for (int i = 2; i <= n; i++) sb.append("-define(L").append(i).append(", ?L").append(i - 1).append(" ?L").append(i - 1).append(").\n");
    for (int u = 0; u < uses; u++) sb.append("f").append(u).append("() -> ?L").append(n).append(".\n");
    return sb.toString();
  }

  private Expansion expandWithin(String text, @Nullable IncludeResolver resolver) throws Exception {
    ProgressIndicatorBase indicator = new ProgressIndicatorBase();
    Future<Expansion> f = ApplicationManager.getApplication().executeOnPooledThread(
      () -> ProgressManager.getInstance().runProcess(() -> ErlangMacroExpander.expand(text, new ErlangLexer(getProject()), 4000, resolver), indicator));
    try {
      return f.get(BOUND_MS, TimeUnit.MILLISECONDS);
    }
    catch (TimeoutException e) {
      indicator.cancel();
      throw new AssertionError("the expansion did not end within " + BOUND_MS + " ms");
    }
  }

  /** B1: 2^30 expansions of empty bodies end with a limit outcome within seconds. */
  public void testExponentialEmptyExpansionStops() throws Exception {
    Expansion x = expandWithin(emptyChain(30, 1), null);
    assertEquals(Integer.valueOf(1), x.outcomes.get(Outcome.LIMIT_TOKENS));
    assertNull(x.outcomes.get(Outcome.SUBSTITUTED));
    // the work limit stopped it, not luck
    assertTrue(String.valueOf(x.work), x.work > ErlangMacroExpander.MAX_WORK_PER_FORM && x.work < ErlangMacroExpander.MAX_WORK_PER_FORM + 1000);
  }

  /** B1: a cancelled indicator stops the expansion promptly, also inside one long form (here without work limits). */
  public void testCancelStopsTheExpansion() throws Exception {
    ErlangMacroExpander.setWorkLimitsForTests(Long.MAX_VALUE, Long.MAX_VALUE, getTestRootDisposable());
    ProgressIndicatorBase indicator = new ProgressIndicatorBase();
    String text = emptyChain(30, 1);
    Future<Object> f = ApplicationManager.getApplication().executeOnPooledThread(() -> {
      try {
        return ProgressManager.getInstance().runProcess(() -> ErlangMacroExpander.expand(text, new ErlangLexer(getProject()), 4000), indicator);
      }
      catch (ProcessCanceledException e) {
        return e;
      }
    });
    Thread.sleep(300);
    long t0 = System.nanoTime();
    indicator.cancel();
    Object r;
    try {
      r = f.get(3, TimeUnit.SECONDS);
    }
    catch (TimeoutException e) {
      throw new AssertionError("the expansion went on for 3 s after the cancel");
    }
    assertTrue(String.valueOf(r), r instanceof ProcessCanceledException);
    assertTrue((System.nanoTime() - t0) / 1_000_000 < 3000);
  }

  /** B2: 10,000 uses of a parameter whose argument expands to 48,400 tokens: a limit outcome, not out of memory. */
  public void testHugeSubstitutionStops() throws Exception {
    StringBuilder sb = new StringBuilder("-module(m).\n-define(A, ");
    for (int i = 0; i < 220; i++) sb.append(" x");
    sb.append(").\n-define(B, ");
    for (int i = 0; i < 220; i++) sb.append(" ?A");
    sb.append(").\n-define(T(X), X");
    for (int i = 1; i < 10_000; i++) sb.append(", X");
    sb.append(").\nf() -> ?T(?B).\n");
    Expansion x;
    try {
      x = expandWithin(sb.toString(), null);
    }
    catch (ExecutionException e) {
      throw new AssertionError("the expansion failed: " + e.getCause(), e.getCause());
    }
    assertEquals(Integer.valueOf(1), x.outcomes.get(Outcome.LIMIT_TOKENS));
    // tokens added and scanned: bounded by the limits, not hundreds of millions
    assertTrue(String.valueOf(x.work), x.work <= ErlangMacroExpander.MAX_WORK_PER_FORM + ErlangMacroExpander.MAX_TOKENS_PER_FORM);
  }
}
