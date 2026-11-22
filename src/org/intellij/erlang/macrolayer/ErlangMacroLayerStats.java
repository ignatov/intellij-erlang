package org.intellij.erlang.macrolayer;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.LongAccumulator;
import java.util.concurrent.atomic.LongAdder;

/**
 * Counters of the macro layer's entry points, for measurement only. Active only when the build log is on
 * ({@link ErlangMacroLayerLog#isEnabled()}); when it is off every method returns at once after one volatile read.
 * <p>
 * Each call of an entry point (the resolve hook, the variable hook, the error filter, the header target, ...) is counted
 * per file, entry, outcome (which gate stopped it, or how it was answered) and caller kind (the UI thread, the
 * highlighting daemon, another progress indicator, none). Time is counted only for the outermost entry of a thread
 * (a resolve inside the copy is a nested entry: counted, not timed), and copy builds and lock waits are counted
 * separately and subtracted from the entry that caused them, so "exclusive" is the time of the gate and the query.
 * Every 64th outermost call per entry also records the first stack frame outside the layer and the resolve machinery
 * (the consumer) in a sampled histogram.
 */
public final class ErlangMacroLayerStats {
  private ErlangMacroLayerStats() {
  }

  static final class Cell {
    final LongAdder calls = new LongAdder();
    final LongAdder nanos = new LongAdder();
    final LongAdder exclusive = new LongAdder();
    final LongAccumulator max = new LongAccumulator(Math::max, 0);
  }

  private static final Map<String, Cell> CELLS = new ConcurrentHashMap<>();
  private static final Map<String, LongAdder> CALLERS = new ConcurrentHashMap<>();
  private static final Map<String, AtomicLong> SAMPLE_TICKS = new ConcurrentHashMap<>();

  /** Per thread: [0] nesting depth of entries, [1] nanos of builds and lock waits inside the current outermost entry. */
  private static final ThreadLocal<long[]> STATE = ThreadLocal.withInitial(() -> new long[2]);

  public static boolean on() {
    return ErlangMacroLayerLog.isEnabled();
  }

  /** Starts an entry. Returns 0 when off. */
  public static long enter() {
    if (!on()) return 0;
    long[] s = STATE.get();
    s[0]++;
    if (s[0] == 1) s[1] = 0;
    return System.nanoTime();
  }

  /** Ends an entry started by {@link #enter()}. */
  public static void exit(@NotNull String entry, @NotNull String outcome, @Nullable String file, long t0) {
    if (t0 == 0 || !on()) return;
    long dt = System.nanoTime() - t0;
    long[] s = STATE.get();
    boolean outer = s[0] == 1;
    s[0] = Math.max(0, s[0] - 1);
    String caller = callerKind();
    String key = (file == null ? "-" : file) + "\t" + entry + "\t" + (outer ? outcome : "nested:" + outcome) + "\t" + caller;
    Cell c = CELLS.computeIfAbsent(key, k -> new Cell());
    c.calls.increment();
    if (outer) {
      c.nanos.add(dt);
      c.exclusive.add(Math.max(0, dt - s[1]));
      c.max.accumulate(dt);
      sampleCaller(entry);
    }
  }

  /** A copy build or a lock wait inside the current entry (its time is taken out of the entry's exclusive time). */
  static void inner(@NotNull String kind, @Nullable String file, long nanos) {
    if (!on()) return;
    long[] s = STATE.get();
    if (s[0] > 0) s[1] += nanos;
    String key = (file == null ? "-" : file) + "\t" + kind + "\t-\t" + callerKind();
    Cell c = CELLS.computeIfAbsent(key, k -> new Cell());
    c.calls.increment();
    c.nanos.add(nanos);
    c.exclusive.add(nanos);
    c.max.accumulate(nanos);
  }

  /** A plain counted phase (build phases, table, summary): not subtracted from anything. */
  static void phase(@NotNull String kind, @Nullable String file, long nanos) {
    if (!on()) return;
    String key = (file == null ? "-" : file) + "\t" + kind + "\t-\t" + callerKind();
    Cell c = CELLS.computeIfAbsent(key, k -> new Cell());
    c.calls.increment();
    c.nanos.add(nanos);
    c.exclusive.add(nanos);
    c.max.accumulate(nanos);
  }

  private static @NotNull String callerKind() {
    if (ApplicationManager.getApplication() != null && ApplicationManager.getApplication().isDispatchThread()) return "edt";
    ProgressIndicator pi = ProgressManager.getGlobalProgressIndicator();
    if (pi == null) return "no-indicator";
    String n = pi.getClass().getSimpleName();
    if (n.contains("Daemon")) return "daemon";
    return n.isEmpty() ? pi.getClass().getName() : n;
  }

  private static void sampleCaller(@NotNull String entry) {
    long tick = SAMPLE_TICKS.computeIfAbsent(entry, k -> new AtomicLong()).getAndIncrement();
    if ((tick & 63) != 0) return;
    CALLERS.computeIfAbsent(entry + "\t" + consumer(), k -> new LongAdder()).increment();
  }

  /** The first frame outside the layer, the reference and resolve machinery and the JDK: the consumer. */
  private static @NotNull String consumer() {
    for (StackTraceElement f : new Throwable().getStackTrace()) {
      String c = f.getClassName();
      if (c.startsWith("org.intellij.erlang.macrolayer.") || c.startsWith("java.") || c.startsWith("jdk.") ||
          c.startsWith("kotlin") || c.startsWith("com.intellij.psi.impl.source.resolve.") ||
          c.startsWith("com.intellij.psi.util.") || c.startsWith("com.intellij.openapi.util.") ||
          c.startsWith("com.intellij.util.") || c.startsWith("com.intellij.psi.PsiReference") ||
          c.startsWith("com.intellij.psi.impl.PsiCachedValue") ||
          c.startsWith("org.intellij.erlang.psi.impl.ErlangPsiPolyVariantCachingReferenceBase") ||
          c.startsWith("org.intellij.erlang.psi.impl.ErlangVariableReferenceImpl") ||
          c.startsWith("org.intellij.erlang.psi.impl.ErlangPsiImplUtil") ||
          c.startsWith("com.intellij.codeInsight.highlighting.HighlightErrorFilter") ||
          c.startsWith("com.intellij.openapi.application.")) {
        continue;
      }
      return c.substring(c.lastIndexOf('.') + 1) + "." + f.getMethodName();
    }
    return "?";
  }

  public static void reset() {
    CELLS.clear();
    CALLERS.clear();
    SAMPLE_TICKS.clear();
  }

  /** Rows "file entry outcome caller" to {calls, nanos, exclusive nanos, max nanos}. */
  public static @NotNull Map<String, long[]> snapshot() {
    Map<String, long[]> m = new TreeMap<>();
    CELLS.forEach((k, c) -> m.put(k, new long[]{c.calls.sum(), c.nanos.sum(), c.exclusive.sum(), c.max.get()}));
    return m;
  }

  /** Sampled consumers, "entry consumer" to samples (1 sample per 64 outermost calls of the entry). */
  public static @NotNull Map<String, Long> callers() {
    Map<String, Long> m = new TreeMap<>();
    CALLERS.forEach((k, v) -> m.put(k, v.sum()));
    return m;
  }

  /** A readable table: one line per row, sorted by file, then by time. */
  public static @NotNull String summary() {
    StringBuilder sb = new StringBuilder();
    sb.append("file\tentry\toutcome\tcaller\tcalls\tms\texclusive_ms\tmax_ms\n");
    snapshot().entrySet().stream()
      .sorted((a, b) -> {
        String fa = a.getKey().substring(0, a.getKey().indexOf('\t')), fb = b.getKey().substring(0, b.getKey().indexOf('\t'));
        int c = fa.compareTo(fb);
        return c != 0 ? c : Long.compare(b.getValue()[1], a.getValue()[1]);
      })
      .forEach(e -> sb.append(e.getKey()).append('\t').append(e.getValue()[0]).append('\t')
        .append(String.format(Locale.ROOT, "%.3f\t%.3f\t%.3f", e.getValue()[1] / 1e6, e.getValue()[2] / 1e6, e.getValue()[3] / 1e6))
        .append('\n'));
    sb.append("-- sampled consumers (1 per 64 outermost calls)\n");
    callers().forEach((k, v) -> sb.append(k).append('\t').append(v).append('\n'));
    return sb.toString();
  }
}
