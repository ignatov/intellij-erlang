package org.intellij.erlang.macrolayer;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.LongAdder;

/**
 * The build log of the macro layer.
 * <p>
 * Switch: system property {@code erlang.macro.layer.log}, else the environment variable {@code ERLANG_MACRO_LAYER_LOG}.
 * {@code on}: every event goes to idea.log at INFO level (category {@code #org.intellij.erlang.macrolayer.ErlangMacroLayerLog}).
 * Any other value is a file path: one tab-separated line per event is appended to that file.
 * Events: {@code BUILD} (one copy build: file, thread, UI thread or not, duration, source and expanded tokens,
 * the reason of the cache miss, the trigger and a stack summary), {@code SUMMARY} (one header summary made by lexing),
 * {@code EDT_MISS} (the UI thread asked and no copy was cached: master's answer), {@code CANCELLED}.
 * Oracle queries are counted per file and per kind ({@link #queries()}); they are not written one by one.
 */
public final class ErlangMacroLayerLog {
  private static final Logger LOG = Logger.getInstance(ErlangMacroLayerLog.class);

  /**
   * One event. The last three columns are the wall-clock time of the event's end and of its start
   * (epoch milliseconds; start = end - duration), and free-form details ({@code key=value} pairs separated by spaces).
   */
  public record Event(@NotNull String kind, @NotNull String file, @NotNull String thread, boolean edt, long nanos,
                      int sourceTokens, int expandedTokens, @NotNull String reason, @NotNull String trigger,
                      @NotNull String stack, long wallMillis, @NotNull String details) {
    public String line() {
      return String.join("\t", kind, file, thread, edt ? "edt" : "bgt", String.format(Locale.ROOT, "%.3f", nanos / 1e6),
                         String.valueOf(sourceTokens), String.valueOf(expandedTokens), reason, trigger, stack,
                         String.valueOf(wallMillis), String.format(Locale.ROOT, "%.1f", wallMillis - nanos / 1e6), details);
    }
  }

  private static final String SETTING = read();

  static {
    // When the log is a file, the entry-point counters go to "<log>.stats.tsv" at JVM exit
    if (SETTING != null && !"on".equalsIgnoreCase(SETTING)) {
      try {
        Runtime.getRuntime().addShutdownHook(new Thread(() -> writeStats(Paths.get(SETTING + ".stats.tsv")), "macro layer stats"));
      }
      catch (RuntimeException ignored) {
      }
    }
  }

  /** Writes the entry-point counters ({@link ErlangMacroLayerStats#summary()}) to a file. */
  public static void writeStats(@NotNull Path file) {
    try {
      Files.writeString(file, ErlangMacroLayerStats.summary(), StandardCharsets.UTF_8);
    }
    catch (IOException | RuntimeException ex) {
      LOG.debug(ex);
    }
  }
  private static volatile boolean ourTestEnabled;
  private static final ConcurrentLinkedQueue<Event> EVENTS = new ConcurrentLinkedQueue<>();
  private static final AtomicInteger EVENT_COUNT = new AtomicInteger();
  private static final int MAX_KEPT = 200_000;
  private static final Map<String, AtomicInteger> QUERIES = new ConcurrentHashMap<>();

  private ErlangMacroLayerLog() {
  }

  private static @Nullable String read() {
    String v = System.getProperty("erlang.macro.layer.log");
    if (v == null || v.isEmpty()) v = System.getenv("ERLANG_MACRO_LAYER_LOG");
    return v == null || v.isBlank() || "off".equalsIgnoreCase(v.trim()) ? null : v.trim();
  }

  public static boolean isEnabled() {
    return SETTING != null || ourTestEnabled;
  }

  @TestOnly
  public static void enableForTests(boolean on) {
    ourTestEnabled = on;
  }

  @TestOnly
  public static void reset() {
    EVENTS.clear();
    EVENT_COUNT.set(0);
    QUERIES.clear();
    QUERY_NANOS.reset();
    ErlangMacroLayerStats.reset();
  }

  @TestOnly
  public static @NotNull List<Event> events() {
    return new ArrayList<>(EVENTS);
  }

  /** Oracle queries, key "kind file". */
  public static @NotNull Map<String, Integer> queries() {
    Map<String, Integer> m = new TreeMap<>();
    QUERIES.forEach((k, v) -> m.put(k, v.get()));
    return m;
  }

  private static final LongAdder QUERY_NANOS = new LongAdder();

  /** Time spent answering oracle questions in a built copy (builds excluded). */
  public static long queryNanosTotal() {
    return QUERY_NANOS.sum();
  }

  public static void queryNanos(long nanos) {
    if (isEnabled()) QUERY_NANOS.add(nanos);
  }

  public static void query(@NotNull String kind, @NotNull String file) {
    if (!isEnabled()) return;
    QUERIES.computeIfAbsent(kind + " " + file, k -> new AtomicInteger()).incrementAndGet();
  }

  public static void event(@NotNull String kind, @NotNull String file, long nanos, int sourceTokens, int expandedTokens,
                           @NotNull String reason, @NotNull String trigger) {
    event(kind, file, nanos, sourceTokens, expandedTokens, reason, trigger, "");
  }

  public static void event(@NotNull String kind, @NotNull String file, long nanos, int sourceTokens, int expandedTokens,
                           @NotNull String reason, @NotNull String trigger, @NotNull String details) {
    if (!isEnabled()) return;
    Thread t = Thread.currentThread();
    boolean edt = ApplicationManager.getApplication() != null && ApplicationManager.getApplication().isDispatchThread();
    Event e = new Event(kind, file, t.getName(), edt, nanos, sourceTokens, expandedTokens, reason, trigger, stackSummary(),
                        System.currentTimeMillis(), details);
    if (EVENT_COUNT.incrementAndGet() <= MAX_KEPT) EVENTS.add(e);
    String s = SETTING;
    if (s == null) return;
    if ("on".equalsIgnoreCase(s)) {
      LOG.info("macro layer " + e.line());
      return;
    }
    try {
      Path p = Paths.get(s);
      synchronized (ErlangMacroLayerLog.class) {
        Files.writeString(p, e.line() + "\n", StandardCharsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.APPEND);
      }
    }
    catch (IOException | RuntimeException ex) {
      LOG.debug(ex);
    }
  }

  /** The first frames outside the layer, the caches and the JDK: who asked. */
  private static @NotNull String stackSummary() {
    StringBuilder sb = new StringBuilder();
    int n = 0;
    for (StackTraceElement f : new Throwable().getStackTrace()) {
      String c = f.getClassName();
      if (c.startsWith("org.intellij.erlang.macrolayer.") || c.startsWith("java.") || c.startsWith("jdk.") ||
          c.startsWith("com.intellij.psi.util.") || c.startsWith("com.intellij.util.CachedValue") ||
          c.startsWith("com.intellij.psi.impl.source.resolve.") || c.startsWith("com.intellij.openapi.util.") ||
          c.startsWith("com.intellij.psi.impl.PsiCachedValue") || c.contains("Computable") || c.contains("CachingReference") ||
          c.startsWith("com.intellij.openapi.application.") || c.startsWith("com.intellij.util.concurrency.") ||
          c.startsWith("kotlin")) {
        continue;
      }
      if (sb.length() > 0) sb.append(" < ");
      sb.append(c.substring(c.lastIndexOf('.') + 1)).append('.').append(f.getMethodName());
      if (++n == 6) break;
    }
    return sb.toString();
  }
}
