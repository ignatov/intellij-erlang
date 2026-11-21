package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.openapi.diagnostic.ControlFlowException;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.testFramework.LoggedErrorProcessor;
import com.intellij.util.ThrowableRunnable;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.*;
import java.util.concurrent.atomic.AtomicLong;

/**
 * The oracle of the fuzz suite: runs actions, catches what they throw and what they log as errors (from any thread),
 * watches the time of each action, attributes every failure to the current case and action, dedupes failures by a
 * signature (kind, exception class and first plugin frame) and writes the first cases of each signature to disk with
 * their replay command.
 */
public final class FuzzRecorder {
  /** Frames of these packages are the harness, not the plugin. */
  private static final String HARNESS = "org.intellij.erlang.macrolayer.fuzz.";
  private static final long MAX_BYTES = 50L * 1024 * 1024;
  private static final int CASES_PER_SIGNATURE = 3;

  public static final class Finding {
    public final String signature;
    public final String kind;
    public int count;
    public final String firstCase;
    public final String replay;
    public final List<String> files = new ArrayList<>();
    /** The distinct actions (offsets left out) that hit the signature. */
    public final Set<String> via = new TreeSet<>();

    Finding(String signature, String kind, String firstCase, String replay) {
      this.signature = signature;
      this.kind = kind;
      this.firstCase = firstCase;
      this.replay = replay;
    }
  }

  private final String myTestId;
  private final Path myOut;
  private final Set<String> myKnown;
  private final Map<String, Finding> myFindings = new LinkedHashMap<>();
  private final Map<String, Integer> myNoise = new TreeMap<>();
  private final AtomicLong myBytes = new AtomicLong();
  private final long myActionMillis = FuzzConfig.actionMillis();

  // the current case and action
  private volatile String myCaseId = "-";
  private volatile long myCaseSeed;
  private volatile Map<String, String> myCaseFiles = Map.of();
  private final List<String> myTrail = Collections.synchronizedList(new ArrayList<>());
  private volatile String myAction = "-";
  private volatile long myActionStart;
  private volatile Thread myActionThread;
  private volatile boolean myHangReported;
  private final List<Throwable> myLogged = Collections.synchronizedList(new ArrayList<>());
  private int myCases;
  private long myActions;
  private final Thread myWatchdog;
  private volatile boolean myStopped;

  public FuzzRecorder(@NotNull String testId) {
    myTestId = testId;
    myOut = FuzzConfig.out().resolve(testId.replaceAll("[^A-Za-z0-9_.-]", "_"));
    myKnown = readKnown();
    myWatchdog = new Thread(this::watch, "macro-fuzz-watchdog");
    myWatchdog.setDaemon(true);
    myWatchdog.start();
  }

  private static Set<String> readKnown() {
    Set<String> r = new HashSet<>();
    try {
      Path p = FuzzConfig.known();
      if (Files.isRegularFile(p)) {
        for (String line : Files.readAllLines(p, StandardCharsets.UTF_8)) {
          if (line.isBlank() || line.startsWith("#")) continue;
          int tab = line.indexOf('\t');
          r.add(tab < 0 ? line.trim() : line.substring(0, tab).trim());
        }
      }
    }
    catch (IOException ignored) {
    }
    return r;
  }

  /** Runs the whole test body with every logged error collected instead of failing the test at once. */
  public void runAll(@NotNull ThrowableRunnable<Throwable> body) throws Throwable {
    LoggedErrorProcessor processor = new LoggedErrorProcessor() {
      @Override
      public @NotNull Set<Action> processError(@NotNull String category, @NotNull String message, String @NotNull [] details,
                                               @Nullable Throwable t) {
        Throwable e = t != null ? t : new Throwable(message);
        if (t != null && !message.isEmpty() && !message.equals(t.getMessage())) e = new LoggedError(message, t);
        String sup = mySuppressed;
        if (sup != null && message.contains(sup)) return EnumSet.noneOf(Action.class);
        if (!(e instanceof ControlFlowException)) myLogged.add(e);
        return EnumSet.noneOf(Action.class);
      }
    };
    try {
      LoggedErrorProcessor.executeWith(processor, body);
    }
    finally {
      drainLogged();
      myStopped = true;
    }
  }

  private volatile String mySuppressed;

  /** Runs the action with logged errors whose message contains {@code text} left out (an expected platform assertion). */
  public boolean guardSuppressing(@NotNull String text, @NotNull String label, @NotNull Body body) {
    mySuppressed = text;
    try {
      return guard(label, () -> {
        try {
          body.run();
        }
        finally {
          com.intellij.testFramework.PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
        }
      });
    }
    finally {
      mySuppressed = null;
    }
  }

  public static final class LoggedError extends RuntimeException {
    LoggedError(String message, Throwable cause) {
      super(message, cause);
    }
  }

  public void startCase(@NotNull String caseId, long seed, @NotNull Map<String, String> files) {
    drainLogged();
    myCaseId = caseId;
    myCaseSeed = seed;
    myCaseFiles = new LinkedHashMap<>(files);
    myTrail.clear();
    myCases++;
  }

  public void setCaseFiles(@NotNull Map<String, String> files) {
    myCaseFiles = new LinkedHashMap<>(files);
  }

  public int cases() {
    return myCases;
  }

  public long actions() {
    return myActions;
  }

  public @NotNull List<String> trail() {
    synchronized (myTrail) {
      return new ArrayList<>(myTrail);
    }
  }

  public interface Body {
    void run() throws Throwable;
  }

  /** Whether the action is selected by {@code MACRO_FUZZ_ONLY}. */
  public static boolean selected(@NotNull String label) {
    String only = FuzzConfig.only();
    return only == null || label.contains(only);
  }

  /**
   * Runs one action; a throwable (other than a cancellation) and every error logged meanwhile become failures of the
   * current case. Returns whether the action completed without a failure.
   */
  public boolean guard(@NotNull String label, @NotNull Body body) {
    if (!selected(label)) return true;
    drainLogged();
    myTrail.add(label);
    myActions++;
    String previousAction = myAction;
    Thread previousThread = myActionThread;
    long previousStart = myActionStart;
    myAction = label;
    myActionThread = Thread.currentThread();
    myHangReported = false;
    myActionStart = System.currentTimeMillis();
    boolean ok = true;
    try {
      body.run();
    }
    catch (Throwable t) {
      if (!isCancellation(t)) {
        record("exception", t, label);
        ok = false;
      }
    }
    finally {
      myActionStart = previousStart;
      myActionThread = previousThread;
      myAction = previousAction;
    }
    if (drainLogged(label)) ok = false;
    return ok;
  }

  public static boolean isCancellation(Throwable t) {
    for (Throwable c = t; c != null; c = c.getCause()) {
      if (c instanceof ProcessCanceledException || c instanceof java.util.concurrent.CancellationException) return true;
      if (c == c.getCause()) break;
    }
    return false;
  }

  private void drainLogged() {
    drainLogged(myAction);
  }

  private boolean drainLogged(String label) {
    List<Throwable> logged;
    synchronized (myLogged) {
      if (myLogged.isEmpty()) return false;
      logged = new ArrayList<>(myLogged);
      myLogged.clear();
    }
    boolean any = false;
    for (Throwable t : logged) {
      if (isCancellation(t)) continue;
      record("log-error", t, label);
      any = true;
    }
    return any;
  }

  /** A failure that is not a throwable: a wrong answer, a data change, an inconsistency. */
  public void fail(@NotNull String kind, @NotNull String detail, @NotNull String message) {
    Throwable t = new Throwable(kind + ": " + message);
    String sig = kind + ": " + detail;
    add(sig, kind, t, myAction, true);
  }

  private void record(String kind, Throwable t, String label) {
    String frame = pluginFrames(t);
    Throwable root = t instanceof LoggedError && t.getCause() != null ? t.getCause() : t;
    String exClass = root.getClass().getSimpleName();
    String sig;
    boolean plugin = frame != null || mentionsPlugin(t);
    // an element made invalid on purpose may be refused with the platform's own exception
    if (label.contains("[invalid ") && root instanceof com.intellij.psi.PsiInvalidElementAccessException) plugin = false;
    if (kind.equals("log-error")) {
      String msg = t.getMessage() == null ? "" : t.getMessage();
      sig = "log-error " + firstWords(msg, 2) + classNames(msg) + (t instanceof LoggedError || t.getClass() != Throwable.class ? " (" + exClass + ")" : "")
            + " @ " + (frame != null ? frame : firstFrame(t));
    }
    else {
      sig = "exception " + exClass + " @ " + (frame != null ? frame : firstFrame(t));
    }
    add(sig, kind, t, label, plugin);
  }

  private static boolean mentionsPlugin(Throwable t) {
    for (Throwable c = t; c != null; c = c.getCause()) {
      String m = c.getMessage();
      if (m != null && (m.contains("org.intellij.erlang") || m.contains("Erlang"))) return true;
      if (c == c.getCause()) break;
    }
    return false;
  }

  /** The simple names of PSI and location classes a message names (they tell root causes apart). */
  private static String classNames(String msg) {
    java.util.regex.Matcher m = java.util.regex.Pattern.compile("\\b([A-Z][A-Za-z0-9]*(?:Impl|Target))\\b").matcher(msg);
    Set<String> names = new LinkedHashSet<>();
    while (m.find()) names.add(m.group(1));
    return names.isEmpty() ? "" : " " + String.join(",", names);
  }

  private static String firstWords(String msg, int n) {
    String[] w = msg.replaceAll("[0-9]+", "#").trim().split("\\s+");
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < Math.min(n, w.length); i++) {
      if (i > 0) sb.append(' ');
      sb.append(w[i].length() > 40 ? w[i].substring(0, 40) : w[i]);
    }
    return sb.toString();
  }

  /** Up to three distinct plugin frames from the top of the innermost cause that has one, joined by " < ". */
  public static @Nullable String pluginFrames(Throwable t) {
    for (Throwable c = t; c != null; c = c.getCause()) {
      List<String> frames = new ArrayList<>();
      for (StackTraceElement e : c.getStackTrace()) {
        String cls = e.getClassName();
        if (cls.startsWith("org.intellij.erlang.") && !cls.startsWith(HARNESS) && !isTestClass(cls)) {
          String f = simple(cls.contains("$") ? cls.substring(0, cls.indexOf('$')) : cls) + "." + e.getMethodName();
          if (!frames.contains(f)) frames.add(f);
          if (frames.size() == 3) break;
        }
      }
      if (!frames.isEmpty()) return String.join(" < ", frames);
      if (c == c.getCause()) break;
    }
    return null;
  }

  public static @Nullable String firstPluginFrame(Throwable t) {
    for (Throwable c = t; c != null; c = c.getCause()) {
      for (StackTraceElement e : c.getStackTrace()) {
        String cls = e.getClassName();
        if (cls.startsWith("org.intellij.erlang.") && !cls.startsWith(HARNESS) && !isTestClass(cls)) {
          return simple(cls) + "." + e.getMethodName();
        }
      }
      if (c == c.getCause()) break;
    }
    return null;
  }

  private static boolean isTestClass(String cls) {
    String s = simple(cls.contains("$") ? cls.substring(0, cls.indexOf('$')) : cls);
    return s.endsWith("Test") || s.endsWith("TestCase") || s.endsWith("TestBase") || s.endsWith("TestSupport");
  }

  private static String firstFrame(Throwable t) {
    Throwable root = t;
    while (root.getCause() != null && root.getCause() != root) root = root.getCause();
    for (StackTraceElement e : root.getStackTrace()) {
      String cls = e.getClassName();
      if (cls.startsWith("java.") || cls.startsWith("jdk.") || cls.startsWith("kotlin.") || cls.contains("Logger")) continue;
      return simple(cls) + "." + e.getMethodName();
    }
    return "?";
  }

  private static String simple(String cls) {
    int dot = cls.lastIndexOf('.');
    return dot < 0 ? cls : cls.substring(dot + 1);
  }

  private synchronized void add(String sig, String kind, Throwable t, String label, boolean plugin) {
    if (!plugin) {
      String key = sig + "  [" + label.replaceAll("@[0-9]+", "") + "]";
      if (myNoise.merge(key, 1, Integer::sum) == 1 && myNoise.size() <= 200 && myBytes.get() < MAX_BYTES) {
        writeCase("noise " + sig, t, label, 0);
      }
      return;
    }
    Finding f = myFindings.get(sig);
    String caseDesc = myCaseId + " action=" + label;
    if (f == null) {
      f = new Finding(sig, kind, caseDesc, replayCommand());
      myFindings.put(sig, f);
      System.err.println("MACRO-FUZZ: new failure " + sig + " in " + caseDesc);
    }
    f.count++;
    if (f.via.size() < 40) f.via.add(label.replaceAll("@[0-9]+", "@").replaceAll("case [a-z]+:[0-9]+", "case"));
    if (f.files.size() < CASES_PER_SIGNATURE && myBytes.get() < MAX_BYTES) {
      Path p = writeCase(sig, t, label, f.files.size());
      if (p != null) f.files.add(p.toString());
    }
  }

  public String replayCommand() {
    String[] parts = myCaseId.split(":");
    String index = parts.length > 1 ? parts[parts.length - 1] : "0";
    return "MACRO_FUZZ_SEED=" + FuzzConfig.seed() + " MACRO_FUZZ_CASE=" + index +
           (FuzzConfig.inputs() != null ? " MACRO_FUZZ_INPUTS=" + FuzzConfig.inputs() : "") +
           " ./gradlew --no-daemon test --rerun --tests '" + myTestId.replace('#', '.') + "'";
  }

  private Path writeCase(String sig, Throwable t, String label, int n) {
    try {
      Files.createDirectories(myOut.resolve("cases"));
      String id = (sig.startsWith("noise ") ? "noise-" : "") + Integer.toHexString(sig.hashCode()) + "-" + n;
      Path p = myOut.resolve("cases").resolve(id + ".txt");
      StringBuilder sb = new StringBuilder();
      sb.append("signature: ").append(sig).append('\n');
      sb.append("test: ").append(myTestId).append('\n');
      sb.append("case: ").append(myCaseId).append(" seed=").append(myCaseSeed).append('\n');
      sb.append("action: ").append(label).append('\n');
      sb.append("replay: ").append(replayCommand()).append('\n');
      sb.append("trail:\n");
      List<String> trail = trail();
      if (trail.size() > 40) sb.append("  ... ").append(trail.size() - 40).append(" earlier actions\n");
      for (String s : trail.subList(Math.max(0, trail.size() - 40), trail.size())) sb.append("  ").append(s).append('\n');
      sb.append("stack:\n");
      StringWriter sw = new StringWriter();
      t.printStackTrace(new PrintWriter(sw));
      String[] lines = sw.toString().split("\n");
      for (int i = 0; i < Math.min(lines.length, 150); i++) sb.append(lines[i]).append('\n');
      for (Map.Entry<String, String> e : myCaseFiles.entrySet()) {
        String text = e.getValue();
        if (text.length() > 200_000) text = text.substring(0, 200_000) + "\n...[cut]";
        sb.append("==== file ").append(e.getKey()).append(" ====\n").append(text).append("\n==== end ====\n");
      }
      byte[] bytes = sb.toString().getBytes(StandardCharsets.UTF_8);
      myBytes.addAndGet(bytes.length);
      Files.write(p, bytes);
      return p;
    }
    catch (IOException e) {
      return null;
    }
  }

  private void watch() {
    while (!myStopped) {
      try {
        Thread.sleep(500);
      }
      catch (InterruptedException e) {
        return;
      }
      long start = myActionStart;
      Thread th = myActionThread;
      if (start == 0 || th == null || myHangReported) continue;
      long ms = System.currentTimeMillis() - start;
      if (ms > myActionMillis) {
        myHangReported = true;
        Throwable dump = new Throwable("action '" + myAction + "' runs for " + ms + " ms (limit " + myActionMillis + " ms)");
        dump.setStackTrace(th.getStackTrace());
        String frame = firstPluginFrame(dump);
        add("hang @ " + (frame != null ? frame : firstFrame(dump)) + " [" + myAction.replaceAll("@[0-9]+", "") + "]", "hang", dump,
            myAction, true);
      }
    }
  }

  public synchronized Map<String, Finding> findings() {
    return new LinkedHashMap<>(myFindings);
  }

  public synchronized List<String> unknownSignatures() {
    List<String> r = new ArrayList<>();
    for (String s : myFindings.keySet()) if (!myKnown.contains(s)) r.add(s);
    return r;
  }

  /** Writes the summary of the run and returns it. */
  public synchronized String finish(long millis) {
    myStopped = true;
    StringBuilder sb = new StringBuilder();
    sb.append(myTestId).append(": cases=").append(myCases).append(" actions=").append(myActions).append(" seconds=")
      .append(millis / 1000).append(" seed=").append(FuzzConfig.seed()).append('\n');
    for (Finding f : myFindings.values()) {
      sb.append(myKnown.contains(f.signature) ? "  known " : "  NEW   ").append(f.signature).append("  x").append(f.count)
        .append("  first: ").append(f.firstCase).append('\n');
      sb.append("        via: ").append(f.via).append('\n');
      for (String file : f.files) sb.append("        ").append(file).append('\n');
    }
    if (!myNoise.isEmpty()) {
      sb.append("  platform-only failures (no plugin frame), not counted:\n");
      int i = 0;
      for (Map.Entry<String, Integer> e : myNoise.entrySet()) {
        if (i++ >= 30) {
          sb.append("    ...\n");
          break;
        }
        sb.append("    ").append(e.getKey()).append(" x").append(e.getValue()).append('\n');
      }
    }
    try {
      Files.createDirectories(myOut);
      Files.writeString(myOut.resolve("summary.txt"), sb.toString(), StandardCharsets.UTF_8, StandardOpenOption.CREATE,
                        StandardOpenOption.TRUNCATE_EXISTING);
      Path all = myOut.getParent().resolve("runs.tsv");
      Files.writeString(all, new Date() + "\t" + myTestId + "\tcases=" + myCases + "\tactions=" + myActions + "\tfindings=" +
                             myFindings.size() + "\tseed=" + FuzzConfig.seed() + "\n", StandardCharsets.UTF_8,
                        StandardOpenOption.CREATE, StandardOpenOption.APPEND);
    }
    catch (IOException ignored) {
    }
    System.err.println("MACRO-FUZZ summary: " + sb);
    return sb.toString();
  }
}
