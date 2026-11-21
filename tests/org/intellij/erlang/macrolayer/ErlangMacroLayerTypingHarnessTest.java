package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.codeInsight.daemon.impl.DaemonCodeAnalyzerEx;
import com.intellij.codeInsight.daemon.impl.DaemonCodeAnalyzerImpl;
import com.intellij.codeInsight.daemon.impl.EditorTracker;
import com.intellij.codeInspection.InspectionProfileEntry;
import com.intellij.codeInspection.LocalInspectionEP;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.util.ui.UIUtil;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.lang.management.GarbageCollectorMXBean;
import java.lang.management.ManagementFactory;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * Spike 5: the typing harness. It runs the real daemon (update by timer, as in the IDE) on a large file, types
 * characters 80 ms apart into the largest function, and measures the time from the last key press to the end of
 * highlighting, for several arms in one JVM, interleaved: {@code off} and {@code off2} (the layer off: master's
 * behaviour; their difference is the noise floor), {@code layer}, and ablations ({@code layer-novar},
 * {@code layer-noresolve}, {@code layer-noerr}: the layer with one consumer switched off).
 * <p>
 * Runs only when {@code S5_HARNESS} is set, so the normal {@code test} task does nothing here. The class uses the
 * layer through reflection, so the same file compiles and runs on master (there all arms are master).
 * Settings: {@code S5_FILES} (erl_types,xmerl_scan), {@code S5_TEXTS} (a,b), {@code S5_ARMS} (off,layer,off2),
 * {@code S5_ROUNDS} (10), {@code S5_CHARS} (80), {@code S5_GAP_MS} (80), {@code S5_OUT} (a TSV file of the results).
 * {@code S5_READER}: {@code nc} adds a background reader that resolves the reference at the caret every 50 ms in a plain
 * (not cancellable) read action on a pooled thread, {@code c} the same in a cancellable non-blocking read action; it
 * stands for a consumer such as the target-element search of the IDE; {@code ih} (round 5) does what the IDE's
 * highlighting of the identifier at the caret does, in a cancellable non-blocking read action: it resolves the
 * reference at the caret and asks every reference of the file with the same text whether it refers to that target
 * ({@code isReferenceTo}); {@code form} (round 5) resolves every reference of the top-level form at the caret in a
 * cancellable non-blocking read action, a stand-in for the IDE's caret-driven callers that ask the layer in a form
 * that needs the copy. An arm name may carry the reader as a suffix
 * ({@code layer+nc}); then it overrides {@code S5_READER}. {@code S5_HOG=<n>}: n threads spin during each run
 * (CPU starvation, as on a loaded host).
 */
public class ErlangMacroLayerTypingHarnessTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  static final String OTP = "/opt/homebrew/Cellar/erlang/29.1/lib/erlang/lib";
  static final String TEXT_A = "Zq = 1, ".repeat(30).substring(0, 200);
  static final String TEXT_B = ("lists:map(fun(Q) -> Q" + " + Q".repeat(60)).substring(0, 200);

  private int myN;
  private final Set<String> myCopied = new HashSet<>();
  private final List<String> myRows = new ArrayList<>();
  private Disposable myArm;
  private volatile long myLastFinished;
  private volatile FileEditor myWatched;
  private volatile int myCaret;
  private volatile long myLastStarting;
  private volatile int myStartingCount, myFinishedCount, myCanceledCount;
  private volatile boolean myReading;
  private volatile boolean myHogging;
  private final List<Long> myReaderNanos = Collections.synchronizedList(new ArrayList<>());

  public ErlangMacroLayerTypingHarnessTest() {
    super(true);
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        // S5_SDK=real: the installed OTP 29.1 as the SDK (sources of every OTP app, as in the IDE run); else the empty mock
        String home = "real".equals(System.getenv("S5_SDK")) ? "/opt/homebrew/Cellar/erlang/29.1/lib/erlang" : "testData/mockSdk-R29/";
        return ErlangSdkType.createMockSdk(home, ErlangSdkRelease.V_29_0);
      }
    };
  }

  private static String env(String name, String def) {
    String v = System.getenv(name);
    return v == null || v.isBlank() ? def : v.trim();
  }

  private static void out(String s) {
    System.out.println("S5-HARNESS " + s);
  }

  // ------------------------------------------------------------------------------------------------ the layer, by reflection

  private static Class<?> cls(String name) {
    try {
      return Class.forName("org.intellij.erlang.macrolayer." + name);
    }
    catch (ClassNotFoundException e) {
      return null;
    }
  }

  private static Object call(Class<?> c, String method, Object... args) {
    if (c == null) return null;
    try {
      for (Method m : c.getDeclaredMethods()) {
        if (m.getName().equals(method) && m.getParameterCount() == args.length) {
          m.setAccessible(true);
          return m.invoke(null, args);
        }
      }
      return null;
    }
    catch (ReflectiveOperationException e) {
      throw new RuntimeException(e);
    }
  }

  private static final Class<?> LAYER = cls("ErlangMacroLayer");
  private static final Class<?> LOG = cls("ErlangMacroLayerLog");
  private static final Class<?> STATS = cls("ErlangMacroLayerStats");

  /** Sets the arm: the layer on or off, and the ablations. */
  @SuppressWarnings({"unchecked", "rawtypes"})
  private void arm(String arm) {
    if (myArm != null) Disposer.dispose(myArm);
    myArm = Disposer.newDisposable();
    // "off" arms and arms ending in "-q" run without the layer's counters (quiet); the others count
    boolean quiet = !arm.startsWith("layer") || arm.endsWith("-q");
    if (LOG != null) call(LOG, "enableForTests", !quiet);
    if (LAYER == null) return;
    Class<Enum> mode = (Class<Enum>)cls("ErlangMacroLayer$Mode");
    call(LAYER, "setModeForTests", Enum.valueOf(mode, arm.startsWith("layer") ? "LAYER" : "OFF"), myArm);
    call(LAYER, "setAblationForTests", arm.contains("noresolve"), arm.contains("novar"), arm.contains("noerr"), myArm);
  }

  @SuppressWarnings("unchecked")
  private static Map<String, long[]> statsSnapshot() {
    Object m = call(STATS, "snapshot");
    return m == null ? Map.of() : (Map<String, long[]>)m;
  }

  @SuppressWarnings("unchecked")
  private static List<Object> events() {
    Object m = call(LOG, "events");
    return m == null ? List.of() : (List<Object>)m;
  }

  // ------------------------------------------------------------------------------------------------ fixtures

  private static final Pattern INCLUDE_LIB = Pattern.compile("-include_lib\\(\\s*\"([^/\"]+)/");

  private static Path findApp(String name) throws Exception {
    try (Stream<Path> s = Files.list(Paths.get(OTP))) {
      return s.filter(p -> p.getFileName().toString().matches(Pattern.quote(name) + "-[0-9.]+")).findFirst().orElse(null);
    }
  }

  private static String read(Path file) throws Exception {
    byte[] bytes = Files.readAllBytes(file);
    String text = new String(bytes, StandardCharsets.UTF_8);
    if (text.contains("�")) text = new String(bytes, StandardCharsets.ISO_8859_1);
    return text.replace("\r\n", "\n").replace('\r', '\n');
  }

  /** Copies an app (src and include; only include for a library), an .app file and a rebar.config, as in a project. */
  private String copyApp(String name, boolean full) throws Exception {
    String root = "apps/" + name;
    if (!myCopied.add(name)) return root;
    Path app = findApp(name);
    if (app == null) return root;
    Set<String> libs = new TreeSet<>();
    for (String dir : full ? List.of("src", "include") : List.of("include")) {
      Path d = app.resolve(dir);
      if (!Files.isDirectory(d)) continue;
      try (Stream<Path> s = Files.walk(d)) {
        for (Path p : s.filter(Files::isRegularFile).toList()) {
          String fn = p.getFileName().toString();
          if (!fn.endsWith(".erl") && !fn.endsWith(".hrl")) continue;
          String text = read(p);
          myFixture.addFileToProject(root + "/" + app.relativize(p), text);
          Matcher m = INCLUDE_LIB.matcher(text);
          while (m.find()) libs.add(m.group(1));
        }
      }
    }
    myFixture.addFileToProject(root + "/ebin/" + name + ".app", "{application, " + name + ", [{vsn, \"1\"}]}.\n");
    myFixture.addFileToProject(root + "/rebar.config", "{erl_opts, [{i, \"include\"}, {i, \"src\"}]}.\n");
    for (String lib : libs) if (!lib.equals(name)) copyApp(lib, false);
    return root;
  }

  private static void enableAll(com.intellij.testFramework.fixtures.CodeInsightTestFixture fixture) {
    List<InspectionProfileEntry> tools = new ArrayList<>();
    for (LocalInspectionEP ep : LocalInspectionEP.LOCAL_INSPECTION.getExtensionList()) {
      if (ep.implementationClass == null || !ep.implementationClass.startsWith("org.intellij.erlang.")) continue;
      if (ep.implementationClass.contains(".dialyzer.")) continue;
      tools.add(ep.instantiateTool());
    }
    fixture.enableInspections(tools.toArray(new InspectionProfileEntry[0]));
  }

  // ------------------------------------------------------------------------------------------------ the daemon

  private void pump(long ms) throws Exception {
    long end = System.currentTimeMillis() + ms;
    do {
      UIUtil.dispatchAllInvocationEvents();
      Thread.sleep(2);
    }
    while (System.currentTimeMillis() < end);
  }

  /** Waits until the daemon finished on the file; returns the time of the last daemonFinished event, or -1. */
  private long waitHighlighted(PsiFile file, long timeoutMs) throws Exception {
    DaemonCodeAnalyzerImpl d = (DaemonCodeAnalyzerImpl)DaemonCodeAnalyzer.getInstance(getProject());
    long end = System.currentTimeMillis() + timeoutMs;
    while (System.currentTimeMillis() < end) {
      PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
      UIUtil.dispatchAllInvocationEvents();
      if (d.isErrorAnalyzingFinished(file) && !d.isRunningOrPending()) {
        pump(30);
        if (d.isErrorAnalyzingFinished(file) && !d.isRunningOrPending()) return myLastFinished;
      }
      Thread.sleep(5);
    }
    return -1;
  }

  /** The start of a `Var = ...` line near the middle of the largest function, as the VM harness picks it. */
  private static int pickSpot(PsiFile f, Document doc) {
    ErlangFunction best = null;
    for (ErlangFunction fn : PsiTreeUtil.findChildrenOfType(f, ErlangFunction.class)) {
      if (best == null || fn.getTextLength() > best.getTextLength()) best = fn;
    }
    assertNotNull(best);
    int l0 = doc.getLineNumber(best.getTextRange().getStartOffset()), l1 = doc.getLineNumber(best.getTextRange().getEndOffset());
    List<Integer> cands = new ArrayList<>();
    for (int l = l0 + 1; l < l1; l++) {
      if (line(doc, l).matches("^\\s+[A-Z][A-Za-z0-9_]*\\s*=[^=].*") && line(doc, l - 1).trim().endsWith(",")) cands.add(l);
    }
    if (cands.isEmpty()) {
      for (int l = l0 + 1; l < l1; l++) if (line(doc, l - 1).trim().endsWith(",") && !line(doc, l).isBlank()) cands.add(l);
    }
    int mid = (l0 + l1) / 2;
    int l = cands.stream().min(Comparator.comparingInt(x -> Math.abs(x - mid))).orElseThrow();
    String t = line(doc, l);
    int col = t.length() - t.replaceAll("^\\s+", "").length();
    return doc.getLineStartOffset(l) + col;
  }

  private static String line(Document doc, int l) {
    return doc.getText(new TextRange(doc.getLineStartOffset(l), doc.getLineEndOffset(l)));
  }

  private static long gcMillis() {
    long n = 0;
    for (GarbageCollectorMXBean b : ManagementFactory.getGarbageCollectorMXBeans()) n += Math.max(0, b.getCollectionTime());
    return n;
  }

  private static long cpuNanos() {
    if (ManagementFactory.getOperatingSystemMXBean() instanceof com.sun.management.OperatingSystemMXBean os) return os.getProcessCpuTime();
    return 0;
  }

  // ------------------------------------------------------------------------------------------------ the run

  public void testHarness() throws Exception {
    if (System.getenv("S5_HARNESS") == null) return;
    setUpProjectSdk();
    enableAll(myFixture);
    DaemonCodeAnalyzerImpl daemon = (DaemonCodeAnalyzerImpl)DaemonCodeAnalyzer.getInstance(getProject());
    getProject().getMessageBus().connect(getTestRootDisposable()).subscribe(DaemonCodeAnalyzer.DAEMON_EVENT_TOPIC, new DaemonCodeAnalyzer.DaemonListener() {
      @Override
      public void daemonFinished(Collection<? extends FileEditor> fileEditors) {
        FileEditor w = myWatched;
        if (w != null && fileEditors.contains(w)) {
          myLastFinished = System.currentTimeMillis();
          myFinishedCount++;
        }
      }

      @Override
      public void daemonStarting(Collection<? extends FileEditor> fileEditors) {
        FileEditor w = myWatched;
        if (w != null && fileEditors.contains(w)) {
          myLastStarting = System.currentTimeMillis();
          myStartingCount++;
        }
      }

      @Override
      public void daemonCanceled(String reason, Collection<? extends FileEditor> fileEditors) {
        FileEditor w = myWatched;
        if (w != null && fileEditors.contains(w)) myCanceledCount++;
      }
    });
    String xmerl = copyApp("xmerl", true);
    String dialyzer = "apps/dialyzer";
    myFixture.addFileToProject(dialyzer + "/ebin/dialyzer.app", "{application, dialyzer, [{vsn, \"1\"}]}.\n");
    Map<String, String[]> files = new LinkedHashMap<>();
    files.put("erl_types", new String[]{OTP + "/dialyzer-6.0.3/src/erl_types.erl", dialyzer + "/src"});
    files.put("xmerl_scan", new String[]{OTP + "/xmerl-2.2/src/xmerl_scan.erl", xmerl + "/src"});
    List<String> fileKeys = List.of(env("S5_FILES", "erl_types,xmerl_scan").split(","));
    List<String> texts = List.of(env("S5_TEXTS", "a,b").split(","));
    List<String> arms = List.of(env("S5_ARMS", "off,layer,off2").split(","));
    int rounds = Integer.parseInt(env("S5_ROUNDS", "10"));
    int chars = Integer.parseInt(env("S5_CHARS", "80"));
    int gap = Integer.parseInt(env("S5_GAP_MS", "80"));
    String outFile = env("S5_OUT", null);
    out("start arms=" + arms + " files=" + fileKeys + " texts=" + texts + " rounds=" + rounds + " chars=" + chars + " gap=" + gap +
        " layerClasses=" + (LAYER != null) + " cpus=" + Runtime.getRuntime().availableProcessors());
    myRows.add("round\tfile\ttext\tarm\tfirst_hl_ms\tlast_key_to_hl_ms\ttyping_s\tmax_edt_key_ms\tcpu_ms\tgc_ms\tbuilds\tbuild_ms\tcancelled\t" +
               "lock_wait_ms\tlayer_excl_ms\tlayer_incl_ms\tload\tmax_build_ms\tmax_lock_wait_ms\tlock_waits\treader\tfinal_pass_ms\tdaemon\t" +
               "asked\tcomputed\tgated\ttable_builds\ttable_gets");
    daemon.setUpdateByTimerEnabled(true);
    // warm-up: one run per arm and file, not recorded
    for (String fk : fileKeys) {
      String[] f = files.get(fk);
      for (String arm : arms) runOne(-1, fk, f[1], read(Paths.get(f[0])), texts.getFirst(), TEXT_A.substring(0, 16), arm, gap);
    }
    myRows.subList(1, myRows.size()).clear();
    for (int round = 0; round < rounds; round++) {
      for (String fk : fileKeys) {
        String[] f = files.get(fk);
        String text = read(Paths.get(f[0]));
        for (String tk : texts) {
          String typed = (tk.equals("a") ? TEXT_A : TEXT_B).substring(0, Math.min(chars, 200));
          List<String> order = new ArrayList<>(arms);
          Collections.rotate(order, round);
          if (round % 2 == 1) Collections.reverse(order);
          for (String arm : order) runOne(round, fk, f[1], text, tk, typed, arm, gap);
        }
      }
      out("round " + round + " done load=" + load());
      if (outFile != null) Files.write(Paths.get(outFile), myRows, StandardCharsets.UTF_8);
    }
    daemon.setUpdateByTimerEnabled(false);
    if (outFile != null) {
      Files.write(Paths.get(outFile), myRows, StandardCharsets.UTF_8);
      Object summary = call(STATS, "summary");
      if (summary != null) Files.writeString(Paths.get(outFile + ".stats.tsv"), (String)summary, StandardCharsets.UTF_8);
      List<String> ev = new ArrayList<>();
      for (Object e : events()) ev.add((String)e.getClass().getMethod("line").invoke(e));
      Files.write(Paths.get(outFile + ".events.tsv"), ev, StandardCharsets.UTF_8);
    }
    if (myArm != null) Disposer.dispose(myArm);
  }

  private static String load() {
    return String.format(Locale.ROOT, "%.1f", ManagementFactory.getOperatingSystemMXBean().getSystemLoadAverage());
  }

  /** The background reader: resolves the reference at the caret every 50 ms, cancellable or not. */
  private Thread startReader(PsiFile file, String kind) {
    if (kind == null || kind.isEmpty()) return null;
    myReading = true;
    myReaderNanos.clear();
    Thread t = new Thread(() -> {
      while (myReading) {
        long r0 = System.nanoTime();
        try {
          java.util.concurrent.Callable<Object> c = () -> {
            if (kind.equals("form")) {
              com.intellij.psi.PsiElement at = file.isValid() ? file.findElementAt(Math.max(0, myCaret - 1)) : null;
              while (at != null && at.getParent() != null && !(at.getParent() instanceof PsiFile)) at = at.getParent();
              if (at == null) return null;
              for (com.intellij.psi.PsiElement e : PsiTreeUtil.collectElements(at, x -> x.getReferences().length > 0)) {
                com.intellij.openapi.progress.ProgressManager.checkCanceled();
                for (PsiReference r : e.getReferences()) r.resolve();
              }
              return null;
            }
            PsiReference ref = file.isValid() ? file.findReferenceAt(Math.max(0, myCaret - 1)) : null;
            Object target = ref == null ? null : ref.resolve();
            if (kind.equals("ih") && target instanceof com.intellij.psi.PsiElement te) {
              String name = ref.getCanonicalText();
              for (com.intellij.psi.PsiElement e : PsiTreeUtil.collectElements(file, x -> x.getReferences().length > 0)) {
                com.intellij.openapi.progress.ProgressManager.checkCanceled();
                for (PsiReference r : e.getReferences()) if (name.equals(r.getCanonicalText())) r.isReferenceTo(te);
              }
            }
            return target;
          };
          if (kind.equals("nc")) com.intellij.openapi.application.ReadAction.compute(c::call);
          else com.intellij.openapi.application.ReadAction.nonBlocking(c).executeSynchronously();
        }
        catch (Throwable e) {
          if (e instanceof com.intellij.openapi.progress.ProcessCanceledException) continue;
        }
        myReaderNanos.add(System.nanoTime() - r0);
        try {
          Thread.sleep(50);
        }
        catch (InterruptedException e) {
          return;
        }
      }
    }, "s5 reader " + kind);
    t.start();
    return t;
  }

  private void runOne(int round, String fk, String dir, String text, String tk, String typed, String armName, int gap) throws Exception {
    String reader = env("S5_READER", "");
    String arm = armName;
    if (armName.contains("+")) {
      reader = armName.substring(armName.indexOf('+') + 1);
      arm = armName.substring(0, armName.indexOf('+'));
    }
    arm(arm);
    PsiFile file = myFixture.addFileToProject(dir + "/h" + (myN++) + "_" + fk + ".erl", text);
    VirtualFile v = file.getVirtualFile();
    myFixture.configureFromExistingVirtualFile(v);
    Editor editor = myFixture.getEditor();
    EditorTracker.getInstance(getProject()).setActiveEditorsInTests(List.of(editor));
    myWatched = FileEditorManager.getInstance(getProject()).getSelectedEditor(v);
    long t0 = System.currentTimeMillis();
    myLastFinished = 0;
    ((DaemonCodeAnalyzerImpl)DaemonCodeAnalyzer.getInstance(getProject())).restart(file, "harness");
    long first = waitHighlighted(file, 600_000);
    long firstMs = first > 0 ? first - t0 : -1;
    Document doc = editor.getDocument();
    editor.getCaretModel().moveToOffset(pickSpot(file, doc));
    pump(300);
    waitHighlighted(file, 120_000);
    // counters before
    Map<String, long[]> s0 = statsSnapshot();
    int e0 = events().size();
    long cpu0 = cpuNanos(), gc0 = gcMillis();
    long maxKey = 0;
    long tStart = System.currentTimeMillis();
    long tLast = 0;
    myCaret = editor.getCaretModel().getOffset();
    int hogs = Integer.parseInt(env("S5_HOG", "0"));
    myHogging = hogs > 0;
    List<Thread> hogThreads = new ArrayList<>();
    for (int i = 0; i < hogs; i++) {
      Thread h = new Thread(() -> {
        long x = 0;
        while (myHogging) x += System.nanoTime() & 7;
        if (x == 42) System.out.print("");
      }, "s5 hog " + i);
      h.setDaemon(true);
      h.start();
      hogThreads.add(h);
    }
    Thread readerThread = startReader(file, reader);
    for (char c : typed.toCharArray()) {
      long k0 = System.nanoTime();
      tLast = System.currentTimeMillis();
      myFixture.type(c);
      PsiDocumentManager.getInstance(getProject()).commitDocument(doc);
      myCaret = editor.getCaretModel().getOffset();
      maxKey = Math.max(maxKey, (System.nanoTime() - k0) / 1_000_000);
      long rest = gap - (System.currentTimeMillis() - tLast);
      if (rest > 0) pump(rest);
    }
    myLastFinished = 0;
    myStartingCount = myFinishedCount = myCanceledCount = 0;
    long done = waitHighlighted(file, 600_000);
    long finalPass = done > 0 && myLastStarting > 0 ? done - myLastStarting : -1;
    String daemonRuns = "starts=" + myStartingCount + " finishes=" + myFinishedCount + " cancels=" + myCanceledCount + " final_pass_ms=" + finalPass;
    long hl = done > 0 ? done - tLast : -1;
    if (readerThread != null) {
      myReading = false;
      readerThread.join(30_000);
    }
    myHogging = false;
    for (Thread h : hogThreads) h.join(5_000);
    long cpu = (cpuNanos() - cpu0) / 1_000_000, gc = gcMillis() - gc0;
    Map<String, long[]> s1 = statsSnapshot();
    // layer counters of this run
    long buildMs = 0, lockMs = 0, excl = 0, incl = 0;
    int builds = 0, cancelled = 0;
    long asked = 0, computed = 0, gated = 0, tableBuilds = 0, tableGets = 0;
    for (Map.Entry<String, long[]> e : s1.entrySet()) {
      long[] b = s0.getOrDefault(e.getKey(), new long[4]);
      String[] k = e.getKey().split("\t");
      long dn = e.getValue()[1] - b[1], dx = e.getValue()[2] - b[2], dc = e.getValue()[0] - b[0];
      if (k[1].equals("build")) {
        buildMs += dn;
        builds += (int)dc;
      }
      else if (k[1].equals("build-cancelled")) cancelled += (int)dc;
      else if (k[1].equals("lock-wait")) lockMs += dn;
      else if (k[1].startsWith("resolve") || k[1].startsWith("variable") || k[1].startsWith("error-filter") || k[1].startsWith("header-target")) {
        excl += dx;
        incl += dn;
        // round 5: questions that passed the gate (outermost calls only), of which computed in the copy, and gate stops
        if (k[2].startsWith("gate:") || k[2].equals("ablated")) gated += dc;
        else if (!k[2].startsWith("nested:")) {
          asked += dc;
          if (k[2].contains("computed") || k[2].startsWith("function-table:") || k[2].startsWith("asked:hidden") || k[2].startsWith("asked:shown")) computed += dc;
        }
      }
      else if (k[1].equals("table-build")) tableBuilds += dc;
      else if (k[1].equals("table-get")) tableGets += dc;
    }
    double maxBuild = 0, maxWait = 0;
    int waits = 0;
    List<Object> ev = events();
    for (Object e : ev.subList(Math.min(e0, ev.size()), ev.size())) {
      String[] c = ((String)e.getClass().getMethod("line").invoke(e)).split("\t");
      double ms = Double.parseDouble(c[4]);
      if (c[0].equals("BUILD")) maxBuild = Math.max(maxBuild, ms);
      if (c[0].equals("LOCK_WAIT")) {
        maxWait = Math.max(maxWait, ms);
        waits++;
      }
    }
    String row = String.join("\t", String.valueOf(round), fk, tk, arm, String.valueOf(firstMs), String.valueOf(hl),
                             String.format(Locale.ROOT, "%.1f", (tLast - tStart) / 1000.0), String.valueOf(maxKey), String.valueOf(cpu),
                             String.valueOf(gc), String.valueOf(builds), String.format(Locale.ROOT, "%.1f", buildMs / 1e6),
                             String.valueOf(cancelled), String.format(Locale.ROOT, "%.1f", lockMs / 1e6),
                             String.format(Locale.ROOT, "%.1f", excl / 1e6), String.format(Locale.ROOT, "%.1f", incl / 1e6), load(),
                             String.format(Locale.ROOT, "%.1f", maxBuild), String.format(Locale.ROOT, "%.1f", maxWait), String.valueOf(waits));
    if (readerThread != null) {
      List<Long> rn = new ArrayList<>(myReaderNanos);
      long rmax = rn.stream().mapToLong(x -> x).max().orElse(0);
      row += "\treader=" + reader + " reads=" + rn.size() + " max_read_ms=" + rmax / 1_000_000;
    }
    else row += "\t-";
    row += "\t" + finalPass + "\t" + daemonRuns + "\t" + asked + "\t" + computed + "\t" + gated + "\t" + tableBuilds + "\t" + tableGets;
    row = row.replaceFirst("\t" + Pattern.quote(arm) + "\t", "\t" + Matcher.quoteReplacement(armName) + "\t");
    myRows.add(row);
    out("run " + row + " events+" + (events().size() - e0));
    EditorTracker.getInstance(getProject()).setActiveEditorsInTests(List.of());
    myWatched = null;
    FileEditorManager.getInstance(getProject()).closeFile(v);
    WriteAction.run(() -> v.delete(this));
    pump(50);
  }
}
