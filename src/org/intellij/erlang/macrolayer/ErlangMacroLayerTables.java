package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.DaemonProgressIndicator;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationListener;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.WrappedProgressIndicator;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.ModificationTracker;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.UserDataHolderEx;
import com.intellij.openapi.util.registry.Registry;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.PsiSearchHelper;
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.util.CachedValue;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.psi.util.PsiModificationTracker;
import com.intellij.psi.util.PsiTreeUtil;
import java.lang.management.GarbageCollectorMXBean;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.lang.ref.WeakReference;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.LockSupport;
import java.util.function.BooleanSupplier;
import java.util.function.Predicate;
import java.util.function.Supplier;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.macrolayer.ErlangMacroLayer.Copy;
import org.intellij.erlang.macrolayer.ErlangMacroLayerCopies.CopyHolder;
import org.intellij.erlang.macrolayer.ErlangMacroLayer.Mode;
import org.intellij.erlang.macrolayer.ErlangMacroLayer.Outcome;
import org.intellij.erlang.macrolayer.ErlangMacroLayer.Table;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Expansion;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Form;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.FormStatus;
import org.intellij.erlang.parser.macro.ErlangMacroExpander;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangMacroResolve;
import org.intellij.erlang.psi.impl.ErlangModuleReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.psi.impl.ErlangQAtomBasedReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangRecordReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangTypeReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangVariableReferenceImpl;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

/**
 * The per-file macro table ({@link ErlangMacroLayer.Table}) and what it depends on: which macros a file can see (its
 * own and its headers', in include order), which are plain, the header summaries the expander replays, and the gate
 * of a file the layer works on at all ({@link #layerFile}).
 * <p>
 * Owns: the table cache on each file ({@code TableCache}), the include tracker (what the file's includes resolve to;
 * it changes on a VFS structure change, a roots change or a rebar.config edit), the text tracker (the file's own PSI
 * changes), and the header summaries (a CachedValue on each header, by its text). A table is valid while the text
 * tracker, the include tracker and every header's directive tracker keep their counts.
 * <p>
 * Threading: under a read action, cancellable. On the UI thread only a table that is already valid is returned (and
 * no header is lexed there); concurrent askers of the same file wait for one computation instead of starting their
 * own. A table never builds a copy.
 */
final class ErlangMacroLayerTables {
  private ErlangMacroLayerTables() {
  }




  static final Key<TableCache> TABLE_KEY = Key.create("erlang.macro.layer.table");
  static final Key<IncludeTracker> INCLUDE_TRACKER_KEY = Key.create("erlang.macro.layer.include.tracker");
  /** Stamps of an {@link IncludeTracker} that could not check: each differs from every stamp a cached value keeps. */
  static final AtomicLong UNCHECKED_STAMPS = new AtomicLong();

  /**
   * What the includes of a file, and of its headers transitively, resolve to. Its count changes only when one
   * of them resolves to another file or to none. Include resolution ({@link ErlangPsiImplUtil#getDirectlyIncludedFiles})
   * reads only the file tree, the project roots, the application index, the include paths of {@code rebar.config} (in
   * a small IDE) and the include attributes. The attributes are covered by the other dependencies (the file's own text
   * and the header directive trackers); the rest can change only with a VFS structure event (creation, deletion, move,
   * rename, copy), a roots change, or an edit of a {@code rebar.config}. So the tracker re-runs master's
   * resolution, once, when the VFS structure count, the roots count or the {@code rebar.config} count
   * ({@link ErlangMacroLayerRetention#buildConfigVersion}) moved since its last check, and compares
   * the result with the last one. A header that appears or disappears changes the result (an include now resolves, or
   * no longer does, or resolves elsewhere); an unrelated file event does not. Where it cannot check (the UI thread,
   * which must not query indices; no read access; dumb mode), it answers a fresh stamp, so the caller sees the cached
   * table and copy as stale and gets master's answer; the next background question checks.
   */
  static final class IncludeTracker implements ModificationTracker {
    private final @NotNull ErlangFile myFile;
    private long myVfs = -1;
    private long myRoots = -1;
    private long myConfig = -1;
    private @Nullable List<Object> myResolution;
    private long myVersion;

    IncludeTracker(@NotNull ErlangFile file) {
      myFile = file;
    }

    @Override
    public long getModificationCount() {
      Project project = myFile.getProject();
      long vfs = VirtualFileManager.VFS_STRUCTURE_MODIFICATIONS.getModificationCount();
      long roots = ProjectRootManager.getInstance(project).getModificationCount();
      long config = ErlangMacroLayerRetention.getInstance(project).buildConfigVersion();
      synchronized (this) {
        if (vfs == myVfs && roots == myRoots && config == myConfig) return myVersion;
      }
      var app = ApplicationManager.getApplication();
      if (app.isDispatchThread() && !ErlangMacroLayer.isEdtBuildAllowed() || !app.isReadAccessAllowed() || project.isDisposed() ||
          DumbService.isDumb(project) || !myFile.isValid()) {
        return -UNCHECKED_STAMPS.incrementAndGet();
      }
      return update(resolveIncludes(myFile, new HashMap<>()), vfs, roots, config);
    }

    /** Records the resolution seen with the given VFS structure, roots and rebar.config counts; returns the version. */
    synchronized long update(@NotNull List<Object> resolution, long vfs, long roots, long config) {
      if (myResolution != null && !myResolution.equals(resolution)) myVersion++;
      myResolution = resolution;
      myVfs = vfs;
      myRoots = roots;
      myConfig = config;
      return myVersion;
    }
  }

  static final Key<TextTracker> TEXT_TRACKER_KEY = Key.create("erlang.macro.layer.text.tracker");

  /**
   * The file's own text and tree. The PSI modification stamp of a file also moves when the platform clears the
   * caches of every PSI file (a file moved or a directory deleted anywhere in the project does that), so the copy does
   * not depend on it. This tracker counts the PSI tree change events of the file itself instead
   * ({@link ErlangMacroLayerRetention} listens): every reparse and every PSI edit fires them, a cache clearing does not.
   * A text that comes back to an earlier text (undo) still counts: its reparse made new elements, and cached answers
   * keep elements.
   */
  static final class TextTracker implements ModificationTracker {
    private final AtomicLong myVersion = new AtomicLong();
    /** System.nanoTime() of the last change; valid when {@link ErlangMacroLayer#myChanged}. */
    private volatile long myLastChange;
    private volatile boolean myChanged;

    void changed() {
      myLastChange = System.nanoTime();
      myChanged = true;
      myVersion.incrementAndGet();
    }

    /** The file changed less than the given time ago. */
    boolean changedWithin(long millis) {
      return myChanged && System.nanoTime() - myLastChange < millis * 1_000_000L;
    }

    @Override
    public long getModificationCount() {
      return myVersion.get();
    }
  }

  static @Nullable TextTracker textTrackerIfAny(@NotNull PsiFile file) {
    return file.getUserData(TEXT_TRACKER_KEY);
  }

  static @NotNull TextTracker textTracker(@NotNull PsiFile file) {
    TextTracker t = file.getUserData(TEXT_TRACKER_KEY);
    if (t != null) return t;
    ErlangMacroLayerRetention.getInstance(file.getProject()); // its listener counts the changes from now on
    return ((UserDataHolderEx)file).putUserDataIfAbsent(TEXT_TRACKER_KEY, new TextTracker());
  }

  static @NotNull IncludeTracker includeTracker(@NotNull ErlangFile file) {
    IncludeTracker t = file.getUserData(INCLUDE_TRACKER_KEY);
    return t != null ? t : ((UserDataHolderEx)file).putUserDataIfAbsent(INCLUDE_TRACKER_KEY, new IncludeTracker(file));
  }

  /**
   * The include graph of a file with master's resolution: for the file and every header it reaches, the include keys
   * and the files they resolve to (null: none), in a fixed order. The direct includes of each visited file are kept in
   * {@code direct}, so that the table reuses them.
   */
  static @NotNull List<Object> resolveIncludes(@NotNull ErlangFile file, @NotNull Map<ErlangFile, Map<String, ErlangFile>> direct) {
    List<Object> r = new ArrayList<>();
    Set<ErlangFile> seen = new HashSet<>();
    ArrayDeque<ErlangFile> queue = new ArrayDeque<>();
    queue.add(file);
    seen.add(file);
    while (!queue.isEmpty()) {
      ProgressManager.checkCanceled();
      ErlangFile f = queue.poll();
      Map<String, ErlangFile> includes = direct.computeIfAbsent(f, ErlangMacroLayerTables::directIncludes);
      r.add(f.getOriginalFile().getVirtualFile());
      for (Map.Entry<String, ErlangFile> e : includes.entrySet()) {
        ErlangFile target = e.getValue();
        r.add(e.getKey());
        r.add(target == null ? null : target.getOriginalFile().getVirtualFile());
        if (target != null && seen.add(target)) queue.add(target);
      }
    }
    return r;
  }
  static final Key<CachedValue<ErlangMacroExpander.HeaderSummary>> SUMMARY_KEY = Key.create("erlang.macro.layer.summary");
  public static final AtomicInteger SUMMARIES_BUILT = new AtomicInteger();
  public static final AtomicInteger TABLES_BUILT = new AtomicInteger();
  public static final AtomicInteger HEADER_TARGETS_MATERIALIZED = new AtomicInteger();

  /** A real .erl file the layer may work on: enabled, not a copy, not a console, not a header, not during indexing. */
  static @Nullable ErlangFile layerFile(@Nullable PsiElement element) {
    if (!ErlangMacroLayer.isEnabled() || noReadAccess()) return null;
    ErlangFile file = ErlangMacroLayer.realFile(element);
    if (file == null) return null;
    VirtualFile vf = file.getOriginalFile().getVirtualFile();
    String name = vf != null ? vf.getName() : file.getName();
    if (!name.endsWith(".erl")) return null;
    if (DumbService.isDumb(file.getProject())) return null;
    return file;
  }

  static final AtomicBoolean NO_READ_ACCESS_LOGGED = new AtomicBoolean();

  /**
   * The caller holds no read access (a platform listener on the UI thread outside a read action,
   * as the next-edit rename listener does). The layer then answers what master answers: nothing. Logged once.
   */
  public static boolean noReadAccess() {
    var app = ApplicationManager.getApplication();
    if (app == null || app.isReadAccessAllowed()) return false;
    if (NO_READ_ACCESS_LOGGED.compareAndSet(false, true)) {
      Logger.getInstance(ErlangMacroLayer.class).info(
        "macro layer: asked without read access (" + Thread.currentThread().getName() + "); master's answer");
    }
    return true;
  }

  /** The summary of a header, made by lexing its text; cached on the header, one per header text. */
  /** The header's summary is computed for its current text (asking for it costs nothing). */
  static boolean hasUpToDateSummary(@NotNull PsiFile header) {
    CachedValue<ErlangMacroExpander.HeaderSummary> v = header.getUserData(SUMMARY_KEY);
    return v != null && v.hasUpToDateValue();
  }

  public static @NotNull ErlangMacroExpander.HeaderSummary summaryOf(@NotNull PsiFile header) {
    return CachedValuesManager.getManager(header.getProject()).getCachedValue(header, SUMMARY_KEY, () -> {
      long t0 = System.nanoTime();
      SUMMARIES_BUILT.incrementAndGet();
      VirtualFile vf = header.getOriginalFile().getVirtualFile();
      ErlangMacroExpander.HeaderSummary s = ErlangMacroExpander.summarize(header.getViewProvider().getContents(),
                                                                         new ErlangLexer(header.getProject()),
                                                                         vf != null ? vf : header);
      ErlangMacroLayerLog.event("SUMMARY", header.getName(), System.nanoTime() - t0, s.tokens, s.directives.size(),
                                "hash=" + Long.toHexString(s.hash), "defines=" + s.defines);
      ErlangMacroLayerStats.phase("summary-build", header.getName(), System.nanoTime() - t0);
      return CachedValueProvider.Result.create(s, header);
    }, false);
  }

  /**
   * The table of a file, the counts of its dependencies when it was computed, and the stamps (the
   * project's PSI modification count, the VFS structure count, the roots count) under which it was last found valid.
   * Every dependency of the table changes only with a PSI change, a VFS structure change or a roots change, so while
   * these three stamps stay the same the table is valid without asking its dependencies (the header trackers, which
   * cost the most in a file with many headers). After a change anywhere the dependencies are asked once, and the
   * table is computed again only when one of them moved.
   */
  record TableEntry(@NotNull Table table, long @NotNull [] depCounts, long psi, long vfs, long roots) {
  }

  record TableBuild(@NotNull Thread thread, @NotNull CompletableFuture<Table> result) {
  }

  static final class TableCache {
    volatile @Nullable TableEntry entry;
    /** The table computation in progress, if any: concurrent askers wait for it instead of computing their own. */
    final AtomicReference<TableBuild> building = new AtomicReference<>();
  }

  static @NotNull TableCache tableCache(@NotNull ErlangFile file) {
    TableCache c = file.getUserData(TABLE_KEY);
    return c != null ? c : ((UserDataHolderEx)file).putUserDataIfAbsent(TABLE_KEY, new TableCache());
  }

  /** The table of a file; on the UI thread only a cached one (never computed there). */
  static @Nullable Table table(@NotNull ErlangFile file) {
    return table(file, false);
  }

  /** {@code explicit}: the caller is a search that must get the layer's answers (see {@link ErlangMacroLayer#prepareSearch}). */
  static @Nullable Table table(@NotNull ErlangFile file, boolean explicit) {
    if (!ErlangMacroLayerStats.on()) return table0(file, explicit);
    long g0 = System.nanoTime();
    Table t = table0(file, explicit);
    ErlangMacroLayerStats.phase(t == null ? "table-get:edt-miss" : "table-get", file.getName(), System.nanoTime() - g0);
    return t;
  }

  static @Nullable Table table0(@NotNull ErlangFile file, boolean explicit) {
    Project project = file.getProject();
    TableCache cache = tableCache(file);
    long psi = PsiModificationTracker.getInstance(project).getModificationCount();
    long vfs = VirtualFileManager.VFS_STRUCTURE_MODIFICATIONS.getModificationCount();
    long roots = ProjectRootManager.getInstance(project).getModificationCount();
    Table valid = validTable(file, cache, psi, vfs, roots);
    if (valid != null) return valid;
    if (ApplicationManager.getApplication().isDispatchThread() && !ErlangMacroLayer.isEdtBuildAllowed()) return null;
    // The rules of a copy build hold for a table build too (in xmerl_scan.erl, about 15 ms of include
    // resolution, which the next key press cancelled): an uncancellable caller neither starts one nor waits for one,
    // and while the file changes only its highlighting starts one (another caller waits for a running one)
    String refusal = ErlangMacroLayerCallers.nonCancellableCaller();
    if (ErlangMacroLayerCallers.NO_INDICATOR.equals(refusal)) {
      // Under an indicator of its own that a write action cancels
      return ErlangMacroLayerCopies.underOwnIndicator(() -> table0(file, explicit), file, "table");
    }
    if (refusal == null && cache.building.get() == null && !explicit) refusal = ErlangMacroLayerCopies.buildRefusal(file);
    if (ErlangMacroLayerCopies.TYPING.equals(refusal) && ErlangMacroLayerCopies.lastTableNeedsCopy(cache)) {
      // A passive query waits for the table of this version (the daemon builds it) rather
      // than give an answer that the platform would keep (see awaitWhileTyped); only in a file whose last table had a
      // form that needs the copy: elsewhere master's answer is the layer's answer
      TableEntry before = cache.entry;
      if (ErlangMacroLayerCopies.awaitWhileTyped(file, () -> cache.building.get() != null || cache.entry != before, "table")) return table0(file, explicit);
    }
    if (refusal != null) {
      ErlangMacroLayerStats.phase("table-skipped:" + refusal, file.getName(), 0);
      ErlangMacroLayerLog.event("SKIPPED", file.getName(), 0, 0, 0, refusal, "table");
      return null;
    }
    return buildTable(file, cache, psi, vfs, roots);
  }

  /** The cached table if it is valid for these stamps (checking its dependencies once after a change), else null. */
  static @Nullable Table validTable(@NotNull ErlangFile file, @NotNull TableCache cache, long psi, long vfs, long roots) {
    TableEntry e = cache.entry;
    if (e == null) return null;
    if (e.psi == psi && e.vfs == vfs && e.roots == roots) return e.table;
    long c0 = ErlangMacroLayerStats.on() ? System.nanoTime() : 0;
    boolean same = Arrays.equals(e.table.depCounts(), e.depCounts);
    if (c0 != 0) ErlangMacroLayerStats.phase("table-check", file.getName(), System.nanoTime() - c0);
    if (!same) return null;
    cache.entry = new TableEntry(e.table, e.depCounts, psi, vfs, roots);
    return e.table;
  }

  /** Computes the table once for all threads that ask at the same time; the others wait, cancellably. */
  static @NotNull Table buildTable(@NotNull ErlangFile file, @NotNull TableCache cache, long psi, long vfs, long roots) {
    while (true) {
      TableBuild mine = new TableBuild(Thread.currentThread(), new CompletableFuture<>());
      TableBuild other = cache.building.compareAndExchange(null, mine);
      if (other == null) {
        Table t = null;
        try {
          t = validTable(file, cache, psi, vfs, roots); // another thread may have finished just before
          if (t == null) {
            t = computeTable(file);
            cache.entry = new TableEntry(t, t.depCounts(), psi, vfs, roots);
          }
          return t;
        }
        finally {
          cache.building.set(null);
          mine.result().complete(t);
        }
      }
      if (other.thread() == Thread.currentThread()) return computeTable(file);
      // both threads hold a read action: no change between the other thread's computation and this question
      Table t = ErlangMacroLayerCopies.awaitCancellably(other.result());
      if (t != null) return t;
    }
  }

  static @NotNull String key(@NotNull String attribute, @Nullable String string) {
    return attribute + "\0" + string;
  }

  static @Nullable String includeString(@Nullable ErlangIncludeString s) {
    return s == null ? null : StringUtil.unquoteString(s.getText());
  }

  /**
   * The largest text the layer copies and parses, in characters: four times the largest real module of the corpus
   * (OTP's erl_parse.erl, 956,504 bytes). A larger file or expansion (a generated module) gets master's behaviour.
   */
  public static final int MAX_COPY_CHARS = 4_000_000;
  static volatile int ourTestMaxCopyChars;
  static final Set<String> TOO_LARGE_LOGGED = ConcurrentHashMap.newKeySet();

  static int maxCopyChars() {
    int t = ourTestMaxCopyChars;
    return t > 0 ? t : MAX_COPY_CHARS;
  }

  @TestOnly
  public static void setMaxCopyCharsForTests(int chars, @NotNull Disposable parent) {
    ourTestMaxCopyChars = chars;
    Disposer.register(parent, () -> ourTestMaxCopyChars = 0);
  }

  /** Logs once per file that it is too large for the layer; the refusal reason. */
  static @NotNull String tooLarge(@NotNull PsiFile file, int chars) {
    VirtualFile vf = file.getOriginalFile().getVirtualFile();
    String path = vf != null ? vf.getPath() : file.getName();
    if (TOO_LARGE_LOGGED.add(path)) Logger.getInstance(ErlangMacroLayer.class).info("Macro layer: " + path + " is too large (" + chars + " characters): master's behaviour");
    return "too-large";
  }

  static @NotNull Table computeTable(@NotNull ErlangFile file) {
    long c0 = ErlangMacroLayerStats.on() ? System.nanoTime() : 0;
    try {
      return computeTable0(file);
    }
    finally {
      if (c0 != 0) ErlangMacroLayerStats.phase("table-build", file.getName(), System.nanoTime() - c0);
    }
  }

  static @NotNull Table computeTable0(@NotNull ErlangFile file) {
    TABLES_BUILT.incrementAndGet();
    Project project = file.getProject();
    List<Object> deps = new ArrayList<>();
    deps.add(textTracker(file));
    // the counts first: a change during the resolution below makes the tracker check again
    long vfs = VirtualFileManager.VFS_STRUCTURE_MODIFICATIONS.getModificationCount();
    long roots = ProjectRootManager.getInstance(project).getModificationCount();
    long config = ErlangMacroLayerRetention.getInstance(project).buildConfigVersion();
    Map<ErlangFile, Map<String, ErlangFile>> direct = new HashMap<>();
    IncludeTracker includes = includeTracker(file);
    includes.update(resolveIncludes(file, direct), vfs, roots, config);
    deps.add(includes);
    // own macros
    Set<String> names = new HashSet<>();
    Set<String> bodyVars = new HashSet<>();
    // the macros whose -define has a syntax error in the main tree (see classify)
    Set<String> broken = new HashSet<>();
    Map<String, List<ErlangMacroExpander.DefineShape>> shapes = new HashMap<>();
    for (ErlangMacrosDefinition d : file.getMacroses()) {
      ProgressManager.checkCanceled();
      String n = d.getName();
      if (n != null) names.add(n);
      if (n != null && hasSyntaxError(d)) broken.add(n);
      ErlangMacrosBody body = d.getMacrosBody();
      if (body != null) for (ErlangQVar v : PsiTreeUtil.findChildrenOfType(body, ErlangQVar.class)) bodyVars.add(v.getText());
      if (n != null) {
        ErlangMacroExpander.DefineShape shape = ErlangMacroExpander.defineShape(leafTokens(d));
        shapes.computeIfAbsent(n, k -> new ArrayList<>()).add(shape != null && shape.name().equals(n) ? shape : unknownShape(n));
      }
    }
    // headers, the way master resolves includes
    Map<String, ErlangMacroExpander.HeaderSummary> top = new HashMap<>();
    Map<ErlangMacroExpander.HeaderSummary, Map<String, ErlangMacroExpander.HeaderSummary>> nested = new IdentityHashMap<>();
    List<VirtualFile> headers = new ArrayList<>();
    String refusal = file.getTextLength() > maxCopyChars() ? tooLarge(file, file.getTextLength()) : null;
    boolean[] missing = {false};
    try {
      for (Map.Entry<String, ErlangFile> e : direct.get(file).entrySet()) {
        // An include that does not resolve no longer turns the layer off for the whole file
        if (e.getValue() == null || e.getValue().getOriginalFile().getVirtualFile() == null) {
          missing[0] = true;
          continue;
        }
        top.put(e.getKey(), visitHeader(e.getValue(), new ArrayList<>(), nested, headers, names, bodyVars, shapes, direct, missing, broken));
      }
    }
    catch (Refusal r) {
      refusal = r.getMessage();
    }
    for (VirtualFile h : headers) deps.add(ErlangMacroLayerRetention.getInstance(project).tracker(h));
    Table t = new Table(refusal, deps.toArray(), includes, file.getOriginalFile().getVirtualFile());
    t.missingInclude = missing[0];
    if (refusal == null) {
      t.names.addAll(names);
      t.bodyVars.addAll(bodyVars);
      classify(t, shapes, broken);
      t.top.putAll(top);
      t.nested.putAll(nested);
    }
    t.headers.addAll(headers);
    return t;
  }

  /**
   * Which visible macros need substitution. A macro is plain when every definition of its name (in the file and
   * in its headers, any arity, any conditional branch) has a locally plain body ({@link ErlangMacroExpander#isPlainBody})
   * and every macro name used in those bodies is predefined or is itself plain (transitively); a name used in a body
   * that the file cannot see makes the macro not plain.
   */
  static void classify(@NotNull Table t, @NotNull Map<String, List<ErlangMacroExpander.DefineShape>> shapes,
                               @NotNull Set<String> broken) {
    for (String name : t.names) {
      List<ErlangMacroExpander.DefineShape> list = shapes.get(name);
      if (list == null || list.isEmpty()) {
        t.nonPlain.add(name);
        continue;
      }
      for (ErlangMacroExpander.DefineShape shape : list) {
        t.freeBodyVars.addAll(shape.freeVars());
        t.macroFreeVars.computeIfAbsent(name, k -> new HashSet<>()).addAll(shape.freeVars());
        t.macroNestedUses.computeIfAbsent(name, k -> new HashSet<>()).addAll(shape.nestedUses());
        // a -define with a syntax error in the main tree: no error is shown inside a well-formed body (it may be a
        // fragment that is valid where it is used), so each use is parsed in the copy and the error comes at the use
        if (!shape.plainBody() || broken.contains(name)) t.nonPlain.add(name);
      }
    }
    // A macro whose bodies come back to itself is not plain (epp: "circular macro")
    for (String name : t.names) {
      if (t.nonPlain.contains(name)) continue;
      ArrayDeque<String> queue = new ArrayDeque<>(t.macroNestedUses.getOrDefault(name, Set.of()));
      Set<String> seen = new HashSet<>();
      while (!queue.isEmpty()) {
        String m = queue.poll();
        if (m.equals(name)) {
          t.nonPlain.add(name);
          break;
        }
        if (seen.add(m)) queue.addAll(t.macroNestedUses.getOrDefault(m, Set.of()));
      }
    }
    boolean changed = true;
    while (changed) {
      changed = false;
      for (String name : t.names) {
        if (t.nonPlain.contains(name)) continue;
        uses:
        for (ErlangMacroExpander.DefineShape shape : shapes.get(name)) {
          for (String used : shape.nestedUses()) {
            if (ErlangMacroExpander.PREDEFINED.contains(used)) continue;
            if (!t.names.contains(used) || t.nonPlain.contains(used)) {
              t.nonPlain.add(name);
              changed = true;
              break uses;
            }
          }
        }
      }
    }
    // With an unresolved include, the macros whose bodies reach a macro the file cannot see
    changed = true;
    while (changed) {
      changed = false;
      for (String name : t.names) {
        if (t.usesUnseen.contains(name)) continue;
        for (String used : t.macroNestedUses.getOrDefault(name, Set.of())) {
          if (ErlangMacroExpander.PREDEFINED.contains(used)) continue;
          if (!t.names.contains(used) || t.usesUnseen.contains(used)) {
            t.usesUnseen.add(name);
            changed = true;
            break;
          }
        }
      }
    }
  }

  static boolean hasSyntaxError(@NotNull ErlangMacrosDefinition d) {
    return PsiTreeUtil.findChildOfType(d, PsiErrorElement.class) != null;
  }

  static @NotNull ErlangMacroExpander.DefineShape unknownShape(@NotNull String name) {
    return new ErlangMacroExpander.DefineShape(name, -1, List.of(), false, Set.of(), Set.of());
  }

  /**
   * The error lies in the body of a {@code -define} that epp accepts. For epp a body is any sequence of tokens up
   * to the {@code )} before the {@code .} that ends the form, so a parser error there is false: the define reads as a
   * well-formed define to the expander ({@link ErlangMacroExpander#defineShape}: a macro name, a parameter list of
   * distinct variables, a {@code ,} and the closing {@code )}), and a {@code .} follows it. A define without its
   * {@code )} or its {@code .}, with a bad name or a bad parameter list, keeps its errors. Main tree only, no copy.
   */
  public static boolean isInWellFormedDefineBody(@NotNull PsiErrorElement error) {
    if (!ErlangMacroLayer.isEnabled() || noReadAccess()) return false;
    ErlangMacrosDefinition d = PsiTreeUtil.getParentOfType(error, ErlangMacrosDefinition.class);
    if (d == null || !(d.getParent() instanceof PsiFile)) return false;
    // the error is after the parameter list: in the body or at its closing parenthesis
    ErlangArgumentDefinitionList params = d.getArgumentDefinitionList();
    ErlangMacrosName name = d.getMacrosName();
    PsiElement head = params != null ? params : name;
    if (head == null || error.getTextRange().getStartOffset() < head.getTextRange().getEndOffset()) return false;
    // the form ends with "." (the define element ends with it, or it is the next token)
    PsiElement last = PsiTreeUtil.getDeepestVisibleLast(d);
    PsiElement next = PsiTreeUtil.nextVisibleLeaf(d);
    boolean dot = last != null && last.getNode().getElementType() == ErlangTypes.ERL_DOT ||
                  next != null && next.getNode().getElementType() == ErlangTypes.ERL_DOT;
    if (!dot) return false;
    ErlangMacroExpander.DefineShape shape = ErlangMacroExpander.defineShape(leafTokens(d));
    return shape != null && shape.params().size() == new HashSet<>(shape.params()).size();
  }

  /** The significant leaves of an element as expander tokens (types and texts only). */
  static @NotNull List<ErlangMacroExpander.Tok> leafTokens(@NotNull PsiElement element) {
    List<ErlangMacroExpander.Tok> r = new ArrayList<>();
    int end = element.getTextRange().getEndOffset();
    for (PsiElement leaf = PsiTreeUtil.firstChild(element); leaf != null; leaf = PsiTreeUtil.nextLeaf(leaf)) {
      int start = leaf.getTextRange().getStartOffset();
      if (start >= end) break;
      if (leaf instanceof PsiWhiteSpace || leaf instanceof PsiComment || leaf.getTextLength() == 0) continue;
      r.add(new ErlangMacroExpander.Tok(leaf.getNode().getElementType(), leaf.getText(), ErlangMacroExpander.SRC_NONE, start,
                                        start + leaf.getTextLength(), null));
    }
    return r;
  }

  static final class Refusal extends Exception {
    Refusal(String reason) {
      super(reason, null, false, false);
    }
  }

  /** The includes of a file (from its stubs or PSI), by attribute and string, with master's resolution (null: none). */
  static @NotNull Map<String, ErlangFile> directIncludes(@NotNull ErlangFile file) {
    Map<String, ErlangFile> r = new LinkedHashMap<>();
    for (ErlangInclude inc : file.getIncludes()) {
      ProgressManager.checkCanceled();
      List<ErlangFile> files = ErlangPsiImplUtil.getDirectlyIncludedFiles(inc, file);
      r.put(key("include", includeString(inc.getIncludeStringSafe())), files.isEmpty() ? null : files.getFirst());
    }
    for (ErlangIncludeLib inc : file.getIncludeLibs()) {
      ProgressManager.checkCanceled();
      List<ErlangFile> files = ErlangPsiImplUtil.getDirectlyIncludedFiles(inc, file);
      r.put(key("include_lib", includeString(inc.getIncludeStringSafe())), files.isEmpty() ? null : files.getFirst());
    }
    return r;
  }

  static @NotNull ErlangMacroExpander.HeaderSummary visitHeader(
    @NotNull ErlangFile header, @NotNull List<VirtualFile> path,
    @NotNull Map<ErlangMacroExpander.HeaderSummary, Map<String, ErlangMacroExpander.HeaderSummary>> nested,
    @NotNull List<VirtualFile> headers, @NotNull Set<String> names, @NotNull Set<String> bodyVars,
    @NotNull Map<String, List<ErlangMacroExpander.DefineShape>> shapes,
    @NotNull Map<ErlangFile, Map<String, ErlangFile>> direct, boolean @NotNull [] missing, @NotNull Set<String> broken) throws Refusal {
    VirtualFile vf = header.getOriginalFile().getVirtualFile();
    if (vf == null) throw new Refusal("unresolved-include");
    if (path.contains(vf)) throw new Refusal("include-cycle");
    if (path.size() > ErlangMacroExpander.MAX_DEPTH) throw new Refusal("include-depth");
    if (!headers.contains(vf)) headers.add(vf);
    ErlangMacroExpander.HeaderSummary s = summaryOf(header);
    for (ErlangMacroExpander.Directive d : s.directives) {
      if (!"define".equals(d.attribute)) continue;
      names.add(d.name());
      for (int i = 4; i < d.tokens.size(); i++) {
        if (d.tokens.get(i).type() == ErlangTypes.ERL_VAR) bodyVars.add(d.tokens.get(i).text());
      }
    }
    if (nested.containsKey(s)) return s;
    for (ErlangMacroExpander.DefineShape shape : s.defineShapes()) shapes.computeIfAbsent(shape.name(), k -> new ArrayList<>()).add(shape);
    for (ErlangMacrosDefinition d : header.getMacroses()) {
      if (d.getName() != null && hasSyntaxError(d)) broken.add(d.getName());
    }
    Map<String, ErlangMacroExpander.HeaderSummary> children = new HashMap<>();
    nested.put(s, children);
    if (s.includes().isEmpty()) return s;
    Map<String, ErlangFile> resolved = direct.computeIfAbsent(header, ErlangMacroLayerTables::directIncludes);
    path.add(vf);
    try {
      for (ErlangMacroExpander.Directive d : s.includes()) {
        String k = key(d.attribute, d.includeString());
        ErlangFile child = resolved.get(k);
        if (child == null || child.getOriginalFile().getVirtualFile() == null) {
          missing[0] = true;
          continue;
        }
        children.put(k, visitHeader(child, path, nested, headers, names, bodyVars, shapes, direct, missing, broken));
      }
    }
    finally {
      path.removeLast();
    }
    return s;
  }
}
