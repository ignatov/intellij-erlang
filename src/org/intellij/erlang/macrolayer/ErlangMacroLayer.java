package org.intellij.erlang.macrolayer;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.codeInsight.daemon.impl.DaemonProgressIndicator;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.WrappedProgressIndicator;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.UserDataHolderEx;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.psi.util.CachedValue;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.ModificationTracker;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.PsiSearchHelper;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.psi.util.PsiModificationTracker;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.parser.macro.ErlangMacroExpander;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Expansion;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Form;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.FormStatus;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.psi.impl.ErlangModuleReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangQAtomBasedReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangRecordReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangTypeReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangVariableReferenceImpl;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.util.*;
import java.lang.ref.WeakReference;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Spike 3: macro expansion as a separate layer, used only as an oracle.
 * <p>
 * The main tree, the stubs and the indices of a file are not touched. On demand, the layer expands the same-file
 * macros of the file's PSI text ({@link ErlangMacroExpander}), parses the expanded text as one non-physical Erlang
 * file whose original file is the real file (the "copy"), and caches it on the real file. A caller asks the copy only
 * when master's path has no answer, and every answer is mapped back to an element of a real file.
 * <p>
 * Switch: system property {@code erlang.macro.layer}, else the environment variable {@code ERLANG_MACRO_LAYER}:
 * {@code off} or {@code layer} (default). Form cap: {@code erlang.macro.layer.cap} / {@code ERLANG_MACRO_LAYER_CAP},
 * default 2,000 significant tokens per expanded form. Remote fallback ({@code m:f()} to a function made by a macro
 * in m): {@code erlang.macro.layer.remote} / {@code ERLANG_MACRO_LAYER_REMOTE} = {@code on}, default off.
 */
public final class ErlangMacroLayer {
  public enum Mode {OFF, LAYER}

  /** Marks a copy and points to its real file. */
  public static final Key<PsiFile> COPY_OF = Key.create("erlang.macro.layer.copy.of");

  public static final AtomicInteger EXPANSIONS_BUILT = new AtomicInteger();
  public static final AtomicInteger COPIES_PARSED = new AtomicInteger();
  public static final AtomicInteger FALLBACK_ASKED = new AtomicInteger();
  public static final AtomicInteger FALLBACK_ANSWERED = new AtomicInteger();
  public static final AtomicInteger ERRORS_HIDDEN = new AtomicInteger();
  public static final AtomicInteger GUARD_VIOLATIONS = new AtomicInteger();
  public static final AtomicInteger REMOTE_ANSWERED = new AtomicInteger();

  private static final Mode DEFAULT_MODE = readMode();
  public static final int FORM_CAP = readInt("erlang.macro.layer.cap", "ERLANG_MACRO_LAYER_CAP", 2000);
  private static final boolean REMOTE_DEFAULT = "on".equals(read("erlang.macro.layer.remote", "ERLANG_MACRO_LAYER_REMOTE"));
  private static volatile @Nullable Mode ourTestMode;
  private static volatile @Nullable Boolean ourTestRemote;
  /** Diagnostic only: {@code erlang.macro.layer.edt=allow} lets the UI thread build a copy (default: never). */
  private static final boolean EDT_BUILD_DEFAULT = "allow".equals(read("erlang.macro.layer.edt", "ERLANG_MACRO_LAYER_EDT"));
  private static volatile @Nullable Boolean ourTestEdtBuild;
  /**
   * Round 5: {@code erlang.macro.layer.gate} / {@code ERLANG_MACRO_LAYER_GATE} = {@code wide} restores the gate of round 4
   * (a form asks the copy as soon as it uses a visible macro; for the soundness check of the narrow gate). Default:
   * narrow (only forms that need substitution ask).
   */
  private static final boolean GATE_WIDE_DEFAULT = "wide".equals(read("erlang.macro.layer.gate", "ERLANG_MACRO_LAYER_GATE"));
  private static volatile @Nullable Boolean ourTestGateWide;

  static boolean isGateWide() {
    Boolean b = ourTestGateWide;
    return b != null ? b : GATE_WIDE_DEFAULT;
  }

  @TestOnly
  public static void setGateWideForTests(boolean wide, @NotNull Disposable parent) {
    Boolean previous = ourTestGateWide;
    ourTestGateWide = wide;
    Disposer.register(parent, () -> ourTestGateWide = previous);
  }

  static boolean isEdtBuildAllowed() {
    Boolean b = ourTestEdtBuild;
    return b != null ? b : EDT_BUILD_DEFAULT;
  }

  @TestOnly
  public static void setEdtBuildForTests(boolean allow, @NotNull Disposable parent) {
    Boolean previous = ourTestEdtBuild;
    ourTestEdtBuild = allow;
    Disposer.register(parent, () -> ourTestEdtBuild = previous);
  }

  private ErlangMacroLayer() {
  }

  /** Spike 5: the outcome of one entry-point call, for {@link ErlangMacroLayerStats}; null when the log is off. */
  static final class Outcome {
    String value = "exception";
    String file;
  }

  private static void mark(@Nullable Outcome o, @NotNull String value) {
    if (o != null) o.value = value;
  }

  private static void markFile(@Nullable Outcome o, @Nullable PsiFile file) {
    if (o != null && file != null) o.file = file.getName();
  }

  /** Diagnostic ablation switches for the measurement harness (spike 5); all off by default. */
  static volatile boolean ourAblateResolve, ourAblateVariable, ourAblateErrorFilter;

  @TestOnly
  public static void setAblationForTests(boolean resolve, boolean variable, boolean errorFilter, @NotNull Disposable parent) {
    ourAblateResolve = resolve;
    ourAblateVariable = variable;
    ourAblateErrorFilter = errorFilter;
    Disposer.register(parent, () -> {
      ourAblateResolve = false;
      ourAblateVariable = false;
      ourAblateErrorFilter = false;
    });
  }

  private static @Nullable String read(String property, String env) {
    String value = System.getProperty(property);
    if (value == null || value.isEmpty()) value = System.getenv(env);
    return value == null || value.isEmpty() ? null : value.trim().toLowerCase(Locale.ROOT);
  }

  private static int readInt(String property, String env, int def) {
    String v = read(property, env);
    try {
      return v == null ? def : Integer.parseInt(v);
    }
    catch (NumberFormatException e) {
      return def;
    }
  }

  private static Mode readMode() {
    String v = read("erlang.macro.layer", "ERLANG_MACRO_LAYER");
    return "off".equals(v) || "none".equals(v) || "false".equals(v) ? Mode.OFF : Mode.LAYER;
  }

  public static @NotNull Mode getMode() {
    Mode m = ourTestMode;
    return m != null ? m : DEFAULT_MODE;
  }

  public static boolean isEnabled() {
    return getMode() == Mode.LAYER;
  }

  public static boolean isRemoteEnabled() {
    Boolean b = ourTestRemote;
    return b != null ? b : REMOTE_DEFAULT;
  }

  @TestOnly
  public static void setModeForTests(@NotNull Mode mode, @NotNull Disposable parent) {
    Mode previous = ourTestMode;
    ourTestMode = mode;
    Disposer.register(parent, () -> ourTestMode = previous);
  }

  @TestOnly
  public static void setRemoteForTests(boolean on, @NotNull Disposable parent) {
    Boolean previous = ourTestRemote;
    ourTestRemote = on;
    Disposer.register(parent, () -> ourTestRemote = previous);
  }

  // ------------------------------------------------------------------------------------------------ the copy

  /** The expanded copy of a real file. */
  public static final class Copy {
    public final @NotNull PsiFile realFile;
    public final @NotNull Expansion expansion;
    /** Null when no form is expanded: then the copy is not parsed at all. */
    public final @Nullable ErlangFile copyFile;
    /** Forms (by index) whose expanded text has an error element in the copy. */
    private final BitSet myBroken = new BitSet();
    public final long buildNanos;
    private volatile Map<String, PsiElement> myFunctionsMadeByMacros;
    /** Answers of the oracle for this copy, by kind and real offset; valid as long as the copy. */
    final ConcurrentHashMap<String, Object> answers = new ConcurrentHashMap<>();

    Copy(@NotNull PsiFile realFile, @NotNull Expansion expansion, @Nullable ErlangFile copyFile, long buildNanos) {
      this.realFile = realFile;
      this.expansion = expansion;
      this.copyFile = copyFile;
      this.buildNanos = buildNanos;
      if (copyFile != null) {
        for (PsiErrorElement e : PsiTreeUtil.collectElementsOfType(copyFile, PsiErrorElement.class)) {
          Form f = expansion.formAtExpanded(e.getTextRange().getStartOffset());
          if (f == null) f = previousForm(e.getTextRange().getStartOffset());
          if (f != null) myBroken.set(expansion.forms.indexOf(f));
        }
      }
    }

    private @Nullable Form previousForm(int expOffset) {
      Form best = null;
      for (Form f : expansion.forms) {
        if (f.expEnd <= expOffset) best = f;
        else break;
      }
      return best;
    }

    /** The form is expanded and its expanded text parses without errors. */
    public boolean isClean(@Nullable Form f) {
      return f != null && f.status == FormStatus.EXPANDED && copyFile != null && !myBroken.get(expansion.forms.indexOf(f));
    }

    public boolean isBroken(@NotNull Form f) {
      return myBroken.get(expansion.forms.indexOf(f));
    }

    /** A target in the copy may be used when it lies in a form that is not expanded, or in a clean expanded form. */
    boolean isUsableTarget(int expOffset) {
      Form f = expansion.formAtExpanded(expOffset);
      if (f == null) return true;
      return f.status != FormStatus.EXPANDED || isClean(f);
    }

    /**
     * Functions whose name exists only in an expansion, by "name/arity", with real targets (the source of the name).
     * A key made twice (two macro calls, or a macro call and a plain function) is left out: erlc rejects it, and
     * master's answer (unresolved) keeps the file red. A key master already sees is left out too.
     */
    public @NotNull Map<String, PsiElement> functionsMadeByMacros() {
      Map<String, PsiElement> map = myFunctionsMadeByMacros;
      if (map != null) return map;
      map = new LinkedHashMap<>();
      long f0 = ErlangMacroLayerStats.on() ? System.nanoTime() : 0;
      if (copyFile != null) {
        Map<String, Integer> made = new HashMap<>();
        for (ErlangFunction fn : copyFile.getFunctions()) made.merge(fn.getName() + "/" + fn.getArity(), 1, Integer::sum);
        for (ErlangFunction fn : copyFile.getFunctions()) {
          ProgressManager.checkCanceled();
          String key = fn.getName() + "/" + fn.getArity();
          if (made.get(key) > 1) continue;
          PsiElement id = fn.getNameIdentifier();
          int at = id.getTextRange().getStartOffset();
          if (expansion.kindAt(at) == ErlangMacroExpander.SRC_VERBATIM) continue;
          if (!isUsableTarget(at)) continue;
          if (realFile instanceof ErlangFile rf && rf.getFunction(fn.getName(), fn.getArity()) != null) continue;
          PsiElement real = toReal(fn);
          if (real != null) map.put(key, real);
        }
      }
      myFunctionsMadeByMacros = map;
      if (f0 != 0) ErlangMacroLayerStats.phase("functions-made-by-macros", realFile.getName(), System.nanoTime() - f0);
      return map;
    }

    private volatile Map<String, PsiElement> myRecordsMadeByMacros;
    private volatile Map<String, PsiElement> myTypesMadeByMacros;

    /** Round 5: records whose definition exists only in an expansion, by name, with real targets (see functions). */
    @NotNull Map<String, PsiElement> recordsMadeByMacros() {
      Map<String, PsiElement> map = myRecordsMadeByMacros;
      if (map == null) {
        map = madeByMacros(copyFile == null ? List.of() : copyFile.getRecords(),
                           name -> realFile instanceof ErlangFile rf && rf.getRecord(name) != null);
        myRecordsMadeByMacros = map;
      }
      return map;
    }

    /** Round 5: local types whose definition exists only in an expansion, by name, with real targets. */
    @NotNull Map<String, PsiElement> typesMadeByMacros() {
      Map<String, PsiElement> map = myTypesMadeByMacros;
      if (map == null) {
        map = madeByMacros(copyFile == null ? List.of() : copyFile.getTypes(),
                           name -> realFile instanceof ErlangFile rf && rf.getType(name) != null);
        myTypesMadeByMacros = map;
      }
      return map;
    }

    private @NotNull Map<String, PsiElement> madeByMacros(@NotNull List<? extends PsiNameIdentifierOwner> defs,
                                                          @NotNull java.util.function.Predicate<String> masterSees) {
      Map<String, Integer> made = new HashMap<>();
      for (PsiNameIdentifierOwner d : defs) made.merge(String.valueOf(d.getName()), 1, Integer::sum);
      Map<String, PsiElement> map = new LinkedHashMap<>();
      for (PsiNameIdentifierOwner d : defs) {
        ProgressManager.checkCanceled();
        String name = String.valueOf(d.getName());
        PsiElement id = d.getNameIdentifier();
        if (id == null || made.get(name) > 1 || masterSees.test(name)) continue;
        int at = id.getTextRange().getStartOffset();
        if (expansion.kindAt(at) == ErlangMacroExpander.SRC_VERBATIM || !isUsableTarget(at)) continue;
        PsiElement real = toReal(d);
        if (real != null) map.put(name, real);
      }
      return map;
    }

    /** The copy elements that correspond to a real range: its verbatim copy, else every copy of an argument token. */
    int @NotNull [] toCopyOffsets(@NotNull TextRange real) {
      int e = expansion.toExpanded(real.getStartOffset());
      if (e >= 0) return new int[]{e};
      return expansion.argumentCopies(real.getStartOffset());
    }

    /** Maps an element of the copy to a real element, or returns an element of another file as it is. */
    public @Nullable PsiElement toReal(@Nullable PsiElement target) {
      if (target == null || !target.isValid()) return null;
      PsiFile file = target.getContainingFile();
      if (file == null) return null;
      if (file != copyFile) return file.getUserData(COPY_OF) == null ? target : null;
      if (!isUsableTarget(target.getTextRange().getStartOffset())) return null;
      PsiElement real = mapElement(target, target);
      if (real != null) return real;
      if (target instanceof PsiNameIdentifierOwner owner) {
        PsiElement id = owner.getNameIdentifier();
        if (id != null) return mapElement(id, owner);
      }
      return null;
    }

    /** {@code owner}: the copy element the answer stands for (the element itself, or the owner of a name identifier). */
    private @Nullable PsiElement mapElement(@NotNull PsiElement copyElement, @NotNull PsiElement owner) {
      TextRange r = copyElement.getTextRange();
      ErlangMacroExpander.HeaderTok h = expansion.headerOrigin(r.getStartOffset(), r.getEndOffset());
      if (h != null && h.header() instanceof VirtualFile vf) {
        return new ErlangMacroLayerHeaderTarget(realFile.getProject(), vf, h.directive(), h.index(), copyElement.getText(),
                                                copyElement.getClass(), ErlangMacroLayerHeaderTarget.kindOf(owner));
      }
      TextRange src = expansion.toSource(r.getStartOffset(), r.getEndOffset());
      if (src == null || src.isEmpty()) return null;
      return findReal(realFile, src, copyElement.getClass());
    }
  }

  /** The element of the real file with exactly this range; of the given class if one exists, else the topmost. */
  static @Nullable PsiElement findReal(@NotNull PsiFile file, @NotNull TextRange range, @NotNull Class<?> preferred) {
    PsiElement e = file.findElementAt(range.getStartOffset());
    PsiElement topmost = null;
    while (e != null && !(e instanceof PsiFile)) {
      TextRange er = e.getTextRange();
      if (!range.contains(er)) break;
      if (er.equals(range)) {
        if (preferred.isInstance(e)) return e;
        topmost = e;
      }
      e = e.getParent();
    }
    return topmost;
  }

  /** The real Erlang file of an element, or null for copies, consoles and non-Erlang files. */
  static @Nullable ErlangFile realFile(@Nullable PsiElement element) {
    if (element == null) return null;
    PsiFile file = element.getContainingFile();
    if (!(file instanceof ErlangFile ef)) return null;
    if (file.getUserData(COPY_OF) != null) return null;
    if (file.getOriginalFile().getUserData(ErlangPsiImplUtil.ERLANG_CONSOLE) != null) return null;
    return ef;
  }

  // ------------------------------------------------------------------------------------------------ the table

  /**
   * The cheap per-file table: which macro names the file can see (its own and those of its headers, transitively),
   * which variables appear in macro bodies, and the header summaries for the expander. It never builds a copy. Its
   * dependencies are the dependencies of the copy: the file's own text, the directive lines of the included headers,
   * and the {@link IncludeTracker} of the file (what its includes and those of its headers resolve to).
   */
  static final class Table {
    final @Nullable String refusal;
    final Set<String> names = new HashSet<>();
    final Set<String> bodyVars = new HashSet<>();
    final Map<String, ErlangMacroExpander.HeaderSummary> top = new HashMap<>();
    final Map<ErlangMacroExpander.HeaderSummary, Map<String, ErlangMacroExpander.HeaderSummary>> nested = new IdentityHashMap<>();
    final List<VirtualFile> headers = new ArrayList<>();
    final Object[] deps;
    final @NotNull IncludeTracker includes;
    /** Round 5: the visible macros that are not plain (see {@link #classify}); the others need no substitution. */
    final Set<String> nonPlain = new HashSet<>();
    /** Round 5: the variables of macro bodies that are not parameters of their macro. */
    final Set<String> freeBodyVars = new HashSet<>();
    final ConcurrentHashMap<Integer, Boolean> formUse = new ConcurrentHashMap<>();
    final ConcurrentHashMap<Integer, Boolean> formNeedsCopy = new ConcurrentHashMap<>();
    final ConcurrentHashMap<Integer, Boolean> formError = new ConcurrentHashMap<>();
    final ConcurrentHashMap<Integer, Set<String>> formArgVars = new ConcurrentHashMap<>();
    final ConcurrentHashMap<Integer, Set<String>> formSubstitutedArgVars = new ConcurrentHashMap<>();
    volatile Boolean topLevelMacroForm;
    /** Round 5: some top-level form of the file needs the copy ({@link #anyFormNeedsCopy}). */
    volatile Boolean anyFormNeedsCopy;

    /** Round 5: the copy of this version of the file (a new table, a new holder: the same dependencies). */
    final @NotNull CopyHolder holder;

    Table(@Nullable String refusal, Object[] deps, @NotNull IncludeTracker includes, @Nullable VirtualFile file) {
      this.refusal = refusal;
      this.deps = deps;
      this.includes = includes;
      this.holder = new CopyHolder(file);
    }

    /** The modification counts of the dependencies (the file's text, its include resolution, its headers' directives). */
    long @NotNull [] depCounts() {
      long[] r = new long[deps.length];
      for (int i = 0; i < deps.length; i++) r[i] = ((ModificationTracker)deps[i]).getModificationCount();
      return r;
    }

    ErlangMacroExpander.IncludeResolver resolver() {
      return (owner, attribute, string) -> {
        String key = attribute + "\0" + string;
        if (owner == null) return top.get(key);
        Map<String, ErlangMacroExpander.HeaderSummary> m = nested.get(owner);
        return m == null ? null : m.get(key);
      };
    }

    long headerVersions(@NotNull Project project) {
      long v = 0;
      for (VirtualFile h : headers) v += ErlangMacroLayerRetention.getInstance(project).tracker(h).getModificationCount();
      return v;
    }
  }

  private static final Key<TableCache> TABLE_KEY = Key.create("erlang.macro.layer.table");
  private static final Key<IncludeTracker> INCLUDE_TRACKER_KEY = Key.create("erlang.macro.layer.include.tracker");
  /** Stamps of an {@link IncludeTracker} that could not check: each differs from every stamp a cached value keeps. */
  private static final AtomicLong UNCHECKED_STAMPS = new AtomicLong();

  /**
   * Round 5: what the includes of a file, and of its headers transitively, resolve to. Its count changes only when one
   * of them resolves to another file or to none. Include resolution ({@link ErlangPsiImplUtil#getDirectlyIncludedFiles})
   * reads only the file tree, the project roots, the application index and the include attributes. The attributes are
   * covered by the other dependencies (the file's own text and the header directive trackers); the rest can change only
   * with a VFS structure event (creation, deletion, move, rename, copy) or a roots change. So the tracker re-runs
   * master's resolution, once, when the VFS structure count or the roots count moved since its last check, and compares
   * the result with the last one. A header that appears or disappears changes the result (an include now resolves, or
   * no longer does, or resolves elsewhere); an unrelated file event does not. Where it cannot check (the UI thread,
   * which must not query indices; no read access; dumb mode), it answers a fresh stamp, so the caller sees the cached
   * table and copy as stale and gets master's answer; the next background question checks.
   */
  static final class IncludeTracker implements ModificationTracker {
    private final @NotNull ErlangFile myFile;
    private long myVfs = -1;
    private long myRoots = -1;
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
      synchronized (this) {
        if (vfs == myVfs && roots == myRoots) return myVersion;
      }
      var app = ApplicationManager.getApplication();
      if (app.isDispatchThread() && !isEdtBuildAllowed() || !app.isReadAccessAllowed() || project.isDisposed() ||
          DumbService.isDumb(project) || !myFile.isValid()) {
        return -UNCHECKED_STAMPS.incrementAndGet();
      }
      return update(resolveIncludes(myFile, new HashMap<>()), vfs, roots);
    }

    /** Records the resolution seen with the given VFS structure and roots counts; returns the version. */
    synchronized long update(@NotNull List<Object> resolution, long vfs, long roots) {
      if (myResolution != null && !myResolution.equals(resolution)) myVersion++;
      myResolution = resolution;
      myVfs = vfs;
      myRoots = roots;
      return myVersion;
    }
  }

  private static final Key<TextTracker> TEXT_TRACKER_KEY = Key.create("erlang.macro.layer.text.tracker");

  /**
   * Round 5: the file's own text and tree. The PSI modification stamp of a file also moves when the platform clears the
   * caches of every PSI file (a file moved or a directory deleted anywhere in the project does that), so the copy does
   * not depend on it. This tracker counts the PSI tree change events of the file itself instead
   * ({@link ErlangMacroLayerRetention} listens): every reparse and every PSI edit fires them, a cache clearing does not.
   * A text that comes back to an earlier text (undo) still counts: its reparse made new elements, and cached answers
   * keep elements.
   */
  static final class TextTracker implements ModificationTracker {
    private final AtomicLong myVersion = new AtomicLong();
    /** Round 5: System.nanoTime() of the last change; valid when {@link #myChanged}. */
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
  private static @NotNull List<Object> resolveIncludes(@NotNull ErlangFile file, @NotNull Map<ErlangFile, Map<String, ErlangFile>> direct) {
    List<Object> r = new ArrayList<>();
    Set<ErlangFile> seen = new HashSet<>();
    ArrayDeque<ErlangFile> queue = new ArrayDeque<>();
    queue.add(file);
    seen.add(file);
    while (!queue.isEmpty()) {
      ProgressManager.checkCanceled();
      ErlangFile f = queue.poll();
      Map<String, ErlangFile> includes = direct.computeIfAbsent(f, ErlangMacroLayer::directIncludes);
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
  private static final Key<CachedValue<ErlangMacroExpander.HeaderSummary>> SUMMARY_KEY = Key.create("erlang.macro.layer.summary");
  public static final AtomicInteger SUMMARIES_BUILT = new AtomicInteger();
  public static final AtomicInteger TABLES_BUILT = new AtomicInteger();
  public static final AtomicInteger HEADER_TARGETS_MATERIALIZED = new AtomicInteger();

  /** A real .erl file the layer may work on: enabled, not a copy, not a console, not a header, not during indexing. */
  static @Nullable ErlangFile layerFile(@Nullable PsiElement element) {
    if (!isEnabled()) return null;
    ErlangFile file = realFile(element);
    if (file == null) return null;
    VirtualFile vf = file.getOriginalFile().getVirtualFile();
    String name = vf != null ? vf.getName() : file.getName();
    if (!name.endsWith(".erl")) return null;
    if (DumbService.isDumb(file.getProject())) return null;
    return file;
  }

  /** The summary of a header, made by lexing its text; cached on the header, one per header text. */
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
   * Round 5, fix 5: the table of a file, the counts of its dependencies when it was computed, and the stamps (the
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

  private static @NotNull TableCache tableCache(@NotNull ErlangFile file) {
    TableCache c = file.getUserData(TABLE_KEY);
    return c != null ? c : ((UserDataHolderEx)file).putUserDataIfAbsent(TABLE_KEY, new TableCache());
  }

  /** The table of a file; on the UI thread only a cached one (never computed there). */
  static @Nullable Table table(@NotNull ErlangFile file) {
    if (!ErlangMacroLayerStats.on()) return table0(file);
    long g0 = System.nanoTime();
    Table t = table0(file);
    ErlangMacroLayerStats.phase(t == null ? "table-get:edt-miss" : "table-get", file.getName(), System.nanoTime() - g0);
    return t;
  }

  private static @Nullable Table table0(@NotNull ErlangFile file) {
    Project project = file.getProject();
    TableCache cache = tableCache(file);
    long psi = PsiModificationTracker.getInstance(project).getModificationCount();
    long vfs = VirtualFileManager.VFS_STRUCTURE_MODIFICATIONS.getModificationCount();
    long roots = ProjectRootManager.getInstance(project).getModificationCount();
    Table valid = validTable(file, cache, psi, vfs, roots);
    if (valid != null) return valid;
    if (ApplicationManager.getApplication().isDispatchThread() && !isEdtBuildAllowed()) return null;
    return buildTable(file, cache, psi, vfs, roots);
  }

  /** The cached table if it is valid for these stamps (checking its dependencies once after a change), else null. */
  private static @Nullable Table validTable(@NotNull ErlangFile file, @NotNull TableCache cache, long psi, long vfs, long roots) {
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
  private static @NotNull Table buildTable(@NotNull ErlangFile file, @NotNull TableCache cache, long psi, long vfs, long roots) {
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
      Table t = awaitCancellably(other.result());
      if (t != null) return t;
    }
  }

  private static @NotNull String key(@NotNull String attribute, @Nullable String string) {
    return attribute + "\0" + string;
  }

  private static @Nullable String includeString(@Nullable ErlangIncludeString s) {
    return s == null ? null : StringUtil.unquoteString(s.getText());
  }

  private static @NotNull Table computeTable(@NotNull ErlangFile file) {
    long c0 = ErlangMacroLayerStats.on() ? System.nanoTime() : 0;
    try {
      return computeTable0(file);
    }
    finally {
      if (c0 != 0) ErlangMacroLayerStats.phase("table-build", file.getName(), System.nanoTime() - c0);
    }
  }

  private static @NotNull Table computeTable0(@NotNull ErlangFile file) {
    TABLES_BUILT.incrementAndGet();
    Project project = file.getProject();
    List<Object> deps = new ArrayList<>();
    deps.add(textTracker(file));
    // the counts first: a change during the resolution below makes the tracker check again
    long vfs = VirtualFileManager.VFS_STRUCTURE_MODIFICATIONS.getModificationCount();
    long roots = ProjectRootManager.getInstance(project).getModificationCount();
    Map<ErlangFile, Map<String, ErlangFile>> direct = new HashMap<>();
    IncludeTracker includes = includeTracker(file);
    includes.update(resolveIncludes(file, direct), vfs, roots);
    deps.add(includes);
    // own macros
    Set<String> names = new HashSet<>();
    Set<String> bodyVars = new HashSet<>();
    Map<String, List<ErlangMacroExpander.DefineShape>> shapes = new HashMap<>();
    for (ErlangMacrosDefinition d : file.getMacroses()) {
      ProgressManager.checkCanceled();
      String n = d.getName();
      if (n != null) names.add(n);
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
    String refusal = null;
    try {
      for (Map.Entry<String, ErlangFile> e : direct.get(file).entrySet()) {
        if (e.getValue() == null) throw new Refusal("unresolved-include");
        top.put(e.getKey(), visitHeader(e.getValue(), new ArrayList<>(), nested, headers, names, bodyVars, shapes, direct));
      }
    }
    catch (Refusal r) {
      refusal = r.getMessage();
    }
    for (VirtualFile h : headers) deps.add(ErlangMacroLayerRetention.getInstance(project).tracker(h));
    Table t = new Table(refusal, deps.toArray(), includes, file.getOriginalFile().getVirtualFile());
    if (refusal == null) {
      t.names.addAll(names);
      t.bodyVars.addAll(bodyVars);
      classify(t, shapes);
      t.top.putAll(top);
      t.nested.putAll(nested);
    }
    t.headers.addAll(headers);
    return t;
  }

  /**
   * Round 5: which visible macros need substitution. A macro is plain when every definition of its name (in the file and
   * in its headers, any arity, any conditional branch) has a locally plain body ({@link ErlangMacroExpander#isPlainBody})
   * and every macro name used in those bodies is predefined or is itself plain (transitively); a name used in a body
   * that the file cannot see makes the macro not plain.
   */
  private static void classify(@NotNull Table t, @NotNull Map<String, List<ErlangMacroExpander.DefineShape>> shapes) {
    for (String name : t.names) {
      List<ErlangMacroExpander.DefineShape> list = shapes.get(name);
      if (list == null || list.isEmpty()) {
        t.nonPlain.add(name);
        continue;
      }
      for (ErlangMacroExpander.DefineShape shape : list) {
        t.freeBodyVars.addAll(shape.freeVars());
        if (!shape.plainBody()) t.nonPlain.add(name);
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
  }

  private static @NotNull ErlangMacroExpander.DefineShape unknownShape(@NotNull String name) {
    return new ErlangMacroExpander.DefineShape(name, -1, List.of(), false, Set.of(), Set.of());
  }

  /** The significant leaves of an element as expander tokens (types and texts only). */
  private static @NotNull List<ErlangMacroExpander.Tok> leafTokens(@NotNull PsiElement element) {
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

  private static final class Refusal extends Exception {
    Refusal(String reason) {
      super(reason, null, false, false);
    }
  }

  /** The includes of a file (from its stubs or PSI), by attribute and string, with master's resolution (null: none). */
  private static @NotNull Map<String, ErlangFile> directIncludes(@NotNull ErlangFile file) {
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

  private static @NotNull ErlangMacroExpander.HeaderSummary visitHeader(
    @NotNull ErlangFile header, @NotNull List<VirtualFile> path,
    @NotNull Map<ErlangMacroExpander.HeaderSummary, Map<String, ErlangMacroExpander.HeaderSummary>> nested,
    @NotNull List<VirtualFile> headers, @NotNull Set<String> names, @NotNull Set<String> bodyVars,
    @NotNull Map<String, List<ErlangMacroExpander.DefineShape>> shapes,
    @NotNull Map<ErlangFile, Map<String, ErlangFile>> direct) throws Refusal {
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
        if (d.tokens.get(i).type() == org.intellij.erlang.ErlangTypes.ERL_VAR) bodyVars.add(d.tokens.get(i).text());
      }
    }
    if (nested.containsKey(s)) return s;
    for (ErlangMacroExpander.DefineShape shape : s.defineShapes()) shapes.computeIfAbsent(shape.name(), k -> new ArrayList<>()).add(shape);
    Map<String, ErlangMacroExpander.HeaderSummary> children = new HashMap<>();
    nested.put(s, children);
    if (s.includes().isEmpty()) return s;
    Map<String, ErlangFile> resolved = direct.computeIfAbsent(header, ErlangMacroLayer::directIncludes);
    path.add(vf);
    try {
      for (ErlangMacroExpander.Directive d : s.includes()) {
        String k = key(d.attribute, d.includeString());
        ErlangFile child = resolved.get(k);
        if (child == null) throw new Refusal("unresolved-include");
        children.put(k, visitHeader(child, path, nested, headers, names, bodyVars, shapes, direct));
      }
    }
    finally {
      path.removeLast();
    }
    return s;
  }

  // ------------------------------------------------------------------------------------------------ the copy cache

  /** Holds a copy strongly while the retention service keeps it, else weakly. */
  static final class CopyHolder {
    final @Nullable VirtualFile file;
    volatile @Nullable Copy strong;
    volatile @NotNull WeakReference<Copy> weak = new WeakReference<>(null);
    volatile boolean everBuilt;
    /** The build in progress, if any: its thread and the copy it will give (null when it ends without one). */
    final AtomicReference<Build> building = new AtomicReference<>();

    CopyHolder(@Nullable VirtualFile file) {
      this.file = file;
    }

    @Nullable Copy get() {
      Copy c = strong;
      return c != null ? c : weak.get();
    }
  }

  record Build(@NotNull Thread thread, @NotNull CompletableFuture<Copy> result) {
  }

  /**
   * Round 5: the copy of a holder, built once. Highlighting asks from several threads at once: one thread builds, the
   * others wait for its result. A waiter holds a read action, which a write action must be able to cancel, so the wait
   * checks for cancellation (a monitor wait cannot). A build that ends without a copy (cancelled or failed) completes
   * with null; then a waiter that is still running builds itself. Null also when the building thread asks again
   * inside its own build (the layer then gives master's answer).
   */
  private static @Nullable Copy obtain(@NotNull CopyHolder holder, @NotNull ErlangFile file, @NotNull Table table,
                                       @NotNull String trigger) {
    while (true) {
      Copy copy = holder.get();
      if (copy != null) return copy;
      String uncancellable = nonCancellableCaller();
      if (uncancellable != null) {
        // round 5, fix 6: such a caller neither builds nor waits for another thread's build
        ErlangMacroLayerStats.phase("build-skipped:" + uncancellable, file.getName(), 0);
        ErlangMacroLayerLog.event("SKIPPED", file.getName(), 0, 0, 0, uncancellable, trigger);
        return null;
      }
      Build running = holder.building.get();
      if (running == null) {
        String refusal = buildRefusal(file);
        if (refusal != null) {
          ErlangMacroLayerStats.phase("build-skipped:" + refusal, file.getName(), 0);
          ErlangMacroLayerLog.event("SKIPPED", file.getName(), 0, 0, 0, refusal, trigger);
          return null;
        }
      }
      Build mine = new Build(Thread.currentThread(), new CompletableFuture<>());
      Build other = holder.building.compareAndExchange(null, mine);
      if (other == null) {
        Copy built = null;
        try {
          built = holder.get(); // another build may have ended just before
          if (built == null) {
            boolean stats = ErlangMacroLayerStats.on();
            long b0 = stats ? System.nanoTime() : 0;
            try {
              built = build(file, table, trigger, holder.everBuilt ? "collected" : null);
            }
            finally {
              if (stats) ErlangMacroLayerStats.inner("build", file.getName(), System.nanoTime() - b0);
            }
            holder.weak = new WeakReference<>(built);
            holder.everBuilt = true;
          }
          return built;
        }
        finally {
          holder.building.set(null);
          mine.result().complete(built);
        }
      }
      if (other.thread() == Thread.currentThread()) return null;
      boolean stats = ErlangMacroLayerStats.on();
      long w0 = stats ? System.nanoTime() : 0;
      try {
        copy = awaitCancellably(other.result());
      }
      finally {
        if (stats) {
          long waited = System.nanoTime() - w0;
          ErlangMacroLayerStats.inner("lock-wait", file.getName(), waited);
          if (waited > 20_000_000L) {
            ErlangMacroLayerLog.event("LOCK_WAIT", file.getName(), waited, 0, 0, "held-by=" + other.thread().getName(), trigger,
                                      "copyThere=" + (holder.get() != null));
          }
        }
      }
      if (copy != null) return copy;
    }
  }

  /** Waits for a build of another thread; throws {@link ProcessCanceledException} as soon as this thread is cancelled. */
  private static <T> @Nullable T awaitCancellably(@NotNull CompletableFuture<T> build) {
    while (true) {
      ProgressManager.checkCanceled();
      try {
        return build.get(10, TimeUnit.MILLISECONDS);
      }
      catch (TimeoutException ignored) {
      }
      catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        throw new ProcessCanceledException(e);
      }
      catch (ExecutionException e) {
        return null; // not completed exceptionally; a failed build completes with null
      }
    }
  }

  /**
   * Round 5, fix 2: after an edit of a file, only its highlighting starts a copy build for {@link #quietMillis()}; other
   * callers (the target-element search, {@code isReferenceTo} of the identifier highlighting, ... which the next key
   * press cancels) use a copy that is there or being built, else they get master's answer. Highlighting is a caller
   * under the daemon's progress indicator (also through the wrappers of the inspection threads).
   */
  private static @Nullable String buildRefusal(@NotNull PsiFile file) {
    if (isEdtBuildAllowed()) return null; // diagnostic: builds allowed anywhere
    if (isHighlighting(ProgressManager.getGlobalProgressIndicator())) return null;
    TextTracker tt = textTrackerIfAny(file);
    long quiet = quietMillis();
    if (tt != null && quiet > 0 && tt.changedWithin(quiet)) return "typing";
    return null;
  }

  /**
   * Round 5, fix 6: the caller cannot be cancelled: it runs in a non-cancellable section, or with no progress indicator
   * (a plain read action on a pooled thread). A key press is a write action: it waits until every read action ends,
   * and a caller that cannot be cancelled would hold it for a whole build (or a wait for one). Such a caller gets the
   * copy that is there, or master's answer. Null when the caller is cancellable (or builds are allowed anywhere).
   */
  private static @Nullable String nonCancellableCaller() {
    if (isEdtBuildAllowed()) return null; // diagnostic: builds allowed anywhere
    ProgressManager pm = ProgressManager.getInstance();
    if (pm.isInNonCancelableSection()) return "non-cancellable";
    if (ProgressManager.getGlobalProgressIndicator() == null) return "no-indicator";
    return null;
  }

  static boolean isHighlighting(@Nullable ProgressIndicator indicator) {
    ProgressIndicator p = indicator;
    for (int depth = 0; p != null && depth < 16; depth++) {
      if (p instanceof DaemonProgressIndicator) return true;
      p = p instanceof WrappedProgressIndicator w ? w.getOriginalProgressIndicator() : null;
    }
    return false;
  }

  /** Round 5: {@code erlang.macro.layer.quiet} / {@code ERLANG_MACRO_LAYER_QUIET}, ms, default 1,000. */
  private static final int QUIET_MS = readInt("erlang.macro.layer.quiet", "ERLANG_MACRO_LAYER_QUIET", 1000);
  private static volatile @Nullable Integer ourTestQuiet;

  static long quietMillis() {
    Integer q = ourTestQuiet;
    return q != null ? q : QUIET_MS;
  }

  /** Sets the quiet time after an edit during which only highlighting builds the copy (0: any caller builds). */
  @TestOnly
  public static void setQuietMillisForTests(int millis, @NotNull Disposable parent) {
    Integer previous = ourTestQuiet;
    ourTestQuiet = millis;
    Disposer.register(parent, () -> ourTestQuiet = previous);
  }

  /** The cached copy of a real file; built lazily from the PSI text. */
  public static @Nullable Copy getCopy(@NotNull PsiFile file) {
    return getCopy(file, "direct-get");
  }

  static @Nullable Copy getCopy(@NotNull PsiFile psiFile, @NotNull String trigger) {
    ErlangFile file = layerFile(psiFile);
    if (file == null) return null;
    Table table = table(file);
    if (table == null) {
      ErlangMacroLayerLog.event("EDT_MISS", file.getName(), 0, 0, 0, "table", trigger);
      return null;
    }
    if (table.refusal != null) return null;
    Project project = file.getProject();
    boolean edt = ApplicationManager.getApplication().isDispatchThread() && !isEdtBuildAllowed();
    if (edt) {
      CopyHolder h = table.holder;
      Copy c = h.get();
      if (c == null) ErlangMacroLayerLog.event("EDT_MISS", file.getName(), 0, 0, 0, "copy", trigger);
      else ErlangMacroLayerRetention.getInstance(project).touch(h, c);
      return c;
    }
    CopyHolder holder = table.holder;
    Copy copy = obtain(holder, file, table, trigger);
    if (copy == null) return null;
    long r0 = ErlangMacroLayerStats.on() ? System.nanoTime() : 0;
    ErlangMacroLayerRetention.getInstance(project).touch(holder, copy);
    if (r0 != 0) ErlangMacroLayerStats.phase("retention-touch", null, System.nanoTime() - r0);
    return copy;
  }

  /** The cached copy if it is built and up to date; never builds. */
  @TestOnly
  public static @Nullable Copy peekCopy(@NotNull PsiFile file) {
    if (!(file instanceof ErlangFile ef) || ef.getUserData(TABLE_KEY) == null) return null;
    Project project = file.getProject();
    Table t = validTable(ef, tableCache(ef), PsiModificationTracker.getInstance(project).getModificationCount(),
                         VirtualFileManager.VFS_STRUCTURE_MODIFICATIONS.getModificationCount(),
                         ProjectRootManager.getInstance(project).getModificationCount());
    return t == null ? null : t.holder.get();
  }

  /** The gate steps for a variable reference, for tests. */
  @TestOnly
  public static @NotNull String explainForTests(@NotNull PsiReference ref) {
    PsiElement element = ref.getElement();
    ErlangFile file = layerFile(element);
    if (file == null) return "no layer file";
    Table t = table(file);
    if (t == null) return "no table";
    PsiElement form = topForm(element);
    Copy c = peekCopy(file);
    return "refusal=" + t.refusal + " names=" + t.names + " bodyVars=" + t.bodyVars + " form=" + (form == null ? null : form.getClass().getSimpleName()) +
           " formUses=" + (form != null && formUsesMacro(t, form)) + " argVars=" + (form == null ? null : argumentVars(t, form)) +
           " refClass=" + ref.getClass().getSimpleName() + " copy=" + (c == null ? null : c.expansion.text.length()) +
           " answers=" + (c == null ? null : c.answers.keySet());
  }

  /** Why a form needs the copy (the first reason found), or null; for tests and measurements. */
  @TestOnly
  static @Nullable String whyFormNeedsCopyForTests(@NotNull Table t, @NotNull PsiElement form) {
    if (!formUsesMacro(t, form)) return null;
    if (form instanceof PsiErrorElement) return "error-form";
    PsiElement first = PsiTreeUtil.firstChild(form);
    if (first == null || first instanceof PsiErrorElement || "?".equals(first.getText())) return "starts-with-?";
    if (formHasError(t, form)) return "parse-error";
    if ("-".equals(first.getText())) {
      PsiElement attribute = PsiTreeUtil.nextVisibleLeaf(first);
      if (attribute != null && ErlangMacroExpander.isDirectiveName(attribute.getText())) return null;
    }
    int end = form.getTextRange().getEndOffset();
    for (PsiElement leaf = first; leaf != null && leaf.getTextRange().getStartOffset() < end; leaf = PsiTreeUtil.nextLeaf(leaf)) {
      if (leaf.getNode().getElementType() != org.intellij.erlang.ErlangTypes.ERL_QMARK) continue;
      PsiElement parent = leaf.getParent();
      if (parent instanceof ErlangMacrosArg) continue;
      if (!(parent instanceof ErlangMacros m) || leaf != m.getFirstChild()) return "odd-?:" + parent.getClass().getSimpleName();
      ErlangMacrosName nameElement = m.getMacrosName();
      if (nameElement == null) return "no-name";
      String name = StringUtil.unquoteString(nameElement.getText(), '\'');
      if (!t.names.contains(name)) continue;
      if (t.nonPlain.contains(name)) return "not-plain:" + name;
      if (!isPlainPosition(m)) {
        StringBuilder chain = new StringBuilder();
        for (PsiElement e = m.getParent(); e != null && e != form && chain.length() < 120; e = e.getParent()) chain.append(e.getClass().getSimpleName().replace("Erlang", "").replace("Impl", "")).append('<');
        return "position:" + name + ":" + chain;
      }
    }
    return null;
  }

  @TestOnly
  static boolean formUsesMacroForTests(@NotNull Table t, @Nullable PsiElement form) {
    return formUsesMacro(t, form);
  }

  /** Why the layer gives master's behaviour for the whole file (unresolved include, cycle, ...), or null. */
  @TestOnly
  public static @Nullable String refusalForTests(@NotNull PsiFile file) {
    if (!(file instanceof ErlangFile ef)) return "not-erlang";
    if (!file.getName().endsWith(".erl")) return "not-erl";
    return computeTable(ef).refusal;
  }

  /** Builds the copy without the cache (also used by measurement tests). */
  public static @NotNull Copy build(@NotNull PsiFile file) {
    Table t = file instanceof ErlangFile ef && !DumbService.isDumb(file.getProject()) ? computeTable(ef) : null;
    return build(file, t != null && t.refusal == null ? t : null, "direct", null);
  }

  private static final Key<long[]> LAST_BUILD_STATE = Key.create("erlang.macro.layer.last.build");
  private static volatile @Nullable Runnable ourBuildHookForTests;

  /** Runs at the start of every cached build (not of {@link #build(PsiFile)}), on the building thread. */
  @TestOnly
  public static void setBuildHookForTests(@Nullable Runnable hook, @NotNull Disposable parent) {
    ourBuildHookForTests = hook;
    Disposer.register(parent, () -> ourBuildHookForTests = null);
  }

  private static @NotNull Copy build(@NotNull PsiFile file, @Nullable Table table, @NotNull String trigger, @Nullable String why) {
    if (!ErlangMacroLayerLog.isEnabled()) return build0(file, table, trigger, why, null);
    BuildProbe probe = new BuildProbe();
    try {
      return build0(file, table, trigger, why, probe);
    }
    catch (com.intellij.openapi.progress.ProcessCanceledException e) {
      long dt = System.nanoTime() - probe.t0;
      ErlangMacroLayerStats.phase("build-cancelled", file.getName(), dt);
      ErlangMacroLayerLog.event("CANCELLED", file.getName(), dt, 0, 0, "phase=" + probe.phase, trigger, probe.details());
      throw e;
    }
  }

  /** Spike 5: what one build spends, by phase, with GC and thread CPU time; for the log only. */
  private static final class BuildProbe {
    final long t0 = System.nanoTime();
    final long cpu0 = threadCpu();
    final long gc0 = gcMillis();
    final long gcCount0 = gcCount();
    long lex, expand, parse, errors;
    String phase = "start";

    String details() {
      com.intellij.openapi.progress.ProgressIndicator pi = ProgressManager.getGlobalProgressIndicator();
      return String.format(Locale.ROOT, "lex=%.1f expand=%.1f parse=%.1f errors=%.1f cpu=%.1f gcMs=%d gcCount=%d indicator=%s nonCancelable=%s",
                           lex / 1e6, expand / 1e6, parse / 1e6, errors / 1e6, (threadCpu() - cpu0) / 1e6, gcMillis() - gc0,
                           gcCount() - gcCount0, pi == null ? "none" : pi.getClass().getSimpleName(),
                           ProgressManager.getInstance().isInNonCancelableSection());
    }

    static long threadCpu() {
      java.lang.management.ThreadMXBean b = java.lang.management.ManagementFactory.getThreadMXBean();
      return b.isCurrentThreadCpuTimeSupported() ? b.getCurrentThreadCpuTime() : 0;
    }

    static long gcMillis() {
      long n = 0;
      for (java.lang.management.GarbageCollectorMXBean b : java.lang.management.ManagementFactory.getGarbageCollectorMXBeans()) {
        n += Math.max(0, b.getCollectionTime());
      }
      return n;
    }

    static long gcCount() {
      long n = 0;
      for (java.lang.management.GarbageCollectorMXBean b : java.lang.management.ManagementFactory.getGarbageCollectorMXBeans()) {
        n += Math.max(0, b.getCollectionCount());
      }
      return n;
    }
  }

  private static @NotNull Copy build0(@NotNull PsiFile file, @Nullable Table table, @NotNull String trigger, @Nullable String why,
                                      @Nullable BuildProbe probe) {
    long t0 = System.nanoTime();
    Project project = file.getProject();
    long[] now = {textTracker(file).getModificationCount(), table != null ? table.headerVersions(project) : 0,
      table != null ? table.includes.getModificationCount() : 0};
    long[] last = file.getUserData(LAST_BUILD_STATE);
    String reason = why != null ? why : last == null ? "first" : last[0] != now[0] ? "own-text" :
                                        last[1] != now[1] ? "header-directives" :
                                        last[2] != now[2] ? "include-resolution" : "same-state";
    Runnable hook = ourBuildHookForTests;
    if (hook != null && !"direct".equals(trigger)) hook.run();
    if (!"direct".equals(trigger)) file.putUserData(LAST_BUILD_STATE, now);
    EXPANSIONS_BUILT.incrementAndGet();
    CharSequence text = file.getViewProvider().getContents();
    if (file.getTextLength() != text.length() || !file.textMatches(text)) text = file.getText();
    if (probe != null) probe.phase = "expand";
    Expansion expansion = ErlangMacroExpander.expand(text, new ErlangLexer(project), FORM_CAP,
                                                     table != null ? table.resolver() : null);
    long p0 = System.nanoTime();
    if (probe != null) {
      probe.lex = expansion.lexNanos;
      probe.expand = p0 - t0 - expansion.lexNanos;
      probe.phase = "parse";
    }
    ErlangFile copyFile = null;
    if (expansion.hasExpansions()) {
      COPIES_PARSED.incrementAndGet();
      PsiFile parsed = PsiFileFactory.getInstance(project)
        .createFileFromText(file.getName(), ErlangLanguage.INSTANCE, expansion.text, false, false);
      if (parsed instanceof ErlangFile ef) {
        parsed.putUserData(COPY_OF, file);
        if (parsed instanceof PsiFileImpl impl) impl.setOriginalFile(file);
        copyFile = ef;
        parsed.getNode().getFirstChildNode(); // parse now, so that the build time is honest
      }
    }
    long e0 = System.nanoTime();
    if (probe != null) {
      probe.parse = e0 - p0;
      probe.phase = "errors";
    }
    Copy copy = new Copy(file, expansion, copyFile, System.nanoTime() - t0);
    if (probe != null) {
      probe.errors = System.nanoTime() - e0;
      ErlangMacroLayerStats.phase("build:lex", file.getName(), probe.lex);
      ErlangMacroLayerStats.phase("build:expand", file.getName(), probe.expand);
      ErlangMacroLayerStats.phase("build:parse", file.getName(), probe.parse);
      ErlangMacroLayerStats.phase("build:errors-walk", file.getName(), probe.errors);
    }
    ErlangMacroLayerLog.event("BUILD", file.getName(), copy.buildNanos, expansion.sourceTokens, expansion.expandedTokens,
                              reason, trigger, probe != null ? probe.details() : "");
    return copy;
  }

  // ------------------------------------------------------------------------------------------------ exits

  /** The I2 guard: nothing from a copy leaves the layer. */
  static @Nullable PsiElement exit(@Nullable PsiElement e, @NotNull PsiFile asking) {
    if (e == null) return null;
    PsiFile f = e.getContainingFile();
    boolean ok = e.isValid() && f != null && f.getUserData(COPY_OF) == null && (!asking.isPhysical() || e.isPhysical());
    if (ok) return e;
    GUARD_VIOLATIONS.incrementAndGet();
    if (ApplicationManager.getApplication().isUnitTestMode()) {
      throw new AssertionError("macro layer: an element of an expanded copy left the layer: " + e + " in " + f);
    }
    return null;
  }

  // ------------------------------------------------------------------------------------------------ gates

  /** The top-level element (a child of the file) that contains the element. */
  static @Nullable PsiElement topForm(@NotNull PsiElement element) {
    PsiElement form = element;
    while (form.getParent() != null && !(form.getParent() instanceof PsiFile)) form = form.getParent();
    return form.getParent() instanceof PsiFile ? form : null;
  }

  /** The text has a use {@code ?NAME} of a name the file can see. Superset: strings and comments count too. */
  public static boolean usesKnownMacro(@NotNull CharSequence s, @NotNull Set<String> names) {
    int n = s.length();
    for (int i = 0; i < n; i++) {
      if (s.charAt(i) != '?') continue;
      int j = i + 1;
      if (j < n && s.charAt(j) == '?') {
        i = j;
        continue;
      }
      while (j < n && Character.isWhitespace(s.charAt(j))) j++;
      int k;
      String name;
      if (j < n && s.charAt(j) == '\'') {
        k = j + 1;
        while (k < n && s.charAt(k) != '\'') k++;
        name = s.subSequence(j + 1, Math.min(k, n)).toString();
      }
      else {
        k = j;
        while (k < n && (Character.isLetterOrDigit(s.charAt(k)) || s.charAt(k) == '_' || s.charAt(k) == '@')) k++;
        if (k == j) continue;
        name = s.subSequence(j, k).toString();
      }
      if (names.contains(name)) return true;
    }
    return false;
  }

  /** The form (or, for a top-level error, the form before it) uses a macro the file can see. Never builds a copy. */
  private static boolean formUsesMacro(@NotNull Table t, @Nullable PsiElement form) {
    if (form == null || t.names.isEmpty()) return false;
    int at = form.getTextRange().getStartOffset();
    Boolean b = t.formUse.get(at);
    if (b == null) {
      b = usesKnownMacro(form.getNode().getChars(), t.names);
      if (!b && (form instanceof PsiErrorElement || PsiTreeUtil.firstChild(form) instanceof PsiErrorElement)) {
        PsiElement prev = PsiTreeUtil.skipWhitespacesAndCommentsBackward(form);
        b = prev != null && usesKnownMacro(prev.getNode().getChars(), t.names);
      }
      t.formUse.put(at, b);
    }
    return b;
  }

  /** Names of the variables inside macro calls (their arguments) of the form. */
  private static @NotNull Set<String> argumentVars(@NotNull Table t, @NotNull PsiElement form) {
    return t.formArgVars.computeIfAbsent(form.getTextRange().getStartOffset(), k -> {
      Set<String> r = new HashSet<>();
      // a variable inside a macro use: some element between it and the form starts with '?' (a superset)
      for (ErlangQVar v : PsiTreeUtil.findChildrenOfType(form, ErlangQVar.class)) {
        for (PsiElement p = v.getParent(); p != null && p != form; p = p.getParent()) {
          PsiElement first = PsiTreeUtil.firstChild(p);
          if (first != null && first != v && first.textMatches("?")) {
            r.add(v.getText());
            break;
          }
        }
      }
      return r;
    });
  }

  /**
   * Round 5: names of the variables inside the arguments of the macro uses of the form that need substitution (the
   * macro is not plain, or the use is not in a plain slot). Every enclosing use counts: {@code ?PLAIN(?OTHER(X))}
   * counts X for ?OTHER. An element that starts with {@code ?} but is not a macro use counts too (a superset).
   */
  private static @NotNull Set<String> substitutedArgumentVars(@NotNull Table t, @NotNull PsiElement form) {
    return t.formSubstitutedArgVars.computeIfAbsent(form.getTextRange().getStartOffset(), k -> {
      Set<String> r = new HashSet<>();
      for (ErlangQVar v : PsiTreeUtil.findChildrenOfType(form, ErlangQVar.class)) {
        if (r.contains(v.getText())) continue;
        for (PsiElement p = v.getParent(); p != null && p != form; p = p.getParent()) {
          PsiElement first = PsiTreeUtil.firstChild(p);
          if (first == null || first == v || !first.textMatches("?")) continue;
          if (!(first.getParent() instanceof ErlangMacros m)) {
            r.add(v.getText());
            break;
          }
          ErlangMacrosName nameElement = m.getMacrosName();
          String name = nameElement == null ? null : StringUtil.unquoteString(nameElement.getText(), '\'');
          if (name == null || t.names.contains(name) && (t.nonPlain.contains(name) || !isPlainPosition(m))) {
            r.add(v.getText());
            break;
          }
        }
      }
      return r;
    });
  }

  private static boolean formHasError(@NotNull Table t, @NotNull PsiElement form) {
    return t.formError.computeIfAbsent(form.getTextRange().getStartOffset(), k -> PsiTreeUtil.hasErrorElements(form));
  }

  /**
   * Round 5: the form uses a visible macro and needs substitution: the copy may answer for it. That is the case when
   * the form is broken in the main tree, or starts with {@code ?}, or has a use {@code ?NAME} of a visible macro that
   * is not plain ({@link #classify}), or of a plain macro in a position other than a plain expression slot
   * ({@link #isPlainPosition}). A directive ({@code -define}, {@code -include}, {@code -ifdef}, ...) never needs it:
   * the copy keeps directives as they are and never answers inside them. With the wide gate: every form with a use.
   */
  /**
   * Round 5: some top-level form of the file needs the copy (computed once per table, with every form's answer). When
   * none does, every question is answered "none" after this one check: no form can be answered by the copy, and no
   * form can make a name (a form that could, a top-level macro use or a broken form with a macro use, needs the copy).
   */
  static boolean anyFormNeedsCopy(@NotNull ErlangFile file, @NotNull Table t) {
    Boolean b = t.anyFormNeedsCopy;
    if (b != null) return b;
    b = false;
    if (!t.names.isEmpty()) {
      for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) {
        if (c instanceof PsiWhiteSpace || c instanceof PsiComment) continue;
        ProgressManager.checkCanceled();
        if (formNeedsCopy(t, c)) {
          b = true;
          break;
        }
      }
    }
    t.anyFormNeedsCopy = b;
    return b;
  }

  static boolean formNeedsCopy(@NotNull Table t, @Nullable PsiElement form) {
    if (!formUsesMacro(t, form)) return false;
    if (isGateWide()) return true;
    return t.formNeedsCopy.computeIfAbsent(form.getTextRange().getStartOffset(), k -> computeFormNeedsCopy(t, form));
  }

  private static boolean computeFormNeedsCopy(@NotNull Table t, @NotNull PsiElement form) {
    if (form instanceof PsiErrorElement) return true;
    PsiElement first = PsiTreeUtil.firstChild(form);
    if (first == null || first instanceof PsiErrorElement || "?".equals(first.getText())) return true;
    if (formHasError(t, form)) return true;
    if ("-".equals(first.getText())) {
      PsiElement attribute = PsiTreeUtil.nextVisibleLeaf(first);
      if (attribute != null && ErlangMacroExpander.isDirectiveName(attribute.getText())) return false;
    }
    int end = form.getTextRange().getEndOffset();
    for (PsiElement leaf = first; leaf != null && leaf.getTextRange().getStartOffset() < end; leaf = PsiTreeUtil.nextLeaf(leaf)) {
      if (leaf.getNode().getElementType() != org.intellij.erlang.ErlangTypes.ERL_QMARK) continue;
      PsiElement parent = leaf.getParent();
      if (parent instanceof ErlangMacrosArg) continue; // ??X: the expander leaves it as it is
      if (!(parent instanceof ErlangMacros m) || leaf != m.getFirstChild()) return true;
      ErlangMacrosName nameElement = m.getMacrosName();
      if (nameElement == null) return true;
      String name = StringUtil.unquoteString(nameElement.getText(), '\'');
      if (!t.names.contains(name)) continue; // predefined or invisible: never substituted
      if (t.nonPlain.contains(name) || !isPlainPosition(m)) return true;
    }
    return false;
  }

  /**
   * Round 5: the macro use stands alone in a slot of a value: a whole expression ({@code ?M}), the callee of a local
   * call ({@code ?M(...)}), a whole type operand, or the module of a remote call ({@code ?M:f(...)}: the remote function
   * reference never asks the layer). It is not next to {@code #} (a record name), and not before {@code :} or
   * {@code (} unless it is the callee or the module of a call. In any other position (a record name, the module of a
   * fun or of a remote type, a field name, an arity, a string concatenation, an export entry, ...) the body gives the
   * context of other names, so the use needs substitution even when its macro is plain.
   */
  private static boolean isPlainPosition(@NotNull ErlangMacros m) {
    PsiElement prev = PsiTreeUtil.prevVisibleLeaf(m);
    if (prev != null && "#".equals(prev.getText())) return false;
    PsiElement unit = m;
    PsiElement parent = m.getParent();
    if (parent instanceof ErlangQAtom && onlyChild(parent, m)) {
      unit = parent;
      parent = parent.getParent();
    }
    if (parent instanceof ErlangFunctionCallExpression call && unit instanceof ErlangQAtom && call.getQAtom() == unit) return true;
    if (parent instanceof ErlangModuleRef && unit instanceof ErlangQAtom && parent.getParent() instanceof ErlangGlobalFunctionCallExpression) {
      return true;
    }
    boolean slot = parent instanceof ErlangMaxExpression && onlyChild(parent, unit) ||
                   unit == m && parent instanceof ErlangIntType && onlyChild(parent, m);
    if (!slot) return false;
    PsiElement next = PsiTreeUtil.nextVisibleLeaf(m);
    return next == null || !":".equals(next.getText()) && !"(".equals(next.getText());
  }

  private static boolean onlyChild(@NotNull PsiElement parent, @NotNull PsiElement child) {
    for (PsiElement c = parent.getFirstChild(); c != null; c = c.getNextSibling()) {
      if (c == child || c instanceof PsiWhiteSpace || c instanceof PsiComment) continue;
      return false;
    }
    return true;
  }

  /**
   * Round 5: the element may be what the layer gives as the declaration of a name made by a macro: an atom name leaf
   * in a top-level form that starts with a macro use or is broken (such as {@code foo} in {@code ?GEN(foo).}), or in a
   * {@code -define} body (such as {@code mr} in {@code -define(R, -record(mr, {f})).}). A structural check only; it
   * asks no copy, so the UI thread may call it.
   */
  public static boolean isNameMadeByMacro(@Nullable PsiElement element) {
    // the layer maps to the topmost element with the token's range: it may be an expression around the leaf
    PsiElement first = element instanceof LeafPsiElement || element == null ? element : PsiTreeUtil.firstChild(element);
    if (first == null || !first.getTextRange().equals(element.getTextRange())) return false;
    if (!(first instanceof LeafPsiElement leaf) || leaf.getElementType() != org.intellij.erlang.ErlangTypes.ERL_ATOM_NAME) return false;
    if (element instanceof PsiNamedElement || element instanceof ErlangQAtom) return false; // master's own handlers
    if (!isEnabled() || realFile(element) == null) return false;
    if (PsiTreeUtil.getParentOfType(element, ErlangMacrosBody.class) != null) return true;
    return isInTopLevelMacroForm(element);
  }

  private static boolean isInTopLevelMacroForm(@NotNull PsiElement element) {
    PsiElement form = topForm(element);
    if (form == null) return false;
    if (form instanceof PsiErrorElement) return true;
    PsiElement first = PsiTreeUtil.firstChild(form);
    return first != element && first != null && "?".equals(first.getText());
  }

  /**
   * The description of a target that only the layer gives (a header target, the name of a function made by a macro),
   * for the Erlang description provider; null for other elements.
   */
  public static @Nullable String describeLayerTarget(@NotNull PsiElement element, @NotNull ElementDescriptionLocation location) {
    if (element instanceof ErlangMacroLayerHeaderTarget target) return target.describe(location);
    if (isNameMadeByMacro(element)) {
      // a name in a macro use at the top level makes a function; in a -define body it may name anything
      String kind = isInTopLevelMacroForm(element) ? "function" : "name";
      return ErlangMacroLayerHeaderTarget.describe(kind, element.getText(), location);
    }
    return null;
  }

  // ------------------------------------------------------------------------------------------------ macro bodies

  /** Includers searched for the uses of a header macro, at most. */
  private static final int MAX_BODY_USERS = 50;

  /**
   * Round 5: resolves a name inside a {@code -define} body through the expanded copies of the macro's uses: in the
   * file itself, and for a header, in the .erl files that use the macro and include the header (at most
   * {@link #MAX_BODY_USERS}). The body alone may not parse (a fragment), and a header body names things of its
   * includers, so master cannot resolve it there. Every use contributes its answer, mapped back to real elements;
   * different uses may resolve differently, and then all answers are returned. Never builds a copy on the UI thread.
   */
  public static @NotNull List<PsiElement> resolveInMacroBody(@NotNull PsiElement leaf) {
    if (!isEnabled() || leaf.getTextLength() == 0) return List.of();
    // the search for the includers queries the word index: never on the UI thread
    if (ApplicationManager.getApplication().isDispatchThread() && !isEdtBuildAllowed()) return List.of();
    PsiFile file = leaf.getContainingFile();
    if (!(file instanceof ErlangFile ef) || file.getUserData(COPY_OF) != null || DumbService.isDumb(file.getProject())) return List.of();
    ErlangMacrosDefinition def = PsiTreeUtil.getParentOfType(leaf, ErlangMacrosDefinition.class);
    if (def == null || def.getName() == null || !PsiTreeUtil.isAncestor(def.getMacrosBody(), leaf, false)) return List.of();
    int start = leaf.getTextRange().getStartOffset();
    VirtualFile vf = file.getOriginalFile().getVirtualFile();
    ErlangMacroExpander.HeaderTok origin = null;
    List<ErlangFile> users = new ArrayList<>();
    if (layerFile(ef) != null) users.add(ef);
    else if (vf != null) {
      origin = headerOrigin(ef, vf, start, leaf.getText());
      if (origin == null) return List.of();
      users.addAll(includersUsing(ef.getProject(), vf, def.getName()));
    }
    LinkedHashSet<PsiElement> result = new LinkedHashSet<>();
    for (ErlangFile user : users) {
      ProgressManager.checkCanceled();
      Copy copy = getCopy(user, "macro-body");
      if (copy == null || copy.copyFile == null) continue;
      for (int e : copy.expansion.bodyCopies(start, origin)) {
        if (!copy.isClean(copy.expansion.formAtExpanded(e))) continue;
        PsiReference ref = copy.copyFile.findReferenceAt(e);
        if (ref == null) continue;
        TextRange abs = ref.getRangeInElement().shiftRight(ref.getElement().getTextRange().getStartOffset());
        if (abs.getStartOffset() != e || !leaf.textMatches(abs.subSequence(copy.expansion.text))) continue;
        List<PsiElement> targets = new ArrayList<>();
        if (ref instanceof PsiPolyVariantReference poly) for (ResolveResult rr : poly.multiResolve(false)) targets.add(rr.getElement());
        if (targets.isEmpty()) targets.add(ref.resolve());
        for (PsiElement t : targets) {
          PsiElement real = exit(copy.toReal(t), user);
          if (real != null) result.add(real);
        }
      }
    }
    return new ArrayList<>(result);
  }

  /** The origin of a header token in the header's summary, or null when it is not a token of a directive. */
  private static @Nullable ErlangMacroExpander.HeaderTok headerOrigin(@NotNull ErlangFile header, @NotNull VirtualFile vf, int start,
                                                                       @NotNull String text) {
    List<ErlangMacroExpander.Directive> directives = summaryOf(header).directives;
    for (int d = 0; d < directives.size(); d++) {
      List<ErlangMacroExpander.Tok> toks = directives.get(d).tokens;
      if (toks.isEmpty() || toks.getFirst().start() > start || toks.getLast().end() <= start) continue;
      for (int i = 0; i < toks.size(); i++) {
        if (toks.get(i).start() == start && toks.get(i).text().equals(text)) return new ErlangMacroExpander.HeaderTok(vf, d, i);
      }
    }
    return null;
  }

  /** Round 5: the .erl files of the project that use the macro of the header (for rename conflicts); never on the UI thread. */
  public static @NotNull List<ErlangFile> includersOfHeader(@Nullable PsiFile header, @NotNull String macro) {
    VirtualFile vf = header == null ? null : header.getOriginalFile().getVirtualFile();
    if (vf == null || !isEnabled() || DumbService.isDumb(header.getProject())) return List.of();
    if (ApplicationManager.getApplication().isDispatchThread() && !isEdtBuildAllowed()) return List.of();
    return includersUsing(header.getProject(), vf, macro);
  }

  /** The .erl files of the project that contain the macro's name and include the header (directly or not). */
  private static @NotNull List<ErlangFile> includersUsing(@NotNull Project project, @NotNull VirtualFile header, @NotNull String macro) {
    List<ErlangFile> r = new ArrayList<>();
    PsiSearchHelper.getInstance(project).processAllFilesWithWord(macro, GlobalSearchScope.projectScope(project), f -> {
      ProgressManager.checkCanceled();
      ErlangFile ef = f instanceof ErlangFile x ? layerFile(x) : null;
      Table t = ef == null ? null : table(ef);
      if (t != null && t.refusal == null && t.headers.contains(header)) r.add(ef);
      return r.size() < MAX_BODY_USERS;
    }, true);
    return r;
  }

  /**
   * Round 5: the reference lies in a {@code -define} body, master resolves it to nothing, and one of the macro's uses
   * resolves it to the element: for find usages and rename of that element.
   */
  public static boolean isReferenceThroughMacroUses(@NotNull PsiReference ref, @NotNull PsiElement element) {
    PsiElement owner = ref.getElement();
    if (!isEnabled() || PsiTreeUtil.getParentOfType(owner, ErlangMacrosBody.class) == null) return false;
    PsiFile file = owner.getContainingFile();
    PsiElement leaf = file == null ? null : file.findElementAt(owner.getTextRange().getStartOffset() + ref.getRangeInElement().getStartOffset());
    if (leaf == null) return false;
    PsiManager manager = owner.getManager();
    for (PsiElement target : resolveInMacroBody(leaf)) {
      if (manager.areElementsEquivalent(target, element)) return true;
    }
    return false;
  }

  // ------------------------------------------------------------------------------------------------ resolve

  private static boolean isRemoteCall(@NotNull PsiElement element) {
    return element.getParent() instanceof ErlangGlobalFunctionCallExpression;
  }

  /** Some top-level form of the main tree starts with '?' or is broken: only such forms can make a function. */
  private static boolean hasTopLevelMacroForm(@NotNull ErlangFile file, @NotNull Table t) {
    Boolean b = t.topLevelMacroForm;
    if (b != null) return b;
    b = false;
    if (!t.names.isEmpty()) {
      for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) {
        if (c instanceof PsiWhiteSpace || c instanceof PsiComment) continue;
        // round 5 (narrow gate): only a form with a use of a visible macro can make a name; another form is copied as it is
        if (!isGateWide() && !formUsesMacro(t, c)) continue;
        if (c instanceof PsiErrorElement) {
          b = true;
          break;
        }
        PsiElement leaf = PsiTreeUtil.firstChild(c);
        if (leaf != null && "?".equals(leaf.getText())) {
          b = true;
          break;
        }
      }
    }
    t.topLevelMacroForm = b;
    return b;
  }

  private static @Nullable PsiReference correspondingReference(@NotNull Copy copy, @NotNull PsiReference ref, int occurrence) {
    return correspondingReference(copy, ref, occurrence, false);
  }

  /** {@code verbatimForm}: the reference may also lie in a form that is not expanded (its copy text is the source text). */
  private static @Nullable PsiReference correspondingReference(@NotNull Copy copy, @NotNull PsiReference ref, int occurrence,
                                                               boolean verbatimForm) {
    PsiElement element = ref.getElement();
    TextRange abs = ref.getRangeInElement().shiftRight(element.getTextRange().getStartOffset());
    int[] offsets = copy.toCopyOffsets(abs);
    if (occurrence >= offsets.length || copy.copyFile == null) return null;
    int e = offsets[occurrence];
    Form form = copy.expansion.formAtExpanded(e);
    boolean usable = copy.isClean(form) || verbatimForm && form != null && form.status != FormStatus.EXPANDED;
    if (!usable) return null;
    String refText = abs.subSequence(copy.expansion.sourceText).toString();
    PsiReference candidate = copy.copyFile.findReferenceAt(e);
    if (candidate == null || candidate.getClass() != ref.getClass()) return null;
    TextRange cabs = candidate.getRangeInElement().shiftRight(candidate.getElement().getTextRange().getStartOffset());
    if (cabs.getStartOffset() != e || !refText.contentEquals(cabs.subSequence(copy.expansion.text))) return null;
    return candidate;
  }

  /** Hook: master's resolve found nothing. Returns a real element or null (master's answer). */
  public static @Nullable PsiElement resolveFallback(@NotNull PsiReference ref) {
    if (!ErlangMacroLayerStats.on()) return resolveFallback(ref, null);
    Outcome o = new Outcome();
    long t0 = ErlangMacroLayerStats.enter();
    String entry = ref instanceof ErlangVariableReferenceImpl ? "variable(resolve)" : "resolve:" + ref.getClass().getSimpleName();
    try {
      return resolveFallback(ref, o);
    }
    finally {
      ErlangMacroLayerStats.exit(entry, o.value, o.file, t0);
    }
  }

  private static @Nullable PsiElement resolveFallback(@NotNull PsiReference ref, @Nullable Outcome o) {
    PsiElement element = ref.getElement();
    ErlangFile file = layerFile(element);
    if (file == null) {
      mark(o, "gate:not-layer-file");
      return null;
    }
    markFile(o, file);
    if (ref instanceof ErlangVariableReferenceImpl v) {
      ResolveResult[] rr = multiResolveFallback(v, o);
      return rr.length > 0 ? rr[0].getElement() : null;
    }
    if (ourAblateResolve) {
      mark(o, "ablated");
      return null;
    }
    if (ref instanceof ErlangFunctionReference fr && isRemoteCall(element)) {
      mark(o, isRemoteEnabled() ? "remote" : "gate:remote-off");
      return isRemoteEnabled() ? remoteFallback(file, element, fr) : null;
    }
    if (!isGateWide() && isMacroNameReference(ref, element)) {
      mark(o, "gate:macro-name");
      return null;
    }
    Table t = table(file);
    if (t == null || t.refusal != null || t.names.isEmpty()) {
      mark(o, t == null ? "gate:no-table" : t.refusal != null ? "gate:refused" : "gate:no-names");
      return null;
    }
    if (!isGateWide() && !anyFormNeedsCopy(file, t)) {
      mark(o, "gate:file-needs-no-copy");
      return null;
    }
    PsiElement form = topForm(element);
    boolean formHasUse = formUsesMacro(t, form);
    if (formHasUse && !formNeedsCopy(t, form) && !hasTopLevelMacroForm(file, t)) {
      // round 5: the form's macros need no substitution, and no top-level form can make a name: the copy has no answer
      mark(o, "gate:plain-form");
      return null;
    }
    if (!formHasUse) {
      if (!hasTopLevelMacroForm(file, t)) {
        mark(o, "gate:form-no-use");
        return null;
      }
      // round 5: a record, a record field or a local type may be made by a macro in another form
      if (isMadeNameReference(ref)) return madeNameFallback(file, ref, element);
      // a local function may be made by a macro in another form: the per-file table
      if (!(ref instanceof ErlangFunctionReference fr)) {
        mark(o, "gate:form-no-use");
        return null;
      }
      FALLBACK_ASKED.incrementAndGet();
      ErlangMacroLayerLog.query("function-table", file.getName());
      Copy copy = getCopy(file, "function-table");
      if (copy == null) {
        mark(o, "function-table:no-copy");
        return null;
      }
      PsiElement real = exit(copy.functionsMadeByMacros().get(fr.getName() + "/" + fr.getArity()), file);
      if (real != null) FALLBACK_ANSWERED.incrementAndGet();
      mark(o, real != null ? "function-table:answered" : "function-table:none");
      return real;
    }
    FALLBACK_ASKED.incrementAndGet();
    ErlangMacroLayerLog.query("resolve", file.getName());
    Copy copy = getCopy(file, "resolve");
    if (copy == null) {
      mark(o, "asked:no-copy");
      return null;
    }
    String answerKey = "r" + element.getTextRange().getStartOffset() + ref.getClass().getSimpleName();
    Object cached = copy.answers.get(answerKey);
    if (cached != null) {
      mark(o, cached == NO_ANSWER ? "asked:cached-none" : "asked:cached-answer");
      return cached == NO_ANSWER ? null : (PsiElement)cached;
    }
    long q0 = System.nanoTime();
    PsiElement answer = resolveInCopy(copy, ref, element, file, t);
    ErlangMacroLayerLog.queryNanos(System.nanoTime() - q0);
    copy.answers.put(answerKey, answer == null ? NO_ANSWER : answer);
    mark(o, answer == null ? "asked:computed-none" : "asked:computed-answer");
    return answer;
  }

  private static final Object NO_ANSWER = new Object();

  /**
   * Round 5, fix 3: the reference's own name is a macro use ({@code ?NAME(...)}, {@code fun ?F/1}, {@code #?R{}}, ...).
   * The copy replaces that name with the macro's body, so no reference of the copy stands at the name: the copy can
   * never answer for it.
   */
  private static boolean isMacroNameReference(@NotNull PsiReference ref, @NotNull PsiElement element) {
    TextRange range = ref.getRangeInElement();
    if (range.isEmpty()) return false;
    PsiElement leaf = element.findElementAt(range.getStartOffset());
    return leaf != null && leaf.getNode().getElementType() == org.intellij.erlang.ErlangTypes.ERL_QMARK;
  }

  /** A record, record field or local type reference: the names a macro can make besides functions. */
  private static boolean isMadeNameReference(@NotNull PsiReference ref) {
    if (!(ref instanceof ErlangQAtomBasedReferenceImpl) || ref instanceof ErlangModuleReferenceImpl) return false;
    if (ref instanceof ErlangTypeReferenceImpl) {
      // remote types (m:t()) are not made in this file
      return PsiTreeUtil.getPrevSiblingOfType(ref.getElement(), ErlangModuleRef.class) == null;
    }
    return true;
  }

  /**
   * Round 5: a record, field or type reference in a form without a macro use, in a file with a top-level macro form.
   * The form is copied verbatim into the copy, so the same reference there sees the definitions the expansions made.
   */
  private static @Nullable PsiElement madeNameFallback(@NotNull ErlangFile file, @NotNull PsiReference ref, @NotNull PsiElement element) {
    FALLBACK_ASKED.incrementAndGet();
    ErlangMacroLayerLog.query("made-name", file.getName());
    Copy copy = getCopy(file, "made-name");
    if (copy == null || copy.copyFile == null) return null;
    String answerKey = "m" + element.getTextRange().getStartOffset() + ref.getClass().getName();
    Object cached = copy.answers.get(answerKey);
    if (cached != null) return cached == NO_ANSWER ? null : (PsiElement)cached;
    PsiElement answer = null;
    if (ref instanceof ErlangRecordReferenceImpl rr) answer = exit(copy.recordsMadeByMacros().get(rr.getCanonicalText()), file);
    else if (ref instanceof ErlangTypeReferenceImpl tr) answer = exit(copy.typesMadeByMacros().get(tr.getCanonicalText()), file);
    else {
      // a field: resolve the verbatim copy of the reference (its record is made by a macro)
      PsiReference c = correspondingReference(copy, ref, 0, true);
      if (c != null) answer = exit(copy.toReal(c.resolve()), file);
    }
    if (answer != null) FALLBACK_ANSWERED.incrementAndGet();
    copy.answers.put(answerKey, answer == null ? NO_ANSWER : answer);
    return answer;
  }

  private static @Nullable PsiElement resolveInCopy(@NotNull Copy copy, @NotNull PsiReference ref, @NotNull PsiElement element,
                                                    @NotNull ErlangFile file, @NotNull Table t) {
    int copies = copy.toCopyOffsets(ref.getRangeInElement().shiftRight(element.getTextRange().getStartOffset())).length;
    for (int i = 0; i < copies; i++) {
      PsiReference c = correspondingReference(copy, ref, i);
      if (c == null) continue;
      PsiElement real = exit(copy.toReal(c.resolve()), file);
      if (real != null) {
        FALLBACK_ANSWERED.incrementAndGet();
        return real;
      }
    }
    if (ref instanceof ErlangFunctionReference fr && hasTopLevelMacroForm(file, t)) {
      PsiElement real = exit(copy.functionsMadeByMacros().get(fr.getName() + "/" + fr.getArity()), file);
      if (real != null) FALLBACK_ANSWERED.incrementAndGet();
      return real;
    }
    return null;
  }

  /** Hook for poly-variant resolve (variables): master's multiResolve found nothing. */
  public static @NotNull ResolveResult @NotNull [] multiResolveFallback(@NotNull PsiPolyVariantReference ref) {
    if (!ErlangMacroLayerStats.on()) return multiResolveFallback(ref, null);
    Outcome o = new Outcome();
    long t0 = ErlangMacroLayerStats.enter();
    try {
      return multiResolveFallback(ref, o);
    }
    finally {
      ErlangMacroLayerStats.exit("variable(multiResolve)", o.value, o.file, t0);
    }
  }

  private static @NotNull ResolveResult @NotNull [] multiResolveFallback(@NotNull PsiPolyVariantReference ref, @Nullable Outcome o) {
    PsiElement element = ref.getElement();
    ErlangFile file = layerFile(element);
    if (file == null) {
      mark(o, "gate:not-layer-file");
      return ResolveResult.EMPTY_ARRAY;
    }
    markFile(o, file);
    if (ourAblateVariable) {
      mark(o, "ablated");
      return ResolveResult.EMPTY_ARRAY;
    }
    Table t = table(file);
    if (t == null || t.refusal != null || t.names.isEmpty()) {
      mark(o, t == null ? "gate:no-table" : t.refusal != null ? "gate:refused" : "gate:no-names");
      return ResolveResult.EMPTY_ARRAY;
    }
    if (!isGateWide() && !anyFormNeedsCopy(file, t)) {
      mark(o, "gate:file-needs-no-copy");
      return ResolveResult.EMPTY_ARRAY;
    }
    if ("_".equals(element.getText())) {
      mark(o, "gate:underscore");
      return ResolveResult.EMPTY_ARRAY;
    }
    PsiElement form = topForm(element);
    if (form == null || !formUsesMacro(t, form)) {
      mark(o, "gate:form-no-use");
      return ResolveResult.EMPTY_ARRAY;
    }
    if (!formNeedsCopy(t, form)) {
      mark(o, "gate:plain-form");
      return ResolveResult.EMPTY_ARRAY;
    }
    // the copy can bind the variable only through a macro body that names it, a macro argument, or a broken form;
    // round 5 (narrow gate): only a free variable of a body (not a parameter), and only the arguments of a use that
    // needs substitution (a plain use puts its arguments in value slots: their bindings are master's)
    boolean wide = isGateWide();
    String name = element.getText();
    boolean body = wide ? t.bodyVars.contains(name) : t.freeBodyVars.contains(name);
    boolean arg = !body && (wide ? argumentVars(t, form) : substitutedArgumentVars(t, form)).contains(name);
    boolean err = !body && !arg && formHasError(t, form);
    if (!body && !arg && !err) {
      mark(o, "gate:var-not-candidate");
      return ResolveResult.EMPTY_ARRAY;
    }
    String why = body ? "body-var" : arg ? "arg-var" : "form-error";
    FALLBACK_ASKED.incrementAndGet();
    ErlangMacroLayerLog.query("variable", file.getName());
    Copy copy = getCopy(file, "variable");
    if (copy == null) {
      mark(o, "asked(" + why + "):no-copy");
      return ResolveResult.EMPTY_ARRAY;
    }
    String answerKey = "v" + element.getTextRange().getStartOffset();
    Object cached = copy.answers.get(answerKey);
    if (cached instanceof ResolveResult[] rr) {
      mark(o, "asked(" + why + "):cached-" + (rr.length > 0 ? "answer" : "none"));
      return rr;
    }
    long q0 = System.nanoTime();
    ResolveResult[] answer = multiResolveInCopy(copy, ref, element, file);
    ErlangMacroLayerLog.queryNanos(System.nanoTime() - q0);
    copy.answers.put(answerKey, answer);
    mark(o, "asked(" + why + "):computed-" + (answer.length > 0 ? "answer" : "none"));
    return answer;
  }

  private static @NotNull ResolveResult @NotNull [] multiResolveInCopy(@NotNull Copy copy, @NotNull PsiPolyVariantReference ref,
                                                                      @NotNull PsiElement element, @NotNull ErlangFile file) {
    int copies = copy.toCopyOffsets(ref.getRangeInElement().shiftRight(element.getTextRange().getStartOffset())).length;
    LinkedHashSet<PsiElement> result = new LinkedHashSet<>();
    for (int i = 0; i < copies; i++) {
      PsiReference c = correspondingReference(copy, ref, i);
      if (!(c instanceof PsiPolyVariantReference pc)) continue;
      for (ResolveResult rr : pc.multiResolve(false)) {
        PsiElement real = exit(copy.toReal(rr.getElement()), file);
        if (real != null) result.add(real);
      }
    }
    if (result.isEmpty()) return ResolveResult.EMPTY_ARRAY;
    FALLBACK_ANSWERED.incrementAndGet();
    return PsiElementResolveResult.createResults(result);
  }

  /** Remote call m:f() where f is made by a macro in m: the per-file table of m's file. Off by default. */
  private static @Nullable PsiElement remoteFallback(@NotNull ErlangFile file, @NotNull PsiElement element, @NotNull ErlangFunctionReference fr) {
    if (!(element.getParent() instanceof ErlangGlobalFunctionCallExpression g)) return null;
    ErlangQAtom moduleAtom = g.getModuleRef().getQAtom();
    String moduleName = ErlangPsiImplUtil.getName(moduleAtom);
    for (ErlangFile target : ErlangModuleIndex.getFilesByName(file.getProject(), moduleName, GlobalSearchScope.allScope(file.getProject()))) {
      ErlangMacroLayerLog.query("remote", file.getName());
      Copy copy = getCopy(target, "remote");
      if (copy == null) continue;
      PsiElement real = copy.functionsMadeByMacros().get(fr.getName() + "/" + fr.getArity());
      if (real != null) {
        REMOTE_ANSWERED.incrementAndGet();
        return exit(real, file);
      }
    }
    return null;
  }

  // ------------------------------------------------------------------------------------------------ syntax errors

  /** Hook of the error filter: the error is in an expanded form whose expansion parses without errors. */
  public static boolean isErrorExplainedByExpansion(@NotNull PsiErrorElement error) {
    if (!ErlangMacroLayerStats.on()) return isErrorExplainedByExpansion(error, null);
    Outcome o = new Outcome();
    long t0 = ErlangMacroLayerStats.enter();
    try {
      return isErrorExplainedByExpansion(error, o);
    }
    finally {
      ErlangMacroLayerStats.exit("error-filter", o.value, o.file, t0);
    }
  }

  private static boolean isErrorExplainedByExpansion(@NotNull PsiErrorElement error, @Nullable Outcome o) {
    ErlangFile file = layerFile(error);
    if (file == null) {
      mark(o, "gate:not-layer-file");
      return false;
    }
    markFile(o, file);
    if (ourAblateErrorFilter) {
      mark(o, "ablated");
      return false;
    }
    Table t = table(file);
    if (t == null || t.refusal != null || !formUsesMacro(t, topForm(error))) {
      mark(o, t == null ? "gate:no-table" : t.refusal != null ? "gate:refused" : "gate:form-no-use");
      return false;
    }
    ErlangMacroLayerLog.query("error-filter", file.getName());
    Copy copy = getCopy(file, "error-filter");
    if (copy == null || copy.copyFile == null) {
      mark(o, copy == null ? "asked:no-copy" : "asked:no-expansion");
      return false;
    }
    long q0 = System.nanoTime();
    try {
      boolean hide = explained(copy, error);
      mark(o, hide ? "asked:hidden" : "asked:shown");
      return hide;
    }
    finally {
      ErlangMacroLayerLog.queryNanos(System.nanoTime() - q0);
    }
  }

  private static boolean explained(@NotNull Copy copy, @NotNull PsiErrorElement error) {
    int offset = error.getTextRange().getStartOffset();
    Form f = copy.expansion.formAtSource(offset);
    if (f == null) {
      for (Form g : copy.expansion.forms) {
        if (g.srcEnd <= offset) f = g;
        else break;
      }
      if (f != null && !copy.expansion.sourceText.subSequence(f.srcEnd, offset).toString().isBlank()) f = null;
    }
    boolean hide = copy.isClean(f);
    if (hide) ERRORS_HIDDEN.incrementAndGet();
    return hide;
  }
}
