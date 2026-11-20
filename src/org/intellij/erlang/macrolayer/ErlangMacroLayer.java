package org.intellij.erlang.macrolayer;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.progress.ProgressManager;
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
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.search.GlobalSearchScope;
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
import org.intellij.erlang.psi.impl.ErlangVariableReferenceImpl;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.util.*;
import java.lang.ref.WeakReference;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

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
      PsiElement real = mapElement(target);
      if (real != null) return real;
      if (target instanceof PsiNameIdentifierOwner owner) {
        PsiElement id = owner.getNameIdentifier();
        if (id != null) return mapElement(id);
      }
      return null;
    }

    private @Nullable PsiElement mapElement(@NotNull PsiElement copyElement) {
      TextRange r = copyElement.getTextRange();
      ErlangMacroExpander.HeaderTok h = expansion.headerOrigin(r.getStartOffset(), r.getEndOffset());
      if (h != null && h.header() instanceof VirtualFile vf) {
        return new ErlangMacroLayerHeaderTarget(realFile.getProject(), vf, h.directive(), h.index(), copyElement.getText(),
                                                copyElement.getClass());
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
   * file creation/deletion/moves and project roots (include resolution can change only then).
   */
  static final class Table {
    final @Nullable String refusal;
    final Set<String> names = new HashSet<>();
    final Set<String> bodyVars = new HashSet<>();
    final Map<String, ErlangMacroExpander.HeaderSummary> top = new HashMap<>();
    final Map<ErlangMacroExpander.HeaderSummary, Map<String, ErlangMacroExpander.HeaderSummary>> nested = new IdentityHashMap<>();
    final List<VirtualFile> headers = new ArrayList<>();
    final Object[] deps;
    final ConcurrentHashMap<Integer, Boolean> formUse = new ConcurrentHashMap<>();
    final ConcurrentHashMap<Integer, Boolean> formError = new ConcurrentHashMap<>();
    final ConcurrentHashMap<Integer, Set<String>> formArgVars = new ConcurrentHashMap<>();
    volatile Boolean topLevelMacroForm;

    Table(@Nullable String refusal, Object[] deps) {
      this.refusal = refusal;
      this.deps = deps;
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

  private static final Key<CachedValue<Table>> TABLE_KEY = Key.create("erlang.macro.layer.table");
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
      return CachedValueProvider.Result.create(s, header);
    }, false);
  }

  /** The table of a file; on the UI thread only a cached one (never computed there). */
  static @Nullable Table table(@NotNull ErlangFile file) {
    Project project = file.getProject();
    CachedValue<Table> cv = file.getUserData(TABLE_KEY);
    if (cv == null) {
      cv = CachedValuesManager.getManager(project).createCachedValue(() -> {
        Table t = computeTable(file);
        return CachedValueProvider.Result.create(t, t.deps);
      }, false);
      cv = ((UserDataHolderEx)file).putUserDataIfAbsent(TABLE_KEY, cv);
    }
    if (ApplicationManager.getApplication().isDispatchThread() && !isEdtBuildAllowed() && !cv.hasUpToDateValue()) return null;
    return cv.getValue();
  }

  private static @NotNull String key(@NotNull String attribute, @Nullable String string) {
    return attribute + "\0" + string;
  }

  private static @Nullable String includeString(@Nullable ErlangIncludeString s) {
    return s == null ? null : StringUtil.unquoteString(s.getText());
  }

  private static @NotNull Table computeTable(@NotNull ErlangFile file) {
    TABLES_BUILT.incrementAndGet();
    Project project = file.getProject();
    List<Object> deps = new ArrayList<>();
    deps.add(file);
    deps.add(VirtualFileManager.VFS_STRUCTURE_MODIFICATIONS);
    deps.add(ProjectRootManager.getInstance(project));
    // own macros
    Set<String> names = new HashSet<>();
    Set<String> bodyVars = new HashSet<>();
    for (ErlangMacrosDefinition d : file.getMacroses()) {
      ProgressManager.checkCanceled();
      String n = d.getName();
      if (n != null) names.add(n);
      ErlangMacrosBody body = d.getMacrosBody();
      if (body != null) for (ErlangQVar v : PsiTreeUtil.findChildrenOfType(body, ErlangQVar.class)) bodyVars.add(v.getText());
    }
    // headers, the way master resolves includes
    Map<String, ErlangMacroExpander.HeaderSummary> top = new HashMap<>();
    Map<ErlangMacroExpander.HeaderSummary, Map<String, ErlangMacroExpander.HeaderSummary>> nested = new IdentityHashMap<>();
    List<VirtualFile> headers = new ArrayList<>();
    String refusal = null;
    try {
      for (Map.Entry<String, ErlangFile> e : directIncludes(file).entrySet()) {
        if (e.getValue() == null) throw new Refusal("unresolved-include");
        top.put(e.getKey(), visitHeader(e.getValue(), new ArrayList<>(), nested, headers, names, bodyVars));
      }
    }
    catch (Refusal r) {
      refusal = r.getMessage();
    }
    for (VirtualFile h : headers) deps.add(ErlangMacroLayerRetention.getInstance(project).tracker(h));
    Table t = new Table(refusal, deps.toArray());
    if (refusal == null) {
      t.names.addAll(names);
      t.bodyVars.addAll(bodyVars);
      t.top.putAll(top);
      t.nested.putAll(nested);
    }
    t.headers.addAll(headers);
    return t;
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
    @NotNull List<VirtualFile> headers, @NotNull Set<String> names, @NotNull Set<String> bodyVars) throws Refusal {
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
    Map<String, ErlangMacroExpander.HeaderSummary> children = new HashMap<>();
    nested.put(s, children);
    if (s.includes().isEmpty()) return s;
    Map<String, ErlangFile> resolved = directIncludes(header);
    path.add(vf);
    try {
      for (ErlangMacroExpander.Directive d : s.includes()) {
        String k = key(d.attribute, d.includeString());
        ErlangFile child = resolved.get(k);
        if (child == null) throw new Refusal("unresolved-include");
        children.put(k, visitHeader(child, path, nested, headers, names, bodyVars));
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

    CopyHolder(@Nullable VirtualFile file) {
      this.file = file;
    }

    @Nullable Copy get() {
      Copy c = strong;
      return c != null ? c : weak.get();
    }
  }

  /** The cached copy of a real file; built lazily from the PSI text. */
  public static @Nullable Copy getCopy(@NotNull PsiFile file) {
    return getCopy(file, "direct-get");
  }

  private static final Key<CachedValue<CopyHolder>> HOLDER_KEY = Key.create("erlang.macro.layer.holder");

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
    CachedValue<CopyHolder> cv = file.getUserData(HOLDER_KEY);
    if (cv == null) {
      cv = CachedValuesManager.getManager(project).createCachedValue(() -> {
        Table t = table(file);
        Object[] deps = t != null ? t.deps : new Object[]{file, PsiModificationTracker.getInstance(project)};
        return CachedValueProvider.Result.create(new CopyHolder(file.getOriginalFile().getVirtualFile()), deps);
      }, false);
      cv = ((UserDataHolderEx)file).putUserDataIfAbsent(HOLDER_KEY, cv);
    }
    boolean edt = ApplicationManager.getApplication().isDispatchThread() && !isEdtBuildAllowed();
    if (edt) {
      CopyHolder h = cv.hasUpToDateValue() ? cv.getValue() : null;
      Copy c = h != null ? h.get() : null;
      if (c == null) ErlangMacroLayerLog.event("EDT_MISS", file.getName(), 0, 0, 0, "copy", trigger);
      else ErlangMacroLayerRetention.getInstance(project).touch(h, c);
      return c;
    }
    CopyHolder holder = cv.getValue();
    Copy copy = holder.get();
    if (copy == null) {
      // highlighting asks from several threads at once; build once per file and let the others wait
      synchronized (holder) {
        copy = holder.get();
        if (copy == null) {
          copy = build(file, table, trigger, holder.everBuilt ? "collected" : null);
          holder.weak = new WeakReference<>(copy);
          holder.everBuilt = true;
        }
      }
    }
    ErlangMacroLayerRetention.getInstance(project).touch(holder, copy);
    return copy;
  }

  /** The cached copy if it is built and up to date; never builds. */
  @TestOnly
  public static @Nullable Copy peekCopy(@NotNull PsiFile file) {
    CachedValue<CopyHolder> cv = file.getUserData(HOLDER_KEY);
    if (cv == null || !cv.hasUpToDateValue()) return null;
    return cv.getValue().get();
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

  private static @NotNull Copy build(@NotNull PsiFile file, @Nullable Table table, @NotNull String trigger, @Nullable String why) {
    long t0 = System.nanoTime();
    Project project = file.getProject();
    long[] now = {file.getModificationStamp(), table != null ? table.headerVersions(project) : 0,
      VirtualFileManager.VFS_STRUCTURE_MODIFICATIONS.getModificationCount(),
      ProjectRootManager.getInstance(project).getModificationCount()};
    long[] last = file.getUserData(LAST_BUILD_STATE);
    String reason = why != null ? why : last == null ? "first" : last[0] != now[0] ? "own-text" :
                                        last[1] != now[1] ? "header-directives" :
                                        last[2] != now[2] ? "vfs-structure" : last[3] != now[3] ? "roots" : "same-state";
    if (!"direct".equals(trigger)) file.putUserData(LAST_BUILD_STATE, now);
    EXPANSIONS_BUILT.incrementAndGet();
    CharSequence text = file.getViewProvider().getContents();
    if (file.getTextLength() != text.length() || !file.textMatches(text)) text = file.getText();
    Expansion expansion = ErlangMacroExpander.expand(text, new ErlangLexer(project), FORM_CAP,
                                                     table != null ? table.resolver() : null);
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
    Copy copy = new Copy(file, expansion, copyFile, System.nanoTime() - t0);
    ErlangMacroLayerLog.event("BUILD", file.getName(), copy.buildNanos, expansion.sourceTokens, expansion.expandedTokens,
                              reason, trigger);
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
      for (ErlangMacrosCall c : PsiTreeUtil.findChildrenOfType(form, ErlangMacrosCall.class)) {
        for (ErlangQVar v : PsiTreeUtil.findChildrenOfType(c, ErlangQVar.class)) r.add(v.getText());
      }
      return r;
    });
  }

  private static boolean formHasError(@NotNull Table t, @NotNull PsiElement form) {
    return t.formError.computeIfAbsent(form.getTextRange().getStartOffset(), k -> PsiTreeUtil.hasErrorElements(form));
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
    PsiElement element = ref.getElement();
    TextRange abs = ref.getRangeInElement().shiftRight(element.getTextRange().getStartOffset());
    int[] offsets = copy.toCopyOffsets(abs);
    if (occurrence >= offsets.length || copy.copyFile == null) return null;
    int e = offsets[occurrence];
    if (!copy.isClean(copy.expansion.formAtExpanded(e))) return null;
    String refText = abs.subSequence(copy.expansion.sourceText).toString();
    PsiReference candidate = copy.copyFile.findReferenceAt(e);
    if (candidate == null || candidate.getClass() != ref.getClass()) return null;
    TextRange cabs = candidate.getRangeInElement().shiftRight(candidate.getElement().getTextRange().getStartOffset());
    if (cabs.getStartOffset() != e || !refText.contentEquals(cabs.subSequence(copy.expansion.text))) return null;
    return candidate;
  }

  /** Hook: master's resolve found nothing. Returns a real element or null (master's answer). */
  public static @Nullable PsiElement resolveFallback(@NotNull PsiReference ref) {
    PsiElement element = ref.getElement();
    ErlangFile file = layerFile(element);
    if (file == null) return null;
    if (ref instanceof ErlangVariableReferenceImpl v) {
      ResolveResult[] rr = multiResolveFallback(v);
      return rr.length > 0 ? rr[0].getElement() : null;
    }
    if (ref instanceof ErlangFunctionReference fr && isRemoteCall(element)) {
      return isRemoteEnabled() ? remoteFallback(file, element, fr) : null;
    }
    Table t = table(file);
    if (t == null || t.refusal != null || t.names.isEmpty()) return null;
    boolean formHasUse = formUsesMacro(t, topForm(element));
    if (!formHasUse) {
      // a local function may be made by a macro in another form: the per-file table
      if (!(ref instanceof ErlangFunctionReference fr) || !hasTopLevelMacroForm(file, t)) return null;
      FALLBACK_ASKED.incrementAndGet();
      ErlangMacroLayerLog.query("function-table", file.getName());
      Copy copy = getCopy(file, "function-table");
      if (copy == null) return null;
      PsiElement real = exit(copy.functionsMadeByMacros().get(fr.getName() + "/" + fr.getArity()), file);
      if (real != null) FALLBACK_ANSWERED.incrementAndGet();
      return real;
    }
    FALLBACK_ASKED.incrementAndGet();
    ErlangMacroLayerLog.query("resolve", file.getName());
    Copy copy = getCopy(file, "resolve");
    if (copy == null) return null;
    String answerKey = "r" + element.getTextRange().getStartOffset() + ref.getClass().getSimpleName();
    Object cached = copy.answers.get(answerKey);
    if (cached != null) return cached == NO_ANSWER ? null : (PsiElement)cached;
    long q0 = System.nanoTime();
    PsiElement answer = resolveInCopy(copy, ref, element, file, t);
    ErlangMacroLayerLog.queryNanos(System.nanoTime() - q0);
    copy.answers.put(answerKey, answer == null ? NO_ANSWER : answer);
    return answer;
  }

  private static final Object NO_ANSWER = new Object();

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
    PsiElement element = ref.getElement();
    ErlangFile file = layerFile(element);
    if (file == null) return ResolveResult.EMPTY_ARRAY;
    Table t = table(file);
    if (t == null || t.refusal != null || t.names.isEmpty()) return ResolveResult.EMPTY_ARRAY;
    PsiElement form = topForm(element);
    if (form == null || !formUsesMacro(t, form)) return ResolveResult.EMPTY_ARRAY;
    // the copy can bind the variable only through a macro body that names it, a macro argument, or a broken form
    if (!t.bodyVars.contains(element.getText()) && !argumentVars(t, form).contains(element.getText()) &&
        !formHasError(t, form)) {
      return ResolveResult.EMPTY_ARRAY;
    }
    FALLBACK_ASKED.incrementAndGet();
    ErlangMacroLayerLog.query("variable", file.getName());
    Copy copy = getCopy(file, "variable");
    if (copy == null) return ResolveResult.EMPTY_ARRAY;
    String answerKey = "v" + element.getTextRange().getStartOffset();
    Object cached = copy.answers.get(answerKey);
    if (cached instanceof ResolveResult[] rr) return rr;
    long q0 = System.nanoTime();
    ResolveResult[] answer = multiResolveInCopy(copy, ref, element, file);
    ErlangMacroLayerLog.queryNanos(System.nanoTime() - q0);
    copy.answers.put(answerKey, answer);
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
    ErlangFile file = layerFile(error);
    if (file == null) return false;
    Table t = table(file);
    if (t == null || t.refusal != null || !formUsesMacro(t, topForm(error))) return false;
    ErlangMacroLayerLog.query("error-filter", file.getName());
    Copy copy = getCopy(file, "error-filter");
    if (copy == null || copy.copyFile == null) return false;
    long q0 = System.nanoTime();
    try {
      return explained(copy, error);
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
