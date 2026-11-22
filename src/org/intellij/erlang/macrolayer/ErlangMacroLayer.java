package org.intellij.erlang.macrolayer;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.codeInsight.daemon.impl.DaemonProgressIndicator;
import com.intellij.openapi.application.ApplicationListener;
import com.intellij.openapi.progress.EmptyProgressIndicator;
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
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.util.CachedValue;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.ModificationTracker;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.registry.Registry;
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
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.parser.macro.ErlangMacroExpander;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Expansion;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Form;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.FormStatus;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangMacroResolve;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.psi.impl.ErlangModuleReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangQAtomBasedReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangRecordReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangTypeReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangVariableReferenceImpl;
import org.intellij.erlang.macrolayer.ErlangMacroLayerTables.IncludeTracker;
import org.intellij.erlang.macrolayer.ErlangMacroLayerTables.TableBuild;
import org.intellij.erlang.macrolayer.ErlangMacroLayerTables.TableCache;
import org.intellij.erlang.macrolayer.ErlangMacroLayerTables.TableEntry;
import org.intellij.erlang.macrolayer.ErlangMacroLayerTables.TextTracker;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.lang.management.GarbageCollectorMXBean;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.util.*;
import java.lang.ref.WeakReference;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.LockSupport;
import java.util.function.BooleanSupplier;
import java.util.function.Predicate;
import java.util.function.Supplier;

/**
 * Macro expansion as a separate layer, used only as an oracle.
 * <p>
 * The main tree, the stubs and the indices of a file are not touched. On demand, the layer expands the same-file
 * macros of the file's PSI text ({@link ErlangMacroExpander}), parses the expanded text as one non-physical Erlang
 * file whose original file is the real file (the "copy"), and caches it on the real file. A caller asks the copy only
 * when master's path has no answer, and every answer is mapped back to an element of a real file.
 * <p>
 * Switch: the Registry key {@code erlang.macro.layer} (default on), read at each call, so a user can turn the layer
 * off in the IDE (Help | Find Action | Registry) without a restart. The system property {@code erlang.macro.layer}, else
 * the environment variable {@code ERLANG_MACRO_LAYER} ({@code off} or {@code layer}), overrides it. Form cap: {@code erlang.macro.layer.cap} / {@code ERLANG_MACRO_LAYER_CAP},
 * default 4,000 significant tokens per expanded form. Remote fallback ({@code m:f()} to a function made by a macro
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

  /** The Registry key of the switch (registered in ErlangPlugin.xml); see {@link ErlangMacroLayerSettings}. */
  public static final String REGISTRY_KEY = ErlangMacroLayerSettings.REGISTRY_KEY;
  public static final int FORM_CAP = ErlangMacroLayerSettings.FORM_CAP;

  public static @NotNull Mode getMode() {
    return ErlangMacroLayerSettings.getMode();
  }

  public static boolean isEnabled() {
    return ErlangMacroLayerSettings.isEnabled();
  }

  public static boolean isRemoteEnabled() {
    return ErlangMacroLayerSettings.isRemoteEnabled();
  }

  public static int formCap() {
    return ErlangMacroLayerSettings.formCap();
  }

  static boolean isGateWide() {
    return ErlangMacroLayerSettings.isGateWide();
  }

  static boolean isEdtBuildAllowed() {
    return ErlangMacroLayerSettings.isEdtBuildAllowed();
  }

  static @Nullable String read(String property, String env) {
    return ErlangMacroLayerSettings.read(property, env);
  }

  @TestOnly
  public static void setModeForTests(@NotNull Mode mode, @NotNull Disposable parent) {
    ErlangMacroLayerSettings.setModeForTests(mode, parent);
  }

  @TestOnly
  public static void setGateWideForTests(boolean wide, @NotNull Disposable parent) {
    ErlangMacroLayerSettings.setGateWideForTests(wide, parent);
  }

  @TestOnly
  public static void setEdtBuildForTests(boolean allow, @NotNull Disposable parent) {
    ErlangMacroLayerSettings.setEdtBuildForTests(allow, parent);
  }

  private ErlangMacroLayer() {
  }

  /** The outcome of one entry-point call, for {@link ErlangMacroLayerStats}; null when the log is off. */
  static final class Outcome {
    String value = "exception";
    String file;
  }

  static void mark(@Nullable Outcome o, @NotNull String value) {
    if (o != null) o.value = value;
  }

  static void markFile(@Nullable Outcome o, @Nullable PsiFile file) {
    if (o != null && file != null) o.file = file.getName();
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
    /**
     * Answers of the oracle for this copy, by kind and real offset. Read and write them only through {@link #answers()}:
     * an answer can point into another file, which can change while this copy stays.
     */
    private final ConcurrentHashMap<String, Object> myAnswers = new ConcurrentHashMap<>();
    private volatile long myAnswersStamp = -1;

    /**
     * The kept answers, valid until the next PSI change in any file: the map is cleared when the PSI modification count
     * moved since it was filled, like the platform's resolve cache. The copy itself stays, so an unrelated edit rebuilds
     * no copy; only its answers are computed again (a deleted target gives none, a new target is found).
     */
    @NotNull Map<String, Object> answers() {
      long now = PsiModificationTracker.getInstance(realFile.getProject()).getModificationCount();
      if (now != myAnswersStamp) {
        synchronized (myAnswers) {
          if (now != myAnswersStamp) {
            myAnswers.clear();
            myAnswersStamp = now;
          }
        }
      }
      return myAnswers;
    }

    /** The position of each form in {@code expansion.forms} (a list search per question was quadratic in large files). */
    private final Map<Form, Integer> myFormIndex = new IdentityHashMap<>();

    private int indexOf(@NotNull Form f) {
      Integer i = myFormIndex.get(f);
      return i == null ? -1 : i;
    }

    Copy(@NotNull PsiFile realFile, @NotNull Expansion expansion, @Nullable ErlangFile copyFile, long buildNanos) {
      this.realFile = realFile;
      this.expansion = expansion;
      this.copyFile = copyFile;
      this.buildNanos = buildNanos;
      for (int i = 0; i < expansion.forms.size(); i++) myFormIndex.put(expansion.forms.get(i), i);
      if (copyFile != null) {
        for (PsiErrorElement e : PsiTreeUtil.collectElementsOfType(copyFile, PsiErrorElement.class)) {
          Form f = expansion.formAtExpanded(e.getTextRange().getStartOffset());
          if (f == null) f = previousForm(e.getTextRange().getStartOffset());
          if (f != null) myBroken.set(indexOf(f));
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
      return f != null && f.status == FormStatus.EXPANDED && copyFile != null && !myBroken.get(indexOf(f));
    }

    public boolean isBroken(@NotNull Form f) {
      return myBroken.get(indexOf(f));
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

    /** Records whose definition exists only in an expansion, by name, with real targets (see functions). */
    @NotNull Map<String, PsiElement> recordsMadeByMacros() {
      Map<String, PsiElement> map = myRecordsMadeByMacros;
      if (map == null) {
        map = madeByMacros(copyFile == null ? List.of() : copyFile.getRecords(),
                           name -> realFile instanceof ErlangFile rf && rf.getRecord(name) != null);
        myRecordsMadeByMacros = map;
      }
      return map;
    }

    /** Local types whose definition exists only in an expansion, by name, with real targets. */
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
                                                          @NotNull Predicate<String> masterSees) {
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

    private volatile Map<TextRange, String> myMadeNameKinds;

    /**
     * The names that only the expansions make (functions, records, record fields, local types), by the range
     * of their token in the real file, with their kind. The layer gives these tokens as the targets of the names.
     */
    @NotNull Map<TextRange, String> madeNameKinds() {
      Map<TextRange, String> m = myMadeNameKinds;
      if (m != null) return m;
      m = new HashMap<>();
      for (PsiElement e : functionsMadeByMacros().values()) putMadeName(m, e, "function");
      Map<String, PsiElement> records = recordsMadeByMacros();
      for (PsiElement e : records.values()) putMadeName(m, e, "record");
      for (PsiElement e : typesMadeByMacros().values()) putMadeName(m, e, "type");
      if (copyFile != null) {
        for (ErlangRecordDefinition d : copyFile.getRecords()) {
          ProgressManager.checkCanceled();
          ErlangTypedRecordFields fields = records.containsKey(String.valueOf(d.getName())) ? d.getTypedRecordFields() : null;
          if (fields == null) continue;
          for (ErlangTypedExpr t : fields.getTypedExprList()) putMadeName(m, toReal(t.getQAtom()), "record field");
          for (ErlangQAtom q : PsiTreeUtil.getChildrenOfTypeAsList(fields, ErlangQAtom.class)) putMadeName(m, toReal(q), "record field");
        }
      }
      myMadeNameKinds = m;
      return m;
    }

    private void putMadeName(@NotNull Map<TextRange, String> m, @Nullable PsiElement real, @NotNull String kind) {
      if (real != null && real.getContainingFile() == realFile) m.putIfAbsent(real.getTextRange(), kind);
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
      if (target instanceof PsiNameIdentifierOwner owner && (real == null || isOtherElementOfSource(real, target))) {
        // the source text of the target may be some other element of the real file: a record field with a default
        // value, `sf = 0` in a -define body, is an assignment there; the name is the target then
        PsiElement id = owner.getNameIdentifier();
        PsiElement byName = id != null && id != owner ? mapElement(id, owner) : null;
        if (byName != null || real == null) return byName;
      }
      return real;
    }

    /** The element found in the real file for the copy's target is of another kind: it only has the same text range. */
    private static boolean isOtherElementOfSource(@NotNull PsiElement real, @NotNull PsiElement target) {
      return !(real instanceof ErlangMacroLayerHeaderTarget) && !target.getClass().isInstance(real);
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

  /**
   * The real Erlang file of an element, or null for the layer's copies, consoles, non-Erlang files and the platform's
   * copies of a file (the one completion works on: the platform reuses it with new text each time, and the layer's caches
   * on it would answer for an old text; the layer's completion contributor works on the original file).
   */
  static @Nullable ErlangFile realFile(@Nullable PsiElement element) {
    if (element == null) return null;
    PsiFile file = element.getContainingFile();
    if (!(file instanceof ErlangFile ef)) return null;
    if (file.getUserData(COPY_OF) != null) return null;
    if (file != file.getOriginalFile() || !file.getViewProvider().isEventSystemEnabled()) return null;
    if (file.getOriginalFile().getUserData(ErlangPsiImplUtil.ERLANG_CONSOLE) != null) return null;
    return ef;
  }

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
    /** The visible macros that are not plain (see {@link #classify}); the others need no substitution. */
    final Set<String> nonPlain = new HashSet<>();
    /** The variables of macro bodies that are not parameters of their macro. */
    final Set<String> freeBodyVars = new HashSet<>();
    /** Per visible macro, the free variables of all its definitions, and the macros those bodies use. */
    final Map<String, Set<String>> macroFreeVars = new HashMap<>();
    final Map<String, Set<String>> macroNestedUses = new HashMap<>();
    /** Per form, the free variables of the macros it uses ({@link #formFreeBodyVars}). */
    final ConcurrentHashMap<Integer, Set<String>> formFreeVars = new ConcurrentHashMap<>();
    final ConcurrentHashMap<Integer, Boolean> formUse = new ConcurrentHashMap<>();
    final ConcurrentHashMap<Integer, Boolean> formNeedsCopy = new ConcurrentHashMap<>();
    final ConcurrentHashMap<Integer, Boolean> formError = new ConcurrentHashMap<>();
    final ConcurrentHashMap<Integer, Set<String>> formArgVars = new ConcurrentHashMap<>();
    final ConcurrentHashMap<Integer, Set<String>> formSubstitutedArgVars = new ConcurrentHashMap<>();
    volatile Boolean topLevelMacroForm;
    /**
     * An include of the file or of one of its headers does not resolve. The layer still works; a form that uses a
     * macro the file cannot see ({@link #usesUnseen}) gets master's behaviour, since that macro may come from there.
     */
    volatile boolean missingInclude;
    /** The visible macros whose bodies use, directly or through other macros, a macro the file cannot see. */
    final Set<String> usesUnseen = new HashSet<>();
    /** Some top-level form of the file needs the copy ({@link #anyFormNeedsCopy}). */
    volatile Boolean anyFormNeedsCopy;
    /** The forms that need the copy need it only because they are broken ({@link #onlyBrokenFormsNeedCopy}). */
    volatile Boolean onlyBrokenFormsNeedCopy;

    /** The copy of this version of the file (a new table, a new holder: the same dependencies). */
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

  public static final AtomicInteger SUMMARIES_BUILT = ErlangMacroLayerTables.SUMMARIES_BUILT;
  public static final AtomicInteger TABLES_BUILT = ErlangMacroLayerTables.TABLES_BUILT;
  public static final AtomicInteger HEADER_TARGETS_MATERIALIZED = ErlangMacroLayerTables.HEADER_TARGETS_MATERIALIZED;
  /** See {@link ErlangMacroLayerTables#MAX_COPY_CHARS}. */
  public static final int MAX_COPY_CHARS = ErlangMacroLayerTables.MAX_COPY_CHARS;

  /** A real .erl file the layer may work on: enabled, not a copy, not a console, not a header, not during indexing. */
  static @Nullable ErlangFile layerFile(@Nullable PsiElement element) {
    return ErlangMacroLayerTables.layerFile(element);
  }

  public static boolean noReadAccess() {
    return ErlangMacroLayerTables.noReadAccess();
  }

  static @Nullable Table table(@NotNull ErlangFile file) {
    return ErlangMacroLayerTables.table(file);
  }

  static @Nullable Table table(@NotNull ErlangFile file, boolean explicit) {
    return ErlangMacroLayerTables.table(file, explicit);
  }

  public static @NotNull ErlangMacroExpander.HeaderSummary summaryOf(@NotNull PsiFile header) {
    return ErlangMacroLayerTables.summaryOf(header);
  }

  static boolean hasUpToDateSummary(@NotNull PsiFile header) {
    return ErlangMacroLayerTables.hasUpToDateSummary(header);
  }

  public static boolean isInWellFormedDefineBody(@NotNull PsiErrorElement error) {
    return ErlangMacroLayerTables.isInWellFormedDefineBody(error);
  }

  @TestOnly
  public static void setMaxCopyCharsForTests(int chars, @NotNull Disposable parent) {
    ErlangMacroLayerTables.setMaxCopyCharsForTests(chars, parent);
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
   * The copy of a holder, built once. Highlighting asks from several threads at once: one thread builds, the
   * others wait for its result. A waiter holds a read action, which a write action must be able to cancel, so the wait
   * checks for cancellation (a monitor wait cannot). A build that ends without a copy (cancelled or failed) completes
   * with null; then a waiter that is still running builds itself. Null also when the building thread asks again
   * inside its own build (the layer then gives master's answer).
   */
  private static @Nullable Copy obtain(@NotNull CopyHolder holder, @NotNull ErlangFile file, @NotNull Table table,
                                       @NotNull String trigger, boolean explicit) {
    while (true) {
      Copy copy = holder.get();
      if (copy != null) return copy;
      String uncancellable = ErlangMacroLayerCallers.nonCancellableCaller();
      if (ErlangMacroLayerCallers.NO_INDICATOR.equals(uncancellable)) {
        // Under an indicator of its own that a write action cancels
        return underOwnIndicator(() -> obtain(holder, file, table, trigger, explicit), file, trigger);
      }
      if (uncancellable != null) {
        // Such a caller neither builds nor waits for another thread's build
        ErlangMacroLayerStats.phase("build-skipped:" + uncancellable, file.getName(), 0);
        ErlangMacroLayerLog.event("SKIPPED", file.getName(), 0, 0, 0, uncancellable, trigger);
        return null;
      }
      Build running = holder.building.get();
      if (running == null && !explicit) {
        String refusal = buildRefusal(file);
        if (TYPING.equals(refusal) && !onlyBrokenFormsNeedCopy(file, table) &&
            awaitWhileTyped(file, () -> holder.get() != null || holder.building.get() != null, trigger)) {
          continue; // the copy is there, a build runs, or the quiet time is over
        }
        if (refusal == null && !isEdtBuildAllowed() && onlyBrokenFormsNeedCopy(file, table)) {
          // A form broken while the user types, with plain macros only: no caller builds for it, not
          // even highlighting. The copy of such a form is broken too, so the copy answers nothing in the whole file:
          // a question needs a clean form (resolve), and the error filter hides an error only in a clean form.
          refusal = "broken-plain";
        }
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
  static <T> @Nullable T awaitCancellably(@NotNull CompletableFuture<T> build) {
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
   * While a file changes (for {@link #quietMillis()} after an edit), a passive query at
   * the caret ({@link #isPassiveCaller}: the identifier highlighting, ctrl-hover, parameter info, highlight usages)
   * starts no copy build: the next key press would cancel it. It uses a copy that is there or being built, else it
   * gets master's answer. Every other caller builds and waits, cancellably: highlighting, and every search or
   * navigation, also when it runs on worker threads that do not show the action that started it. A caller that is
   * wrongly taken for a passive one would get master's answer, so only callers that are positively recognised are
   * refused; a caller that is wrongly taken for an active one only costs a build.
   */
  static @Nullable String buildRefusal(@NotNull PsiFile file) {
    if (isEdtBuildAllowed()) return null; // diagnostic: builds allowed anywhere
    if (ErlangMacroLayerCallers.isHighlighting(ProgressManager.getGlobalProgressIndicator())) return null;
    if (typedWithinQuietTime(file)) return ErlangMacroLayerCallers.isPassiveCaller(file) ? TYPING : null;
    return null;
  }

  static final String TYPING = "typing";

  /** The file's PSI or its document changed less than {@link #quietMillis()} ago. */
  private static boolean typedWithinQuietTime(@NotNull PsiFile file) {
    TextTracker tt = ErlangMacroLayerTables.textTrackerIfAny(file);
    long quiet = quietMillis();
    return quiet > 0 && (tt != null && tt.changedWithin(quiet) || isBeingTyped(file, quiet));
  }

  /** The last table of the file had a form that needs the copy (a passive query may then wait for the new one). */
  static boolean lastTableNeedsCopy(@NotNull TableCache cache) {
    TableEntry e = cache.entry;
    return e != null && e.table().refusal == null && Boolean.TRUE.equals(e.table().anyFormNeedsCopy) &&
           !Boolean.TRUE.equals(e.table().onlyBrokenFormsNeedCopy);
  }

  /**
   * The longest wait of a passive query for the copy, ms ({@code erlang.macro.layer.passive.wait},
   * default 2,000). The wait ends earlier when the quiet time is over (by default 1 s after the last change).
   */
  private static final int PASSIVE_WAIT_MS = ErlangMacroLayerSettings.readInt("erlang.macro.layer.passive.wait", "ERLANG_MACRO_LAYER_PASSIVE_WAIT", 2000);
  private static volatile @Nullable Integer ourTestPassiveWait;

  static int passiveWaitMillis() {
    Integer w = ourTestPassiveWait;
    return w != null ? w : PASSIVE_WAIT_MS;
  }

  /** Sets the longest wait of a passive query for the copy (0: a passive query is refused at once). */
  @TestOnly
  public static void setPassiveWaitMillisForTests(int millis, @NotNull Disposable parent) {
    Integer previous = ourTestPassiveWait;
    ourTestPassiveWait = millis;
    Disposer.register(parent, () -> ourTestPassiveWait = previous);
  }
  /** Waits of passive queries, and those that ended with the copy or the table there, for tests and measurements. */
  static final AtomicInteger PASSIVE_WAITS = new AtomicInteger();
  static final AtomicInteger PASSIVE_WAITS_READY = new AtomicInteger();

  /**
   * A passive query that the layer refuses while the file is typed in waits for the copy (or
   * the table) of this version of the file instead of answering at once. The platform keeps the answer of the
   * identifier highlighting at the caret on range markers of its targets, and drops it only on a change of the
   * document or of the PSI of the file, or when the editor closes ({@code IdentifierHighlightingManagerImpl} of 262 and
   * 263); a copy build changes none of these, and the platform runs the identifier highlighting again only on caret,
   * selection, document and similar events. So an answer given before the copy is there would stay until the next
   * edit. The wait builds nothing and ends, returning true (ask again), as soon as {@code ready} holds (the daemon's
   * highlighting, which is never refused, builds the copy soon after typing stops) or the quiet time is over (then the
   * query builds itself, as every caller does after the quiet time). It is cancellable: the next key press is a write
   * action, which cancels the read action of the query (in the IDE the platform then runs the query again), so a wait
   * starts no build and nothing per key press. When a write action waits to start and the wait is not cancelled (a
   * blocking read action), it gives up within 50 ms and returns false (master's answer), so a key press never waits
   * for it. It returns false also after {@link #PASSIVE_WAIT_MS}. After a give-up, the later questions of the same
   * query (the same root progress indicator) do not wait again. No restart of anything: it cannot loop.
   */
  static boolean awaitWhileTyped(@NotNull PsiFile file, @NotNull BooleanSupplier ready, @NotNull String trigger) {
    ProgressIndicator root = ErlangMacroLayerCallers.rootIndicator(ProgressManager.getGlobalProgressIndicator());
    if (root == null || gaveUp(root)) return false;
    ErlangMacroLayerRetention retention = ErlangMacroLayerRetention.getInstance(file.getProject());
    long w0 = System.nanoTime();
    long cap = Math.max(0, passiveWaitMillis()) * 1_000_000L;
    String outcome = "cancelled";
    PASSIVE_WAITS.incrementAndGet();
    try {
      while (true) {
        ProgressManager.checkCanceled();
        if (retention.isWriteActionPending()) {
          // a non-blocking read action is being cancelled now: let it see that, so that no answer of it is kept
          long g0 = System.nanoTime();
          while (System.nanoTime() - g0 < 50_000_000L) {
            ProgressManager.checkCanceled();
            LockSupport.parkNanos(1_000_000L);
          }
          outcome = "write-action";
          giveUp(root);
          return false;
        }
        if (ready.getAsBoolean() || !typedWithinQuietTime(file)) {
          outcome = "ready";
          PASSIVE_WAITS_READY.incrementAndGet();
          return true;
        }
        if (System.nanoTime() - w0 >= cap) {
          outcome = "cap";
          giveUp(root);
          return false;
        }
        LockSupport.parkNanos(10_000_000L);
        if (Thread.interrupted()) {
          Thread.currentThread().interrupt();
          throw new ProcessCanceledException();
        }
      }
    }
    finally {
      long dt = System.nanoTime() - w0;
      ErlangMacroLayerStats.phase("passive-wait:" + outcome, file.getName(), dt);
      ErlangMacroLayerLog.event("WAITED", file.getName(), dt, 0, 0, outcome, trigger);
    }
  }

  /** Root progress indicators of passive queries whose wait gave up (weak keys, identity). */
  private static final Map<ProgressIndicator, Boolean> GAVE_UP = Collections.synchronizedMap(new WeakHashMap<>());

  private static boolean gaveUp(@NotNull ProgressIndicator root) {
    return GAVE_UP.containsKey(root);
  }

  private static void giveUp(@NotNull ProgressIndicator root) {
    GAVE_UP.put(root, Boolean.TRUE);
  }

  /** Stack walks of {@link ErlangMacroLayerCallers#isPassiveCaller}, for tests and measurements. */
  static final AtomicInteger STACK_WALKS = ErlangMacroLayerCallers.STACK_WALKS;
  /** Stack walks made by {@link #prepareSearch} (one per search in a file just typed in). */
  static final AtomicInteger SEARCH_STACK_WALKS = ErlangMacroLayerCallers.SEARCH_STACK_WALKS;

  static <T> T asPassiveQuery(@NotNull Supplier<T> computation) {
    return ErlangMacroLayerCallers.asPassiveQuery(computation);
  }

  @TestOnly
  public static <T> T computeAsPassiveQueryForTests(@NotNull Supplier<T> computation) {
    return ErlangMacroLayerCallers.computeAsPassiveQueryForTests(computation);
  }

  /**
   * Called by the layer's reference searcher ({@link ErlangMacroLayerReferencesSearcher}) and by the search of
   * names made by macros, on the thread that starts a search, before any answer is read. For every Erlang file of the
   * search scope that changed within {@link #quietMillis()} (the only files for which a build can be refused) and that
   * contains the name searched for, it builds the copy now, cancellably, unless the search itself is a passive query.
   * The worker threads of the search then find the copy there, whatever their stacks show; so a wrong "passive" on a
   * worker can no longer make a rename or a find usages miss the layer's answers in a file just typed in.
   */
  public static void prepareSearch(@NotNull PsiElement target, @NotNull SearchScope scope) {
    if (!isEnabled() || ApplicationManager.getApplication().isDispatchThread() || ErlangMacroLayerCallers.nonCancellableCaller() != null) return;
    if (ErlangMacroLayerCallers.isHighlighting(ProgressManager.getGlobalProgressIndicator())) return; // highlighting is never refused a build
    if (!(target instanceof ErlangMacroLayerHeaderTarget) && !target.getLanguage().isKindOf(ErlangLanguage.INSTANCE)) return;
    Project project = target.getProject();
    if (DumbService.isDumb(project)) return;
    List<VirtualFile> changed = ErlangMacroLayerRetention.getInstance(project).filesChangedWithin(quietMillis());
    if (changed.isEmpty()) return;
    String name = target instanceof PsiNamedElement named ? named.getName() : target.getText();
    List<ErlangFile> files = new ArrayList<>();
    for (VirtualFile vf : changed) {
      ProgressManager.checkCanceled();
      if (!vf.isValid() || !scope.contains(vf)) continue;
      ErlangFile f = PsiManager.getInstance(project).findFile(vf) instanceof ErlangFile ef ? ErlangMacroLayerTables.layerFile(ef) : null;
      if (f == null || name != null && !StringUtil.contains(f.getViewProvider().getContents(), name)) continue;
      files.add(f);
    }
    if (files.isEmpty()) return;
    SEARCH_STACK_WALKS.incrementAndGet();
    if (ErlangMacroLayerCallers.hasPassiveFrame()) return;
    for (ErlangFile f : files) {
      ProgressManager.checkCanceled();
      getCopy(f, "search", true);
    }
  }

  /**
   * The text of the file's document changed less than {@code quiet} ms ago. The PSI tracker alone misses typing in a
   * real editor: it moves only when the document is committed, and while the user types into a large file the commits
   * may fall behind the keys for more than the quiet time; the caret-driven callers then read the last committed PSI,
   * and each next key cancels the copy build they start.
   */
  private static boolean isBeingTyped(@NotNull PsiFile file, long quiet) {
    Document document = PsiDocumentManager.getInstance(file.getProject()).getCachedDocument(file);
    if (document == null) return false;
    Long changed = document.getUserData(DOCUMENT_CHANGED_AT);
    return changed != null && System.nanoTime() - changed < quiet * 1_000_000L;
  }

  /** System.nanoTime() of the last change of a document (set by the listener of the retention service). */
  private static final Key<Long> DOCUMENT_CHANGED_AT = Key.create("erlang.macro.layer.document.changed.at");

  static void documentChanged(@NotNull Document document) {
    document.putUserData(DOCUMENT_CHANGED_AT, System.nanoTime());
  }

  /**
   * Every form of the file that needs the copy needs it only because it is broken in the main tree: it does
   * not start with {@code ?}, and each of its macro uses is a plain macro in a plain slot. Such a form is broken in
   * the copy too (a plain body is one balanced expression in the place of a use the main tree already parsed), so the
   * copy answers nothing there; it is most likely a form the user is typing. Computed once per table.
   */
  static boolean onlyBrokenFormsNeedCopy(@NotNull ErlangFile file, @NotNull Table t) {
    Boolean b = t.onlyBrokenFormsNeedCopy;
    if (b != null) return b;
    b = !isGateWide();
    for (PsiElement c = file.getFirstChild(); c != null && b; c = c.getNextSibling()) {
      if (c instanceof PsiWhiteSpace || c instanceof PsiComment) continue;
      ProgressManager.checkCanceled();
      if (formNeedsCopy(t, c) && !needsCopyOnlyBecauseBroken(t, c)) b = false;
    }
    t.onlyBrokenFormsNeedCopy = b;
    return b;
  }

  private static boolean needsCopyOnlyBecauseBroken(@NotNull Table t, @NotNull PsiElement form) {
    PsiElement first = PsiTreeUtil.firstChild(form);
    if (first == null || "?".equals(first.getText())) return false;
    return !usesNeedSubstitution(t, form, first);
  }

  /**
   * Runs a table or copy request of a caller that has no progress indicator (not the UI thread, not a
   * non-cancellable section) under a new indicator, which an application listener cancels before a write action
   * starts. So the caller gets the layer's answer, and a key press waits for no build: the build or the wait stops at
   * its next cancellation check, and the caller gets master's answer for this question. The platform's own helpers for
   * this ({@code ProgressIndicatorUtils.runInReadActionWithWriteActionPriority}) are deprecated, and the coroutine job of
   * the thread can be read only through experimental API ({@code ThreadContext}), so the layer does not use them.
   * A write action that already waited for the read lock before the listener was added is not seen; it waits for
   * this one build at most.
   */
  static <T> @Nullable T underOwnIndicator(@NotNull Supplier<T> request, @NotNull PsiFile file,
                                                  @NotNull String trigger) {
    var app = ApplicationManager.getApplication();
    EmptyProgressIndicator indicator = new EmptyProgressIndicator();
    Disposable listening = Disposer.newDisposable("macro layer: a request without a progress indicator");
    try {
      app.addApplicationListener(new ApplicationListener() {
        @Override
        public void beforeWriteActionStart(@NotNull Object action) {
          indicator.cancel();
        }
      }, listening);
      OWN_INDICATOR_REQUESTS.incrementAndGet();
      return ProgressManager.getInstance().runProcess(request::get, indicator);
    }
    catch (ProcessCanceledException e) {
      if (!indicator.isCanceled()) throw e;
      ErlangMacroLayerLog.event("SKIPPED", file.getName(), 0, 0, 0, "no-indicator:write-action", trigger);
      return null;
    }
    finally {
      Disposer.dispose(listening);
    }
  }

  /** Requests run by {@link #underOwnIndicator}, for tests and measurements. */
  static final AtomicInteger OWN_INDICATOR_REQUESTS = new AtomicInteger();



  /** {@code erlang.macro.layer.quiet} / {@code ERLANG_MACRO_LAYER_QUIET}, ms, default 1,000. */
  private static final int QUIET_MS = ErlangMacroLayerSettings.readInt("erlang.macro.layer.quiet", "ERLANG_MACRO_LAYER_QUIET", 1000);
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
    return getCopy(psiFile, trigger, false);
  }

  private static @Nullable Copy getCopy(@NotNull PsiFile psiFile, @NotNull String trigger, boolean explicit) {
    ErlangFile file = ErlangMacroLayerTables.layerFile(psiFile);
    if (file == null) return null;
    Table table = ErlangMacroLayerTables.table(file, explicit);
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
    Copy copy = obtain(holder, file, table, trigger, explicit);
    if (copy == null) return null;
    long r0 = ErlangMacroLayerStats.on() ? System.nanoTime() : 0;
    ErlangMacroLayerRetention.getInstance(project).touch(holder, copy);
    if (r0 != 0) ErlangMacroLayerStats.phase("retention-touch", null, System.nanoTime() - r0);
    return copy;
  }

  /** The cached copy if it is built and up to date; never builds. */
  @TestOnly
  public static @Nullable Copy peekCopy(@NotNull PsiFile file) {
    if (!(file instanceof ErlangFile ef) || ef.getUserData(ErlangMacroLayerTables.TABLE_KEY) == null) return null;
    Project project = file.getProject();
    Table t = ErlangMacroLayerTables.validTable(ef, ErlangMacroLayerTables.tableCache(ef), PsiModificationTracker.getInstance(project).getModificationCount(),
                         VirtualFileManager.VFS_STRUCTURE_MODIFICATIONS.getModificationCount(),
                         ProjectRootManager.getInstance(project).getModificationCount());
    return t == null ? null : t.holder.get();
  }

  /** The gate steps for a variable reference, for tests. */
  @TestOnly
  public static @NotNull String explainForTests(@NotNull PsiReference ref) {
    PsiElement element = ref.getElement();
    ErlangFile file = ErlangMacroLayerTables.layerFile(element);
    if (file == null) return "no layer file";
    Table t = ErlangMacroLayerTables.table(file);
    if (t == null) return "no table";
    PsiElement form = topForm(element);
    Copy c = peekCopy(file);
    return "refusal=" + t.refusal + " names=" + t.names + " bodyVars=" + t.bodyVars + " form=" + (form == null ? null : form.getClass().getSimpleName()) +
           " formUses=" + (form != null && formUsesMacro(t, form)) + " argVars=" + (form == null ? null : argumentVars(t, form)) +
           " refClass=" + ref.getClass().getSimpleName() + " copy=" + (c == null ? null : c.expansion.text.length()) +
           " answers=" + (c == null ? null : c.myAnswers.keySet());
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
      if (leaf.getNode().getElementType() != ErlangTypes.ERL_QMARK) continue;
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
    return ErlangMacroLayerTables.computeTable(ef).refusal;
  }

  /** Builds the copy without the cache (also used by measurement tests). */
  public static @NotNull Copy build(@NotNull PsiFile file) {
    Table t = file instanceof ErlangFile ef && !DumbService.isDumb(file.getProject()) ? ErlangMacroLayerTables.computeTable(ef) : null;
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
    catch (ProcessCanceledException e) {
      long dt = System.nanoTime() - probe.t0;
      ErlangMacroLayerStats.phase("build-cancelled", file.getName(), dt);
      ErlangMacroLayerLog.event("CANCELLED", file.getName(), dt, 0, 0, "phase=" + probe.phase, trigger, probe.details());
      throw e;
    }
  }

  /** What one build spends, by phase, with GC and thread CPU time; for the log only. */
  private static final class BuildProbe {
    final long t0 = System.nanoTime();
    final long cpu0 = threadCpu();
    final long gc0 = gcMillis();
    final long gcCount0 = gcCount();
    long lex, expand, parse, errors;
    String phase = "start";

    String details() {
      ProgressIndicator pi = ProgressManager.getGlobalProgressIndicator();
      return String.format(Locale.ROOT, "lex=%.1f expand=%.1f parse=%.1f errors=%.1f cpu=%.1f gcMs=%d gcCount=%d indicator=%s nonCancelable=%s",
                           lex / 1e6, expand / 1e6, parse / 1e6, errors / 1e6, (threadCpu() - cpu0) / 1e6, gcMillis() - gc0,
                           gcCount() - gcCount0, pi == null ? "none" : pi.getClass().getSimpleName(),
                           ProgressManager.getInstance().isInNonCancelableSection());
    }

    static long threadCpu() {
      ThreadMXBean b = ManagementFactory.getThreadMXBean();
      return b.isCurrentThreadCpuTimeSupported() ? b.getCurrentThreadCpuTime() : 0;
    }

    static long gcMillis() {
      long n = 0;
      for (GarbageCollectorMXBean b : ManagementFactory.getGarbageCollectorMXBeans()) {
        n += Math.max(0, b.getCollectionTime());
      }
      return n;
    }

    static long gcCount() {
      long n = 0;
      for (GarbageCollectorMXBean b : ManagementFactory.getGarbageCollectorMXBeans()) {
        n += Math.max(0, b.getCollectionCount());
      }
      return n;
    }
  }

  private static @NotNull Copy build0(@NotNull PsiFile file, @Nullable Table table, @NotNull String trigger, @Nullable String why,
                                      @Nullable BuildProbe probe) {
    long t0 = System.nanoTime();
    Project project = file.getProject();
    long[] now = {ErlangMacroLayerTables.textTracker(file).getModificationCount(), table != null ? table.headerVersions(project) : 0,
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
    Expansion expansion = ErlangMacroExpander.expand(text, new ErlangLexer(project), formCap(),
                                                     table != null ? table.resolver() : null);
    long p0 = System.nanoTime();
    if (probe != null) {
      probe.lex = expansion.lexNanos;
      probe.expand = p0 - t0 - expansion.lexNanos;
      probe.phase = "parse";
    }
    ErlangFile copyFile = null;
    if (expansion.hasExpansions() && expansion.text.length() > ErlangMacroLayerTables.maxCopyChars()) ErlangMacroLayerTables.tooLarge(file, expansion.text.length());
    else if (expansion.hasExpansions()) {
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
    return anyMacroName(s, names::contains);
  }

  /**
   * The text uses a macro that the file cannot see (with an unresolved include it may come from there), or a
   * visible macro whose body reaches one. Strings and comments count too: a refusal too many, never a wrong answer.
   */
  static boolean usesUnseenMacro(@NotNull CharSequence s, @NotNull Table t) {
    return anyMacroName(s, name -> !ErlangMacroExpander.PREDEFINED.contains(name) && (!t.names.contains(name) || t.usesUnseen.contains(name)));
  }

  /** The piece and the pieces after it up to the end of its source form (the recovery may split a form). */
  private static boolean sourceFormUsesUnseenMacro(@NotNull PsiElement form, @NotNull Table t) {
    for (PsiElement e = form; e != null; e = PsiTreeUtil.skipWhitespacesAndCommentsForward(e)) {
      if (usesUnseenMacro(e.getNode().getChars(), t)) return true;
      if (endsSourceForm(e)) return false;
    }
    return false;
  }

  private static boolean anyMacroName(@NotNull CharSequence s, @NotNull Predicate<String> test) {
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
      if (test.test(name)) return true;
    }
    return false;
  }

  /** The form (or, for a top-level error, the form before it) uses a macro the file can see. Never builds a copy. */
  static boolean formUsesMacro(@NotNull Table t, @Nullable PsiElement form) {
    if (form == null || t.names.isEmpty()) return false;
    int at = form.getTextRange().getStartOffset();
    Boolean b = t.formUse.get(at);
    if (b == null) {
      b = formUsesNames(t.names, form);
      // A form that uses a macro the file cannot see: master's behaviour (the macro may be in a missing header)
      if (b && t.missingInclude && sourceFormUsesUnseenMacro(form, t)) b = false;
      t.formUse.put(at, b);
    }
    return b;
  }

  private static boolean formUsesNames(@NotNull Set<String> names, @NotNull PsiElement form) {
    boolean b = usesKnownMacro(form.getNode().getChars(), names) || splitUseAtEdge(form, names);
    if (!b && (form instanceof PsiErrorElement || PsiTreeUtil.firstChild(form) instanceof PsiErrorElement)) {
      PsiElement prev = PsiTreeUtil.skipWhitespacesAndCommentsBackward(form);
      b = prev != null && usesKnownMacro(prev.getNode().getChars(), names);
    }
    // The rest of a form that the recovery split ("reverse(L)." after "?LISTS" or "lists?COLON"): the pieces
    // before it, back to the one that ends the form, belong to the same form of the source
    for (PsiElement prev = PsiTreeUtil.skipWhitespacesAndCommentsBackward(form); !b && prev != null && !endsSourceForm(prev);
         prev = PsiTreeUtil.skipWhitespacesAndCommentsBackward(prev)) {
      b = usesKnownMacro(prev.getNode().getChars(), names);
    }
    return b;
  }

  /** The top-level element ends with the dot that ends a form (not the dot of a record field access, R#rec.f). */
  static boolean endsSourceForm(@NotNull PsiElement e) {
    PsiElement last = PsiTreeUtil.getDeepestVisibleLast(e);
    if (last == null || last.getNode().getElementType() != ErlangTypes.ERL_DOT) return false;
    PsiElement name = PsiTreeUtil.prevVisibleLeaf(last);
    PsiElement radix = name == null ? null : PsiTreeUtil.prevVisibleLeaf(name);
    return radix == null || radix.getNode().getElementType() != ErlangTypes.ERL_RADIX;
  }

  /**
   * {@link #formUsesMacro} for a structural question (does a token reference exist here), which
   * must never build, wait or be refused: with the valid table of the file, else with the macro names of its last table
   * (one edit old: the names change only with a {@code -define}, an include or a header's directives; not cached, the
   * per-form caches of an old table are keyed by old offsets). Null when the file has no table yet. The identifier
   * highlighting and ctrl-hover look up the references of every element they visit, and right after a key press each
   * lookup was refused the new ErlangMacroLayerTables.table (17,170 refusals of 20,840 in one IDE typing session).
   */
  static @Nullable Boolean formUsesMacroNoWait(@NotNull ErlangFile file, @Nullable PsiElement form) {
    TableCache cache = file.getUserData(ErlangMacroLayerTables.TABLE_KEY);
    TableEntry last = cache == null ? null : cache.entry;
    if (last == null) return null;
    Project project = file.getProject();
    Table valid = ErlangMacroLayerTables.validTable(file, cache, PsiModificationTracker.getInstance(project).getModificationCount(),
                             VirtualFileManager.VFS_STRUCTURE_MODIFICATIONS.getModificationCount(),
                             ProjectRootManager.getInstance(project).getModificationCount());
    if (valid != null) return valid.refusal == null && formUsesMacro(valid, form);
    Table t = last.table();
    return t.refusal == null && form != null && !t.names.isEmpty() && formUsesNames(t.names, form);
  }

  /**
   * The free variables (not parameters) of every definition of every macro that the form uses, directly or
   * through the macros those bodies use. Only these variables can occur in the form's expansion besides the form's
   * own tokens. A superset: every {@code ?NAME} of the form's text counts, strings and comments too.
   */
  static @NotNull Set<String> formFreeBodyVars(@NotNull Table t, @NotNull PsiElement form) {
    return t.formFreeVars.computeIfAbsent(form.getTextRange().getStartOffset(), k -> {
      Set<String> r = new HashSet<>();
      ArrayDeque<String> queue = new ArrayDeque<>(macroNamesIn(form.getNode().getChars(), t.names));
      Set<String> seen = new HashSet<>(queue);
      while (!queue.isEmpty()) {
        String m = queue.poll();
        r.addAll(t.macroFreeVars.getOrDefault(m, Set.of()));
        for (String n : t.macroNestedUses.getOrDefault(m, Set.of())) if (t.names.contains(n) && seen.add(n)) queue.add(n);
      }
      return r;
    });
  }

  /** The names of the text's uses {@code ?NAME} that are in {@code names} (as {@link #usesKnownMacro} finds them). */
  static @NotNull Set<String> macroNamesIn(@NotNull CharSequence s, @NotNull Set<String> names) {
    Set<String> r = new HashSet<>();
    int n = s.length();
    for (int i = 0; i < n; i++) {
      if (s.charAt(i) != '?') continue;
      int j = i + 1;
      if (j < n && s.charAt(j) == '?') {
        i = j;
        continue;
      }
      while (j < n && Character.isWhitespace(s.charAt(j))) j++;
      String name;
      if (j < n && s.charAt(j) == '\'') {
        int k = j + 1;
        while (k < n && s.charAt(k) != '\'') k++;
        name = s.subSequence(j + 1, Math.min(k, n)).toString();
      }
      else {
        int k = j;
        while (k < n && (Character.isLetterOrDigit(s.charAt(k)) || s.charAt(k) == '_' || s.charAt(k) == '@')) k++;
        if (k == j) continue;
        name = s.subSequence(j, k).toString();
      }
      if (names.contains(name)) r.add(name);
    }
    return r;
  }

  /**
   * The main tree split a use {@code ?name(...)} between two top-level forms: error recovery stops before
   * {@code atom (}, so {@code foo?is_timeout(a)} ends one form with the bare {@code ?} and starts the next with
   * {@code is_timeout(a)}. Both forms use the macro.
   */
  private static boolean splitUseAtEdge(@NotNull PsiElement form, @NotNull Set<String> names) {
    PsiElement last = PsiTreeUtil.lastChild(form);
    while (last != null && (last instanceof PsiWhiteSpace || last instanceof PsiComment || last.getTextLength() == 0)) last = PsiTreeUtil.prevLeaf(last);
    if (last != null && last.textMatches("?")) {
      PsiElement name = PsiTreeUtil.nextVisibleLeaf(last);
      if (name != null && names.contains(StringUtil.unquoteString(name.getText(), '\''))) return true;
    }
    PsiElement first = PsiTreeUtil.firstChild(form);
    while (first != null && (first instanceof PsiWhiteSpace || first instanceof PsiComment || first.getTextLength() == 0)) first = PsiTreeUtil.nextLeaf(first);
    PsiElement prev = first == null ? null : PsiTreeUtil.prevVisibleLeaf(first);
    if (prev != null && prev.textMatches("?") && names.contains(StringUtil.unquoteString(first.getText(), '\''))) return true;
    // the form ends broken right before a form that starts with a use: f(_) ?ARROW ok. (with -define(ARROW, ->))
    PsiElement next = last == null ? null : PsiTreeUtil.nextVisibleLeaf(last);
    if (next == null || !next.textMatches("?") || PsiTreeUtil.isAncestor(form, next, false) || !PsiTreeUtil.hasErrorElements(form)) return false;
    PsiElement name = PsiTreeUtil.nextVisibleLeaf(next);
    return name != null && names.contains(StringUtil.unquoteString(name.getText(), '\''));
  }

  /** Names of the variables inside macro calls (their arguments) of the form. */
  static @NotNull Set<String> argumentVars(@NotNull Table t, @NotNull PsiElement form) {
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
   * Names of the variables inside the arguments of the macro uses of the form that need substitution (the
   * macro is not plain, or the use is not in a plain slot). Every enclosing use counts: {@code ?PLAIN(?OTHER(X))}
   * counts X for ?OTHER. An element that starts with {@code ?} but is not a macro use counts too (a superset).
   */
  static @NotNull Set<String> substitutedArgumentVars(@NotNull Table t, @NotNull PsiElement form) {
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

  static boolean formHasError(@NotNull Table t, @NotNull PsiElement form) {
    return t.formError.computeIfAbsent(form.getTextRange().getStartOffset(), k -> PsiTreeUtil.hasErrorElements(form));
  }

  /**
   * The form uses a visible macro and needs substitution: the copy may answer for it. That is the case when
   * the form is broken in the main tree, or starts with {@code ?}, or has a use {@code ?NAME} of a visible macro that
   * is not plain ({@link #classify}), or of a plain macro in a position other than a plain expression slot
   * ({@link #isPlainPosition}). A directive ({@code -define}, {@code -include}, {@code -ifdef}, ...) never needs it:
   * the copy keeps directives as they are and never answers inside them. With the wide gate: every form with a use.
   */
  /**
   * Some top-level form of the file needs the copy (computed once per table, with every form's answer). When
   * none does, every question is answered "none" after this one check: no form can be answered by the copy, and no
   * form can make a name (a form that could, a top-level macro use or a broken form with a macro use, needs the copy).
   */
  static boolean anyFormNeedsCopy(@NotNull ErlangFile file, @NotNull Table t) {
    Boolean b = t.anyFormNeedsCopy;
    if (b != null) return b;
    // one thread computes it (about 25 ms in erl_types.erl); the others wait instead of repeating the walk
    synchronized (t) {
      b = t.anyFormNeedsCopy;
      if (b != null) return b;
      return computeAnyFormNeedsCopy(file, t);
    }
  }

  private static boolean computeAnyFormNeedsCopy(@NotNull ErlangFile file, @NotNull Table t) {
    boolean b = false;
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
    return usesNeedSubstitution(t, form, first);
  }

  /** Some {@code ?} of the form is not a use of a plain macro in a plain slot (or of a predefined or invisible macro). */
  private static boolean usesNeedSubstitution(@NotNull Table t, @NotNull PsiElement form, @NotNull PsiElement first) {
    int end = form.getTextRange().getEndOffset();
    for (PsiElement leaf = first; leaf != null && leaf.getTextRange().getStartOffset() < end; leaf = PsiTreeUtil.nextLeaf(leaf)) {
      if (leaf.getNode().getElementType() != ErlangTypes.ERL_QMARK) continue;
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
   * The macro use stands alone in a slot of a value: a whole expression ({@code ?M}), the callee of a local
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
   * The element may be what the layer gives as the declaration of a name made by a macro: an atom name leaf
   * in a top-level form that starts with a macro use or is broken (such as {@code foo} in {@code ?GEN(foo).}), or in a
   * {@code -define} body (such as {@code mr} in {@code -define(R, -record(mr, {f})).}). A structural check only; it
   * asks no copy, so the UI thread may call it.
   */
  public static boolean isNameMadeByMacro(@Nullable PsiElement element) {
    // the platform asks with any element (a directory, a binary file, another language's element),
    // whose text range or first child's text range may be null: only an Erlang leaf or Erlang element can be such a name
    if (!(element instanceof LeafPsiElement) && !(element instanceof ErlangCompositeElement)) return false;
    // the layer maps to the topmost element with the token's range: it may be an expression around the leaf
    PsiElement first = element instanceof LeafPsiElement ? element : PsiTreeUtil.firstChild(element);
    TextRange range = element.getTextRange();
    if (first == null || range == null || !range.equals(first.getTextRange())) return false;
    if (!(first instanceof LeafPsiElement leaf) || leaf.getElementType() != ErlangTypes.ERL_ATOM_NAME) return false;
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
      // a name in a macro use at the top level makes a function; in a -define body it may name anything (the copy, if
      // there is one, tells what)
      String kind = isInTopLevelMacroForm(element) ? "function" : "name";
      String made = madeNameKind(element);
      return ErlangMacroLayerHeaderTarget.describe(made != null ? made : kind, element.getText(), location);
    }
    return null;
  }

  /**
   * The token is the name that a macro makes ({@code hfoo} in {@code ?HGEN(hfoo).}, {@code mr}
   * in {@code -define(R, -record(mr, {mf})).}), as the copy tells; it is then its own declaration (the uses resolve to
   * it). A structural check first; the copy only for such tokens, under the layer's build rule (a passive query waits).
   */
  /**
   * The functions of the main tree that continue a function whose first clauses a top-level macro use makes with this
   * name ({@code ?HEAD(X) positive; sign(0) -> zero.}: the recovery made {@code sign(0) -> zero.} a function of its
   * own): renaming the made name renames them too.
   */
  public static @NotNull List<ErlangFunction> functionsContinuingMadeName(@NotNull PsiElement madeName) {
    ErlangFile file = ErlangMacroLayerTables.layerFile(madeName);
    if (file == null || !isNameMadeByMacro(madeName)) return List.of();
    String name = StringUtil.unquoteString(madeName.getText(), '\'');
    List<ErlangFunction> r = new ArrayList<>();
    for (ErlangFunction f : file.getFunctions()) {
      if (!name.equals(f.getName()) || f.getParent() != file) continue;
      PsiElement prev = PsiTreeUtil.skipWhitespacesAndCommentsBackward(f);
      if (prev != null && !endsSourceForm(prev)) r.add(f);
    }
    return r;
  }

  /**
   * The names of the other clauses of a function whose first clause a macro makes ({@code ?HEAD(X) positive;
   * sign(0) -> zero.}): in the copy they are clauses of the same function; these are their tokens in the real file.
   */
  public static @NotNull List<PsiElement> otherClauseNamesOfMadeFunction(@NotNull PsiElement madeName) {
    ErlangFile file = ErlangMacroLayerTables.layerFile(madeName);
    if (file == null || !isNameMadeByMacro(madeName)) return List.of();
    Copy copy = getCopy(file, "rename");
    if (copy == null || copy.copyFile == null) return List.of();
    TextRange range = madeName.getTextRange();
    List<PsiElement> r = new ArrayList<>();
    for (ErlangFunction fn : copy.copyFile.getFunctions()) {
      PsiElement real = copy.toReal(fn.getNameIdentifier());
      if (real == null || !range.equals(real.getTextRange())) continue;
      for (ErlangFunctionClause clause : fn.getFunctionClauseList()) {
        ErlangQAtom q = clause.getQAtom();
        PsiElement other = q == null ? null : copy.toReal(q);
        if (other == null || range.equals(other.getTextRange()) || other.getContainingFile() != file) continue;
        PsiElement leaf = file.findElementAt(other.getTextRange().getStartOffset());
        if (leaf != null && leaf.getText().equals(madeName.getText())) r.add(leaf);
      }
    }
    return r;
  }

  public static boolean isMadeNameDeclaration(@NotNull PsiElement leaf) {
    if (!isNameMadeByMacro(leaf)) return false;
    ErlangFile file = ErlangMacroLayerTables.layerFile(leaf);
    Copy copy = file == null ? null : getCopy(file, "named-element");
    String kind = copy == null ? null : copy.madeNameKinds().get(leaf.getTextRange());
    // a local type made by a macro is left out: no rename is offered at its token (ErlangMacroLayerRename6Test)
    return "function".equals(kind) || "record".equals(kind) || "record field".equals(kind);
  }

  /** The kind of a name made by a macro at this token of a real .erl file, from a copy that is there; or null. */
  private static @Nullable String madeNameKind(@NotNull PsiElement token) {
    ErlangFile file = ErlangMacroLayerTables.layerFile(token);
    Copy copy = file == null ? null : cachedCopy(file);
    return copy == null ? null : copy.madeNameKinds().get(token.getTextRange());
  }

  /**
   * Whether some form of the file needs the copy, from the valid ErlangMacroLayerTables.table (computed if it is not
   * known for it yet; a walk of the forms, cached on the table), else from the file's last table if that one knows;
   * null when there is no table or the answer is not known. Never computes a table, never builds, never waits.
   */
  static @Nullable Boolean anyFormNeedsCopyNoWait(@NotNull ErlangFile file) {
    TableCache cache = file.getUserData(ErlangMacroLayerTables.TABLE_KEY);
    TableEntry last = cache == null ? null : cache.entry;
    if (last == null) return null;
    Table valid = cachedTable(file);
    if (valid != null) return valid.refusal == null && !valid.names.isEmpty() && anyFormNeedsCopy(file, valid);
    Table t = last.table();
    if (t.refusal != null || t.names.isEmpty()) return false;
    return t.anyFormNeedsCopy;
  }

  /** The table of the file if it is up to date; never computes one, never waits. */
  static @Nullable Table cachedTable(@NotNull ErlangFile file) {
    if (file.getUserData(ErlangMacroLayerTables.TABLE_KEY) == null) return null;
    Project project = file.getProject();
    return ErlangMacroLayerTables.validTable(file, ErlangMacroLayerTables.tableCache(file), PsiModificationTracker.getInstance(project).getModificationCount(),
                      VirtualFileManager.VFS_STRUCTURE_MODIFICATIONS.getModificationCount(),
                      ProjectRootManager.getInstance(project).getModificationCount());
  }

  /** The copy of the file if it is built and up to date; never builds, never computes a table. */
  static @Nullable Copy cachedCopy(@NotNull ErlangFile file) {
    if (file.getUserData(ErlangMacroLayerTables.TABLE_KEY) == null) return null;
    Project project = file.getProject();
    Table t = ErlangMacroLayerTables.validTable(file, ErlangMacroLayerTables.tableCache(file), PsiModificationTracker.getInstance(project).getModificationCount(),
                         VirtualFileManager.VFS_STRUCTURE_MODIFICATIONS.getModificationCount(),
                         ProjectRootManager.getInstance(project).getModificationCount());
    return t == null ? null : t.holder.get();
  }

  /**
   * The element to rename when a rename starts at a token inside a macro, or null when the layer cannot tell
   * now (the caller then refuses the rename with a message). The uses of a name made by a macro resolve to the name's
   * leaf ({@code made} in {@code ?MK(made).}, {@code mr} in {@code -define(R, -record(mr, {mf})).}), and the uses of a
   * free variable of a header macro body to an {@link ErlangMacroLayerHeaderTarget}; started at the token itself, the
   * rename would take the atom or the variable of the token and edit only that token. Every other element is given
   * back as it is.
   */
  public static @Nullable PsiElement renameTargetOfMacroToken(@NotNull PsiElement element) {
    if (!isEnabled()) return element;
    if (element instanceof ErlangQVar v) {
      PsiElement h = headerBodyVariableTarget(v);
      return h != null ? h : element;
    }
    if (!(element instanceof ErlangQAtom q) || q.getAtom() == null) return element;
    PsiElement leaf = PsiTreeUtil.firstChild(q);
    if (leaf == null || !isNameMadeByMacro(leaf)) return element;
    ErlangFile file = ErlangMacroLayerTables.layerFile(leaf);
    if (file == null) return element; // a header body: master's rename and the header targets
    // on the UI thread only a copy that is there (getCopy never builds there); elsewhere it builds
    Copy copy = getCopy(file, "rename-target");
    if (copy == null) {
      Table t = file.getUserData(ErlangMacroLayerTables.TABLE_KEY) == null ? null : ErlangMacroLayerTables.table(file);
      if (t != null && (t.refusal != null || !anyFormNeedsCopy(file, t)) || !mayMakeAName(leaf)) return element;
      return null;
    }
    return copy.madeNameKinds().containsKey(leaf.getTextRange()) ? leaf : element;
  }

  /**
   * The token may be a name that an expansion makes: an argument of a top-level form that starts with a macro
   * use, or a token of a -define body that can make a top-level form (it starts with {@code -} or has a {@code ->}) or
   * that uses another macro. A token of any other body (a field fragment such as {@code id :: atom()}, a constant) is
   * renamed by master's path, which resolves it itself.
   */
  private static boolean mayMakeAName(@NotNull PsiElement leaf) {
    ErlangMacrosBody body = PsiTreeUtil.getParentOfType(leaf, ErlangMacrosBody.class);
    if (body == null) return true; // a top-level macro form
    PsiElement first = PsiTreeUtil.firstChild(body);
    while (first != null && (first instanceof PsiWhiteSpace || first instanceof PsiComment || first.getTextLength() == 0)) {
      first = PsiTreeUtil.nextLeaf(first);
    }
    if (first != null && "-".equals(first.getText())) return true;
    int end = body.getTextRange().getEndOffset();
    for (PsiElement l = PsiTreeUtil.firstChild(body); l != null && l.getTextRange().getStartOffset() < end; l = PsiTreeUtil.nextLeaf(l)) {
      String t = l.getText();
      if ("->".equals(t) || "?".equals(t)) return true;
    }
    return false;
  }

  /** A variable of a header macro body that is not a parameter of its macro: the uses in the includers see it. */
  public static boolean isHeaderBodyVariable(@NotNull PsiElement element) {
    return element instanceof ErlangQVar v && isEnabled() && headerBodyVariableTarget(v) != null;
  }

  private static @Nullable ErlangMacroLayerHeaderTarget headerBodyVariableTarget(@NotNull ErlangQVar v) {
    PsiFile f = v.getContainingFile();
    if (!(f instanceof ErlangFile ef) || f.getUserData(COPY_OF) != null || f.getName().endsWith(".erl")) return null;
    ErlangMacrosDefinition def = PsiTreeUtil.getParentOfType(v, ErlangMacrosDefinition.class);
    if (def == null || def.getName() == null || !PsiTreeUtil.isAncestor(def.getMacrosBody(), v, false)) return null;
    if (isMacroParameter(def, v.getText()) || "_".equals(v.getText())) return null;
    VirtualFile vf = ef.getOriginalFile().getVirtualFile();
    ErlangMacroExpander.HeaderTok h = vf == null ? null : ErlangMacroLayerBodySearch.headerOrigin(ef, vf, v.getTextRange().getStartOffset(), v.getText());
    return h == null ? null : new ErlangMacroLayerHeaderTarget(ef.getProject(), vf, h.directive(), h.index(), v.getText(), v.getClass(),
                                                                "variable");
  }

  /** The name is a parameter of the macro definition. */
  public static boolean isMacroParameter(@NotNull ErlangMacrosDefinition def, @NotNull String name) {
    ErlangArgumentDefinitionList args = def.getArgumentDefinitionList();
    if (args == null) return false;
    for (ErlangArgumentDefinition a : args.getArgumentDefinitionList()) if (a.getText().equals(name)) return true;
    return false;
  }

  /** Project-wide word searches for the includers of a header macro (tests). */
  public static final AtomicInteger INCLUDER_SEARCHES = ErlangMacroLayerBodySearch.INCLUDER_SEARCHES;

  public static @NotNull List<PsiElement> resolveInMacroBody(@NotNull PsiElement leaf) {
    return ErlangMacroLayerBodySearch.resolveInMacroBody(leaf);
  }

  public static @NotNull List<ErlangFile> includersOfHeader(@Nullable PsiFile header, @NotNull String macro) {
    return ErlangMacroLayerBodySearch.includersOfHeader(header, macro);
  }

  public static boolean isReferenceThroughMacroUses(@NotNull PsiReference ref, @NotNull PsiElement element) {
    return ErlangMacroLayerBodySearch.isReferenceThroughMacroUses(ref, element);
  }

  /** Some top-level form of the main tree starts with '?' or is broken: only such forms can make a function. */
  static boolean hasTopLevelMacroForm(@NotNull ErlangFile file, @NotNull Table t) {
    Boolean b = t.topLevelMacroForm;
    if (b != null) return b;
    b = false;
    if (!t.names.isEmpty()) {
      for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) {
        if (c instanceof PsiWhiteSpace || c instanceof PsiComment) continue;
        // the narrow gate: only a form with a use of a visible macro can make a name; another form is copied as it is
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
  public static @Nullable PsiElement resolveFallback(@NotNull PsiReference ref) {
    return ErlangMacroLayerAnswers.resolveFallback(ref);
  }

  public static @NotNull ResolveResult @NotNull [] multiResolveFallback(@NotNull PsiPolyVariantReference ref) {
    return ErlangMacroLayerAnswers.multiResolveFallback(ref);
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
    ErlangFile file = ErlangMacroLayerTables.layerFile(error);
    if (file == null) {
      mark(o, "gate:not-layer-file");
      return false;
    }
    markFile(o, file);
    Table t = ErlangMacroLayerTables.table(file);
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
