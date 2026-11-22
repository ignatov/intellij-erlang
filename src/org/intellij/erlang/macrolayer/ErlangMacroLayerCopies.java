package org.intellij.erlang.macrolayer;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationListener;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.util.PsiModificationTracker;
import com.intellij.psi.util.PsiTreeUtil;
import java.lang.management.GarbageCollectorMXBean;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.lang.ref.WeakReference;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.LockSupport;
import java.util.function.BooleanSupplier;
import java.util.function.Supplier;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.macrolayer.ErlangMacroLayer.Copy;
import org.intellij.erlang.macrolayer.ErlangMacroLayer.Table;
import org.intellij.erlang.macrolayer.ErlangMacroLayerTables.TableCache;
import org.intellij.erlang.macrolayer.ErlangMacroLayerTables.TableEntry;
import org.intellij.erlang.macrolayer.ErlangMacroLayerTables.TextTracker;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Expansion;
import org.intellij.erlang.parser.macro.ErlangMacroExpander;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

/**
 * The expanded copy of a file: when it is built, by whom, and how long it is kept.
 * <p>
 * Build rule: a copy is built on demand for a layer file whose table says some form needs it, never on the UI thread
 * (it uses only a copy that is there). While the file is typed in (within {@link #quietMillis()} of its last document
 * change), a passive query at the caret ({@link ErlangMacroLayerCallers}) builds nothing: it waits for the copy (at most
 * {@code passiveWaitMillis()}) and gives up on the next key press; a caller that cannot be cancelled gets the copy that
 * is there or master's answer; a caller without a progress indicator builds under an indicator of its own that a key
 * press cancels. Concurrent askers of one file wait for one build.
 * <p>
 * Owns: the copy holder on each file version ({@code CopyHolder}: strong while the retention service keeps it, else
 * weak), the build in progress, the time of the last document change. A copy is replaced when the file's text, its
 * include resolution or a directive line of one of its headers changes (the table's dependencies); its kept answers
 * are dropped on any PSI change ({@link ErlangMacroLayer.Copy#answers()}).
 * <p>
 * Threading: under a read action, cancellable; a pending write action makes a waiting caller give up.
 */
final class ErlangMacroLayerCopies {
  private ErlangMacroLayerCopies() {
  }



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
  static @Nullable Copy obtain(@NotNull CopyHolder holder, @NotNull ErlangFile file, @NotNull Table table,
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
        if (refusal == null && !ErlangMacroLayer.isEdtBuildAllowed() && onlyBrokenFormsNeedCopy(file, table)) {
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
   * the caret ({@link ErlangMacroLayer#isPassiveCaller}: the identifier highlighting, ctrl-hover, parameter info, highlight usages)
   * starts no copy build: the next key press would cancel it. It uses a copy that is there or being built, else it
   * gets master's answer. Every other caller builds and waits, cancellably: highlighting, and every search or
   * navigation, also when it runs on worker threads that do not show the action that started it. A caller that is
   * wrongly taken for a passive one would get master's answer, so only callers that are positively recognised are
   * refused; a caller that is wrongly taken for an active one only costs a build.
   */
  static @Nullable String buildRefusal(@NotNull PsiFile file) {
    if (ErlangMacroLayer.isEdtBuildAllowed()) return null; // diagnostic: builds allowed anywhere
    if (ErlangMacroLayerCallers.isHighlighting(ProgressManager.getGlobalProgressIndicator())) return null;
    if (typedWithinQuietTime(file)) return ErlangMacroLayerCallers.isPassiveCaller(file) ? TYPING : null;
    return null;
  }

  static final String TYPING = "typing";

  /** The file's PSI or its document changed less than {@link #quietMillis()} ago. */
  static boolean typedWithinQuietTime(@NotNull PsiFile file) {
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
  static final int PASSIVE_WAIT_MS = ErlangMacroLayerSettings.readInt("erlang.macro.layer.passive.wait", "ERLANG_MACRO_LAYER_PASSIVE_WAIT", 2000);
  static volatile @Nullable Integer ourTestPassiveWait;

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
  static final Map<ProgressIndicator, Boolean> GAVE_UP = Collections.synchronizedMap(new WeakHashMap<>());

  static boolean gaveUp(@NotNull ProgressIndicator root) {
    return GAVE_UP.containsKey(root);
  }

  static void giveUp(@NotNull ProgressIndicator root) {
    GAVE_UP.put(root, Boolean.TRUE);
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
    if (!ErlangMacroLayer.isEnabled() || ApplicationManager.getApplication().isDispatchThread() || ErlangMacroLayerCallers.nonCancellableCaller() != null) return;
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
    ErlangMacroLayer.SEARCH_STACK_WALKS.incrementAndGet();
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
  static boolean isBeingTyped(@NotNull PsiFile file, long quiet) {
    Document document = PsiDocumentManager.getInstance(file.getProject()).getCachedDocument(file);
    if (document == null) return false;
    Long changed = document.getUserData(DOCUMENT_CHANGED_AT);
    return changed != null && System.nanoTime() - changed < quiet * 1_000_000L;
  }

  /** System.nanoTime() of the last change of a document (set by the listener of the retention service). */
  static final Key<Long> DOCUMENT_CHANGED_AT = Key.create("erlang.macro.layer.document.changed.at");

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
    b = !ErlangMacroLayer.isGateWide();
    for (PsiElement c = file.getFirstChild(); c != null && b; c = c.getNextSibling()) {
      if (c instanceof PsiWhiteSpace || c instanceof PsiComment) continue;
      ProgressManager.checkCanceled();
      if (ErlangMacroLayer.formNeedsCopy(t, c) && !needsCopyOnlyBecauseBroken(t, c)) b = false;
    }
    t.onlyBrokenFormsNeedCopy = b;
    return b;
  }

  static boolean needsCopyOnlyBecauseBroken(@NotNull Table t, @NotNull PsiElement form) {
    PsiElement first = PsiTreeUtil.firstChild(form);
    if (first == null || "?".equals(first.getText())) return false;
    return !ErlangMacroLayer.usesNeedSubstitution(t, form, first);
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
  static final int QUIET_MS = ErlangMacroLayerSettings.readInt("erlang.macro.layer.quiet", "ERLANG_MACRO_LAYER_QUIET", 1000);
  static volatile @Nullable Integer ourTestQuiet;

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

  static @Nullable Copy getCopy(@NotNull PsiFile psiFile, @NotNull String trigger, boolean explicit) {
    ErlangFile file = ErlangMacroLayerTables.layerFile(psiFile);
    if (file == null) return null;
    Table table = ErlangMacroLayerTables.table(file, explicit);
    if (table == null) {
      ErlangMacroLayerLog.event("EDT_MISS", file.getName(), 0, 0, 0, "table", trigger);
      return null;
    }
    if (table.refusal != null) return null;
    Project project = file.getProject();
    boolean edt = ApplicationManager.getApplication().isDispatchThread() && !ErlangMacroLayer.isEdtBuildAllowed();
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
    PsiElement form = ErlangMacroLayer.topForm(element);
    Copy c = peekCopy(file);
    return "refusal=" + t.refusal + " names=" + t.names + " bodyVars=" + t.bodyVars + " form=" + (form == null ? null : form.getClass().getSimpleName()) +
           " formUses=" + (form != null && ErlangMacroLayer.formUsesMacro(t, form)) + " argVars=" + (form == null ? null : ErlangMacroLayer.argumentVars(t, form)) +
           " refClass=" + ref.getClass().getSimpleName() + " copy=" + (c == null ? null : c.expansion.text.length()) +
           " answers=" + (c == null ? null : c.answerKeys());
  }

  /** Why a form needs the copy (the first reason found), or null; for tests and measurements. */
  @TestOnly
  static @Nullable String whyFormNeedsCopyForTests(@NotNull Table t, @NotNull PsiElement form) {
    if (!ErlangMacroLayer.formUsesMacro(t, form)) return null;
    if (form instanceof PsiErrorElement) return "error-form";
    PsiElement first = PsiTreeUtil.firstChild(form);
    if (first == null || first instanceof PsiErrorElement || "?".equals(first.getText())) return "starts-with-?";
    if (ErlangMacroLayer.formHasError(t, form)) return "parse-error";
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
      if (!ErlangMacroLayer.isPlainPosition(m)) {
        StringBuilder chain = new StringBuilder();
        for (PsiElement e = m.getParent(); e != null && e != form && chain.length() < 120; e = e.getParent()) chain.append(e.getClass().getSimpleName().replace("Erlang", "").replace("Impl", "")).append('<');
        return "position:" + name + ":" + chain;
      }
    }
    return null;
  }

  @TestOnly
  static boolean formUsesMacroForTests(@NotNull Table t, @Nullable PsiElement form) {
    return ErlangMacroLayer.formUsesMacro(t, form);
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

  static final Key<long[]> LAST_BUILD_STATE = Key.create("erlang.macro.layer.last.build");
  static volatile @Nullable Runnable ourBuildHookForTests;

  /** Runs at the start of every cached build (not of {@link #build(PsiFile)}), on the building thread. */
  @TestOnly
  public static void setBuildHookForTests(@Nullable Runnable hook, @NotNull Disposable parent) {
    ourBuildHookForTests = hook;
    Disposer.register(parent, () -> ourBuildHookForTests = null);
  }

  static @NotNull Copy build(@NotNull PsiFile file, @Nullable Table table, @NotNull String trigger, @Nullable String why) {
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
  static final class BuildProbe {
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

  static @NotNull Copy build0(@NotNull PsiFile file, @Nullable Table table, @NotNull String trigger, @Nullable String why,
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
    ErlangMacroLayer.EXPANSIONS_BUILT.incrementAndGet();
    CharSequence text = file.getViewProvider().getContents();
    if (file.getTextLength() != text.length() || !file.textMatches(text)) text = file.getText();
    if (probe != null) probe.phase = "expand";
    Expansion expansion = ErlangMacroExpander.expand(text, new ErlangLexer(project), ErlangMacroLayer.formCap(),
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
      ErlangMacroLayer.COPIES_PARSED.incrementAndGet();
      PsiFile parsed = PsiFileFactory.getInstance(project)
        .createFileFromText(file.getName(), ErlangLanguage.INSTANCE, expansion.text, false, false);
      if (parsed instanceof ErlangFile ef) {
        parsed.putUserData(ErlangMacroLayer.COPY_OF, file);
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
}
