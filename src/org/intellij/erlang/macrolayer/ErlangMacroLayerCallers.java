package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.DaemonProgressIndicator;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.WrappedProgressIndicator;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.lang.ref.WeakReference;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Supplier;

/**
 * Who is asking: the layer refuses a copy build to a passive query at the caret while the file is typed in, and to a
 * caller that cannot be cancelled. This is the only place that looks at the platform's callers.
 * <p>
 * Passive queries are recognised by class names on the thread's stack ({@link #PASSIVE_CALLERS}): the identifier
 * highlighting ({@code IdentifierHighlightingComputer}, its manager, updater and pass), ctrl-hover ({@code CtrlMouseHandler},
 * {@code CtrlMouseData}), parameter info ({@code com.intellij.codeInsight.hint.}) and highlight usages
 * ({@code HighlightUsagesHandler}), checked against IDEA 2026.2 and 2026.3. If the platform renames them, such a query
 * is no longer seen as passive: it then builds the copy while the user types (slower typing in files that need the
 * copy, no wrong answer); ErlangMacroLayerActiveCallersTest and ErlangMacroLayerMacroUseRuleTest fail then. The
 * daemon's highlighting is recognised by its {@code DaemonProgressIndicator}; a non-cancellable caller by the
 * progress manager.
 * <p>
 * Threading: any thread; the stack walk runs on the asking thread, only when a build would be refused, and its
 * "passive" answer is kept per progress indicator in a thread-local weak memo, so a call costs one walk at most.
 */
final class ErlangMacroLayerCallers {
  private ErlangMacroLayerCallers() {
  }

  /**
   * Classes of passive queries at the caret, which run again after every key press and whose answer is only shown:
   * the identifier highlighting ({@code IdentifierHighlightingComputer} and its manager, updater and pass), ctrl-hover
   * ({@code CtrlMouseHandler}, {@code CtrlMouseData}), parameter info ({@code com.intellij.codeInsight.hint.}) and
   * highlight usages in the file ({@code HighlightUsagesHandler}). None of them runs a rename or a find usages.
   */
  private static final String[] PASSIVE_CALLERS = {"IdentifierHighlight", "HighlightUsages", "CtrlMouse", "com.intellij.codeInsight.hint."};

  /** Stack walks made by {@link #isPassiveCaller}, for tests and measurements. */
  static final AtomicInteger STACK_WALKS = new AtomicInteger();
  /** Stack walks made by {@link ErlangMacroLayer#prepareSearch} (one per search in a file just typed in). */
  static final AtomicInteger SEARCH_STACK_WALKS = new AtomicInteger();

  /**
   * The last progress indicator under which this thread was found to run a passive query (the indicator
   * itself, not a wrapper of it: the search inside the identifier highlighting wraps it). Only this thread reads it,
   * and it matches only the very same indicator object, so it never marks a worker thread or another caller: a search
   * that the platform splits over worker threads walks their own stacks. The identifier highlighting
   * (every refused question in the IDE measurement, CALLERS6.md) runs under an indicator that
   * {@code CoroutinesKt.contextToIndicator} makes for that one call, inside the passive frame
   * ({@code IdentifierHighlightingAccessorImpl.getMarkupData}); the indicator, and so this memo, cannot outlive it.
   * Weak: the memo keeps no indicator alive.
   */
  private static final ThreadLocal<WeakReference<ProgressIndicator>> PASSIVE_MEMO = new ThreadLocal<>();

  /**
   * The caller is a passive query ({@link #PASSIVE_CALLERS} on the thread's stack). Asked only when a build
   * would be refused for a passive caller. The walk stops at the first passive frame (in the IDE 10 to 45 frames
   * below the layer). The next questions of the same passive call on the same thread are answered by
   * {@link #PASSIVE_MEMO} without a walk. The answer "not passive" is not kept: such a caller builds the copy, and
   * then no longer asks.
   */
  static boolean isPassiveCaller(@Nullable PsiFile file) {
    ProgressIndicator root = rootIndicator(ProgressManager.getGlobalProgressIndicator());
    WeakReference<ProgressIndicator> memo = root == null ? null : PASSIVE_MEMO.get();
    if (memo != null && memo.get() == root) return true;
    STACK_WALKS.incrementAndGet();
    long w0 = ErlangMacroLayerStats.on() ? System.nanoTime() : 0;
    boolean passive = hasPassiveFrame();
    if (w0 != 0) {
      ErlangMacroLayerStats.phase(passive ? "stack-walk:passive" : "stack-walk:not-passive", file == null ? null : file.getName(),
                                  System.nanoTime() - w0);
    }
    if (passive && root != null) PASSIVE_MEMO.set(new WeakReference<>(root));
    return passive;
  }

  /** The indicator that the given one wraps ({@link WrappedProgressIndicator}), at any depth; itself if it wraps none. */
  static @Nullable ProgressIndicator rootIndicator(@Nullable ProgressIndicator indicator) {
    ProgressIndicator p = indicator;
    for (int depth = 0; p instanceof WrappedProgressIndicator w && depth < 16; depth++) p = w.getOriginalProgressIndicator();
    return p;
  }

  /** A frame of a passive query ({@link #PASSIVE_CALLERS}) is on this thread's stack; always walks. */
  static boolean hasPassiveFrame() {
    return StackWalker.getInstance().walk(frames -> frames.limit(400).anyMatch(f -> {
      String c = f.getClassName();
      for (String marker : PASSIVE_CALLERS) if (c.contains(marker)) return true;
      return false;
    }));
  }

  /**
   * Runs the computation as a passive query of the caller's progress indicator (a stand-in for the IDE's callers in
   * tests that do not run the IDE's own passive callers). The mark lasts only for the computation.
   */
  @TestOnly
  public static <T> T computeAsPassiveQueryForTests(@NotNull Supplier<T> computation) {
    ProgressIndicator root = rootIndicator(ProgressManager.getGlobalProgressIndicator());
    if (root == null) throw new IllegalStateException("a passive query runs under a progress indicator");
    WeakReference<ProgressIndicator> previous = PASSIVE_MEMO.get();
    PASSIVE_MEMO.set(new WeakReference<>(root));
    try {
      return computation.get();
    }
    finally {
      PASSIVE_MEMO.set(previous);
    }
  }

  /**
   * Runs the computation as a passive query of the caller's progress indicator (the completion
   * auto-popup, which runs after every key and whose answer is only shown): while the file is typed in, it builds
   * nothing and waits for the copy (see {@link ErlangMacroLayer#awaitWhileTyped}). The mark lasts only for the computation.
   */
  static <T> T asPassiveQuery(@NotNull Supplier<T> computation) {
    ProgressIndicator root = rootIndicator(ProgressManager.getGlobalProgressIndicator());
    if (root == null) return computation.get();
    WeakReference<ProgressIndicator> previous = PASSIVE_MEMO.get();
    PASSIVE_MEMO.set(new WeakReference<>(root));
    try {
      return computation.get();
    }
    finally {
      PASSIVE_MEMO.set(previous);
    }
  }

  /**
   * The caller cannot be cancelled: it runs in a non-cancellable section, or with no progress indicator
   * (a plain read action on a pooled thread, or a coroutine read action such as the update of the rename action). A
   * key press is a write action: it waits until every read action ends, and a caller that cannot be cancelled would
   * hold it for a whole build (or a wait for one). In a non-cancellable section the caller gets the copy that is
   * there, or master's answer. A caller with no indicator builds and waits under an indicator of its own that
   * a write action cancels ({@link ErlangMacroLayer#underOwnIndicator}). Null when the caller is cancellable (or builds are allowed anywhere).
   */
  static @Nullable String nonCancellableCaller() {
    if (ErlangMacroLayer.isEdtBuildAllowed()) return null; // diagnostic: builds allowed anywhere
    ProgressManager pm = ProgressManager.getInstance();
    if (pm.isInNonCancelableSection()) return "non-cancellable";
    if (ProgressManager.getGlobalProgressIndicator() == null) return NO_INDICATOR;
    return null;
  }

  static final String NO_INDICATOR = "no-indicator";

  static boolean isHighlighting(@Nullable ProgressIndicator indicator) {
    ProgressIndicator p = indicator;
    for (int depth = 0; p != null && depth < 16; depth++) {
      if (p instanceof DaemonProgressIndicator) return true;
      p = p instanceof WrappedProgressIndicator w ? w.getOriginalProgressIndicator() : null;
    }
    return false;
  }
}
