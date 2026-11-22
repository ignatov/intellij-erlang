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
import org.intellij.erlang.macrolayer.ErlangMacroLayer.Build;
import org.intellij.erlang.macrolayer.ErlangMacroLayer.Copy;
import org.intellij.erlang.macrolayer.ErlangMacroLayer.CopyHolder;
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
 * The search inside a {@code -define} body: a name there (a call in a header macro's body, a variable bound by the
 * body) means what the macro's uses make of it, which the main tree cannot tell (the body alone may not parse, and a
 * header's body names things of its includers). The answers come from the expanded copies of the files that use the
 * macro: the file itself, or for a header, the .erl files that include it and use the macro, at most
 * {@link #MAX_BODY_USERS} (the retention's limit), those whose copy is there first.
 * <p>
 * Owns: the includer list per header and macro, kept on the project until the next PSI change. Threading: under a
 * read action, cancellable, never on the UI thread (the includer search queries the word index); it builds copies
 * through {@link ErlangMacroLayer#getCopy}.
 */
final class ErlangMacroLayerBodySearch {
  private ErlangMacroLayerBodySearch() {
  }



  /**
   * Includers searched for the uses of a header macro, at most: as many copies as the retention keeps strongly, so that
   * one search does not build copies that the next one builds again.
   */
  static final int MAX_BODY_USERS = ErlangMacroLayerRetention.STRONG;
  /** Project-wide word searches for the includers of a header macro (tests). */
  public static final AtomicInteger INCLUDER_SEARCHES = new AtomicInteger();
  static final Key<ConcurrentHashMap<String, Object[]>> INCLUDERS_KEY = Key.create("erlang.macro.layer.includers");

  /**
   * Resolves a name inside a {@code -define} body through the expanded copies of the macro's uses: in the
   * file itself, and for a header, in the .erl files that use the macro and include the header (at most
   * {@link #MAX_BODY_USERS}). The body alone may not parse (a fragment), and a header body names things of its
   * includers, so master cannot resolve it there. Every use contributes its answer, mapped back to real elements;
   * different uses may resolve differently, and then all answers are returned. Never builds a copy on the UI thread.
   */
  public static @NotNull List<PsiElement> resolveInMacroBody(@NotNull PsiElement leaf) {
    if (!ErlangMacroLayer.isEnabled() || leaf.getTextLength() == 0) return List.of();
    // the search for the includers queries the word index: never on the UI thread
    if (ApplicationManager.getApplication().isDispatchThread() && !ErlangMacroLayer.isEdtBuildAllowed()) return List.of();
    PsiFile file = leaf.getContainingFile();
    if (!(file instanceof ErlangFile ef) || file.getUserData(ErlangMacroLayer.COPY_OF) != null || DumbService.isDumb(file.getProject())) return List.of();
    ErlangMacrosDefinition def = PsiTreeUtil.getParentOfType(leaf, ErlangMacrosDefinition.class);
    if (def == null || def.getName() == null || !PsiTreeUtil.isAncestor(def.getMacrosBody(), leaf, false)) return List.of();
    int start = leaf.getTextRange().getStartOffset();
    VirtualFile vf = file.getOriginalFile().getVirtualFile();
    ErlangMacroExpander.HeaderTok origin = null;
    List<ErlangFile> users = new ArrayList<>();
    if (ErlangMacroLayer.layerFile(ef) != null) users.add(ef);
    else if (vf != null) {
      origin = headerOrigin(ef, vf, start, leaf.getText());
      if (origin == null) return List.of();
      users.addAll(includersUsing(ef.getProject(), vf, def.getName()));
    }
    // the includers whose copy is there first: their answers cost nothing
    users.sort(Comparator.comparingInt(u -> ErlangMacroLayer.cachedCopy(u) != null ? 0 : 1));
    LinkedHashSet<PsiElement> result = new LinkedHashSet<>();
    for (ErlangFile user : users) {
      ProgressManager.checkCanceled();
      Copy copy = ErlangMacroLayer.getCopy(user, "macro-body");
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
          PsiElement real = ErlangMacroLayer.exit(copy.toReal(t), user);
          if (real != null) result.add(real);
        }
      }
    }
    return new ArrayList<>(result);
  }

  /** The origin of a header token in the header's summary, or null when it is not a token of a directive. */
  static @Nullable ErlangMacroExpander.HeaderTok headerOrigin(@NotNull ErlangFile header, @NotNull VirtualFile vf, int start,
                                                                       @NotNull String text) {
    List<ErlangMacroExpander.Directive> directives = ErlangMacroLayer.summaryOf(header).directives;
    for (int d = 0; d < directives.size(); d++) {
      List<ErlangMacroExpander.Tok> toks = directives.get(d).tokens;
      if (toks.isEmpty() || toks.getFirst().start() > start || toks.getLast().end() <= start) continue;
      for (int i = 0; i < toks.size(); i++) {
        if (toks.get(i).start() == start && toks.get(i).text().equals(text)) return new ErlangMacroExpander.HeaderTok(vf, d, i);
      }
    }
    return null;
  }

  /** The .erl files of the project that use the macro of the header (for rename conflicts); never on the UI thread. */
  public static @NotNull List<ErlangFile> includersOfHeader(@Nullable PsiFile header, @NotNull String macro) {
    VirtualFile vf = header == null ? null : header.getOriginalFile().getVirtualFile();
    if (vf == null || !ErlangMacroLayer.isEnabled() || DumbService.isDumb(header.getProject())) return List.of();
    if (ApplicationManager.getApplication().isDispatchThread() && !ErlangMacroLayer.isEdtBuildAllowed()) return List.of();
    return includersUsing(header.getProject(), vf, macro);
  }

  /**
   * The .erl files of the project that contain the macro's name and include the header (directly or not), at most
   * {@link #MAX_BODY_USERS}; kept until the next PSI change (a search asks it for every occurrence of a word).
   */
  static @NotNull List<ErlangFile> includersUsing(@NotNull Project project, @NotNull VirtualFile header, @NotNull String macro) {
    long stamp = PsiModificationTracker.getInstance(project).getModificationCount();
    ConcurrentHashMap<String, Object[]> cache = project.getUserData(INCLUDERS_KEY);
    if (cache == null) cache = ((UserDataHolderEx)project).putUserDataIfAbsent(INCLUDERS_KEY, new ConcurrentHashMap<>());
    String key = header.getUrl() + "|" + macro;
    Object[] kept = cache.get(key);
    if (kept != null && (Long)kept[0] == stamp) {
      List<ErlangFile> r = new ArrayList<>();
      PsiManager manager = PsiManager.getInstance(project);
      for (Object o : (List<?>)kept[1]) {
        if (o instanceof VirtualFile vf && vf.isValid() && manager.findFile(vf) instanceof ErlangFile ef) r.add(ef);
      }
      return r;
    }
    List<ErlangFile> r = includersUsing0(project, header, macro);
    List<VirtualFile> files = new ArrayList<>();
    for (ErlangFile f : r) if (f.getVirtualFile() != null) files.add(f.getVirtualFile());
    cache.put(key, new Object[]{stamp, files});
    return r;
  }

  static @NotNull List<ErlangFile> includersUsing0(@NotNull Project project, @NotNull VirtualFile header, @NotNull String macro) {
    INCLUDER_SEARCHES.incrementAndGet();
    List<ErlangFile> r = new ArrayList<>();
    PsiSearchHelper.getInstance(project).processAllFilesWithWord(macro, GlobalSearchScope.projectScope(project), f -> {
      ProgressManager.checkCanceled();
      ErlangFile ef = f instanceof ErlangFile x ? ErlangMacroLayer.layerFile(x) : null;
      Table t = ef == null ? null : ErlangMacroLayer.table(ef);
      if (t != null && t.refusal == null && t.headers.contains(header)) r.add(ef);
      return r.size() < MAX_BODY_USERS;
    }, true);
    return r;
  }

  /**
   * The reference lies in a {@code -define} body, master resolves it to nothing, and one of the macro's uses
   * resolves it to the element: for find usages and rename of that element.
   */
  public static boolean isReferenceThroughMacroUses(@NotNull PsiReference ref, @NotNull PsiElement element) {
    PsiElement owner = ref.getElement();
    if (!ErlangMacroLayer.isEnabled() || PsiTreeUtil.getParentOfType(owner, ErlangMacrosBody.class) == null) return false;
    PsiFile file = owner.getContainingFile();
    PsiElement leaf = file == null ? null : file.findElementAt(owner.getTextRange().getStartOffset() + ref.getRangeInElement().getStartOffset());
    if (leaf == null) return false;
    PsiManager manager = owner.getManager();
    for (PsiElement target : resolveInMacroBody(leaf)) {
      if (manager.areElementsEquivalent(target, element)) return true;
    }
    return false;
  }
}
