package org.intellij.erlang.macrolayer;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationListener;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.editor.EditorFactory;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.FileEditorManagerListener;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.ModificationTracker;
import com.intellij.openapi.util.registry.Registry;
import com.intellij.openapi.util.registry.RegistryValue;
import com.intellij.openapi.util.registry.RegistryValueListener;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiTreeChangeAdapter;
import com.intellij.psi.PsiTreeChangeEvent;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Spike 4: the memory bound of the macro layer and the header directive trackers, per project.
 * <p>
 * At most {@link #STRONG} copies are held strongly: the most recently asked ones. A copy of a file whose editor is
 * closed is released at once. Every other copy is held only weakly, so a GC can collect it; it is then rebuilt on
 * the next question (build reason {@code collected}). Bound: {@code erlang.macro.layer.strong}, default 24.
 */
@Service(Service.Level.PROJECT)
public final class ErlangMacroLayerRetention implements Disposable {
  public static final int STRONG = Math.max(1, Integer.getInteger("erlang.macro.layer.strong", 24));
  /** Round 5: how often a header directive tracker was asked (for tests and measurements). */
  public static final AtomicLong DIRECTIVE_CHECKS = new AtomicLong();

  private final Project myProject;
  private final LinkedHashMap<ErlangMacroLayer.CopyHolder, Boolean> myLru = new LinkedHashMap<>(32, 0.75f, true);
  private final Map<VirtualFile, DirectiveTracker> myTrackers = new ConcurrentHashMap<>();

  public ErlangMacroLayerRetention(@NotNull Project project) {
    myProject = project;
    // the switch goes off: no copy is kept strongly while the layer gives no answer
    Registry.get(ErlangMacroLayer.REGISTRY_KEY).addListener(new RegistryValueListener() {
      @Override
      public void afterValueChanged(@NotNull RegistryValue value) {
        if (!ErlangMacroLayer.isEnabled()) releaseAll();
      }
    }, this);
    project.getMessageBus().connect(this).subscribe(FileEditorManagerListener.FILE_EDITOR_MANAGER, new FileEditorManagerListener() {
      @Override
      public void fileClosed(@NotNull FileEditorManager source, @NotNull VirtualFile file) {
        release(file);
      }
    });
    // round 5: when a document last changed (a key press; the PSI changes later, when the document is committed)
    EditorFactory.getInstance().getEventMulticaster().addDocumentListener(new DocumentListener() {
      @Override
      public void documentChanged(@NotNull DocumentEvent event) {
        // a replacement by the same text (as a test fixture's setText) is no key press
        if (!StringUtil.equals(event.getOldFragment(), event.getNewFragment())) {
          ErlangMacroLayer.documentChanged(event.getDocument());
          VirtualFile file = FileDocumentManager.getInstance().getFile(event.getDocument());
          noteChanged(file);
          if (file != null && isBuildConfig(file.getName())) myBuildConfigVersion.incrementAndGet();
        }
      }
    }, this);
    // round 6 (next): a write action waits to start (a key press): a passive query that waits for the copy gives up
    ApplicationManager.getApplication().addApplicationListener(new ApplicationListener() {
      @Override
      public void beforeWriteActionStart(@NotNull Object action) {
        myWritePendingSince = System.nanoTime();
        myWritePending = true;
      }

      @Override
      public void writeActionStarted(@NotNull Object action) {
        myWritePending = false;
      }

      @Override
      public void afterWriteActionFinished(@NotNull Object action) {
        myWritePending = false;
      }
    }, this);
    // round 5: the own-text version of the files that have a table or a copy (see ErlangMacroLayer.TextTracker)
    PsiManager.getInstance(project).addPsiTreeChangeListener(new PsiTreeChangeAdapter() {
      @Override
      public void childAdded(@NotNull PsiTreeChangeEvent event) {
        changed(event);
      }

      @Override
      public void childRemoved(@NotNull PsiTreeChangeEvent event) {
        changed(event);
      }

      @Override
      public void childReplaced(@NotNull PsiTreeChangeEvent event) {
        changed(event);
      }

      @Override
      public void childMoved(@NotNull PsiTreeChangeEvent event) {
        changed(event);
      }

      @Override
      public void childrenChanged(@NotNull PsiTreeChangeEvent event) {
        changed(event);
      }

      @Override
      public void propertyChanged(@NotNull PsiTreeChangeEvent event) {
        changed(event);
      }

      private void changed(@NotNull PsiTreeChangeEvent event) {
        PsiFile file = event.getFile();
        if (file != null && isBuildConfig(file.getName())) myBuildConfigVersion.incrementAndGet();
        ErlangMacroLayer.TextTracker t = file == null ? null : ErlangMacroLayer.textTrackerIfAny(file);
        if (t != null) {
          t.changed();
          noteChanged(file.getOriginalFile().getVirtualFile());
        }
      }
    }, this);
  }

  private volatile boolean myWritePending;
  private volatile long myWritePendingSince;

  /**
   * Round 6 (next): a write action announced its start and has not started yet (it waits for the read actions to end).
   * A flag older than 5 s is taken as stale (a write action that never started).
   */
  boolean isWriteActionPending() {
    return myWritePending && System.nanoTime() - myWritePendingSince < 5_000_000_000L;
  }

  /** Round 6: changes of the rebar.config files of the project (their include paths take part in include resolution). */
  private final AtomicLong myBuildConfigVersion = new AtomicLong();

  private static boolean isBuildConfig(@NotNull String name) {
    return "rebar.config".equals(name);
  }

  /** Round 6: moves with every change of a rebar.config (a document change or a PSI change). */
  long buildConfigVersion() {
    return myBuildConfigVersion.get();
  }

  /** Round 6: System.nanoTime() of the last change of each .erl file (a document change or a PSI change). */
  private final Map<VirtualFile, Long> myChangedAt = new ConcurrentHashMap<>();

  private void noteChanged(@Nullable VirtualFile file) {
    if (file == null || !file.getName().endsWith(".erl")) return;
    long now = System.nanoTime();
    myChangedAt.put(file, now);
    if (myChangedAt.size() > 64) myChangedAt.values().removeIf(t -> now - t > 60_000_000_000L);
  }

  /** Round 6: the .erl files that changed less than {@code millis} ms ago (see {@link ErlangMacroLayer#prepareSearch}). */
  @NotNull List<VirtualFile> filesChangedWithin(long millis) {
    if (millis <= 0 || myChangedAt.isEmpty()) return List.of();
    long now = System.nanoTime();
    List<VirtualFile> r = new ArrayList<>();
    for (Map.Entry<VirtualFile, Long> e : myChangedAt.entrySet()) if (now - e.getValue() < millis * 1_000_000L) r.add(e.getKey());
    return r;
  }

  public static @NotNull ErlangMacroLayerRetention getInstance(@NotNull Project project) {
    return project.getService(ErlangMacroLayerRetention.class);
  }

  synchronized void touch(@NotNull ErlangMacroLayer.CopyHolder holder, @NotNull ErlangMacroLayer.Copy copy) {
    holder.strong = copy;
    if (!myLru.containsKey(holder) && holder.file != null) {
      // a new holder for a file: the holders of its older versions are stale; let their copies go
      for (Iterator<ErlangMacroLayer.CopyHolder> old = myLru.keySet().iterator(); old.hasNext(); ) {
        ErlangMacroLayer.CopyHolder h = old.next();
        if (holder.file.equals(h.file)) {
          h.strong = null;
          old.remove();
        }
      }
    }
    myLru.put(holder, Boolean.TRUE);
    Iterator<ErlangMacroLayer.CopyHolder> it = myLru.keySet().iterator();
    while (myLru.size() > STRONG && it.hasNext()) {
      ErlangMacroLayer.CopyHolder eldest = it.next();
      eldest.strong = null;
      it.remove();
    }
  }

  synchronized void release(@NotNull VirtualFile file) {
    Iterator<ErlangMacroLayer.CopyHolder> it = myLru.keySet().iterator();
    while (it.hasNext()) {
      ErlangMacroLayer.CopyHolder h = it.next();
      if (file.equals(h.file)) {
        h.strong = null;
        it.remove();
      }
    }
  }

  @TestOnly
  public synchronized void releaseAll() {
    for (ErlangMacroLayer.CopyHolder h : myLru.keySet()) h.strong = null;
    myLru.clear();
  }

  public synchronized int strongCount() {
    return myLru.size();
  }

  @NotNull ModificationTracker tracker(@NotNull VirtualFile header) {
    return myTrackers.computeIfAbsent(header, h -> new DirectiveTracker(h));
  }

  @Override
  public void dispose() {
    releaseAll();
    myTrackers.clear();
    myChangedAt.clear();
  }

  /**
   * Changes only when the directive lines of the header change. Asking it re-lexes the header if its text changed
   * (once per header text, shared by all includers): so an includer notices a directive change when it is next asked.
   */
  /** Stamps that never repeat, for a directive tracker that could not check its header now. */
  private static final AtomicLong UNCHECKED = new AtomicLong();

  private final class DirectiveTracker implements ModificationTracker {
    private final VirtualFile myHeader;
    private final AtomicLong myVersion = new AtomicLong();
    private long myHash;
    private boolean myKnown;

    DirectiveTracker(@NotNull VirtualFile header) {
      myHeader = header;
    }

    @Override
    public long getModificationCount() {
      DIRECTIVE_CHECKS.incrementAndGet();
      PsiFile f = myHeader.isValid() && !myProject.isDisposed() ? PsiManager.getInstance(myProject).findFile(myHeader) : null;
      if (f == null) return myVersion.get();
      // on the UI thread no header is read and lexed: a summary not computed for the current text gives an unchecked
      // stamp (the table then counts as not valid there, and the UI thread builds nothing)
      if (ApplicationManager.getApplication().isDispatchThread() && !ErlangMacroLayer.isEdtBuildAllowed() && !ErlangMacroLayer.hasUpToDateSummary(f)) {
        return -UNCHECKED.incrementAndGet();
      }
      long hash = ErlangMacroLayer.summaryOf(f).hash;
      synchronized (this) {
        if (!myKnown) {
          myKnown = true;
          myHash = hash;
        }
        else if (hash != myHash) {
          myHash = hash;
          myVersion.incrementAndGet();
        }
      }
      return myVersion.get();
    }
  }
}
