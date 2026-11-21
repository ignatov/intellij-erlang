package org.intellij.erlang.macrolayer;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.FileEditorManagerListener;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.ModificationTracker;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiTreeChangeAdapter;
import com.intellij.psi.PsiTreeChangeEvent;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.TestOnly;

import java.util.Iterator;
import java.util.LinkedHashMap;
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

  private final Project myProject;
  private final LinkedHashMap<ErlangMacroLayer.CopyHolder, Boolean> myLru = new LinkedHashMap<>(32, 0.75f, true);
  private final Map<VirtualFile, DirectiveTracker> myTrackers = new ConcurrentHashMap<>();

  public ErlangMacroLayerRetention(@NotNull Project project) {
    myProject = project;
    project.getMessageBus().connect(this).subscribe(FileEditorManagerListener.FILE_EDITOR_MANAGER, new FileEditorManagerListener() {
      @Override
      public void fileClosed(@NotNull FileEditorManager source, @NotNull VirtualFile file) {
        release(file);
      }
    });
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

      private static void changed(@NotNull PsiTreeChangeEvent event) {
        PsiFile file = event.getFile();
        ErlangMacroLayer.TextTracker t = file == null ? null : ErlangMacroLayer.textTrackerIfAny(file);
        if (t != null) t.changed();
      }
    }, this);
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
  }

  /**
   * Changes only when the directive lines of the header change. Asking it re-lexes the header if its text changed
   * (once per header text, shared by all includers): so an includer notices a directive change when it is next asked.
   */
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
      PsiFile f = myHeader.isValid() && !myProject.isDisposed() ? PsiManager.getInstance(myProject).findFile(myHeader) : null;
      if (f == null) return myVersion.get();
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
