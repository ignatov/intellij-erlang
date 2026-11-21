package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.ide.IdeView;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.actionSystem.ex.ActionUtil;
import com.intellij.openapi.actionSystem.impl.PresentationFactory;
import com.intellij.openapi.actionSystem.impl.SimpleDataContext;
import com.intellij.openapi.actionSystem.impl.Utils;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.pom.Navigatable;
import com.intellij.psi.*;
import com.intellij.testFramework.PlatformTestUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.concurrent.Future;

/**
 * Updates actions as the IDE does when a popup or a menu opens: the real expansion of an action group
 * ({@link Utils#expandActionGroupAsync}, which updates every child, in the background for the actions that ask for it,
 * with the data rules applied to the context), and a direct update of every action of the plugin and of every action of
 * the popups, with selections a user can make in the Project view or an editor.
 */
public final class ActionUpdateProbe {
  public static final String[] GROUPS = {"ProjectViewPopupMenu", "EditorPopupMenu", "EditorTabPopupMenu", "NavbarPopupMenu",
    "StructureViewPopupMenu", "MainMenu", "GoToMenu", "RefactoringMenu", "CodeMenu", "EditMenu", "FindMenuGroup", "NewGroup",
    "AnalyzeMenu", "RunContextGroup", "GenerateGroup", "ConsoleEditorPopupMenu", "UsageView.Popup", "CommanderPopupMenu",
    "FavoritesViewPopupMenu", "ScopeViewPopupMenu"};

  private final FuzzRecorder myRec;
  private final Project myProject;
  private final Module myModule;
  private List<AnAction> myAllActions;

  public ActionUpdateProbe(@NotNull FuzzRecorder rec, @NotNull Project project, @NotNull Module module) {
    myRec = rec;
    myProject = project;
    myModule = module;
  }

  /** A selection: what the Project view (or another view) puts in the data context. */
  public @NotNull DataContext selection(@NotNull List<? extends PsiElement> elements, @Nullable Editor editor) {
    SimpleDataContext.Builder b = SimpleDataContext.builder().add(CommonDataKeys.PROJECT, myProject).add(PlatformCoreDataKeys.MODULE, myModule);
    if (editor != null) {
      b.add(CommonDataKeys.EDITOR, editor);
      b.add(CommonDataKeys.CARET, editor.getCaretModel().getCurrentCaret());
    }
    if (!elements.isEmpty()) {
      PsiElement first = elements.get(0);
      b.add(CommonDataKeys.PSI_ELEMENT, first);
      b.add(PlatformCoreDataKeys.PSI_ELEMENT_ARRAY, elements.toArray(PsiElement.EMPTY_ARRAY));
      List<VirtualFile> vfs = new ArrayList<>();
      List<Navigatable> navs = new ArrayList<>();
      List<PsiDirectory> dirs = new ArrayList<>();
      for (PsiElement e : elements) {
        VirtualFile vf = e instanceof PsiFileSystemItem fs ? fs.getVirtualFile() : null;
        if (vf != null) vfs.add(vf);
        if (e instanceof Navigatable n) navs.add(n);
        if (e instanceof PsiDirectory d) dirs.add(d);
      }
      if (!vfs.isEmpty()) {
        b.add(CommonDataKeys.VIRTUAL_FILE, vfs.get(0));
        b.add(CommonDataKeys.VIRTUAL_FILE_ARRAY, vfs.toArray(VirtualFile.EMPTY_ARRAY));
      }
      if (!navs.isEmpty()) {
        b.add(CommonDataKeys.NAVIGATABLE, navs.get(0));
        b.add(CommonDataKeys.NAVIGATABLE_ARRAY, navs.toArray(new Navigatable[0]));
      }
      if (first instanceof PsiFile f) b.add(CommonDataKeys.PSI_FILE, f);
      PsiDirectory[] dirArray = dirs.isEmpty() && first.getContainingFile() != null && first.getContainingFile().getContainingDirectory() != null
                                ? new PsiDirectory[]{first.getContainingFile().getContainingDirectory()} : dirs.toArray(PsiDirectory.EMPTY_ARRAY);
      b.add(LangDataKeys.IDE_VIEW, new IdeView() {
        @Override
        public PsiDirectory @NotNull [] getDirectories() {
          return dirArray;
        }

        @Override
        public @Nullable PsiDirectory getOrChooseDirectory() {
          return dirArray.length > 0 ? dirArray[0] : null;
        }
      });
      b.add(PlatformCoreDataKeys.SELECTED_ITEMS, elements.toArray());
    }
    return b.build();
  }

  /** Expands each group as its popup or menu does, with the selection's context. */
  public void expandGroups(@NotNull String label, @NotNull DataContext raw, @NotNull String place) {
    ActionManager am = ActionManager.getInstance();
    for (String id : GROUPS) {
      AnAction a = am.getAction(id);
      if (!(a instanceof ActionGroup group)) continue;
      myRec.guard(label + " expand " + id, () -> {
        DataContext ctx;
        try {
          ctx = Utils.createAsyncDataContext(raw);
        }
        catch (Throwable t) {
          ctx = raw;
        }
        var promise = Utils.expandActionGroupAsync(group, new PresentationFactory(), ctx, place, ActionUiKind.POPUP, false);
        PlatformTestUtil.waitForPromise(promise, FuzzConfig.actionMillis());
      });
    }
  }

  /** Every action of the plugin, and every action reachable from {@link #GROUPS}. */
  public @NotNull List<AnAction> allActions() {
    if (myAllActions != null) return myAllActions;
    ActionManager am = ActionManager.getInstance();
    Set<AnAction> seen = Collections.newSetFromMap(new IdentityHashMap<>());
    List<AnAction> out = new ArrayList<>();
    for (String id : am.getActionIdList("")) {
      AnAction a = am.getAction(id);
      if (a != null && a.getClass().getName().startsWith("org.intellij.erlang.") && seen.add(a)) out.add(a);
    }
    Deque<AnAction> queue = new ArrayDeque<>();
    for (String id : GROUPS) {
      AnAction a = am.getAction(id);
      if (a != null) queue.add(a);
    }
    while (!queue.isEmpty() && out.size() < 3000) {
      AnAction a = queue.poll();
      if (!seen.add(a)) continue;
      out.add(a);
      if (a instanceof DefaultActionGroup g) {
        for (AnAction c : g.getChildActionsOrStubs()) {
          if (c instanceof ActionStubBase stub) c = am.getAction(stub.getId());
          if (c != null) queue.add(c);
        }
      }
    }
    myAllActions = out;
    return out;
  }

  /** Updates each action as {@link ActionUtil} does, on the thread the action asks for. */
  public void updateAll(@NotNull String label, @NotNull DataContext raw, @NotNull String place, @NotNull Random rnd, int max) {
    List<AnAction> actions = new ArrayList<>(allActions());
    if (max > 0 && actions.size() > max) {
      // the plugin's actions always, a random sample of the rest
      List<AnAction> plugin = new ArrayList<>();
      List<AnAction> rest = new ArrayList<>();
      for (AnAction a : actions) (a.getClass().getName().startsWith("org.intellij.erlang.") ? plugin : rest).add(a);
      Collections.shuffle(rest, rnd);
      actions = new ArrayList<>(plugin);
      actions.addAll(rest.subList(0, Math.max(0, Math.min(rest.size(), max - plugin.size()))));
    }
    DataContext ctx;
    try {
      ctx = Utils.createAsyncDataContext(raw);
    }
    catch (Throwable t) {
      ctx = raw;
    }
    DataContext fctx = ctx;
    ActionManager am = ActionManager.getInstance();
    for (AnAction a : actions) {
      String id = am.getId(a);
      String name = id != null ? id : a.getClass().getSimpleName();
      myRec.guard(label + " update " + name, () -> {
        AnActionEvent e = AnActionEvent.createEvent(a, fctx, a.getTemplatePresentation().clone(), place, ActionUiKind.POPUP, null);
        if (a.getActionUpdateThread() == ActionUpdateThread.BGT) {
          Future<?> f = ApplicationManager.getApplication().executeOnPooledThread(
            () -> ReadAction.run(() -> ActionUtil.performDumbAwareUpdate(a, e, false)));
          PlatformTestUtil.waitForFuture(f, FuzzConfig.actionMillis());
        }
        else {
          ActionUtil.performDumbAwareUpdate(a, e, false);
        }
      });
    }
  }
}
