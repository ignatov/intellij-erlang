/*
 * Copyright 2012-2026 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


package org.intellij.erlang.elp;

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.codeInsight.daemon.impl.InlayHintsPassFactoryInternal;
import com.intellij.codeInsight.folding.CodeFoldingManager;
import com.intellij.codeInsight.hints.declarative.impl.DeclarativeInlayHintsPassFactory;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.TextEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.EnumSet;
import java.util.Set;

/**
 * The engine switch: decides, per file and feature, whether ELP owns a feature so that the plugin's counterpart stays silent.
 * Every plugin feature that ELP replaces asks {@link #ownsFeature} at its entry point; search for the callers to find them all.
 * <p>
 * ELP owns every feature of an Erlang module or header ({@link ElpFiles#isElpFile}) under a root that ELP serves
 * ({@link ElpServedRoots}), in the {@link ElpMode#ELP} position with a path to {@code elp} or the downloaded server
 * ({@link ElpSettings#isElpPositionWithServer}), in a trusted project of an IDE with the LSP module ({@link ElpServerStatus}).
 * There, and in every other file that the server of a served root gets ({@link #isServed}), such as the OTP sources, the plugin
 * does not answer in parallel, whatever the server is doing: while it loads, after a crash or while it restarts the user gets
 * nothing from the plugin's engine. Everywhere else, for example in the {@link ElpMode#PLUGIN} position, under a root that the
 * preflight refused or under another content root, the plugin answers exactly as without ELP.
 * References and resolve are never switched off, because the lexer, the parser, the formatter and the run configurations of the
 * plugin keep working on the PSI.
 */
public final class ElpEngine {
  private ElpEngine() {
  }

  /** The plugin features that ELP replaces in the {@link ElpMode#ELP} position. */
  public enum Feature {
    /** Basic and smart completion: ELP's {@code textDocument/completion}, with the documentation of its items. */
    COMPLETION,
    /** Parameter name inlay hints: ELP's {@code textDocument/inlayHint}. */
    PARAMETER_HINTS,
    /** Quick documentation: ELP's {@code textDocument/hover}. */
    DOCUMENTATION,
    /** Folding of functions: ELP's {@code textDocument/foldingRange}. ELP folds no comments, so the plugin keeps those. */
    FOLDING,
    /** The plugin's intentions: ELP's code actions replace them. */
    INTENTIONS,
    /** Extend selection: ELP's {@code textDocument/selectionRange}. */
    SELECTION,
    /** Go to declaration: ELP's {@code textDocument/definition}, never the plugin's references. */
    NAVIGATION,
    /** All the plugin's inspections and their quick fixes, on the fly and in batch: ELP's diagnostics replace them. */
    DIAGNOSTICS,
    /** Find usages and Show Usages: ELP's {@code textDocument/references} supplies the usages of the plugin's target. */
    FIND_USAGES,
    /** Rename: ELP's {@code textDocument/rename}, through the plugin's own rename handler in the LSP part. */
    RENAME,
    /** Parameter info: ELP's {@code textDocument/signatureHelp}. */
    SIGNATURE_HELP,
    /** Highlight usages in the file: ELP's {@code textDocument/documentHighlight}. */
    DOCUMENT_HIGHLIGHTS,
    /** Call hierarchy: the plugin's is computed from its resolve; the LSP client does not deliver ELP's for an Erlang element. */
    CALL_HIERARCHY,
    /** Go to Symbol and Go to Class: ELP's {@code workspace/symbol}, see {@link #servesWholeProject}. */
    WORKSPACE_SYMBOLS
  }

  private static final ThreadLocal<Set<Feature>> PLUGIN_FALLBACK = ThreadLocal.withInitial(() -> EnumSet.noneOf(Feature.class));

  /**
   * @param context an element of the file the feature works in; for completion and documentation pass an element of the file in the
   *                editor, not of a copy or of the target
   */
  public static boolean ownsFeature(@Nullable PsiElement context, @NotNull Feature feature) {
    if (context == null || !context.isValid()) return false;
    PsiFile file = context.getContainingFile();
    if (file == null) return false;
    return ownsFeature(file.getProject(), file.getOriginalFile().getVirtualFile(), feature);
  }

  public static boolean ownsFeature(@NotNull Project project, @Nullable VirtualFile file, @NotNull Feature feature) {
    if (file == null || !ElpFiles.isElpFile(file) || PLUGIN_FALLBACK.get().contains(feature)) return false;
    return isServed(project, file);
  }

  /**
   * Whether {@code file} is an Erlang module or header that the ELP server of a served root of {@code project} gets, by the same
   * predicate as the server's supported files ({@link ElpFiles#serverGets}): a file under the served root, by the path the project
   * knows or by its real path, and a file outside the project's content, such as an OTP source. Does not look at the server.
   * Cheap: in the {@link ElpMode#PLUGIN} position it looks at an empty list only.
   */
  public static boolean isServed(@NotNull Project project, @NotNull VirtualFile file) {
    ElpServedRoots roots = ElpServedRoots.getInstance();
    if (roots.isEmpty() || !ElpFiles.isElpFile(file) || !isElpProject(project)) return false;
    return ReadAction.compute(() -> {
      if (project.isDisposed()) return false;
      for (VirtualFile root : ElpFiles.workspaceRoots(project)) {
        String path = root.getPath();
        if (roots.contains(path) && ElpFiles.serverGets(project, root, roots.getRealPath(path), file)) return true;
      }
      return false;
    });
  }

  /**
   * For Go to Symbol and Go to Class: whether the workspace root of every content root of {@code project} is a root that ELP serves,
   * see {@link ElpFiles#findWorkspaceRoot}, so that ELP's
   * {@code workspace/symbol} answers for the whole project and the plugin's contributors stay silent, also for the libraries and the
   * SDK. In a project with roots that ELP does not serve the plugin's contributors answer for the files outside the served roots.
   */
  public static boolean servesWholeProject(@NotNull Project project) {
    ElpServedRoots roots = ElpServedRoots.getInstance();
    if (roots.isEmpty() || !isElpProject(project)) return false;
    return ReadAction.compute(() -> {
      if (project.isDisposed()) return false;
      VirtualFile[] contentRoots = ProjectRootManager.getInstance(project).getContentRoots();
      if (contentRoots.length == 0) return false;
      for (VirtualFile contentRoot : contentRoots) {
        VirtualFile root = ElpFiles.findWorkspaceRoot(project, contentRoot);
        if (root == null || !roots.contains(root.getPath())) return false;
      }
      return true;
    });
  }

  private static boolean isElpProject(@NotNull Project project) {
    if (project.isDefault() || project.isDisposed()) return false;
    if (!ElpSettings.getInstance().isElpPositionWithServer() || !ElpPreflight.isTrusted(project)) return false;
    return project.getService(ElpServerStatus.class) != null;
  }

  /**
   * Runs {@code action} with {@code feature} answered by the plugin on this thread, whatever the position: for an ELP entry point
   * that needs the plugin's own implementation, for example the plugin's rename handlers looked up by {@code ElpRenameHandler}.
   */
  public static void runWithPlugin(@NotNull Feature feature, @NotNull Runnable action) {
    Set<Feature> fallback = PLUGIN_FALLBACK.get();
    boolean added = fallback.add(feature);
    try {
      action.run();
    }
    finally {
      if (added) fallback.remove(feature);
    }
  }

  /**
   * Makes the open editors of the project ask {@link #ownsFeature} again after the answer may have changed: the position was
   * switched, or an ELP server started or stopped. Restarts the highlighting, which recomputes the inspections, and the folding.
   * The inlay hint passes skip a document that has not changed since they last ran, so they are told to run again: the plugin's
   * parameter hints are declarative, ELP's come through the LSP client's older inlay provider.
   */
  public static void refreshProject(@NotNull Project project) {
    ApplicationManager.getApplication().invokeLater(() -> {
      InlayHintsPassFactoryInternal.Companion.forceHintsUpdateOnNextPass();
      DaemonCodeAnalyzer.getInstance(project).restart("ELP engine switch");
      CodeFoldingManager foldingManager = CodeFoldingManager.getInstance(project);
      for (FileEditor fileEditor : FileEditorManager.getInstance(project).getAllEditors()) {
        if (!(fileEditor instanceof TextEditor textEditor)) continue;
        VirtualFile file = fileEditor.getFile();
        if (file == null || !ElpFiles.isElpFile(file)) continue;
        Editor editor = textEditor.getEditor();
        DeclarativeInlayHintsPassFactory.Companion.scheduleRecompute(editor, project);
        foldingManager.scheduleAsyncFoldingUpdate(editor);
      }
    }, ModalityState.nonModal(), project.getDisposed());
  }
}
