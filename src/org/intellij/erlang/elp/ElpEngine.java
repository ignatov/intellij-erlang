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
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.TextEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.EnumSet;
import java.util.Set;

/**
 * The engine switch: decides, per file and feature, whether ELP owns a feature so that the plugin's counterpart stays silent.
 * Every plugin feature that ELP can replace asks {@link #ownsFeature} at its entry point; search for the callers to find them all.
 * <p>
 * ELP owns a feature only when all of these hold: the {@link ElpMode#ELP} position is chosen and a path to {@code elp} is set;
 * the file is an Erlang module or header ({@link ElpFiles#isElpFile}); and an ELP server started for that position is running
 * in the project and announced the capability ({@link ElpServerStatus}). Otherwise, for example while the server starts, after it
 * crashed, or in an IDE without the LSP module, the plugin answers as it does in the {@link ElpMode#PLUGIN} position.
 * References and resolve are never switched off, so inspections, the annotator and the refactorings keep working on them.
 */
public final class ElpEngine {
  private ElpEngine() {
  }

  /** The plugin features that ELP replaces in the {@link ElpMode#ELP} position. */
  public enum Feature {
    /** Basic and smart completion: ELP's {@code textDocument/completion}. Stays with the plugin, see {@link #elpMayOwn}. */
    COMPLETION,
    /** Parameter name inlay hints: ELP's {@code textDocument/inlayHint}. */
    PARAMETER_HINTS,
    /** Quick documentation: ELP's {@code textDocument/hover}. */
    DOCUMENTATION,
    /** Folding of functions: ELP's {@code textDocument/foldingRange}. ELP folds no comments, so the plugin keeps those. */
    FOLDING,
    /** The intentions that ELP's code actions duplicate: export a function, add a spec, add documentation. */
    INTENTIONS,
    /** Extend selection: ELP's {@code textDocument/selectionRange}. */
    SELECTION,
    /** Go to declaration: ELP's {@code textDocument/definition}, asked before the plugin's references. */
    NAVIGATION,
    /**
     * The plugin inspections whose problems ELP's diagnostics report too, see {@link ElpDiagnosticCounterparts}. Batch inspection
     * still runs them, because ELP diagnoses only the files open in the editor.
     */
    DIAGNOSTICS,
    /** Find usages and Show Usages: ELP's {@code textDocument/references} supplies the usages of the plugin's target. */
    FIND_USAGES,
    /** Rename: ELP's {@code textDocument/rename}, through the plugin's own rename handler in the LSP part. */
    RENAME,
    /** Parameter info: ELP's {@code textDocument/signatureHelp}. */
    SIGNATURE_HELP,
    /** Highlight usages in the file: ELP's {@code textDocument/documentHighlight}. */
    DOCUMENT_HIGHLIGHTS,
    /** Go to Symbol and Go to Class: ELP's {@code workspace/symbol}. A project feature, see {@link #ownsProjectFeature}. */
    WORKSPACE_SYMBOLS
  }

  private static final ThreadLocal<Set<Feature>> PLUGIN_FALLBACK = ThreadLocal.withInitial(() -> EnumSet.noneOf(Feature.class));

  /**
   * Whether ELP may own {@code feature} in the {@link ElpMode#ELP} position. Completion stays with the plugin: on the plugin's
   * completion tests ELP agreed in 6 of 36, and with an empty prefix it offers only keywords. The LSP client asks ELP for
   * completion only when this allows it, so that the list is filled by exactly one of them.
   */
  public static boolean elpMayOwn(@NotNull Feature feature) {
    return feature != Feature.COMPLETION;
  }

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
    if (file == null || !ElpFiles.isElpFile(file)) return false;
    ElpServerStatus status = statusIfElpPosition(project, feature);
    return status != null && status.isServing(file, feature);
  }

  /**
   * For the features that work on the whole project rather than on a file, such as {@link Feature#WORKSPACE_SYMBOLS}: whether a
   * running ELP server of the project announced the capability.
   */
  public static boolean ownsProjectFeature(@NotNull Project project, @NotNull Feature feature) {
    ElpServerStatus status = statusIfElpPosition(project, feature);
    return status != null && status.isServingProject(feature);
  }

  /**
   * Runs {@code action} with {@code feature} answered by the plugin on this thread, whatever the position: for an ELP entry point
   * that falls back to the plugin, for example when ELP refuses to rename the element at the caret.
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

  @Nullable
  private static ElpServerStatus statusIfElpPosition(@NotNull Project project, @NotNull Feature feature) {
    if (!elpMayOwn(feature) || project.isDefault() || project.isDisposed() || PLUGIN_FALLBACK.get().contains(feature)) return null;
    ElpConfiguration configuration = ElpSettings.getInstance().getEffectiveConfiguration();
    if (!configuration.isEnabled() || configuration.mode() != ElpMode.ELP) return null;
    return project.getService(ElpServerStatus.class);
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
