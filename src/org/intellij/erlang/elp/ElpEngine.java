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
import com.intellij.codeInsight.hints.HintUtilsKt;
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
    /** Basic and smart completion: ELP's {@code textDocument/completion}. */
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
    /** The inspections built on resolve (unresolved and unused names): ELP's diagnostics report the same problems. */
    RESOLVE_INSPECTIONS
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
    if (file == null || project.isDefault() || project.isDisposed() || !ElpFiles.isElpFile(file)) return false;
    ElpConfiguration configuration = ElpSettings.getInstance().getEffectiveConfiguration();
    if (!configuration.isEnabled() || configuration.mode() != ElpMode.ELP) return false;
    ElpServerStatus status = project.getService(ElpServerStatus.class);
    return status != null && status.isServing(file, feature);
  }

  /**
   * Makes the open editors of the project ask {@link #ownsFeature} again after the answer may have changed: the position was
   * switched, or an ELP server started or stopped. Restarts the highlighting, which recomputes the inspections, and the folding.
   * The inlay hint passes skip a document that has not changed since they last ran, so they are told to run again: the plugin's
   * parameter hints are declarative, ELP's come through the LSP client's older inlay provider.
   */
  public static void refreshProject(@NotNull Project project) {
    ApplicationManager.getApplication().invokeLater(() -> {
      HintUtilsKt.refreshParameterHintsOnNextPass();
      InlayHintsPassFactoryInternal.Companion.forceHintsUpdateOnNextPass();
      DaemonCodeAnalyzer.getInstance(project).restart("ELP engine switch");
      CodeFoldingManager foldingManager = CodeFoldingManager.getInstance(project);
      for (FileEditor fileEditor : FileEditorManager.getInstance(project).getAllEditors()) {
        if (!(fileEditor instanceof TextEditor textEditor)) continue;
        VirtualFile file = fileEditor.getFile();
        if (file == null || !ElpFiles.isElpFile(file)) continue;
        Editor editor = textEditor.getEditor();
        foldingManager.scheduleAsyncFoldingUpdate(editor);
      }
    }, ModalityState.nonModal(), project.getDisposed());
  }
}
