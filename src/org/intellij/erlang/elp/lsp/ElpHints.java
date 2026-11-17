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

package org.intellij.erlang.elp.lsp;

import com.intellij.codeInsight.hint.HintManager;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Tells the user that ELP gave no answer under a root that it serves, where the plugin does not answer in its place: an error hint
 * in the editor, or a balloon when no editor is selected.
 */
final class ElpHints {
  private static volatile List<String> ourShownInTests;

  private ElpHints() {
  }

  /** May be called from any thread; the hint shows on the EDT. */
  static void showError(@NotNull Project project, @Nullable Editor editor, @NotNull String text) {
    List<String> shown = ourShownInTests;
    if (shown != null) {
      shown.add(text);
      return;
    }
    ApplicationManager.getApplication().invokeLater(() -> {
      Editor target = editor != null && !editor.isDisposed() ? editor : FileEditorManager.getInstance(project).getSelectedTextEditor();
      if (target != null) HintManager.getInstance().showErrorHint(target, text);
      else ElpNotifications.notify(project, new Notification(ElpNotifications.GROUP, text, NotificationType.ERROR));
    }, ModalityState.nonModal(), project.getDisposed());
  }

  /** Collects the texts of the hints instead of showing them until {@code parent} is disposed. */
  @TestOnly
  @NotNull
  static List<String> recordInTests(@NotNull Disposable parent) {
    List<String> shown = new CopyOnWriteArrayList<>();
    ourShownInTests = shown;
    Disposer.register(parent, () -> ourShownInTests = null);
    return shown;
  }
}
