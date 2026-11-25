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

package org.intellij.erlang.update;

import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.FileEditorManagerListener;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.vfs.VirtualFile;
import org.intellij.erlang.ErlangFileType;
import org.jetbrains.annotations.NotNull;

/**
 * The notification about a new version of the plugin waits until an Erlang module, header or escript is opened.
 */
public final class ErlangUpdateNoticeListener implements FileEditorManagerListener {
  @Override
  public void fileOpened(@NotNull FileEditorManager source, @NotNull VirtualFile file) {
    FileType type = file.getFileType();
    if (type == ErlangFileType.MODULE || type == ErlangFileType.HEADER) {
      ErlangUpdateNoticeService.getInstance().erlangFileOpened(source.getProject());
    }
  }
}
