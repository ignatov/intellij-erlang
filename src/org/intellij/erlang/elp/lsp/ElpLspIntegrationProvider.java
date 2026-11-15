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

import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.lsp.api.LspClient;
import com.intellij.platform.lsp.api.LspClientManager;
import com.intellij.platform.lsp.api.LspIntegrationProvider;
import com.intellij.platform.lsp.api.lsWidget.LspClientWidgetItem;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpFiles;
import org.intellij.erlang.elp.ElpSettings;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.settings.ErlangExternalToolsConfigurable;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Starts ELP for {@code .erl} and {@code .hrl} files once a path to {@code elp} is configured. Registered in
 * {@code erlang-elp-lsp.xml}, which is loaded only when the platform has the LSP module.
 */
public final class ElpLspIntegrationProvider implements LspIntegrationProvider {
  @Override
  public void fileOpened(@NotNull Project project, @NotNull VirtualFile file, @NotNull LspClientStarter clientStarter) {
    if (!ElpFiles.isElpFile(file)) return;
    ElpConfiguration configuration = ElpSettings.getInstance().getEffectiveConfiguration();
    if (!configuration.isEnabled()) return;
    VirtualFile root = ElpFiles.findWorkspaceRoot(project, file);
    if (root == null) return;
    clientStarter.ensureClientStarted(new ElpLspClientDescriptor(project, root, configuration));
  }

  @Override
  public @Nullable LspClientWidgetItem createWidgetItem(@NotNull LspClient lspClient, @Nullable VirtualFile currentFile) {
    return new LspClientWidgetItem(lspClient, currentFile, ErlangIcons.FILE, ErlangExternalToolsConfigurable.class);
  }

  /**
   * Restarts ELP with the new path and mode when the settings change: stops the running servers and starts the ones that the
   * Erlang files open in the editor need.
   */
  public static final class SettingsListener implements ElpSettings.Listener {
    @Override
    public void elpSettingsChanged() {
      for (Project project : ProjectManager.getInstance().getOpenProjects()) {
        if (project.isDisposed() || project.isDefault()) continue;
        LspClientManager.getInstance(project).stopAndRestartClientsIfNeeded(ElpLspIntegrationProvider.class);
      }
    }
  }
}
