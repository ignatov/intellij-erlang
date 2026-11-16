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
import com.intellij.platform.lsp.api.LspServer;
import com.intellij.platform.lsp.api.LspServerManager;
import com.intellij.platform.lsp.api.LspServerSupportProvider;
import com.intellij.platform.lsp.api.lsWidget.LspServerWidgetItem;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpEngine;
import org.intellij.erlang.elp.ElpFiles;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.elp.ElpSettings;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.settings.ErlangExternalToolsConfigurable;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Starts ELP for {@code .erl} and {@code .hrl} files in the {@code ELP} position once a path to {@code elp} is set. Registered in
 * {@code erlang-elp-lsp.xml}, which is loaded only when the platform has the LSP module.
 */
public final class ElpLspIntegrationProvider implements LspServerSupportProvider {
  @Override
  public void fileOpened(@NotNull Project project, @NotNull VirtualFile file, @NotNull LspServerStarter serverStarter) {
    if (!ElpFiles.isElpFile(file)) return;
    ElpConfiguration configuration = ElpSettings.getInstance().getEffectiveConfiguration();
    // In the Plugin position ELP is not started at all; the plugin works as without the integration.
    if (!configuration.isEnabled() || configuration.mode() != ElpMode.ELP) return;
    VirtualFile root = ElpFiles.findWorkspaceRoot(project, file);
    if (root == null) return;
    serverStarter.ensureServerStarted(new ElpLspClientDescriptor(project, root, configuration));
  }

  @Override
  public @Nullable LspServerWidgetItem createLspServerWidgetItem(@NotNull LspServer lspServer, @Nullable VirtualFile currentFile) {
    return new LspServerWidgetItem(lspServer, currentFile, ErlangIcons.FILE, ErlangExternalToolsConfigurable.class);
  }

  /**
   * Restarts ELP with the new path and mode when the settings change: stops the running servers and starts the ones that the
   * Erlang files open in the editor need. The open editors are refreshed at once, because the plugin's features come back as
   * soon as the position is no longer {@code ELP}; they are refreshed again when the new server runs, see
   * {@link ElpLspClientDescriptor#getLspServerListener()}.
   */
  public static final class SettingsListener implements ElpSettings.Listener {
    @Override
    public void elpSettingsChanged() {
      for (Project project : ProjectManager.getInstance().getOpenProjects()) {
        if (project.isDisposed() || project.isDefault()) continue;
        LspServerManager.getInstance(project).stopAndRestartIfNeeded(ElpLspIntegrationProvider.class);
        ElpEngine.refreshProject(project);
      }
    }
  }
}
