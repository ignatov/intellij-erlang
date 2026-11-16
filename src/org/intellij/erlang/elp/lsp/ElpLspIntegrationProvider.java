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

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.lsp.api.LspServer;
import com.intellij.platform.lsp.api.LspServerManager;
import com.intellij.platform.lsp.api.LspServerSupportProvider;
import com.intellij.platform.lsp.api.lsWidget.LspServerWidgetItem;
import org.intellij.erlang.elp.ElpEngine;
import org.intellij.erlang.elp.ElpFiles;
import org.intellij.erlang.elp.ElpPreflight;
import org.intellij.erlang.elp.ElpServedRoots;
import org.intellij.erlang.elp.ElpSettings;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.settings.ErlangExternalToolsConfigurable;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

/**
 * Starts ELP for {@code .erl} and {@code .hrl} files in the {@code ELP} position once a path to {@code elp} is set and
 * {@link ElpPreflight} says that ELP can load the project. Registered in
 * {@code erlang-elp-lsp.xml}, which is loaded only when the platform has the LSP module.
 */
public final class ElpLspIntegrationProvider implements LspServerSupportProvider {
  private static final Logger LOG = Logger.getInstance(ElpLspIntegrationProvider.class);

  private static volatile boolean ourIgnoringOpenedFilesInTests;

  /**
   * Makes the platform's calls of {@link #fileOpened} do nothing until {@code parent} is disposed: for the tests that stand a fake
   * client in for ELP, where the real preflight would refuse the test project and take its root out of {@link ElpServedRoots}.
   */
  @TestOnly
  public static void ignoreOpenedFilesInTests(@NotNull Disposable parent) {
    ourIgnoringOpenedFilesInTests = true;
    Disposer.register(parent, () -> ourIgnoringOpenedFilesInTests = false);
  }

  @Override
  public void fileOpened(@NotNull Project project, @NotNull VirtualFile file, @NotNull LspServerStarter serverStarter) {
    if (ourIgnoringOpenedFilesInTests || !ElpFiles.isElpFile(file)) return;
    // In the Plugin position ELP is not started at all; the plugin works as without the integration. ELP may be offered there.
    if (!ElpSettings.getInstance().isElpPositionWithServer()) {
      ElpOffer.fileOpened(project, file);
      return;
    }
    VirtualFile root = ElpFiles.findWorkspaceRoot(project, file);
    if (root == null) return;
    // The position is application-wide; whether ELP can load this project is checked here, before every start.
    ElpPreflight preflight = ElpPreflight.getInstance(project);
    ElpPreflight.Facts facts = null;
    ElpPreflight.Result result;
    // ELP runs rebar3 and the project's build: nothing starts in a project that the IDE does not trust.
    if (!ElpPreflight.isTrusted(project)) {
      result = ElpPreflight.notTrusted();
    }
    else {
      facts = preflight.cachedFacts();
      // The binary depends on the project (the downloaded build for its OTP release) and is resolved off the EDT with the facts.
      if (facts == null || !facts.trusted() || !facts.settingsKey().equals(ElpPreflight.settingsKey())) {
        preflight.gatherFactsInBackground(() -> startServersIfNeeded(project));
        return;
      }
      result = ElpPreflight.evaluate(facts, ElpPreflight.Layout.of(root));
    }
    if (facts == null || !result.canStart()) {
      LOG.info("ELP is not started for " + root.getPath() + ": " + result.blocking());
      // A root that ELP served and the preflight now refuses goes back to the plugin.
      if (ElpServedRoots.getInstance().remove(root.getPath())) ElpEngine.refreshProject(project);
      if (preflight.shouldTell(result)) {
        ApplicationManager.getApplication().invokeLater(() -> ElpNotifications.notStarted(project, result), project.getDisposed());
      }
      return;
    }
    serverStarter.ensureServerStarted(new ElpLspClientDescriptor(project, root, facts.configuration()));
  }

  /** Asks the platform to call {@link #fileOpened} again for the Erlang files open in the project. */
  static void startServersIfNeeded(@NotNull Project project) {
    ApplicationManager.getApplication().invokeLater(
      () -> LspServerManager.getInstance(project).startServersIfNeeded(ElpLspIntegrationProvider.class),
      ModalityState.nonModal(), project.getDisposed());
  }

  @Override
  public @Nullable LspServerWidgetItem createLspServerWidgetItem(@NotNull LspServer lspServer, @Nullable VirtualFile currentFile) {
    return new ElpWidgetItem(lspServer, currentFile);
  }

  /**
   * The ELP item of the platform's language services widget, which shows the client's state: it says, too, whether ELP has loaded
   * the project, because the client is Running long before ELP answers. A failed load shows as an error.
   */
  static final class ElpWidgetItem extends LspServerWidgetItem {
    ElpWidgetItem(@NotNull LspServer lspServer, @Nullable VirtualFile currentFile) {
      super(lspServer, currentFile, ErlangIcons.FILE, ErlangExternalToolsConfigurable.class);
    }

    @NotNull
    @Override
    protected String getServerLabel() {
      ElpLoadState.State state = loadState();
      String label = super.getServerLabel();
      if (state == ElpLoadState.State.STARTING || state == ElpLoadState.State.LOADING) return label + " (loading the project)";
      if (state == ElpLoadState.State.FAILED) return label + " (could not load the project)";
      return label;
    }

    @Override
    public boolean isError() {
      return super.isError() || loadState() == ElpLoadState.State.FAILED;
    }

    @Nullable
    private ElpLoadState.State loadState() {
      return getLspServer().getDescriptor() instanceof ElpLspClientDescriptor descriptor ? descriptor.getLoadState().state() : null;
    }
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
        ElpPreflight.getInstance(project).invalidate();
        LspServerManager.getInstance(project).stopAndRestartIfNeeded(ElpLspIntegrationProvider.class);
        ElpEngine.refreshProject(project);
      }
    }
  }
}
