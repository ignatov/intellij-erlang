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
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.lsp.api.LspServer;
import com.intellij.platform.lsp.api.LspServerManager;
import com.intellij.platform.lsp.api.LspServerState;
import org.eclipse.lsp4j.InitializeResult;
import org.eclipse.lsp4j.ServerCapabilities;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.intellij.erlang.elp.ElpEngine;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.elp.ElpServerStatus;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.util.Collection;
import java.util.function.Function;

/**
 * Answers {@link ElpServerStatus} from the platform LSP client manager. Registered as a project service in
 * {@code erlang-elp-lsp.xml}.
 */
public final class ElpLspServerStatus implements ElpServerStatus {
  private static volatile Function<Project, Collection<LspServer>> ourClientsForTests;

  private final Project myProject;

  public ElpLspServerStatus(@NotNull Project project) {
    myProject = project;
  }

  @Override
  public boolean isServing(@NotNull VirtualFile file, @NotNull ElpEngine.Feature feature) {
    return findClient(myProject, file, feature) != null;
  }

  @Override
  public boolean isServingProject(@NotNull ElpEngine.Feature feature) {
    return findClient(myProject, null, feature) != null;
  }

  @Nullable
  @Override
  public String whyNotServing(@NotNull VirtualFile file, @NotNull ElpEngine.Feature feature) {
    return findClient(myProject, file, feature) != null ? null : whyNoClient(myProject, file);
  }

  /**
   * @return why no ELP client answers for {@code file} (any file when it is {@code null}), as a message to the user, when
   * {@link #findClient} finds none
   */
  @NotNull
  static String whyNoClient(@NotNull Project project, @Nullable VirtualFile file) {
    String why = ElpServerStatus.NOT_RUNNING;
    if (project.isDisposed()) return why;
    for (LspServer client : clients(project)) {
      if (!(client.getDescriptor() instanceof ElpLspClientDescriptor descriptor)) continue;
      if (descriptor.getConfiguration().mode() != ElpMode.ELP || file != null && !descriptor.isSupportedFile(file)) continue;
      LspServerState state = client.getState();
      if (state == LspServerState.Initializing) return ElpServerStatus.LOADING;
      if (state != LspServerState.Running) continue;
      switch (descriptor.getLoadState().state()) {
        case STARTING, LOADING -> {
          return ElpServerStatus.LOADING;
        }
        case FAILED -> why = ElpServerStatus.COULD_NOT_LOAD;
        case RUNNING -> why = ElpServerStatus.NOT_OFFERED;
        case STOPPED -> {
        }
      }
    }
    return why;
  }

  /**
   * @return a running ELP client of the {@link ElpMode#ELP} position that gets {@code file} (any file when it is {@code null}) and
   * announced the capability that {@code feature} needs. A client still initializing, or stopped, does not count, nor does a running
   * client whose ELP has not loaded the project yet, see {@link ElpLoadState}; the plugin's entry points then do not ask ELP at all. The platform sends a client every file its
   * descriptor supports: the files under its root and those outside the project's content, such as the OTP sources, see
   * {@link ElpLspClientDescriptor#isSupportedFile}.
   */
  @Nullable
  static LspServer findClient(@NotNull Project project, @Nullable VirtualFile file, @NotNull ElpEngine.Feature feature) {
    if (project.isDisposed()) return null;
    for (LspServer client : clients(project)) {
      if (client.getState() != LspServerState.Running) continue;
      if (!(client.getDescriptor() instanceof ElpLspClientDescriptor descriptor)) continue;
      if (descriptor.getConfiguration().mode() != ElpMode.ELP || file != null && !descriptor.isSupportedFile(file)) continue;
      // The client runs long before ELP has loaded the project and answers anything but "elp is still loading".
      if (!descriptor.getLoadState().isRunning()) continue;
      InitializeResult result = client.getInitializeResult();
      if (result != null && supports(result.getCapabilities(), feature)) return client;
    }
    return null;
  }

  @NotNull
  static Collection<LspServer> clients(@NotNull Project project) {
    Function<Project, Collection<LspServer>> clients = ourClientsForTests;
    return clients != null ? clients.apply(project) : LspServerManager.getInstance(project).getServersForProvider(ElpLspIntegrationProvider.class);
  }

  /**
   * Makes {@link #findClient} look at {@code clients} instead of the platform's LSP client manager until {@code parentDisposable} is
   * disposed, so that a test can stand a client with a fake server in for a running ELP; the platform's manager cannot be replaced.
   */
  @TestOnly
  static void setClientsForTests(@NotNull Function<Project, Collection<LspServer>> clients, @NotNull Disposable parentDisposable) {
    ourClientsForTests = clients;
    Disposer.register(parentDisposable, () -> ourClientsForTests = null);
  }

  static boolean supports(@Nullable ServerCapabilities capabilities, @NotNull ElpEngine.Feature feature) {
    if (capabilities == null) return false;
    return switch (feature) {
      case COMPLETION -> capabilities.getCompletionProvider() != null;
      case PARAMETER_HINTS -> isOn(capabilities.getInlayHintProvider());
      case DOCUMENTATION -> isOn(capabilities.getHoverProvider());
      case FOLDING -> isOn(capabilities.getFoldingRangeProvider());
      case INTENTIONS -> isOn(capabilities.getCodeActionProvider());
      case SELECTION -> isOn(capabilities.getSelectionRangeProvider());
      case NAVIGATION -> isOn(capabilities.getDefinitionProvider());
      case FIND_USAGES -> isOn(capabilities.getReferencesProvider());
      case RENAME -> isOn(capabilities.getRenameProvider());
      case SIGNATURE_HELP -> capabilities.getSignatureHelpProvider() != null;
      case DOCUMENT_HIGHLIGHTS -> isOn(capabilities.getDocumentHighlightProvider());
      case WORKSPACE_SYMBOLS -> isOn(capabilities.getWorkspaceSymbolProvider());
      case CALL_HIERARCHY -> isOn(capabilities.getCallHierarchyProvider());
      // Diagnostics are pushed by the server; there is no capability to announce.
      case DIAGNOSTICS -> true;
    };
  }

  private static boolean isOn(@Nullable Either<Boolean, ?> provider) {
    return provider != null && (provider.isRight() || Boolean.TRUE.equals(provider.getLeft()));
  }
}
