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
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.lsp.api.LspClient;
import com.intellij.platform.lsp.api.LspClientManager;
import com.intellij.platform.lsp.api.LspServerState;
import org.eclipse.lsp4j.InitializeResult;
import org.eclipse.lsp4j.ServerCapabilities;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.intellij.erlang.elp.ElpEngine;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.elp.ElpServerStatus;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Answers {@link ElpServerStatus} from the platform LSP client manager. Registered as a project service in
 * {@code erlang-elp-lsp.xml}.
 */
public final class ElpLspServerStatus implements ElpServerStatus {
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

  /**
   * @return a running ELP client of the {@link ElpMode#ELP} position that gets {@code file} (any file when it is {@code null}) and
   * announced the capability that {@code feature} needs. A client still initializing, or stopped, does not count. The platform sends a client every file its
   * descriptor supports, also files outside its root such as the OTP sources, so the root is not checked.
   */
  @Nullable
  static LspClient findClient(@NotNull Project project, @Nullable VirtualFile file, @NotNull ElpEngine.Feature feature) {
    if (project.isDisposed()) return null;
    for (LspClient client : LspClientManager.getInstance(project).getClients(ElpLspIntegrationProvider.class)) {
      if (client.getState() != LspServerState.Running) continue;
      if (!(client.getDescriptor() instanceof ElpLspClientDescriptor descriptor)) continue;
      if (descriptor.getConfiguration().mode() != ElpMode.ELP || file != null && !descriptor.isSupportedFile(file)) continue;
      InitializeResult result = client.getInitializeResult();
      if (result != null && supports(result.getCapabilities(), feature)) return client;
    }
    return null;
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
      case CALL_HIERARCHY -> isOn(capabilities.getCallHierarchyProvider());
      case DOCUMENT_HIGHLIGHTS -> isOn(capabilities.getDocumentHighlightProvider());
      case WORKSPACE_SYMBOLS -> isOn(capabilities.getWorkspaceSymbolProvider());
      // Diagnostics are pushed by the server; there is no capability to announce.
      case RESOLVE_INSPECTIONS -> true;
    };
  }

  private static boolean isOn(@Nullable Either<Boolean, ?> provider) {
    return provider != null && (provider.isRight() || Boolean.TRUE.equals(provider.getLeft()));
  }
}
