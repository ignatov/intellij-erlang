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

import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.lsp.api.*;
import kotlin.Unit;
import kotlin.coroutines.Continuation;
import kotlin.jvm.functions.Function1;
import org.eclipse.lsp4j.InitializeResult;
import org.eclipse.lsp4j.TextDocumentIdentifier;
import org.eclipse.lsp4j.services.LanguageServer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * A client of the platform LSP module that talks to a {@link FakeElpServer} in the same process, with a real
 * {@link ElpLspClientDescriptor}. {@link ElpLspServerStatus#setClientsForTests} puts it where our code finds a running ELP. The
 * state and the announced capabilities can be changed by a test.
 */
final class FakeElpClient implements LspServer {
  private final ElpLspClientDescriptor myDescriptor;
  private final FakeElpServer myServer;
  private volatile LspServerState myState = LspServerState.Running;
  private volatile InitializeResult myInitializeResult;

  FakeElpClient(@NotNull ElpLspClientDescriptor descriptor, @NotNull FakeElpServer server, @Nullable InitializeResult initializeResult) {
    myDescriptor = descriptor;
    myServer = server;
    myInitializeResult = initializeResult;
    // As ELP once it has loaded the project: elp/status Running.
    descriptor.getLoadState().setStateForTests(ElpLoadState.State.RUNNING);
  }

  void setState(@NotNull LspServerState state) {
    myState = state;
  }

  void setInitializeResult(@Nullable InitializeResult result) {
    myInitializeResult = result;
  }

  @NotNull
  @Override
  public Class<? extends LspServerSupportProvider> getProviderClass() {
    return ElpLspIntegrationProvider.class;
  }

  @NotNull
  @Override
  public Project getProject() {
    return myDescriptor.getProject();
  }

  @NotNull
  @Override
  public LspServerDescriptor getDescriptor() {
    return myDescriptor;
  }

  @NotNull
  @Override
  public LspServerState getState() {
    return myState;
  }

  @Nullable
  @Override
  public InitializeResult getInitializeResult() {
    return myState == LspServerState.Running ? myInitializeResult : null;
  }

  @Override
  public void sendNotification(@NotNull Function1<? super LanguageServer, Unit> notification) {
    notification.invoke(myServer);
  }

  // Declared without @Override: LspClient of 2026.3 has it, the LspServer of 2026.1 and the LspClient of 2026.2 do not.
  public void invalidateServerResults() {
  }

  @Nullable
  @Override
  public <Lsp4jResponse> Object sendRequest(@NotNull Function1<? super LanguageServer, ? extends CompletableFuture<Lsp4jResponse>> request,
                                            @NotNull Continuation<? super Lsp4jResponse> continuation) {
    return sendRequestSync(DEFAULT_REQUEST_TIMEOUT_MS, request);
  }

  /** As the platform's client: {@code null} when the server fails or does not answer in time. */
  @Nullable
  @Override
  public <Lsp4jResponse> Lsp4jResponse sendRequestSync(int timeoutMs,
                                                       @NotNull Function1<? super LanguageServer, ? extends CompletableFuture<Lsp4jResponse>> request) {
    if (myState != LspServerState.Running) return null;
    try {
      return request.invoke(myServer).get(timeoutMs, TimeUnit.MILLISECONDS);
    }
    catch (ExecutionException | TimeoutException e) {
      return null;
    }
    catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      return null;
    }
  }

  @NotNull
  @Override
  public TextDocumentIdentifier getDocumentIdentifier(@NotNull VirtualFile file) {
    return new TextDocumentIdentifier(myDescriptor.getFileUri(file));
  }

  @Override
  public int getDocumentVersion(@NotNull Document document) {
    return 0;
  }

  // Declared without @Override: LspClient of 2026.3 has it, the LspServer of 2026.1 and the LspClient of 2026.2 do not.
  public int nextDocumentVersion(@NotNull Document document) {
    return 0;
  }
}
