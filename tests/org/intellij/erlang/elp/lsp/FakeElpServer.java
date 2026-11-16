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

import com.google.gson.reflect.TypeToken;
import org.eclipse.lsp4j.*;
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.eclipse.lsp4j.jsonrpc.messages.ResponseError;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.lsp4j.services.TextDocumentService;
import org.eclipse.lsp4j.services.WorkspaceService;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A language server that answers the requests the real ELP was asked when the answers were recorded, with those answers. A request
 * that was not recorded (another file, another position) is answered with {@code null} and listed in {@link #unanswered()}, so
 * that a test can tell whether our code asked what the recorder asked. Every request is listed in {@link #requests()}.
 */
final class FakeElpServer implements LanguageServer {
  /** A request that reached the server. */
  record Request(@NotNull String method, @NotNull String uri, @Nullable Position position, @Nullable Object params) {
  }

  private final ElpRecordedAnswers myAnswers;
  private final List<Request> myRequests = Collections.synchronizedList(new ArrayList<>());
  private final List<Request> myUnanswered = Collections.synchronizedList(new ArrayList<>());
  private final TextDocumentService myTextDocuments = new TextDocuments();
  /** How many more requests of a method are answered with the error "content modified". */
  private final Map<String, Integer> myContentModified = new ConcurrentHashMap<>();

  FakeElpServer(@NotNull ElpRecordedAnswers answers) {
    myAnswers = answers;
  }

  @NotNull
  List<Request> requests() {
    synchronized (myRequests) {
      return new ArrayList<>(myRequests);
    }
  }

  @NotNull
  List<Request> requests(@NotNull String method) {
    return requests().stream().filter(request -> request.method().equals(method)).toList();
  }

  @NotNull
  List<Request> unanswered() {
    synchronized (myUnanswered) {
      return new ArrayList<>(myUnanswered);
    }
  }

  /**
   * Answers the next {@code times} requests of {@code method} as ELP answers a request that meets a change of the files: with the
   * LSP error {@code ContentModified}.
   */
  void answerContentModified(@NotNull String method, int times) {
    myContentModified.put(method, times);
  }

  void clearRequests() {
    myRequests.clear();
    myUnanswered.clear();
  }

  @NotNull
  private <T> CompletableFuture<T> answer(@NotNull String method, @NotNull TextDocumentIdentifier document, @Nullable Position position,
                                          @Nullable Object params, @NotNull Type type) {
    Request request = new Request(method, document.getUri(), position, params);
    myRequests.add(request);
    if (myContentModified.getOrDefault(method, 0) > 0) {
      myContentModified.merge(method, -1, Integer::sum);
      return CompletableFuture.failedFuture(new ResponseErrorException(new ResponseError(-32801, "content modified", null)));
    }
    for (ElpRecordedAnswers.Case recorded : myAnswers.cases(method)) {
      if (!recorded.uri().equals(document.getUri())) continue;
      if (position != null && !position.equals(recorded.position())) continue;
      if (recorded.error() != null && !recorded.error().isJsonNull()) {
        return CompletableFuture.failedFuture(new IllegalStateException(recorded.error().toString()));
      }
      return CompletableFuture.completedFuture(recorded.result(type));
    }
    myUnanswered.add(request);
    return CompletableFuture.completedFuture(null);
  }

  private final class TextDocuments implements TextDocumentService {
    @Override
    public CompletableFuture<Either<List<? extends Location>, List<? extends LocationLink>>> definition(DefinitionParams params) {
      CompletableFuture<List<Location>> locations = answer("textDocument/definition", params.getTextDocument(), params.getPosition(),
                                                           params, new TypeToken<List<Location>>() {}.getType());
      return locations.thenApply(result -> result == null ? null : Either.forLeft(result));
    }

    @Override
    public CompletableFuture<List<? extends Location>> references(ReferenceParams params) {
      return answer("textDocument/references", params.getTextDocument(), params.getPosition(), params,
                    new TypeToken<List<Location>>() {}.getType());
    }

    @Override
    public CompletableFuture<WorkspaceEdit> rename(RenameParams params) {
      return answer("textDocument/rename", params.getTextDocument(), params.getPosition(), params, WorkspaceEdit.class);
    }

    @Override
    public CompletableFuture<Hover> hover(HoverParams params) {
      return answer("textDocument/hover", params.getTextDocument(), params.getPosition(), params, Hover.class);
    }

    @Override
    public CompletableFuture<SignatureHelp> signatureHelp(SignatureHelpParams params) {
      return answer("textDocument/signatureHelp", params.getTextDocument(), params.getPosition(), params, SignatureHelp.class);
    }

    @Override
    public CompletableFuture<List<? extends DocumentHighlight>> documentHighlight(DocumentHighlightParams params) {
      return answer("textDocument/documentHighlight", params.getTextDocument(), params.getPosition(), params,
                    new TypeToken<List<DocumentHighlight>>() {}.getType());
    }

    @Override
    public CompletableFuture<List<SelectionRange>> selectionRange(SelectionRangeParams params) {
      Position position = params.getPositions().isEmpty() ? null : params.getPositions().getFirst();
      return answer("textDocument/selectionRange", params.getTextDocument(), Objects.requireNonNull(position), params,
                    new TypeToken<List<SelectionRange>>() {}.getType());
    }

    @Override
    public CompletableFuture<List<Either<Command, CodeAction>>> codeAction(CodeActionParams params) {
      return answer("textDocument/codeAction", params.getTextDocument(), params.getRange().getStart(), params,
                    new TypeToken<List<Either<Command, CodeAction>>>() {}.getType());
    }

    @Override
    public CompletableFuture<List<InlayHint>> inlayHint(InlayHintParams params) {
      return answer("textDocument/inlayHint", params.getTextDocument(), null, params, new TypeToken<List<InlayHint>>() {}.getType());
    }

    @Override
    public CompletableFuture<List<FoldingRange>> foldingRange(FoldingRangeRequestParams params) {
      return answer("textDocument/foldingRange", params.getTextDocument(), null, params,
                    new TypeToken<List<FoldingRange>>() {}.getType());
    }

    @Override
    public void didOpen(DidOpenTextDocumentParams params) {
    }

    @Override
    public void didChange(DidChangeTextDocumentParams params) {
    }

    @Override
    public void didClose(DidCloseTextDocumentParams params) {
    }

    @Override
    public void didSave(DidSaveTextDocumentParams params) {
    }
  }

  @Override
  public CompletableFuture<InitializeResult> initialize(InitializeParams params) {
    return CompletableFuture.completedFuture(myAnswers.initializeResult());
  }

  @Override
  public CompletableFuture<Object> shutdown() {
    return CompletableFuture.completedFuture(null);
  }

  @Override
  public void exit() {
  }

  @Override
  public TextDocumentService getTextDocumentService() {
    return myTextDocuments;
  }

  @Override
  public WorkspaceService getWorkspaceService() {
    return new WorkspaceService() {
      @Override
      public void didChangeConfiguration(DidChangeConfigurationParams params) {
      }

      @Override
      public void didChangeWatchedFiles(DidChangeWatchedFilesParams params) {
      }
    };
  }
}
