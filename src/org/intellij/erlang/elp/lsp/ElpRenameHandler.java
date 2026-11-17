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

import com.intellij.ide.TitledHandler;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.ReadonlyStatusHandler;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.lsp.api.LspServer;
import com.intellij.platform.lsp.util.Lsp4jUtilKt;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.refactoring.rename.RenameHandler;
import com.intellij.refactoring.util.CommonRefactoringUtil;
import org.eclipse.lsp4j.*;
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.eclipse.lsp4j.jsonrpc.messages.Either3;
import org.intellij.erlang.elp.ElpEngine;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import kotlin.Unit;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.regex.Pattern;

/**
 * Rename in the {@code ELP} position. The 263 LSP client renames only files without a language of their own, so this handler
 * asks ELP itself: {@code textDocument/prepareRename} when the server offers it, otherwise a trial {@code textDocument/rename}
 * to a scratch name, to learn whether ELP can rename the name at the caret. If it can, the handler asks for the new name, sends
 * {@code textDocument/rename} and applies the returned edit to all files as one global undoable command, without saving. The
 * documents are saved before ELP is asked, see {@link #saveForElp}.
 * A module is renamed by ELP too, with its file; ELP's other file operations are refused. ELP 1.1.0 leaves {@code -behaviour(...)},
 * {@code -import(...)} and the module list of the {@code .app.src} file alone (see {@code ElpRecordedRenameTest.testModule}).
 * The handler is available for every Erlang module and header under a root that ELP serves. If ELP cannot rename the name at the
 * caret, or is not running, the user is told so and nothing is renamed: the plugin's rename does not run there. While this
 * handler is available the plugin's in-place rename handlers are not ({@code ErlangRefactoringSupportProvider}).
 * Registered in {@code erlang-elp-lsp.xml}.
 */
public final class ElpRenameHandler implements RenameHandler, TitledHandler {
  private static final int TIMEOUT_MS = 10_000;
  /** The LSP error {@code ContentModified}. */
  private static final int CONTENT_MODIFIED = -32801;
  private static final int ATTEMPTS = 3;
  static final String CANNOT_RENAME = "ELP cannot rename the name at the caret";
  static final String NOT_RUNNING = "ELP is not running for this file yet: it is loading the project or has stopped.";
  /** How many times an answer computed from an older text of a file is asked for again, see {@link #plan}. */
  private static final int STALE_ATTEMPTS = 5;
  private static final long STALE_PAUSE_MS = 250;
  private static final Logger LOG = Logger.getInstance(ElpRenameHandler.class);
  private static final Pattern NAME = Pattern.compile("[A-Za-z_][A-Za-z0-9_@]*");

  @Override
  public String getActionTitle() {
    return "Rename with ELP";
  }

  @Override
  public boolean isAvailableOnDataContext(@NotNull DataContext dataContext) {
    Editor editor = CommonDataKeys.EDITOR.getData(dataContext);
    PsiFile file = CommonDataKeys.PSI_FILE.getData(dataContext);
    return editor != null && file != null && ElpEngine.ownsFeature(file, ElpEngine.Feature.RENAME);
  }

  @Override
  public boolean isRenaming(@NotNull DataContext dataContext) {
    return isAvailableOnDataContext(dataContext);
  }

  @Override
  public void invoke(@NotNull Project project, PsiElement @NotNull [] elements, @Nullable DataContext dataContext) {
    Editor editor = dataContext != null ? CommonDataKeys.EDITOR.getData(dataContext) : null;
    PsiFile file = dataContext != null ? CommonDataKeys.PSI_FILE.getData(dataContext) : null;
    if (editor != null && file != null) invoke(project, editor, file, dataContext);
  }

  @Override
  public void invoke(@NotNull Project project, @Nullable Editor editor, @Nullable PsiFile file, @Nullable DataContext dataContext) {
    if (editor == null || file == null || dataContext == null) return;
    VirtualFile virtualFile = file.getOriginalFile().getVirtualFile();
    if (virtualFile == null || ElpLspServerStatus.findClient(project, virtualFile, ElpEngine.Feature.RENAME) == null) {
      CommonRefactoringUtil.showErrorHint(project, editor, NOT_RUNNING, getActionTitle(), null);
      return;
    }
    Target target = target(project, editor, file);
    if (target == null) {
      CommonRefactoringUtil.showErrorHint(project, editor, CANNOT_RENAME, getActionTitle(), null);
      return;
    }
    String newName = Messages.showInputDialog(project, "Rename '" + target.oldName() + "' to:", getActionTitle(), null,
                                              target.oldName(), null);
    if (newName == null || newName.isBlank() || newName.trim().equals(target.oldName())) return;
    String problem = rename(project, target, newName.trim());
    if (problem != null) Messages.showErrorDialog(project, problem, getActionTitle());
  }

  /**
   * The rename without the dialog: what this handler does once the user typed {@code newName}.
   *
   * @return why nothing was renamed, or {@code null} when ELP's edit was applied
   */
  @Nullable
  public static String renameAtCaret(@NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file, @NotNull String newName) {
    Target target = target(project, editor, file);
    return target == null ? CANNOT_RENAME : rename(project, target, newName);
  }

  /**
   * @return what ELP would rename at the caret, or {@code null} when ELP is not serving the file or cannot rename the name there
   */
  @Nullable
  private static Target target(@NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file) {
    VirtualFile virtualFile = file.getOriginalFile().getVirtualFile();
    LspServer client = virtualFile != null ? ElpLspServerStatus.findClient(project, virtualFile, ElpEngine.Feature.RENAME) : null;
    if (client == null) return null;
    saveForElp(project, client);
    PsiDocumentManager.getInstance(project).commitAllDocuments();
    Document document = editor.getDocument();
    int offset = editor.getCaretModel().getOffset();
    TextDocumentIdentifier id = client.getDocumentIdentifier(virtualFile);
    Position position = Lsp4jUtilKt.getLsp4jPosition(document, offset);
    String oldName = ProgressManager.getInstance().runProcessWithProgressSynchronously(
      () -> renamableName(client, id, position, document, offset), "Asking ELP", true, project);
    return oldName != null ? new Target(client, id, position, oldName) : null;
  }

  /**
   * Saves the documents: ELP reads a file that is not open in an editor from the disk, where an earlier rename may not have reached.
   * ELP is told at once which of its files changed, before the next request: the platform tells it too, but later, and a request
   * sent before that would get an answer computed from the old files.
   */
  private static void saveForElp(@NotNull Project project, @NotNull LspServer client) {
    FileDocumentManager documents = FileDocumentManager.getInstance();
    List<FileEvent> changed = new ArrayList<>();
    for (Document document : documents.getUnsavedDocuments()) {
      VirtualFile file = documents.getFile(document);
      if (file == null || FileEditorManager.getInstance(project).isFileOpen(file) || !client.getDescriptor().isSupportedFile(file)) continue;
      changed.add(new FileEvent(client.getDescriptor().getFileUri(file), FileChangeType.Changed));
    }
    documents.saveAllDocuments();
    if (changed.isEmpty()) return;
    client.sendNotification(server -> {
      server.getWorkspaceService().didChangeWatchedFiles(new DidChangeWatchedFilesParams(changed));
      return Unit.INSTANCE;
    });
  }

  /**
   * Sends the rename and applies the answer. ELP reads the changes of the files asynchronously and may answer from an older text
   * of a file, with ranges that no longer hold the old name; such an answer is not applied (see {@link #plan}). ELP is told again
   * that the file changed and asked again, after a pause, up to {@value #STALE_ATTEMPTS} times.
   */
  @Nullable
  private static String rename(@NotNull Project project, @NotNull Target target, @NotNull String newName) {
    Function<String, VirtualFile> findFile = target.client().getDescriptor()::findFileByUri;
    for (int attempt = 1; ; attempt++) {
      AtomicReference<String> error = new AtomicReference<>();
      long pause = attempt == 1 ? 0 : STALE_PAUSE_MS * (attempt - 1);
      WorkspaceEdit edit = ProgressManager.getInstance().runProcessWithProgressSynchronously(() -> {
        if (pause > 0) pause(pause);
        return rename(target.client(), new RenameParams(target.id(), target.position(), newName), error);
      }, "Renaming with ELP", true, project);
      if (edit == null) return "ELP did not rename '" + target.oldName() + "': " + StringUtil.notNullize(error.get(), "no answer");
      Plan plan = plan(findFile, edit, target.oldName());
      if (plan.stale() == null) return plan.problem() != null ? plan.problem() : plan.apply(project);
      LOG.info("ELP answered the rename of '" + target.oldName() + "' for an older version of " + plan.stale().getPresentableUrl() +
               "; the answer was rejected (attempt " + attempt + " of " + STALE_ATTEMPTS + ")");
      if (attempt >= STALE_ATTEMPTS) return staleProblem(plan.stale());
      tellChanged(project, target.client(), plan.stale());
    }
  }

  @NotNull
  static String staleProblem(@NotNull VirtualFile file) {
    return "ELP answered for an older version of " + file.getName() + ". Nothing was renamed; try again.";
  }

  private static void pause(long millis) {
    long deadline = System.nanoTime() + millis * 1_000_000;
    ProgressIndicator indicator = ProgressManager.getInstance().getProgressIndicator();
    while (System.nanoTime() < deadline && (indicator == null || !indicator.isCanceled())) {
      try {
        Thread.sleep(Math.min(50, Math.max(1, (deadline - System.nanoTime()) / 1_000_000)));
      }
      catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        return;
      }
    }
  }

  /** Tells ELP that a file that is not open in an editor changed, so that it reads it from the disk again. */
  private static void tellChanged(@NotNull Project project, @NotNull LspServer client, @NotNull VirtualFile file) {
    if (FileEditorManager.getInstance(project).isFileOpen(file) || !client.getDescriptor().isSupportedFile(file)) return;
    List<FileEvent> changed = List.of(new FileEvent(client.getDescriptor().getFileUri(file), FileChangeType.Changed));
    client.sendNotification(server -> {
      server.getWorkspaceService().didChangeWatchedFiles(new DidChangeWatchedFilesParams(changed));
      return Unit.INSTANCE;
    });
  }

  private record Target(@NotNull LspServer client, @NotNull TextDocumentIdentifier id, @NotNull Position position,
                        @NotNull String oldName) {
  }


  /**
   * @return the name at the caret when ELP can rename it, otherwise {@code null}
   */
  @Nullable
  private static String renamableName(@NotNull LspServer client, @NotNull TextDocumentIdentifier id, @NotNull Position position,
                                      @NotNull Document document, int offset) {
    if (offersPrepareRename(client)) {
      Either3<Range, PrepareRenameResult, PrepareRenameDefaultBehavior> prepared = client.sendRequestSync(
        TIMEOUT_MS, server -> server.getTextDocumentService().prepareRename(new PrepareRenameParams(id, position)));
      if (prepared == null) return null;
      Range range = prepared.isFirst() ? prepared.getFirst() : prepared.isSecond() ? prepared.getSecond().getRange() : null;
      if (range == null) return wordAt(document, offset);
      TextRange textRange = Lsp4jUtilKt.getRangeInDocument(document, range);
      return textRange != null ? document.getText(textRange) : null;
    }
    // ELP does not offer prepareRename: a rename to a scratch name, which changes nothing, shows whether it can rename here.
    String name = wordAt(document, offset);
    if (name == null) return null;
    WorkspaceEdit trial = rename(client, new RenameParams(id, position, name + "_elp_trial"), new AtomicReference<>());
    return trial != null && !edits(trial).isEmpty() ? name : null;
  }

  private static boolean offersPrepareRename(@NotNull LspServer client) {
    InitializeResult result = client.getInitializeResult();
    Either<Boolean, RenameOptions> provider = result != null ? result.getCapabilities().getRenameProvider() : null;
    return provider != null && provider.isRight() && Boolean.TRUE.equals(provider.getRight().getPrepareProvider());
  }

  /** The Erlang name (atom or variable characters) that holds or ends at the caret. */
  @Nullable
  static String wordAt(@NotNull Document document, int offset) {
    CharSequence text = document.getCharsSequence();
    int start = offset;
    int end = offset;
    while (start > 0 && isNameChar(text.charAt(start - 1))) start--;
    while (end < text.length() && isNameChar(text.charAt(end))) end++;
    String word = text.subSequence(start, end).toString();
    return NAME.matcher(word).matches() ? word : null;
  }

  private static boolean isNameChar(char c) {
    return Character.isLetterOrDigit(c) || c == '_' || c == '@';
  }

  /**
   * Sends {@code textDocument/rename}. ELP answers "content modified" to a request that meets a change of the files, such as the
   * save before the rename; the LSP specification lets the client send the request again, and it is sent up to
   * {@value #ATTEMPTS} times.
   */
  @Nullable
  private static WorkspaceEdit rename(@NotNull LspServer client, @NotNull RenameParams params, @NotNull AtomicReference<String> error) {
    for (int attempt = 1; ; attempt++) {
      AtomicReference<CompletableFuture<WorkspaceEdit>> future = new AtomicReference<>();
      WorkspaceEdit edit = client.sendRequestSync(TIMEOUT_MS, server -> {
        CompletableFuture<WorkspaceEdit> request = server.getTextDocumentService().rename(params);
        future.set(request);
        return request;
      });
      if (edit != null || future.get() == null) return edit;
      Throwable failure = future.get().handle((result, throwable) -> throwable).getNow(null);
      if (failure == null) return null;
      Throwable cause = rootCause(failure);
      if (attempt < ATTEMPTS && cause instanceof ResponseErrorException e && e.getResponseError().getCode() == CONTENT_MODIFIED) {
        continue;
      }
      error.set(StringUtil.notNullize(cause.getMessage(), failure.toString()));
      return null;
    }
  }

  @NotNull
  private static Throwable rootCause(@NotNull Throwable throwable) {
    Throwable cause = throwable;
    while (cause.getCause() != null && cause.getCause() != cause) cause = cause.getCause();
    return cause;
  }

  /** The text edits of a workspace edit by document URI, from {@code documentChanges} or else from {@code changes}. */
  @NotNull
  static Map<String, List<TextEdit>> edits(@NotNull WorkspaceEdit edit) {
    Map<String, List<TextEdit>> result = new LinkedHashMap<>();
    if (edit.getDocumentChanges() != null) {
      for (Either<TextDocumentEdit, ResourceOperation> change : edit.getDocumentChanges()) {
        if (!change.isLeft()) continue;
        TextDocumentEdit documentEdit = change.getLeft();
        result.computeIfAbsent(documentEdit.getTextDocument().getUri(), uri -> new ArrayList<>()).addAll(textEdits(documentEdit));
      }
    }
    else if (edit.getChanges() != null) {
      edit.getChanges().forEach((uri, edits) -> result.computeIfAbsent(uri, key -> new ArrayList<>()).addAll(edits));
    }
    return result;
  }

  /**
   * The changes of a workspace edit in their order: {@code documentChanges}, or else the text edits of {@code changes}. The
   * document edits made here hold the plain {@link TextEdit}s; the list is raw because its element type differs between the lsp4j
   * versions, see {@link #textEdits}, which reads it.
   */
  @NotNull
  @SuppressWarnings({"unchecked", "rawtypes"})
  private static List<Either<TextDocumentEdit, ResourceOperation>> changes(@NotNull WorkspaceEdit edit) {
    if (edit.getDocumentChanges() != null) return edit.getDocumentChanges();
    List<Either<TextDocumentEdit, ResourceOperation>> result = new ArrayList<>();
    if (edit.getChanges() != null) {
      edit.getChanges().forEach((uri, edits) -> result.add(Either.forLeft(new TextDocumentEdit(
        new VersionedTextDocumentIdentifier(uri, null), (List)edits))));
    }
    return result;
  }

  /**
   * The text edits of a document edit, without its snippet edits. lsp4j 0.24 (IDEA 2026.1 and 2026.2) holds {@link TextEdit}s
   * there, lsp4j 1.0 (2026.3) holds {@code Either<TextEdit, SnippetTextEdit>}; the erased list is the same, so each element is
   * checked: the left of an {@link Either}, a right (a snippet edit) skipped, or else a {@link TextEdit}.
   */
  @NotNull
  private static List<TextEdit> textEdits(@NotNull TextDocumentEdit edit) {
    List<TextEdit> result = new ArrayList<>();
    for (Object element : edit.getEdits()) {
      if (element instanceof Either<?, ?> either) {
        if (either.isLeft() && either.getLeft() instanceof TextEdit textEdit) result.add(textEdit);
      }
      else if (element instanceof TextEdit textEdit) {
        result.add(textEdit);
      }
    }
    return result;
  }

  /**
   * @return the new file name of a rename, or {@code null} when it moves the file to another directory, which this handler refuses
   */
  @Nullable
  private static String newName(@NotNull RenameFile rename) {
    String oldUri = rename.getOldUri();
    String newUri = rename.getNewUri();
    int oldSlash = oldUri.lastIndexOf('/');
    int newSlash = newUri.lastIndexOf('/');
    if (oldSlash < 0 || newSlash < 0 || !oldUri.substring(0, oldSlash).equals(newUri.substring(0, newSlash))) return null;
    return java.net.URLDecoder.decode(newUri.substring(newSlash + 1), java.nio.charset.StandardCharsets.UTF_8);
  }

  /**
   * Applies ELP's answer to {@code textDocument/rename} of {@code oldName}, see {@link #plan}.
   *
   * @param findFile the file of a document URI, as the LSP client finds it
   * @return why the edit was not applied, or {@code null} when it was
   */
  @Nullable
  static String apply(@NotNull Project project, @NotNull Function<String, VirtualFile> findFile, @NotNull WorkspaceEdit edit,
                      @NotNull String oldName) {
    Plan plan = plan(findFile, edit, oldName);
    if (plan.stale() != null) return staleProblem(plan.stale());
    return plan.problem() != null ? plan.problem() : plan.apply(project);
  }

  /**
   * Checks ELP's answer to {@code textDocument/rename} of {@code oldName} against the documents. Its changes take effect in their
   * order, as the LSP specification says: after a file rename the file has the new URI and the old one names nothing. ELP's module
   * rename relies on it, it renames the file first and then edits the {@code -module} line under the new URI.
   * Every text edit of a rename replaces an occurrence of the old name: the text in its range must be the old name, plain or
   * quoted (see {@link #isOccurrence}). ELP answers from an older text of a file when it has not read a change of the file yet;
   * the ranges of such an answer hold something else, or lie outside the document, and the answer is stale. Nothing of an answer
   * is applied unless all of it is checked.
   */
  @NotNull
  private static Plan plan(@NotNull Function<String, VirtualFile> findFile, @NotNull WorkspaceEdit edit, @NotNull String oldName) {
    // The URIs that the renames so far moved: the file now under the URI, or null for a URI that a file left.
    Map<String, VirtualFile> moved = new HashMap<>();
    // A file under two paths, such as a header in src and in rebar3's _build/default/lib/APP/src link to it, which ELP edits under
    // both: the first file found for each canonical file, so that one document gets the edits.
    Map<VirtualFile, VirtualFile> firstFound = new HashMap<>();
    Function<String, VirtualFile> fileAt = uri -> {
      if (moved.containsKey(uri)) return moved.get(uri);
      VirtualFile file = findFile.apply(uri);
      VirtualFile canonical = file != null ? file.getCanonicalFile() : null;
      return file != null ? firstFound.computeIfAbsent(canonical != null ? canonical : file, key -> file) : null;
    };
    Map<Document, List<TextEdit>> textEdits = new LinkedHashMap<>();
    Set<VirtualFile> files = new LinkedHashSet<>();
    Map<VirtualFile, String> renames = new LinkedHashMap<>();
    for (Either<TextDocumentEdit, ResourceOperation> change : changes(edit)) {
      if (change.isRight()) {
        String name = change.getRight() instanceof RenameFile rename ? newName(rename) : null;
        if (name == null) return Plan.refused("ELP's rename creates, deletes or moves files; nothing was changed.");
        RenameFile rename = (RenameFile)change.getRight();
        VirtualFile file = fileAt.apply(rename.getOldUri());
        if (file == null) return Plan.refused("ELP's rename renames " + rename.getOldUri() + ", which does not exist; nothing was changed.");
        VirtualFile existing = fileAt.apply(rename.getNewUri());
        if (existing != null && !existing.equals(file)) {
          return Plan.refused("ELP's rename renames " + file.getPresentableUrl() + " to " + name + ", which exists; nothing was changed.");
        }
        renames.put(file, name);
        files.add(file);
        moved.put(rename.getOldUri(), null);
        moved.put(rename.getNewUri(), file);
        continue;
      }
      String uri = change.getLeft().getTextDocument().getUri();
      VirtualFile file = fileAt.apply(uri);
      Document document = file != null ? FileDocumentManager.getInstance().getDocument(file) : null;
      if (document == null) return Plan.refused("ELP's rename changes " + uri + ", which cannot be opened; nothing was changed.");
      List<TextEdit> edits = textEdits.computeIfAbsent(document, key -> new ArrayList<>());
      files.add(file);
      for (TextEdit textEdit : textEdits(change.getLeft())) {
        TextRange range = rangeIn(document, textEdit.getRange());
        if (range == null || !isOccurrence(document.getText(range), oldName)) return new Plan(null, file, Map.of(), Set.of(), Map.of());
        if (!edits.contains(textEdit)) edits.add(textEdit);
      }
    }
    return new Plan(null, null, textEdits, files, renames);
  }

  /**
   * The range of an LSP range in a document, or {@code null} when it is not inside the document: a line past the last one, a
   * character past the end of its line, or an end before the start.
   */
  @Nullable
  private static TextRange rangeIn(@NotNull Document document, @NotNull Range range) {
    int start = offset(document, range.getStart());
    int end = offset(document, range.getEnd());
    if (start < 0 || end < start) return null;
    TextRange platform = Lsp4jUtilKt.getRangeInDocument(document, range);
    return platform != null && platform.getStartOffset() == start && platform.getEndOffset() == end ? platform : null;
  }

  private static int offset(@NotNull Document document, @Nullable Position position) {
    if (position == null || position.getLine() < 0 || position.getCharacter() < 0) return -1;
    if (position.getLine() >= document.getLineCount()) return -1;
    int lineStart = document.getLineStartOffset(position.getLine());
    int lineEnd = document.getLineEndOffset(position.getLine());
    return position.getCharacter() <= lineEnd - lineStart ? lineStart + position.getCharacter() : -1;
  }

  /**
   * @return whether {@code text} is what a rename of {@code oldName} replaces: the same name, either of them possibly a quoted
   * atom ({@code 'old name'}), compared without the quotes and with the escapes of the quoted form resolved
   */
  static boolean isOccurrence(@NotNull String text, @NotNull String oldName) {
    String name = unquoted(text);
    return name != null && !name.isEmpty() && name.equals(unquoted(oldName));
  }

  /** The name of a plain or a quoted name, or {@code null} for a quoted one that is not well formed. */
  @Nullable
  private static String unquoted(@NotNull String name) {
    if (!name.startsWith("'")) return name.indexOf('\'') < 0 ? name : null;
    if (name.length() < 2 || !name.endsWith("'")) return null;
    StringBuilder result = new StringBuilder();
    for (int i = 1; i < name.length() - 1; i++) {
      char c = name.charAt(i);
      if (c == '\'') return null;
      if (c == '\\') {
        if (++i >= name.length() - 1) return null;
        c = name.charAt(i);
      }
      result.append(c);
    }
    return result.toString();
  }

  /**
   * A checked answer: why it is refused, or the file whose text it does not match (a stale answer), or else what to change.
   */
  private record Plan(@Nullable String problem, @Nullable VirtualFile stale, @NotNull Map<Document, List<TextEdit>> textEdits,
                      @NotNull Set<VirtualFile> files, @NotNull Map<VirtualFile, String> renames) {
    @NotNull
    static Plan refused(@NotNull String problem) {
      return new Plan(problem, null, Map.of(), Set.of(), Map.of());
    }

    /** Applies the text edits and the file renames as one global undoable command. */
    @Nullable
    String apply(@NotNull Project project) {
      if (ReadonlyStatusHandler.getInstance(project).ensureFilesWritable(files).hasReadonlyFiles()) return null;
      WriteCommandAction.writeCommandAction(project).withName("Rename").withGlobalUndo().run(() -> {
        textEdits.forEach(Lsp4jUtilKt::applyTextEdits);
        PsiDocumentManager.getInstance(project).commitAllDocuments();
        renames.forEach((file, name) -> {
          PsiFile psiFile = PsiManager.getInstance(project).findFile(file);
          if (psiFile != null) psiFile.setName(name);
        });
      });
      return null;
    }
  }
}
