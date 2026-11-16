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
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
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
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.rename.RenameHandlerRegistry;
import com.intellij.util.concurrency.annotations.RequiresReadLock;
import org.eclipse.lsp4j.*;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.eclipse.lsp4j.jsonrpc.messages.Either3;
import org.intellij.erlang.elp.ElpEngine;
import org.intellij.erlang.psi.ErlangModule;
import org.intellij.erlang.psi.ErlangModuleRef;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

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
 * documents are saved before ELP is asked: ELP reads a file that is not open in an editor from the disk, where an earlier rename
 * may not have reached.
 * A module is renamed by the plugin, see {@link #isModuleName}; ELP's other file operations are refused.
 * If ELP cannot or does not answer, the plugin's rename runs exactly as in the {@code Plugin} position. While this handler is
 * available the plugin's in-place rename handlers are not ({@code ErlangRefactoringSupportProvider}), so only one rename runs.
 * Registered in {@code erlang-elp-lsp.xml}.
 */
public final class ElpRenameHandler implements RenameHandler, TitledHandler {
  private static final int TIMEOUT_MS = 10_000;
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
    if (isModuleName(file, editor.getCaretModel().getOffset())) {
      renameWithPlugin(project, editor, file, dataContext);
      return;
    }
    Target target = target(project, editor, file);
    if (target == null) {
      renameWithPlugin(project, editor, file, dataContext);
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
    if (isModuleName(file, editor.getCaretModel().getOffset())) return "A module is renamed by the plugin";
    Target target = target(project, editor, file);
    return target == null ? "ELP cannot rename the name at the caret" : rename(project, target, newName);
  }

  /**
   * A module stays with the plugin's rename: ELP 1.1.0 renames the file and the qualified calls and types, but leaves
   * {@code -behaviour(...)}, {@code -import(...)} and the module list of the {@code .app.src} file alone (see
   * {@code ElpRecordedRenameTest.testModule}); the plugin's rename renames the file and all of those.
   *
   * @return whether the caret is on the name of a module: in {@code -module(...)}, or a reference to a module
   */
  @RequiresReadLock
  static boolean isModuleName(@NotNull PsiFile file, int offset) {
    return isModuleNameAt(file, offset) || offset > 0 && isModuleNameAt(file, offset - 1);
  }

  private static boolean isModuleNameAt(@NotNull PsiFile file, int offset) {
    PsiElement element = file.findElementAt(offset);
    if (element == null) return false;
    if (PsiTreeUtil.getParentOfType(element, ErlangModuleRef.class) != null) return true;
    ErlangModule module = PsiTreeUtil.getParentOfType(element, ErlangModule.class);
    return module != null && PsiTreeUtil.isAncestor(module.getQAtom(), element, false);
  }

  /**
   * @return what ELP would rename at the caret, or {@code null} when ELP is not serving the file or cannot rename the name there
   */
  @Nullable
  private static Target target(@NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file) {
    VirtualFile virtualFile = file.getOriginalFile().getVirtualFile();
    LspServer client = virtualFile != null ? ElpLspServerStatus.findClient(project, virtualFile, ElpEngine.Feature.RENAME) : null;
    if (client == null) return null;
    FileDocumentManager.getInstance().saveAllDocuments();
    PsiDocumentManager.getInstance(project).commitAllDocuments();
    Document document = editor.getDocument();
    int offset = editor.getCaretModel().getOffset();
    TextDocumentIdentifier id = client.getDocumentIdentifier(virtualFile);
    Position position = Lsp4jUtilKt.getLsp4jPosition(document, offset);
    String oldName = ProgressManager.getInstance().runProcessWithProgressSynchronously(
      () -> renamableName(client, id, position, document, offset), "Asking ELP", true, project);
    return oldName != null ? new Target(client, id, position, oldName) : null;
  }

  @Nullable
  private static String rename(@NotNull Project project, @NotNull Target target, @NotNull String newName) {
    AtomicReference<String> error = new AtomicReference<>();
    WorkspaceEdit edit = ProgressManager.getInstance().runProcessWithProgressSynchronously(
      () -> rename(target.client(), new RenameParams(target.id(), target.position(), newName), error), "Renaming with ELP", true,
      project);
    if (edit == null) return "ELP did not rename '" + target.oldName() + "': " + StringUtil.notNullize(error.get(), "no answer");
    return apply(project, target.client().getDescriptor()::findFileByUri, edit);
  }

  private record Target(@NotNull LspServer client, @NotNull TextDocumentIdentifier id, @NotNull Position position,
                        @NotNull String oldName) {
  }


  /** The plugin's rename, chosen by the platform as in the {@code Plugin} position. */
  private static void renameWithPlugin(@NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file,
                                       @NotNull DataContext dataContext) {
    ElpEngine.runWithPlugin(ElpEngine.Feature.RENAME, () -> {
      RenameHandler handler = RenameHandlerRegistry.getInstance().getRenameHandler(dataContext);
      if (handler != null && !(handler instanceof ElpRenameHandler)) handler.invoke(project, editor, file, dataContext);
    });
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

  @Nullable
  private static WorkspaceEdit rename(@NotNull LspServer client, @NotNull RenameParams params, @NotNull AtomicReference<String> error) {
    AtomicReference<CompletableFuture<WorkspaceEdit>> future = new AtomicReference<>();
    WorkspaceEdit edit = client.sendRequestSync(TIMEOUT_MS, server -> {
      CompletableFuture<WorkspaceEdit> request = server.getTextDocumentService().rename(params);
      future.set(request);
      return request;
    });
    if (edit == null && future.get() != null) {
      future.get().handle((result, throwable) -> {
        if (throwable != null) error.set(StringUtil.notNullize(rootCause(throwable).getMessage(), throwable.toString()));
        return null;
      });
    }
    return edit;
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
   * Applies ELP's answer to {@code textDocument/rename}. Its changes take effect in their order, as the LSP specification says:
   * after a file rename the file has the new URI and the old one names nothing. ELP's module rename relies on it, it renames the
   * file first and then edits the {@code -module} line under the new URI. Everything is checked before anything changes; then the
   * text edits and the file renames are applied as one global undoable command.
   *
   * @param findFile the file of a document URI, as the LSP client finds it
   * @return why the edit was not applied, or {@code null} when it was
   */
  @Nullable
  static String apply(@NotNull Project project, @NotNull Function<String, VirtualFile> findFile, @NotNull WorkspaceEdit edit) {
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
        if (name == null) return "ELP's rename creates, deletes or moves files; nothing was changed.";
        RenameFile rename = (RenameFile)change.getRight();
        VirtualFile file = fileAt.apply(rename.getOldUri());
        if (file == null) return "ELP's rename renames " + rename.getOldUri() + ", which does not exist; nothing was changed.";
        VirtualFile existing = fileAt.apply(rename.getNewUri());
        if (existing != null && !existing.equals(file)) {
          return "ELP's rename renames " + file.getPresentableUrl() + " to " + name + ", which exists; nothing was changed.";
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
      if (document == null) return "ELP's rename changes " + uri + ", which cannot be opened; nothing was changed.";
      List<TextEdit> edits = textEdits.computeIfAbsent(document, key -> new ArrayList<>());
      files.add(file);
      for (TextEdit textEdit : textEdits(change.getLeft())) {
        if (Lsp4jUtilKt.getRangeInDocument(document, textEdit.getRange()) == null) {
          return "ELP's rename does not match the text of " + file.getPresentableUrl() + "; nothing was changed.";
        }
        if (!edits.contains(textEdit)) edits.add(textEdit);
      }
    }
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
