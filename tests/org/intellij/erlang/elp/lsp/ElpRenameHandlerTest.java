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

import com.intellij.openapi.command.undo.UndoManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import org.eclipse.lsp4j.WorkspaceEdit;
import org.eclipse.lsp4j.jsonrpc.json.MessageJsonHandler;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.util.Map;

/**
 * Applies answers of ELP to {@code textDocument/rename} in the shapes ELP sends, without a server. The JSON follows the answers
 * that ELP 1.1.0 gave on the jsx project, with {@code temp:///src/} for the directory of the test files.
 */
public class ElpRenameHandlerTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String ENCODER = """
    -module(jsx_encoder).
    -export([encoder/3]).

    encoder(Handler, State, Config) -> {Handler, State, Config}.
    """;
  private static final String JSX = """
    -module(jsx).
    -export([encoder/3]).

    encoder(Handler, State, Config) -> jsx_encoder:encoder(Handler, State, Config).
    """;
  private static final String RENAMED_ENCODER = ENCODER.replace("jsx_encoder", "jsx_encoder111");
  private static final String RENAMED_JSX = JSX.replace("jsx_encoder", "jsx_encoder111");
  private static final String RENAME = """
    {"kind": "rename", "oldUri": "temp:///src/jsx_encoder.erl", "newUri": "temp:///src/jsx_encoder111.erl"}""";
  private static final String MODULE_LINE_EDIT = """
    {"edits": [{"newText": "jsx_encoder111", "range": {"start": {"line": 0, "character": 8}, "end": {"line": 0, "character": 19}}}],
     "textDocument": {"uri": "%s", "version": null}}""";
  private static final String CALL_SITE_EDIT = """
    {"edits": [{"newText": "jsx_encoder111", "range": {"start": {"line": 3, "character": 35}, "end": {"line": 3, "character": 46}}}],
     "textDocument": {"uri": "temp:///src/jsx.erl", "version": 0}}""";

  private VirtualFile myEncoder;
  private VirtualFile myJsx;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myEncoder = myFixture.addFileToProject("src/jsx_encoder.erl", ENCODER).getVirtualFile();
    myJsx = myFixture.addFileToProject("src/jsx.erl", JSX).getVirtualFile();
  }

  /** ELP's module rename: the file rename first, then the call site, then the {@code -module} line under the new URI. */
  public void testModuleRenameEditsTheRenamedFileUnderItsNewUri() {
    assertApplied(documentChanges(RENAME, CALL_SITE_EDIT, MODULE_LINE_EDIT.formatted("temp:///src/jsx_encoder111.erl")));
    assertModuleRenamed();

    UndoManager undoManager = UndoManager.getInstance(getProject());
    assertTrue(undoManager.isUndoAvailable(null));
    undoManager.undo(null);
    assertEquals("jsx_encoder.erl", myEncoder.getName());
    assertNull(myEncoder.getParent().findChild("jsx_encoder111.erl"));
    assertTexts(ENCODER, JSX);
  }

  public void testEditToTheOldUriBeforeTheRename() {
    assertApplied(documentChanges(MODULE_LINE_EDIT.formatted("temp:///src/jsx_encoder.erl"), RENAME, CALL_SITE_EDIT));
    assertModuleRenamed();
  }

  /** A function rename: text edits only, no file operation. */
  public void testEditsOnly() {
    assertApplied(documentChanges(CALL_SITE_EDIT, MODULE_LINE_EDIT.formatted("temp:///src/jsx_encoder.erl")));
    assertEquals("jsx_encoder.erl", myEncoder.getName());
    assertTexts(RENAMED_ENCODER, RENAMED_JSX);
  }

  public void testChangesMap() {
    assertApplied(edit("""
      {"changes": {
        "temp:///src/jsx_encoder.erl": [{"newText": "jsx_encoder111", "range": {"start": {"line": 0, "character": 8}, "end": {"line": 0, "character": 19}}}],
        "temp:///src/jsx.erl": [{"newText": "jsx_encoder111", "range": {"start": {"line": 3, "character": 35}, "end": {"line": 3, "character": 46}}}]}}"""));
    assertEquals("jsx_encoder.erl", myEncoder.getName());
    assertTexts(RENAMED_ENCODER, RENAMED_JSX);
  }

  /** The new URI of a file is not the file before the rename that produces it. */
  public void testEditToTheNewUriBeforeTheRenameChangesNothing() {
    assertRefused(documentChanges(MODULE_LINE_EDIT.formatted("temp:///src/jsx_encoder111.erl"), CALL_SITE_EDIT, RENAME));
  }

  /** After a rename the old URI names no file. */
  public void testEditToTheOldUriAfterTheRenameChangesNothing() {
    assertRefused(documentChanges(RENAME, CALL_SITE_EDIT, MODULE_LINE_EDIT.formatted("temp:///src/jsx_encoder.erl")));
  }

  public void testEditToAnUnknownUriChangesNothing() {
    assertRefused(documentChanges(CALL_SITE_EDIT, MODULE_LINE_EDIT.formatted("temp:///src/jsx_missing.erl")));
  }

  public void testRenameOntoAnExistingFileChangesNothing() {
    VirtualFile decoder = myFixture.addFileToProject("src/jsx_decoder.erl", "-module(jsx_decoder).\n").getVirtualFile();
    assertRefused(documentChanges("""
      {"kind": "rename", "oldUri": "temp:///src/jsx_encoder.erl", "newUri": "temp:///src/jsx_decoder.erl"}""",
                                  CALL_SITE_EDIT.replace("jsx_encoder111", "jsx_decoder"),
                                  MODULE_LINE_EDIT.formatted("temp:///src/jsx_decoder.erl").replace("jsx_encoder111", "jsx_decoder")));
    assertEquals("-module(jsx_decoder).\n", document(decoder).getText());
  }

  public void testFileCreationChangesNothing() {
    assertRefused(documentChanges("""
      {"kind": "create", "uri": "temp:///src/jsx_new.erl"}""", CALL_SITE_EDIT));
  }

  public void testMoveToAnotherDirectoryChangesNothing() {
    assertRefused(documentChanges("""
      {"kind": "rename", "oldUri": "temp:///src/jsx_encoder.erl", "newUri": "temp:///jsx_encoder111.erl"}""", CALL_SITE_EDIT));
  }

  private void assertModuleRenamed() {
    assertEquals("jsx_encoder111.erl", myEncoder.getName());
    assertNull(myEncoder.getParent().findChild("jsx_encoder.erl"));
    assertTexts(RENAMED_ENCODER, RENAMED_JSX);
  }

  private void assertApplied(@NotNull WorkspaceEdit edit) {
    assertNull(apply(edit));
  }

  private void assertRefused(@NotNull WorkspaceEdit edit) {
    String problem = apply(edit);
    assertNotNull(problem);
    assertTrue(problem, problem.endsWith("nothing was changed."));
    assertEquals("jsx_encoder.erl", myEncoder.getName());
    assertTexts(ENCODER, JSX);
  }

  private String apply(@NotNull WorkspaceEdit edit) {
    return ElpRenameHandler.apply(getProject(), VirtualFileManager.getInstance()::findFileByUrl, edit);
  }

  private void assertTexts(@NotNull String encoder, @NotNull String jsx) {
    assertEquals(encoder, document(myEncoder).getText());
    assertEquals(jsx, document(myJsx).getText());
  }

  @NotNull
  private static Document document(@NotNull VirtualFile file) {
    Document document = FileDocumentManager.getInstance().getDocument(file);
    assertNotNull(file.getPath(), document);
    return document;
  }

  @NotNull
  private WorkspaceEdit documentChanges(String @NotNull ... changes) {
    return edit("{\"documentChanges\": [" + String.join(",\n", changes) + "]}");
  }

  /** The edit that {@code json} describes, with {@code temp:///src/} standing for the URI of the directory of the test files. */
  @NotNull
  private WorkspaceEdit edit(@NotNull String json) {
    String directory = myEncoder.getParent().getUrl() + "/";
    return new MessageJsonHandler(Map.of()).getGson().fromJson(json.replace("temp:///src/", directory), WorkspaceEdit.class);
  }
}
