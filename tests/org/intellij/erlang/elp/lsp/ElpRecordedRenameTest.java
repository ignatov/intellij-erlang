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

import com.intellij.codeInsight.daemon.impl.DaemonProgressIndicator;
import com.intellij.codeInspection.InspectionEngine;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ex.LocalInspectionToolWrapper;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.command.undo.UndoManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import org.eclipse.lsp4j.WorkspaceEdit;
import org.intellij.erlang.elp.ElpEngine;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.inspection.*;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Rename in the {@code ELP} position with the answers of the real ELP to {@code textDocument/rename}, through the whole handler:
 * the trial rename that tells whether ELP can rename the name at the caret, the rename itself and the edit applied to the project.
 * Checks the texts the user gets, and that the plugin resolves everything in them afterwards.
 */
public class ElpRecordedRenameTest extends ElpRecordedProjectTestBase {
  private static final String RENAME = "textDocument/rename";
  private static final List<String> CLEAN_MODULES = List.of("src/shop.erl", "src/shop_memory.erl", "src/shop_backend.erl");

  public void testExportedFunction() {
    rename("exported_function");
    assertLines("src/shop_backend.erl", 3, "-export([find/2]).", 11, "-spec find(module(), key()) -> {ok, term()} | error.",
                12, "find(Backend, Key) ->");
    assertLines("src/shop.erl", 12, "  case shop_backend:find(shop_memory, Name) of");
    assertResolved(CLEAN_MODULES);
  }

  public void testLocalFunction() {
    rename("local_function");
    assertLines("src/shop.erl", 19, "  add_up([price(Name) || Name <- Names], 0).", 29, "add_up([], Acc) -> Acc;",
                30, "add_up([Price | Rest], Acc) -> add_up(Rest, Acc + Price).");
    assertResolved(CLEAN_MODULES);
  }

  /** Only the variable of the clause at the caret: {@code Name} of {@code total/1} and {@code log_price/1} stays. */
  public void testVariable() {
    rename("variable");
    assertLines("src/shop.erl", 11, "price(ItemName) ->", 12, "  case shop_backend:lookup(shop_memory, ItemName) of",
                19, "  sum([price(Name) || Name <- Names], 0).", 26, "log_price(Name) ->");
    assertResolved(CLEAN_MODULES);
  }

  /** A record of a header: the header, every module that includes it, the record name inside a macro body. */
  public void testRecord() {
    rename("record");
    assertLines("src/shop.hrl", 2, "-define(PRICE(Item), Item#product.price).",
                4, "-record(product, {name :: atom(), price = ?DEFAULT_PRICE :: non_neg_integer()}).");
    assertLines("src/shop.erl", 21, "-spec describe(#product{}) -> string().", 22, "describe(#product{name = Name, price = Price}) ->");
    assertLines("src/shop_memory.erl", 8, "-spec fetch(shop_backend:key()) -> {ok, #product{}} | error.",
                10, "  case lists:keyfind(Key, #product.name, items()) of", 15, "-spec items() -> [#product{}].",
                17, "  [#product{name = apple, price = 3}, #product{name = pear}].");
    assertLines("src/shop_defects.erl", 33, "unknown_field() -> #product{no_such_field = 1}.");
    assertResolved(CLEAN_MODULES);
  }

  public void testRecordField() {
    rename("record_field");
    assertLines("src/shop.hrl", 2, "-define(PRICE(Item), Item#item.cost).",
                4, "-record(item, {name :: atom(), cost = ?DEFAULT_PRICE :: non_neg_integer()}).");
    assertLines("src/shop.erl", 22, "describe(#item{name = Name, cost = Price}) ->");
    assertLines("src/shop_memory.erl", 17, "  [#item{name = apple, cost = 3}, #item{name = pear}].");
    assertResolved(CLEAN_MODULES);
  }

  public void testMacro() {
    rename("macro");
    assertLines("src/shop.hrl", 1, "-define(BASE_PRICE, 10).",
                4, "-record(item, {name :: atom(), price = ?BASE_PRICE :: non_neg_integer()}).");
    assertLines("src/shop.erl", 14, "    error -> ?BASE_PRICE");
    assertResolved(CLEAN_MODULES);
  }

  /** An exported type: the definition, {@code -export_type}, the callback, the specs here and in the other modules. */
  public void testType() {
    rename("type");
    assertLines("src/shop_backend.erl", 4, "-export_type([item_key/0]).", 6, "-type item_key() :: atom().",
                8, "-callback fetch(Key :: item_key()) -> {ok, term()} | error.",
                11, "-spec lookup(module(), item_key()) -> {ok, term()} | error.");
    assertLines("src/shop.erl", 10, "-spec price(shop_backend:item_key()) -> non_neg_integer().",
                17, "-spec total([shop_backend:item_key()]) -> non_neg_integer().", 25, "-spec log_price(shop_backend:item_key()) -> ok.");
    assertLines("src/shop_memory.erl", 8, "-spec fetch(shop_backend:item_key()) -> {ok, #item{}} | error.");
    assertResolved(CLEAN_MODULES);
  }

  /**
   * ELP's own module rename, as recorded: the file is renamed and the qualified calls and types follow, but ELP 1.1.0 leaves
   * {@code -behaviour(shop_backend)} and {@code -import(shop_backend, ...)} alone, although its references list both. That is why a
   * module stays with the plugin's rename, see {@link #testModuleStaysWithThePluginsRename}.
   */
  public void testModule() {
    VirtualFile backend = file("src/shop_backend.erl");
    ElpRecordedAnswers.Case recorded = myAnswers.get(RENAME, "module");
    WorkspaceEdit edit = ElpRecordedAnswers.gson().fromJson(recorded.result(), WorkspaceEdit.class);
    assertNull(ElpRenameHandler.apply(getProject(), myDescriptor::findFileByUri, edit));
    assertEquals("shop_store.erl", backend.getName());
    assertNull(myRoot.findFileByRelativePath("src/shop_backend.erl"));
    assertLines("src/shop_store.erl", 1, "-module(shop_store).");
    assertLines("src/shop.erl", 10, "-spec price(shop_store:key()) -> non_neg_integer().",
                12, "  case shop_store:lookup(shop_memory, Name) of");
    assertLines("src/shop_memory.erl", 2, "-behaviour(shop_backend).", 8, "-spec fetch(shop_store:key()) -> {ok, #item{}} | error.");
    assertLines("src/shop_defects.erl", 10, "-import(shop_backend, [map/2]).");
    assertResolved(List.of("src/shop.erl", "src/shop_store.erl"));
  }

  /**
   * In the ELP position a module is not renamed by ELP: the handler does not ask it, and the plugin's rename renames the file, the
   * qualified calls, {@code -behaviour}, {@code -import} and the module list of the {@code .app.src} file.
   */
  public void testModuleStaysWithThePluginsRename() {
    VirtualFile backend = file("src/shop_backend.erl");
    VirtualFile appSrc = file("src/shop.app.src");
    WriteCommandAction.runWriteCommandAction(getProject(), () -> {
      Document document = document(appSrc);
      document.replaceString(0, document.getTextLength(), document.getText().replace("{modules, []}", "{modules, [shop, shop_backend]}"));
      PsiDocumentManager.getInstance(getProject()).commitDocument(document);
    });
    for (ElpRecordedAnswers.Case recorded : myAnswers.cases(RENAME)) {
      openAt(recorded);
      assertEquals(recorded.kind(), "module".equals(recorded.kind()),
                   ElpRenameHandler.isModuleName(myFixture.getFile(), myFixture.getEditor().getCaretModel().getOffset()));
    }
    for (String file : List.of("src/shop_memory.erl", "src/shop_defects.erl")) {
      myFixture.openFileInEditor(file(file));
      String text = myFixture.getEditor().getDocument().getText();
      int behaviourOrImport = text.indexOf("shop_backend") + 2;
      assertTrue(file, ElpRenameHandler.isModuleName(myFixture.getFile(), behaviourOrImport));
    }

    openAt(myAnswers.get(RENAME, "module"));
    myServer.clearRequests();
    assertEquals("A module is renamed by the plugin",
                 ElpRenameHandler.renameAtCaret(getProject(), myFixture.getEditor(), myFixture.getFile(), "shop_store"));
    assertEmpty("ELP is not asked", myServer.requests());
    ElpEngine.runWithPlugin(ElpEngine.Feature.RENAME, () -> myFixture.renameElementAtCaret("shop_store"));

    assertEquals("shop_store.erl", backend.getName());
    assertLines("src/shop_store.erl", 1, "-module(shop_store).");
    assertLines("src/shop.erl", 10, "-spec price(shop_store:key()) -> non_neg_integer().",
                12, "  case shop_store:lookup(shop_memory, Name) of");
    assertLines("src/shop_memory.erl", 2, "-behaviour(shop_store).", 8, "-spec fetch(shop_store:key()) -> {ok, #item{}} | error.");
    assertLines("src/shop_defects.erl", 10, "-import(shop_store, [map/2]).");
    assertTrue(document(appSrc).getText(), document(appSrc).getText().contains("{modules, [shop, shop_store]}"));
    assertResolved(List.of("src/shop.erl", "src/shop_store.erl", "src/shop_memory.erl"));
  }

  /** The edit spans four files and is one undoable command; nothing is saved. */
  public void testRecordRenameIsOneUndoableCommandAndUnsaved() {
    List<String> files = List.of("src/shop.hrl", "src/shop.erl", "src/shop_memory.erl", "src/shop_defects.erl");
    List<String> before = texts(files);
    rename("record");
    for (String file : files) assertTrue(file, FileDocumentManager.getInstance().isFileModified(file(file)));
    UndoManager undoManager = UndoManager.getInstance(getProject());
    assertTrue(undoManager.isUndoAvailable(null));
    undoManager.undo(null);
    assertEquals(before, texts(files));
  }

  /** In the Plugin position the plugin renames, and ELP is not asked even while a server runs. */
  public void testPluginPositionRenamesWithThePlugin() {
    openAt(myAnswers.get(RENAME, "local_function"));
    position(ElpMode.PLUGIN);
    myFixture.renameElementAtCaret("add_up");
    assertLines("src/shop.erl", 19, "  add_up([price(Name) || Name <- Names], 0).", 29, "add_up([], Acc) -> Acc;",
                30, "add_up([Price | Rest], Acc) -> add_up(Rest, Acc + Price).");
    assertEmpty(myServer.requests());
  }

  /** Runs the rename of a recorded kind at the recorded caret, with the recorded new name, and checks the requests. */
  private void rename(@NotNull String kind) {
    ElpRecordedAnswers.Case recorded = myAnswers.get(RENAME, kind);
    openAt(recorded);
    String newName = recorded.params().get("newName").getAsString();
    myServer.clearRequests();
    assertNull(recorded.toString(), ElpRenameHandler.renameAtCaret(getProject(), myFixture.getEditor(), myFixture.getFile(), newName));
    List<FakeElpServer.Request> requests = myServer.requests(RENAME);
    assertEquals("a trial rename, then the rename", 2, requests.size());
    assertEmpty(myServer.unanswered());
  }

  private void assertLines(@NotNull String file, Object @NotNull ... lineAndText) {
    String[] lines = document(file(file)).getText().split("\n", -1);
    for (int i = 0; i < lineAndText.length; i += 2) {
      int line = (Integer)lineAndText[i];
      assertEquals(file + ":" + line, lineAndText[i + 1], lines[line - 1]);
    }
  }

  /** The plugin's resolve finds every function, record, field, macro and variable of the files, in the Plugin position. */
  private void assertResolved(@NotNull List<String> files) {
    position(ElpMode.PLUGIN);
    List<String> problems = new ArrayList<>();
    for (String file : files) {
      PsiFile psiFile = psiFile(file);
      List<LocalInspectionToolWrapper> wrappers = new ArrayList<>();
      for (ErlangInspectionBase inspection : List.of(new ErlangUnresolvedFunctionInspection(), new ErlangUnresolvedExportFunctionInspection(),
                                                     new ErlangUnresolvedRecordInspection(), new ErlangUnresolvedRecordFieldInspection(),
                                                     new ErlangUnresolvedMacrosInspection(), new ErlangUnboundVariableInspection(),
                                                     new ErlangUnusedVariableInspection(), new ErlangUnusedFunctionInspection())) {
        wrappers.add(new LocalInspectionToolWrapper(inspection));
      }
      DaemonProgressIndicator indicator = new DaemonProgressIndicator();
      Map<LocalInspectionToolWrapper, List<ProblemDescriptor>> found = ProgressManager.getInstance().runProcess(
        () -> InspectionEngine.inspectEx(wrappers, psiFile, psiFile.getTextRange(), psiFile.getTextRange(), true, false, true, indicator,
                                         (tool, descriptor) -> true), indicator);
      found.forEach((tool, descriptors) -> {
        for (ProblemDescriptor descriptor : descriptors) problems.add(file + ": " + descriptor.getDescriptionTemplate());
      });
    }
    assertEmpty(problems);
    position(ElpMode.ELP);
  }

  @NotNull
  private List<String> texts(@NotNull List<String> files) {
    List<String> texts = new ArrayList<>();
    for (String file : files) texts.add(document(file(file)).getText());
    return texts;
  }
}
