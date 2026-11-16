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

import com.intellij.codeInsight.navigation.actions.GotoDeclarationAction;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.lsp.api.LspServerState;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileSystemItem;
import com.intellij.psi.PsiNamedElement;
import org.intellij.erlang.elp.ElpMode;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Go to declaration with the answers of the real ELP to {@code textDocument/definition}: one target per call site, and the plugin's
 * references when ELP's answer gives nothing to go to.
 */
public class ElpRecordedGotoDeclarationTest extends ElpRecordedProjectTestBase {
  private static final String DEFINITION = "textDocument/definition";

  /**
   * Where ELP's answer takes the user, by recorded kind: the file, the line and column, and the name there; {@code null} where the
   * handler gives nothing and the plugin's references answer. A module and an include go to the start of the file, which ELP
   * answers with the whole {@code -module} line or the whole header.
   */
  private static final Map<String, String> ELP_TARGETS = new LinkedHashMap<>();

  static {
    ELP_TARGETS.put("remote_function", "src/shop_backend.erl:12:1 lookup");
    ELP_TARGETS.put("remote_module", "src/shop_backend.erl:1:1 -");
    ELP_TARGETS.put("local_function", "src/shop.erl:11:1 price");
    ELP_TARGETS.put("local_function_in_macro_argument", "src/shop.erl:11:1 price");
    ELP_TARGETS.put("variable", "src/shop.erl:11:7 Name");
    ELP_TARGETS.put("variable_in_macro_argument", "src/shop.erl:13:10 Item");
    ELP_TARGETS.put("record", "src/shop.hrl:4:9 item");
    ELP_TARGETS.put("record_field", "src/shop.hrl:4:16 name");
    ELP_TARGETS.put("record_in_other_module", "src/shop.hrl:4:9 item");
    ELP_TARGETS.put("macro", "src/shop.hrl:1:9 DEFAULT_PRICE");
    ELP_TARGETS.put("macro_with_arguments", "src/shop.hrl:2:9 PRICE");
    ELP_TARGETS.put("macro_in_header", "src/shop.hrl:1:9 DEFAULT_PRICE");
    ELP_TARGETS.put("remote_type", "src/shop_backend.erl:6:7 key");
    ELP_TARGETS.put("local_type", "src/shop_backend.erl:6:7 key");
    ELP_TARGETS.put("behaviour", "src/shop_backend.erl:1:1 -");
    ELP_TARGETS.put("include", "src/shop.hrl:1:1 -");
    // In OTP's lists.erl, which the test project does not have.
    ELP_TARGETS.put("otp_function", null);
    // ELP answers an empty list.
    ELP_TARGETS.put("dynamic_call", null);
    // ELP answers the declaration under the caret; the platform shows its usages then.
    ELP_TARGETS.put("function_declaration", null);
    ELP_TARGETS.put("callback_implementation", null);
  }

  public void testEveryRecordedKindHasAnExpectation() {
    List<String> kinds = new ArrayList<>();
    for (ElpRecordedAnswers.Case recorded : myAnswers.cases(DEFINITION)) kinds.add(recorded.kind());
    assertEquals(new ArrayList<>(ELP_TARGETS.keySet()), kinds);
  }

  /** The handler asks ELP once, at the caret, and turns its answer into exactly one target, or into nothing. */
  public void testElpTargets() {
    for (ElpRecordedAnswers.Case recorded : myAnswers.cases(DEFINITION)) {
      Editor editor = openAt(recorded);
      int offset = editor.getCaretModel().getOffset();
      myServer.clearRequests();
      PsiElement[] targets = elpTargets(editor, offset);
      assertSize(1, myServer.requests(DEFINITION));
      assertEmpty(recorded + ": ELP was asked at another position than the recorder", myServer.unanswered());
      String expected = ELP_TARGETS.get(recorded.kind());
      if (expected == null) {
        assertNull(recorded.toString(), targets);
      }
      else {
        assertNotNull(recorded.toString(), targets);
        assertEquals(recorded.toString(), List.of(expected), describe(targets));
      }
    }
  }

  /**
   * What go to declaration does in the {@code ELP} position: the platform takes the first handler with targets, so the user gets
   * ELP's single target, or the plugin's targets where ELP gives none; never both. Where both answer, they agree on the file and
   * the line.
   */
  public void testOneTargetPerCallSite() {
    for (ElpRecordedAnswers.Case recorded : myAnswers.cases(DEFINITION)) {
      Editor editor = openAt(recorded);
      int offset = editor.getCaretModel().getOffset();
      position(ElpMode.PLUGIN);
      List<String> plugin = describe(GotoDeclarationAction.findAllTargetElements(getProject(), editor, offset));
      position(ElpMode.ELP);
      List<String> elp = describe(onPooledThread(
        () -> ReadAction.compute(() -> GotoDeclarationAction.findAllTargetElements(getProject(), editor, offset))));

      String expected = ELP_TARGETS.get(recorded.kind());
      if (expected == null) {
        assertEquals(recorded + ": the plugin answers", plugin, elp);
        continue;
      }
      assertEquals(recorded.toString(), List.of(expected), elp);
      if (!plugin.isEmpty()) {
        assertEquals(recorded.toString(), 1, plugin.size());
        assertEquals(recorded + ": " + plugin + " vs " + elp, fileAndLine(plugin.getFirst()), fileAndLine(elp.getFirst()));
      }
    }
  }

  /** Macro arguments are where the plugin's resolve is weakest; ELP resolves both a call and a variable there. */
  public void testMacroArguments() {
    for (String kind : List.of("local_function_in_macro_argument", "variable_in_macro_argument")) {
      ElpRecordedAnswers.Case recorded = myAnswers.get(DEFINITION, kind);
      Editor editor = openAt(recorded);
      assertNotNull(kind, elpTargets(editor, editor.getCaretModel().getOffset()));
    }
  }

  public void testNothingFromElpOnTheEdt() {
    Editor editor = openAt(myAnswers.get(DEFINITION, "remote_function"));
    int offset = editor.getCaretModel().getOffset();
    assertNull(new ElpGotoDeclarationHandler().getGotoDeclarationTargets(myFixture.getFile().findElementAt(offset), offset, editor));
    assertEmpty("the request would block the EDT", myServer.requests());
  }

  public void testPluginAnswersWhileTheServerIsNotRunning() {
    Editor editor = openAt(myAnswers.get(DEFINITION, "remote_function"));
    int offset = editor.getCaretModel().getOffset();
    for (LspServerState state : List.of(LspServerState.Initializing, LspServerState.ShutdownUnexpectedly)) {
      myClient.setState(state);
      assertNull(state.name(), elpTargets(editor, offset));
    }
    myClient.setState(LspServerState.Running);
    position(ElpMode.PLUGIN);
    assertNull(elpTargets(editor, offset));
    assertEmpty(myServer.requests());
  }

  @Nullable
  private PsiElement[] elpTargets(@NotNull Editor editor, int offset) {
    PsiFile file = myFixture.getFile();
    return onPooledThread(() -> ReadAction.compute(
      () -> new ElpGotoDeclarationHandler().getGotoDeclarationTargets(file.findElementAt(offset), offset, editor)));
  }

  @NotNull
  private List<String> describe(@Nullable PsiElement[] targets) {
    List<String> result = new ArrayList<>();
    if (targets == null) return result;
    for (PsiElement target : targets) {
      result.add(ReadAction.compute(() -> {
        PsiFile file = target.getContainingFile();
        VirtualFile virtualFile = file.getVirtualFile();
        int start = target instanceof PsiFileSystemItem ? 0 : target.getTextOffset();
        Document document = document(virtualFile);
        String name = target instanceof PsiNamedElement named && !(target instanceof PsiFileSystemItem) ? named.getName() : null;
        if (name == null) name = target instanceof PsiFileSystemItem ? document.getText(new TextRange(0, 1)) : target.getText();
        String place = describe(virtualFile, new TextRange(start, start));
        return place + name;
      }));
    }
    return result;
  }

  @NotNull
  private static String fileAndLine(@NotNull String description) {
    return description.substring(0, description.lastIndexOf(':'));
  }
}
