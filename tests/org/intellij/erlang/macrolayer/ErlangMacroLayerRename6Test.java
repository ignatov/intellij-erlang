package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.TargetElementUtil;
import com.intellij.codeInsight.template.impl.TemplateManagerImpl;
import com.intellij.codeInsight.template.impl.TemplateState;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.actionSystem.impl.SimpleDataContext;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.refactoring.BaseRefactoringProcessor;
import com.intellij.refactoring.rename.PsiElementRenameHandler;
import com.intellij.refactoring.rename.RenameHandler;
import com.intellij.refactoring.rename.RenameHandlerRegistry;
import com.intellij.refactoring.rename.inplace.VariableInplaceRenameHandler;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.usageView.UsageViewUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Paths;
import java.util.*;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 6: rename paths that round 5 did not cover, on testData/macroLayer/nav. The rename runs as the platform's
 * rename action runs it: the handler that the platform picks for the caret, in-place when it is the in-place handler.
 * The layer builds no copy on the UI thread, as in the IDE; highlighting has built the copies first. Every result
 * is either a whole rename (the same edits as the rename from the uses, which erlc compiles, DEFECTS5-REPORT.md) or a
 * refusal that changes nothing.
 */
public class ErlangMacroLayerRename6Test extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private final Map<String, PsiFile> myFiles = new LinkedHashMap<>();
  private final Map<String, String> myOriginal = new LinkedHashMap<>();

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    for (String n : List.of("h.hrl", "i.erl", "i2.erl", "s.erl")) addFile(n, read(Paths.get("testData/macroLayer/nav", n)));
  }

  private void addFile(String name, String text) {
    myOriginal.put(name, text);
    myFiles.put(name, myFixture.addFileToProject("nav/" + name, text));
  }

  private void highlightAll() {
    for (PsiFile f : myFiles.values()) {
      myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
      myFixture.doHighlighting();
    }
  }

  private Editor caretAt(String file, String marker) {
    PsiFile f = myFiles.get(file);
    int at = f.getText().indexOf(marker);
    assertTrue(file + " has no " + marker, at >= 0);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    Editor editor = myFixture.getEditor();
    editor.getCaretModel().moveToOffset(at);
    return editor;
  }

  /** The handler the platform takes at the caret. */
  private RenameHandler handlerAt(Editor editor) {
    return RenameHandlerRegistry.getInstance().getRenameHandler(((EditorEx)editor).getDataContext());
  }

  /** Renames at the caret with the handler the platform takes; in-place with the in-place handlers. Returns its kind. */
  private String renameAt(String file, String marker, String newName) {
    Editor editor = caretAt(file, marker);
    RenameHandler h = handlerAt(editor);
    assertNotNull("no rename at " + marker, h);
    DataContext ctx = ((EditorEx)editor).getDataContext();
    if (h instanceof VariableInplaceRenameHandler vh) {
      PsiElement target = TargetElementUtil.findTargetElement(editor, TargetElementUtil.getInstance().getAllAccepted());
      inPlace(vh, target, editor, ctx, newName);
      return "in-place";
    }
    h.invoke(getProject(), editor, myFiles.get(file), SimpleDataContext.builder().setParent(ctx).add(PsiElementRenameHandler.DEFAULT_NAME, newName).build());
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    return "dialog";
  }

  private void inPlace(VariableInplaceRenameHandler handler, PsiElement target, Editor editor, DataContext ctx, String newName) {
    var d = Disposer.newDisposable();
    try {
      TemplateManagerImpl.setTemplateTesting(d);
      handler.doRename(target, editor, SimpleDataContext.getSimpleContext(CommonDataKeys.PSI_ELEMENT, target, ctx));
      TemplateState state = TemplateManagerImpl.getTemplateState(editor);
      assertNotNull("in-place rename started no template", state);
      TextRange range = state.getCurrentVariableRange();
      WriteCommandAction.writeCommandAction(getProject()).run(
        () -> editor.getDocument().replaceString(range.getStartOffset(), range.getEndOffset(), newName));
      TemplateManagerImpl.getTemplateState(editor).gotoEnd(false);
      PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
    }
    finally {
      Disposer.dispose(d);
    }
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
  }

  /** The texts expected now: the originals with the given replacements, per file ("file", "from", "to"). */
  private void assertTexts(String... edits) {
    Map<String, String> expected = new LinkedHashMap<>(myOriginal);
    for (int i = 0; i < edits.length; i += 3) {
      String text = expected.get(edits[i]);
      assertTrue(edits[i] + " has no " + edits[i + 1], text.contains(edits[i + 1]));
      expected.put(edits[i], text.replace(edits[i + 1], edits[i + 2]));
    }
    for (String n : myFiles.keySet()) assertEquals(n, expected.get(n), myFiles.get(n).getText());
  }

  private void typeAtTheEnd(String file) {
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(myFiles.get(file));
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getTextLength(), "%"));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    com.intellij.openapi.fileEditor.FileDocumentManager.getInstance().saveDocument(d); // the fixture reopens the file
    myOriginal.put(file, myOriginal.get(file) + "%");
  }

  // ------------------------------------------------------------------------------------------------ in-place rename

  public void testInplaceRenameOfASameFileBodyVariable() {
    highlightAll();
    assertEquals("in-place", renameAt("s.erl", "Xs.\n", "Xs2"));
    assertTexts("s.erl", "-define(X, Xs = 1).", "-define(X, Xs2 = 1).", "s.erl", "g() -> ?X, Xs.", "g() -> ?X, Xs2.");
  }

  public void testInplaceRenameOfASameFileBodyVariableFromTheBody() {
    highlightAll();
    assertEquals("in-place", renameAt("s.erl", "Xs = 1)", "Xs2"));
    assertTexts("s.erl", "-define(X, Xs = 1).", "-define(X, Xs2 = 1).", "s.erl", "g() -> ?X, Xs.", "g() -> ?X, Xs2.");
  }

  public void testInplaceRenameOfAVariableBoundByAMacroArgument() {
    highlightAll();
    assertEquals("in-place", renameAt("s.erl", "Z + ?CONST", "Z2"));
    assertTexts("s.erl", "b() -> ?BIND(Z), Z + ?CONST.", "b() -> ?BIND(Z2), Z2 + ?CONST.");
  }

  public void testInplaceRenameOfAVariableBoundByAMacroArgumentFromTheArgument() {
    highlightAll();
    assertEquals("in-place", renameAt("s.erl", "Z), Z + ?CONST", "Z2"));
    assertTexts("s.erl", "b() -> ?BIND(Z), Z + ?CONST.", "b() -> ?BIND(Z2), Z2 + ?CONST.");
  }

  /**
   * Right after a key press: the target is found in the background (as the data rule of the IDE finds it), and the
   * usage search of the rename may build the copy (in the IDE it runs in the background; here on the UI thread, so the
   * test lets the layer build there).
   */
  public void testInplaceRenameRightAfterTyping() throws Exception {
    highlightAll();
    typeAtTheEnd("s.erl");
    Editor editor = caretAt("s.erl", "Xs.\n");
    int caret = editor.getCaretModel().getOffset();
    PsiElement target = inBackground(() -> TargetElementUtil.getInstance().findTargetElement(editor, TargetElementUtil.getInstance().getAllAccepted(), caret));
    assertNotNull(target);
    RenameHandler h = handlerAt(editor);
    assertTrue(String.valueOf(h), h instanceof VariableInplaceRenameHandler);
    ErlangMacroLayer.setEdtBuildForTests(true, getTestRootDisposable());
    inPlace((VariableInplaceRenameHandler)h, target, editor, ((EditorEx)editor).getDataContext(), "Xs2");
    assertTexts("s.erl", "-define(X, Xs = 1).", "-define(X, Xs2 = 1).", "s.erl", "g() -> ?X, Xs.", "g() -> ?X, Xs2.");
  }

  /**
   * A rename whose target was taken before the layer could answer (the use itself, which master cannot bind) would
   * edit only that use: it is refused with a conflict, and nothing changes.
   */
  public void testARenameWhoseTargetWasTakenWithoutTheLayerIsRefused() {
    highlightAll();
    typeAtTheEnd("s.erl");
    Editor editor = caretAt("s.erl", "Xs.\n");
    PsiElement stale = TargetElementUtil.findTargetElement(editor, TargetElementUtil.getInstance().getAllAccepted());
    assertNotNull(stale);
    assertEquals("the use itself: the UI thread has no copy of the edited file", myFiles.get("s.erl").getText().indexOf("Xs.\n"),
                 stale.getTextRange().getStartOffset());
    ErlangMacroLayer.setEdtBuildForTests(true, getTestRootDisposable());
    try {
      myFixture.renameElement(stale, "Xs2");
      fail("no conflict");
    }
    catch (BaseRefactoringProcessor.ConflictsInTestsException e) {
      System.out.println("LAYER-R6 conflict: " + e.getMessages());
      assertTrue(e.getMessages().toString(), e.getMessages().toString().contains("bound by a macro"));
    }
    assertTexts();
  }

  /** A free variable of a header macro body is renamed with the dialog (not in-place), from its use and from the body. */
  public void testHeaderBodyVariableIsRenamedWithTheDialog() {
    highlightAll();
    assertEquals("dialog", renameAt("i.erl", "Xh.\n", "Xh2"));
    assertTexts("h.hrl", "Xh = 1", "Xh2 = 1", "i.erl", "?H_X, Xh.", "?H_X, Xh2.", "i2.erl", "?H_X, Xh.", "?H_X, Xh2.");
  }

  public void testHeaderBodyVariableRenamedFromTheBody() {
    highlightAll();
    assertEquals("dialog", renameAt("h.hrl", "Xh = 1)", "Xh2"));
    assertTexts("h.hrl", "Xh = 1", "Xh2 = 1", "i.erl", "?H_X, Xh.", "?H_X, Xh2.", "i2.erl", "?H_X, Xh.", "?H_X, Xh2.");
  }

  // ------------------------------------------------------------------------------------------------ the token inside the macro

  public void testRecordMadeByASameFileMacroRenamedFromTheBody() {
    highlightAll();
    renameAt("s.erl", "mr, {mf}", "mr2");
    assertTexts("s.erl", "-record(mr, {mf})", "-record(mr2, {mf})", "s.erl", "#mr{mf = 1}", "#mr2{mf = 1}");
  }

  public void testFieldMadeByASameFileMacroRenamedFromTheBody() {
    highlightAll();
    renameAt("s.erl", "mf})", "mf2");
    assertTexts("s.erl", "-record(mr, {mf})", "-record(mr, {mf2})", "s.erl", "#mr{mf = 1}", "#mr{mf2 = 1}");
  }

  public void testFunctionNamedInAMacroArgumentRenamedFromTheArgument() {
    highlightAll();
    renameAt("s.erl", "made).", "made2");
    assertTexts("s.erl", "?MK(made).", "?MK(made2).", "s.erl", "made/0, fixed/0", "made2/0, fixed/0",
                "s.erl", "{made(), fixed()}", "{made2(), fixed()}");
  }

  public void testFunctionNamedInASameFileBodyRenamedFromTheBody() {
    highlightAll();
    renameAt("s.erl", "fixed() -> ok)", "fixed2");
    assertTexts("s.erl", "fixed() -> ok)", "fixed2() -> ok)", "s.erl", "made/0, fixed/0", "made/0, fixed2/0",
                "s.erl", "{made(), fixed()}", "{made(), fixed2()}");
  }

  /** Names that the platform offers no rename for at the body token: nothing to rename, nothing changes. */
  public void testBodyTokensWithoutARenameChangeNothing() {
    highlightAll();
    for (String[] c : new String[][]{{"s.erl", "mt() :: integer"}, {"h.hrl", "hmt() :: integer"}, {"h.hrl", "hfixed() -> ok)"}}) {
      RenameHandler h = handlerAt(caretAt(c[0], c[1]));
      System.out.println("LAYER-R6 " + c[1] + ": " + h);
      assertNull(c[1], h);
    }
    assertTexts();
  }

  // ------------------------------------------------------------------------------------------------ conflicts

  /** A function that uses the macro already has a variable with the new name: the renamed body would match it. */
  public void testSameFileBodyVariableConflict() {
    String s = myOriginal.get("s.erl");
    myFiles.remove("s.erl");
    myOriginal.remove("s.erl");
    addFile("s3.erl", s.replace("-module(s).", "-module(s3).").replace("g/0,", "g/0, k/1,") + "k(Xs2) -> ?X, Xs2.\n");
    highlightAll();
    Editor editor = caretAt("s3.erl", "Xs.\n");
    PsiElement target = TargetElementUtil.findTargetElement(editor, TargetElementUtil.getInstance().getAllAccepted());
    assertNotNull(target);
    try {
      myFixture.renameElement(target, "Xs2");
      fail("no conflict");
    }
    catch (BaseRefactoringProcessor.ConflictsInTestsException e) {
      System.out.println("LAYER-R6 conflict: " + e.getMessages());
      assertTrue(e.getMessages().toString(), e.getMessages().toString().contains("k/1"));
    }
    assertTexts();
  }

  /** Another includer of the header already has the new name of a function, record or type made by a header macro. */
  public void testHeaderNamesWhoseNewNameAnIncluderHas() {
    addFile("i3.erl", "-module(i3).\n-include(\"h.hrl\").\n-export([hdup/0, t3f/0]).\n?H_FIX.\n?H_MKREC.\n?H_MKTYPE.\n" +
                      "-record(r3, {a}).\n-type t3() :: atom().\n-spec t3f() -> t3().\nt3f() -> #r3{a = a}#r3.a.\nhdup() -> ok.\n");
    ErlangMacroLayer.setEdtBuildForTests(true, getTestRootDisposable()); // the usage search of rename, which runs in the background in the IDE
    highlightAll();
    for (String[] c : new String[][]{{"hfixed()}", "hdup", "Function hdup/0"}, {"#hmr{", "r3", "Record r3"}, {"hmt().", "t3", "Type t3"}}) {
      Editor editor = caretAt("i.erl", c[0]);
      if (c[0].startsWith("#")) editor.getCaretModel().moveToOffset(editor.getCaretModel().getOffset() + 1);
      PsiElement target = TargetElementUtil.findTargetElement(editor, TargetElementUtil.getInstance().getAllAccepted());
      assertNotNull(c[0], target);
      try {
        myFixture.renameElement(target, c[1]);
        fail("no conflict for " + c[0]);
      }
      catch (BaseRefactoringProcessor.ConflictsInTestsException e) {
        System.out.println("LAYER-R6 conflict: " + e.getMessages());
        assertTrue(e.getMessages().toString(), e.getMessages().toString().contains(c[2] + " already exists in i3.erl"));
      }
      assertTexts();
    }
  }

  // ------------------------------------------------------------------------------------------------ usage view type

  public void testUsageViewTypesOfNamesMadeBySameFileMacros() throws Exception {
    highlightAll();
    PsiFile s = myFiles.get("s.erl");
    for (String[] c : new String[][]{{"mf})", "record field"}, {"mr, {mf}", "record"}, {"mt() :: integer", "type"},
      {"fixed() -> ok)", "function"}, {"made).", "function"}}) {
      int at = s.getText().indexOf(c[0]);
      String type = inBackground(() -> UsageViewUtil.getType(s.findElementAt(at)));
      System.out.println("LAYER-R6 type of " + c[0] + ": " + type);
      assertEquals(c[0], c[1], type);
    }
  }
}
