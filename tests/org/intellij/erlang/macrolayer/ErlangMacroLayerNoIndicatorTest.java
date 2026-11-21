package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.TargetElementUtil;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.actionSystem.impl.SimpleDataContext;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.refactoring.rename.PsiElementRenameHandler;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 6, items 8 and 9 (from the IDE run of 88d921a3): the update of the rename action runs on a coroutine thread with
 * no progress indicator. The layer refused such a caller (round 5, fix 6), so right after a key press the rename of a
 * function made by a header macro found no target and did nothing. Now such a caller builds and waits under an
 * indicator of its own, which a write action cancels; and the rename of a header body variable goes through the
 * platform's rename action to the whole rename.
 */
public class ErlangMacroLayerNoIndicatorTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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
    ErlangMacroLayerLog.enableForTests(true);
    for (String n : List.of("h.hrl", "i.erl", "i2.erl", "s.erl")) {
      myOriginal.put(n, read(Paths.get("testData/macroLayer/nav", n)));
      myFiles.put(n, myFixture.addFileToProject("nav/" + n, myOriginal.get(n)));
    }
    for (PsiFile f : myFiles.values()) {
      myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
      myFixture.doHighlighting();
    }
    ErlangMacroLayerLog.reset();
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ErlangMacroLayerLog.enableForTests(false);
    }
    finally {
      super.tearDown();
    }
  }

  /** A key press at the end of the file, committed and saved (the fixture reopens files). */
  private void typeInto(String file) {
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(myFiles.get(file));
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getTextLength(), "%"));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    FileDocumentManager.getInstance().saveDocument(d);
    myOriginal.put(file, myOriginal.get(file) + "%");
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

  private void assertTexts(String... edits) {
    Map<String, String> expected = new LinkedHashMap<>(myOriginal);
    for (int i = 0; i < edits.length; i += 3) {
      String text = expected.get(edits[i]);
      assertTrue(edits[i] + " has no " + edits[i + 1], text.contains(edits[i + 1]));
      expected.put(edits[i], text.replace(edits[i + 1], edits[i + 2]));
    }
    for (String n : myFiles.keySet()) assertEquals(n, expected.get(n), myFiles.get(n).getText());
  }

  /** The update of the platform's rename action, on a pooled thread in a read action with no progress indicator. */
  private static boolean renameActionEnabledWithoutIndicator(DataContext context) throws Exception {
    AnAction rename = ActionManager.getInstance().getAction(IdeActions.ACTION_RENAME);
    return inBackgroundWithoutIndicator(() -> {
      assertNull(ProgressManager.getGlobalProgressIndicator());
      AnActionEvent e = AnActionEvent.createEvent(context, rename.getTemplatePresentation().clone(), ActionPlaces.KEYBOARD_SHORTCUT,
                                                   ActionUiKind.NONE, null);
      rename.update(e);
      return e.getPresentation().isEnabled();
    });
  }

  public void testTargetAndRenameOfAFunctionMadeByAHeaderMacroWithoutAnIndicatorRightAfterTyping() throws Exception {
    typeInto("i.erl");
    Editor editor = caretAt("i.erl", "hmade(), hfixed()");
    int caret = editor.getCaretModel().getOffset();
    String target = inBackgroundWithoutIndicator(() -> {
      PsiElement t = TargetElementUtil.getInstance().findTargetElement(editor, TargetElementUtil.getInstance().getAllAccepted(), caret);
      return t == null ? "null" : t.getContainingFile().getName() + ":" + t.getTextOffset() + ":" + t.getText();
    });
    int declaration = myFiles.get("i.erl").getText().indexOf("hmade).");
    assertEquals("the target without an indicator, right after the key press", "i.erl:" + declaration + ":hmade", target);
    assertTrue("the rename action is available", renameActionEnabledWithoutIndicator(((EditorEx)editor).getDataContext()));
    assertTrue("built under the layer's own indicator", ErlangMacroLayer.OWN_INDICATOR_REQUESTS.get() > 0);
  }

  /** The reason of round 5's fix 6: a key press must not wait for the build of a caller that has no indicator. */
  public void testAWriteActionCancelsTheBuildOfACallerWithoutAnIndicator() throws Exception {
    typeInto("s.erl");
    PsiFile s = myFiles.get("s.erl");
    CountDownLatch inBuild = new CountDownLatch(1);
    ErlangMacroLayer.setBuildHookForTests(() -> {
      inBuild.countDown();
      long end = System.nanoTime() + 20_000_000_000L;
      while (System.nanoTime() < end) {
        ProgressManager.checkCanceled();
        try {
          Thread.sleep(5);
        }
        catch (InterruptedException e) {
          Thread.currentThread().interrupt();
          return;
        }
      }
    }, getTestRootDisposable());
    int at = s.getText().indexOf("Z + ?CONST");
    Future<String> caller = ApplicationManager.getApplication().executeOnPooledThread(() -> ReadAction.compute(() -> {
      PsiReference ref = s.findReferenceAt(at);
      PsiElement t = ref == null ? null : ref.resolve();
      return String.valueOf(t);
    }));
    assertTrue("the caller without an indicator builds", inBuild.await(30, TimeUnit.SECONDS));
    long t0 = System.nanoTime();
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(s);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getTextLength(), "%"));
    long waitedMs = (System.nanoTime() - t0) / 1_000_000;
    System.out.println("LAYER-NI the key press waited " + waitedMs + " ms for the caller's build");
    assertEquals("master's answer for the cancelled question", "null", caller.get(30, TimeUnit.SECONDS));
    // a key press that waited for the caller's build would wait for it to end; 30 s is only a safety bound
    assertTrue("the key press waited for the build (" + waitedMs + " ms)", waitedMs < 30_000);
    assertTrue(ErlangMacroLayerLog.events().stream().anyMatch(e -> e.line().contains("no-indicator:write-action")));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
  }

  /**
   * Item 9: the rename of a variable bound in a header macro body, through the platform's rename action (its update on
   * a thread with no indicator, then its action), right after a key press and from the body itself. The new name is
   * given as the dialog's default; the usage search runs on the UI thread here, so the layer may build there (in the IDE
   * it runs in the background).
   */
  public void testRenameOfAHeaderBodyVariableThroughTheRenameAction() throws Exception {
    ErlangMacroLayer.setEdtBuildForTests(true, getTestRootDisposable());
    for (String[] c : new String[][]{{"i.erl", "Xh.\n", "Xh2"}, {"h.hrl", "Xh = 1)", "Xh3"}}) {
      String old = c[0].equals("i.erl") ? "Xh" : "Xh2";
      if (c[0].equals("i.erl")) typeInto("i.erl");
      Editor editor = caretAt(c[0], c[0].equals("i.erl") ? "Xh.\n" : old + " = 1)");
      DataContext context = ((EditorEx)editor).getDataContext();
      assertTrue("the rename action is available at " + c[0], renameActionEnabledWithoutIndicator(context));
      AnAction rename = ActionManager.getInstance().getAction(IdeActions.ACTION_RENAME);
      DataContext withName = SimpleDataContext.builder().setParent(context).add(PsiElementRenameHandler.DEFAULT_NAME, c[2]).build();
      AnActionEvent e = AnActionEvent.createEvent(withName, rename.getTemplatePresentation().clone(), ActionPlaces.KEYBOARD_SHORTCUT,
                                                  ActionUiKind.NONE, null);
      rename.update(e);
      assertTrue(e.getPresentation().isEnabled());
      rename.actionPerformed(e);
      PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
      FileDocumentManager.getInstance().saveAllDocuments();
      for (String n : List.of("h.hrl", "i.erl", "i2.erl")) {
        String text = myFiles.get(n).getText();
        assertFalse(n + " still has " + old + " after the rename from " + c[0] + ":\n" + text, text.contains(old + " =") || text.contains(old + "."));
        assertTrue(n + " has " + c[2], text.contains(c[2]));
      }
    }
  }
}
