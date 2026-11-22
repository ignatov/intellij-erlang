package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.TargetElementUtil;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.refactoring.BaseRefactoringProcessor;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Paths;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 5: rename of names that macros make or use, on testData/macroLayer/nav. The fixture renames on the UI
 * thread, so these tests let the layer build there (in the IDE the usage search of rename runs in the background).
 * Every expected result was compiled with erlc (see DEFECTS5-REPORT.md).
 */
public class ErlangMacroLayerRenameTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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
    ErlangMacroLayer.setEdtBuildForTests(true, getTestRootDisposable());
    for (String n : List.of("h.hrl", "i.erl", "i2.erl", "s.erl")) {
      myOriginal.put(n, read(Paths.get("testData/macroLayer/nav", n)));
      myFiles.put(n, myFixture.addFileToProject("nav/" + n, myOriginal.get(n)));
    }
  }

  private void highlightAll() {
    for (PsiFile f : myFiles.values()) {
      myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
      myFixture.doHighlighting();
    }
  }

  /** Renames the element that go-to or rename takes at marker+shift. */
  private void rename(String file, String marker, int shift, String newName) {
    highlightAll();
    PsiFile f = myFiles.get(file);
    int at = f.getText().indexOf(marker);
    assertTrue(marker, at >= 0);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.getEditor().getCaretModel().moveToOffset(at + shift);
    PsiElement target = TargetElementUtil.findTargetElement(myFixture.getEditor(),
      TargetElementUtil.ELEMENT_NAME_ACCEPTED | TargetElementUtil.REFERENCED_ELEMENT_ACCEPTED);
    assertNotNull("no target at " + marker, target);
    myFixture.renameElement(target, newName);
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
  }

  /** The texts expected after the rename: the originals with the given replacements, per file ("file", "from", "to"). */
  private void assertTexts(String... edits) {
    Map<String, String> expected = new LinkedHashMap<>(myOriginal);
    for (int i = 0; i < edits.length; i += 3) {
      String text = expected.get(edits[i]);
      assertTrue(edits[i] + " has no " + edits[i + 1], text.contains(edits[i + 1]));
      expected.put(edits[i], text.replace(edits[i + 1], edits[i + 2]));
    }
    for (String n : myFiles.keySet()) assertEquals(n, expected.get(n), myFiles.get(n).getText());
  }

  // ------------------------------------------------------------------------------------------------ same file

  public void testFunctionNamedInSameFileMacroBody() {
    rename("s.erl", "fixed()}", 0, "fixed2");
    assertTexts("s.erl", "fixed() -> ok)", "fixed2() -> ok)", "s.erl", "made/0, fixed/0", "made/0, fixed2/0",
                "s.erl", "{made(), fixed()}", "{made(), fixed2()}");
  }

  public void testFunctionNamedInMacroArgument() {
    rename("s.erl", "made(), fixed()", 0, "made2");
    assertTexts("s.erl", "?MK(made).", "?MK(made2).", "s.erl", "made/0, fixed/0", "made2/0, fixed/0",
                "s.erl", "{made(), fixed()}", "{made2(), fixed()}");
  }

  public void testRecordMadeBySameFileMacroBody() {
    rename("s.erl", "#mr{", 1, "mr2");
    assertTexts("s.erl", "-record(mr, {mf})", "-record(mr2, {mf})", "s.erl", "#mr{mf = 1}", "#mr2{mf = 1}");
  }

  public void testFieldMadeBySameFileMacroBody() {
    rename("s.erl", "mf = 1", 0, "mf2");
    assertTexts("s.erl", "-record(mr, {mf})", "-record(mr, {mf2})", "s.erl", "#mr{mf = 1}", "#mr{mf2 = 1}");
  }

  // ------------------------------------------------------------------------------------------------ header used by two files

  public void testFunctionNamedInHeaderMacroBody() {
    rename("i.erl", "hfixed()}", 0, "hfixed2");
    assertTexts("h.hrl", "hfixed() -> ok)", "hfixed2() -> ok)",
                "i.erl", "hmade/0, hfixed/0", "hmade/0, hfixed2/0", "i.erl", "{hmade(), hfixed()}", "{hmade(), hfixed2()}",
                "i2.erl", "hmade/0, hfixed/0", "hmade/0, hfixed2/0", "i2.erl", "{hmade(), hfixed()}", "{hmade(), hfixed2()}");
  }

  public void testRecordAndFieldMadeByHeaderMacroBody() {
    rename("i.erl", "#hmr{", 1, "hmr2");
    assertTexts("h.hrl", "-record(hmr, {hmf})", "-record(hmr2, {hmf})",
                "i.erl", "#hmr{hmf = 1}", "#hmr2{hmf = 1}", "i2.erl", "#hmr{hmf = 1}", "#hmr2{hmf = 1}");
  }

  public void testFieldMadeByHeaderMacroBody() {
    rename("i.erl", "hmf = 1", 0, "hmf2");
    assertTexts("h.hrl", "-record(hmr, {hmf})", "-record(hmr, {hmf2})",
                "i.erl", "#hmr{hmf = 1}", "#hmr{hmf2 = 1}", "i2.erl", "#hmr{hmf = 1}", "#hmr{hmf2 = 1}");
  }

  public void testVariableFromHeaderMacroBody() {
    rename("i.erl", "Xh.\n", 0, "Xh2");
    assertTexts("h.hrl", "Xh = 1", "Xh2 = 1", "i.erl", "?H_X, Xh.", "?H_X, Xh2.", "i2.erl", "?H_X, Xh.", "?H_X, Xh2.");
  }

  /** Another includer already has the new name in a function that uses the macro: a conflict, and nothing changes. */
  public void testVariableFromHeaderMacroBodyConflict() {
    PsiFile i3 = myFixture.addFileToProject("nav/i3.erl", "-module(i3).\n-include(\"h.hrl\").\n-export([k/1]).\nk(Xh2) -> ?H_X, Xh2.\n");
    try {
      rename("i.erl", "Xh.\n", 0, "Xh2");
      fail("no conflict");
    }
    catch (BaseRefactoringProcessor.ConflictsInTestsException e) {
      System.out.println("LAYER-REN conflict: " + e.getMessages());
      assertTrue(e.getMessages().toString(), e.getMessages().toString().contains("i3.erl"));
    }
    assertTexts();
    assertEquals("-module(i3).\n-include(\"h.hrl\").\n-export([k/1]).\nk(Xh2) -> ?H_X, Xh2.\n", i3.getText());
  }

  /**
   * The body of H_CALL calls h_target/1, which both includers define. Renaming the function of one includer would
   * either leave the header calling the old name (master: i.erl stops compiling) or change the header for i2.erl too.
   */
  public void testFunctionCalledInSharedHeaderMacroBodyConflicts() {
    try {
      rename("i.erl", "h_target(X) -> X.", 0, "h_target2");
      fail("no conflict");
    }
    catch (BaseRefactoringProcessor.ConflictsInTestsException e) {
      System.out.println("LAYER-REN conflict: " + e.getMessages());
      assertTrue(e.getMessages().toString(), e.getMessages().toString().contains("i2.erl"));
    }
    assertTexts();
  }

  /** The macro is used only by one includer: renaming its function also renames the call in the header body. */
  public void testFunctionCalledInHeaderMacroBodyOfOneIncluder() {
    WriteCommandAction.runWriteCommandAction(getProject(), () -> {
      try {
        myFiles.get("i2.erl").getVirtualFile().delete(this);
      }
      catch (java.io.IOException e) {
        throw new RuntimeException(e);
      }
    });
    myFiles.remove("i2.erl");
    myOriginal.remove("i2.erl");
    rename("i.erl", "h_target(X) -> X.", 0, "h_target2");
    assertTexts("h.hrl", "h_target(X))", "h_target2(X))", "i.erl", "h_target(X) -> X.", "h_target2(X) -> X.",
                "i.erl", "?H_CALL(h_target(Q))", "?H_CALL(h_target2(Q))");
  }
}
