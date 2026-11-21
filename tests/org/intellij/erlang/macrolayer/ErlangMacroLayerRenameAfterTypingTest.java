package org.intellij.erlang.macrolayer;

import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.find.findUsages.FindUsagesHandlerFactory;
import com.intellij.find.findUsages.FindUsagesManager;
import com.intellij.find.findUsages.FindUsagesOptions;
import com.intellij.find.impl.FindManagerImpl;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.refactoring.rename.RenameProcessor;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.usageView.UsageInfo;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Paths;
import java.util.*;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 5 follow-up: rename and find usages started right after a key press, inside the quiet time of the typing rule
 * (left at its default) and before highlighting has built the new copy. Their searches run as in the IDE: on a pooled
 * thread under a progress indicator that is not the daemon's. They must get the layer's answers, or a rename edits only
 * part of the code.
 */
public class ErlangMacroLayerRenameAfterTypingTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private final Map<String, PsiFile> myFiles = new LinkedHashMap<>();

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    for (String n : List.of("h.hrl", "i.erl", "i2.erl", "s.erl")) {
      myFiles.put(n, myFixture.addFileToProject("nav/" + n, read(Paths.get("testData/macroLayer/nav", n))));
    }
    for (PsiFile f : myFiles.values()) {
      myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
      myFixture.doHighlighting();
    }
  }

  private PsiElement target(String file, String marker) throws Exception {
    PsiFile f = myFiles.get(file);
    int at = f.getText().indexOf(marker);
    return inBackgroundWithIndicator(() -> {
      PsiReference ref = f.findReferenceAt(at);
      return ref == null ? null : ref.resolve();
    });
  }

  /** A key press: one character in a comment at the end of the file, committed. Returns System.nanoTime() of it. */
  private long typeInto(String file) {
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(myFiles.get(file));
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getTextLength(), "%"));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    return System.nanoTime();
  }

  private static String where(Collection<? extends UsageInfo> usages) throws Exception {
    return inBackgroundWithIndicator(() -> {
      Set<String> r = new TreeSet<>();
      for (UsageInfo u : usages) {
        PsiElement e = u.getElement();
        if (e == null) continue;
        PsiFile f = e.getContainingFile();
        r.add(f.getName() + ":" + (StringUtil.offsetToLineNumber(f.getText(), e.getTextRange().getStartOffset()) + 1));
      }
      return String.join(" ", r);
    });
  }

  /** The usage search of rename, as BaseRefactoringProcessor runs it: findUsages() in the background. */
  private String renameUsages(PsiElement target, String newName) throws Exception {
    RenameProcessor p = new RenameProcessor(getProject(), target, newName, false, false);
    return where(List.of(inBackgroundWithIndicator(p::findUsages)));
  }

  /** Find usages as the IDE runs it: the platform's handler and usage searcher, the search in the background under an indicator. */
  private String findUsages(PsiElement target) throws Exception {
    FindUsagesManager manager = ((FindManagerImpl)com.intellij.find.FindManager.getInstance(getProject())).getFindUsagesManager();
    FindUsagesHandler handler = manager.getFindUsagesHandler(target, FindUsagesHandlerFactory.OperationMode.USAGES_WITH_DEFAULT_OPTIONS);
    assertNotNull(handler);
    FindUsagesOptions options = handler.getFindUsagesOptions(null);
    com.intellij.usages.UsageSearcher searcher =
      FindUsagesManager.createUsageSearcher(handler, handler.getPrimaryElements(), handler.getSecondaryElements(), options);
    List<UsageInfo> found = inBackgroundWithIndicator(() -> {
      List<UsageInfo> r = Collections.synchronizedList(new ArrayList<>());
      searcher.generate(u -> {
        if (u instanceof com.intellij.usages.UsageInfo2UsageAdapter a) r.add(a.getUsageInfo());
        return true;
      });
      return new ArrayList<>(r);
    });
    return where(found);
  }

  private static void assertInsideTheQuietTime(long typedAt) {
    long ms = (System.nanoTime() - typedAt) / 1_000_000;
    System.out.println("LAYER-RAT search ended " + ms + " ms after the key press");
    assertTrue("the search must end inside the quiet time: " + ms + " ms", ms < ErlangMacroLayer.quietMillis());
  }

  public void testRenameOfAHeaderBodyVariableRightAfterTyping() throws Exception {
    PsiElement t = target("i.erl", "Xh.\n");
    String expected = renameUsages(t, "Xh2");
    System.out.println("LAYER-RAT Xh before typing: " + expected);
    long typed = typeInto("i.erl");
    String after = renameUsages(t, "Xh2");
    assertInsideTheQuietTime(typed);
    System.out.println("LAYER-RAT Xh right after typing: " + after);
    assertEquals(expected, after);
  }

  public void testRenameOfAFunctionMadeByAMacroRightAfterTyping() throws Exception {
    PsiElement t = target("s.erl", "made(), fixed()");
    String expected = renameUsages(t, "made2");
    System.out.println("LAYER-RAT made before typing: " + expected);
    long typed = typeInto("s.erl");
    String after = renameUsages(t, "made2");
    assertInsideTheQuietTime(typed);
    System.out.println("LAYER-RAT made right after typing: " + after);
    assertEquals(expected, after);
  }

  public void testFindUsagesOfAFunctionMadeByAMacroRightAfterTyping() throws Exception {
    PsiElement t = target("s.erl", "made(), fixed()");
    String expected = findUsages(t);
    System.out.println("LAYER-RAT find made before typing: " + expected);
    long typed = typeInto("s.erl");
    String after = findUsages(t);
    assertInsideTheQuietTime(typed);
    System.out.println("LAYER-RAT find made right after typing: " + after);
    assertEquals(expected, after);
  }
}
