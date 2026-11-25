package org.intellij.erlang.macrolayer.fuzz.regress;

import com.intellij.find.FindManager;
import com.intellij.lang.documentation.ide.IdeDocumentationTargetProvider;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.backend.documentation.DocumentationTarget;
import com.intellij.psi.ElementDescriptionUtil;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.refactoring.rename.RenamePsiElementProcessor;
import com.intellij.usageView.UsageViewTypeLocation;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

/**
 * Regression tests for the findings of the macro fuzz suite (FINDINGS.md in the fuzz scratch folder). Each method fails
 * on the commit the finding names; the builder moves it next to the fix. A logged error fails a light test by itself.
 */
public class MacroFuzzRegressionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  /** F1: the Project view asks whether usages of a directory can be found when its popup opens. */
  public void testF1CanFindUsagesOfEmptyDirectory() throws Exception {
    VirtualFile dir = myFixture.getTempDirFixture().findOrCreateDir("empty/pkg");
    PsiDirectory psi = PsiManager.getInstance(getProject()).findDirectory(dir);
    assertNotNull(psi);
    FindManager.getInstance(getProject()).canFindUsages(psi);
  }

  /** F1: the same check through the description of a directory whose first child is an empty directory. */
  public void testF1DescriptionOfDirectoryWithEmptySubdirectory() throws Exception {
    VirtualFile dir = myFixture.getTempDirFixture().findOrCreateDir("outer/inner");
    PsiDirectory outer = PsiManager.getInstance(getProject()).findDirectory(dir.getParent());
    assertNotNull(outer);
    ElementDescriptionUtil.getElementDescription(outer, UsageViewTypeLocation.INSTANCE);
  }

  /** F1: a binary file (its first child has no text range either) selected in the Project view. */
  public void testF1RenameProcessorOfBinaryFile() throws Exception {
    VirtualFile f = myFixture.getTempDirFixture().createFile("ebin/x.beam");
    com.intellij.openapi.application.WriteAction.run(() -> f.setBinaryContent(new byte[]{'F', 'O', 'R', '1', 0, 0, 0, 4, 'B', 'E', 'A', 'M'}));
    PsiFile psi = PsiManager.getInstance(getProject()).findFile(f);
    assertNotNull(psi);
    RenamePsiElementProcessor.forElement(psi);
    FindManager.getInstance(getProject()).canFindUsages(psi);
  }

  /** F3: a rename that also searches comments and strings asks the description of the target for that search. */
  public void testF3RenameMacroSearchingCommentsAndStrings() {
    myFixture.configureByText("m.erl", "-module(m).\n-define(<caret>C, 1).\nf() -> ?C. % C\n");
    PsiElement target = com.intellij.codeInsight.TargetElementUtil.findTargetElement(myFixture.getEditor(),
      com.intellij.codeInsight.TargetElementUtil.getInstance().getAllAccepted());
    assertNotNull(target);
    myFixture.renameElement(target, "D", true, true);
  }

  /** F3: the same for a function. */
  public void testF3RenameFunctionSearchingCommentsAndStrings() {
    myFixture.configureByText("m.erl", "-module(m).\n-export([f/0]).\n<caret>f() -> ok. % f\n");
    PsiElement target = com.intellij.codeInsight.TargetElementUtil.findTargetElement(myFixture.getEditor(),
      com.intellij.codeInsight.TargetElementUtil.getInstance().getAllAccepted());
    assertNotNull(target);
    myFixture.renameElement(target, "g", true, true);
  }

  /** F2: Quick Documentation on a predefined macro computes the presentation of the macro use. */
  public void testF2QuickDocPresentationOfPredefinedMacro() {
    myFixture.configureByText("m.erl", "-module(m).\nf() -> ?OTP_<caret>RELEASE.\n");
    int offset = myFixture.getCaretOffset();
    for (DocumentationTarget t : IdeDocumentationTargetProvider.getInstance(getProject())
      .documentationTargets(myFixture.getEditor(), myFixture.getFile(), offset)) {
      t.computePresentation();
    }
  }
}
