package org.intellij.erlang.documentation;

import com.intellij.codeInsight.TargetElementUtil;
import com.intellij.codeInsight.navigation.CtrlMouseData;
import com.intellij.codeInsight.navigation.actions.GotoDeclarationOrUsageHandler2;
import com.intellij.find.FindManager;
import com.intellij.find.impl.FindManagerImpl;
import com.intellij.lang.documentation.ide.IdeDocumentationTargetProvider;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.platform.backend.documentation.DocumentationTarget;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.refactoring.rename.RenameHandler;
import com.intellij.refactoring.rename.RenameHandlerRegistry;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

/**
 * Quick documentation, go to declaration, ctrl-hover, find usages and the rename lookup at a predefined macro
 * ({@code ?OTP_RELEASE}: the owner's error "Unexpected element ?OTP_RELEASE ... UsageViewNodeTextLocation") and at an
 * undefined one, through the platform's own target computations. A logged error fails the test. Uses only master's
 * classes, so that it also runs on master.
 */
public class ErlangMacroUseDocumentationTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String[] USES = {"?MODULE", "?MODULE_STRING", "?FILE", "?LINE", "?MACHINE", "?FUNCTION_NAME", "?FUNCTION_ARITY",
    "?OTP_RELEASE", "?FEATURE_AVAILABLE(maybe_expr)", "?FEATURE_ENABLED(maybe_expr)", "?NOT_DEFINED_ANYWHERE"};

  public void testMacroUsesThroughThePlatformsTargets() {
    StringBuilder body = new StringBuilder("-module(doc).\n-export([f/0]).\nf() ->\n  [");
    for (int i = 0; i < USES.length; i++) body.append(i == 0 ? "" : ",\n   ").append(USES[i]);
    body.append("].\n");
    PsiFile f = myFixture.configureByText("doc.erl", body.toString());
    List<String> seen = new ArrayList<>();
    for (String use : USES) {
      int offset = f.getText().indexOf(use) + 2;
      myFixture.getEditor().getCaretModel().moveToOffset(offset);
      for (DocumentationTarget t : IdeDocumentationTargetProvider.getInstance(getProject()).documentationTargets(myFixture.getEditor(), f, offset)) {
        t.computePresentation(); // the owner's stack: PsiElementDocumentationTarget.computePresentation
      }
      CtrlMouseData hover = GotoDeclarationOrUsageHandler2.getCtrlMouseData(myFixture.getEditor(), f, offset);
      if (hover != null) hover.getHintText();
      com.intellij.model.psi.impl.TargetsKt.targetSymbols(f, offset);
      PsiElement target = TargetElementUtil.findTargetElement(myFixture.getEditor(), TargetElementUtil.getInstance().getAllAccepted());
      if (target != null) ((FindManagerImpl)FindManager.getInstance(getProject())).getFindUsagesManager().canFindUsages(target);
      DataContext ctx = ((EditorEx)myFixture.getEditor()).getDataContext();
      RenameHandler rename = RenameHandlerRegistry.getInstance().getRenameHandler(ctx);
      seen.add(use + ":" + (target == null ? "-" : target.getClass().getSimpleName()) + ":" + (rename == null ? "no rename" : "rename"));
    }
    System.out.println("MACRO-USE-DOC " + seen);
  }
}
