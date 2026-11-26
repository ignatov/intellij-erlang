package org.intellij.erlang.macrolayer;

import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.inBackground;

/**
 * Round 6: in a small IDE the include paths of rebar.config ({@code {erl_opts, [{i, Dir}]}}) take part in include
 * resolution. An edit of rebar.config that makes an include resolve to another header changes the layer's answers,
 * though no VFS structure event and no roots change happens.
 */
public class ErlangMacroLayerRebarConfigTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public ErlangMacroLayerRebarConfigTest() {
    super(true);
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    ErlangMacroLayer.setQuietMillisForTests(0, getTestRootDisposable());
  }

  private static String target(PsiFile f) throws Exception {
    return inBackground(() -> {
      PsiReference ref = f.findReferenceAt(f.getText().lastIndexOf("Xa."));
      PsiElement t = ref == null ? null : ref.resolve();
      return t == null ? "null" : t.getContainingFile().getVirtualFile().getParent().getName() + "/" + t.getContainingFile().getName();
    });
  }

  private static String includedDir(PsiFile f) throws Exception {
    return inBackground(() -> {
      List<ErlangFile> files = ErlangPsiImplUtil.getDirectlyIncludedFiles((ErlangFile)f);
      return files.isEmpty() ? "none" : files.getFirst().getVirtualFile().getParent().getName();
    });
  }

  public void testAnEditOfTheIncludePathsOfRebarConfig() throws Exception {
    PsiFile config = myFixture.addFileToProject("app/rebar.config", "{erl_opts, [{i, \"inc1\"}]}.\n");
    myFixture.addFileToProject("app/src/app.app.src", "{application, app, []}.\n");
    myFixture.addFileToProject("app/inc1/h.hrl", "-define(B, Xa = 1).\n");
    myFixture.addFileToProject("app/inc2/h.hrl", "-define(B, Xb = 1).\n");
    PsiFile a = myFixture.addFileToProject("app/src/a.erl", "-module(a).\n-include(\"h.hrl\").\n-export([f/0]).\nf() -> ?B, Xa.\n");
    assertEquals("inc1", includedDir(a));
    assertEquals("the binding in the body of ?B in inc1/h.hrl", "inc1/h.hrl", target(a));

    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(config);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.replaceString(0, d.getTextLength(), "{erl_opts, [{i, \"inc2\"}]}.\n"));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    assertEquals("master's include resolution follows rebar.config", "inc2", includedDir(a));
    assertEquals("?B of inc2/h.hrl binds Xb, not Xa", "null", target(a));

    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.replaceString(0, d.getTextLength(), "{erl_opts, [{i, \"inc1\"}]}.\n"));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    assertEquals("inc1/h.hrl", target(a));
  }
}
