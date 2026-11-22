package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler;
import com.intellij.openapi.editor.Editor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.*;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * #1453: a call in the body of a header macro names a function of each includer that uses the macro. With 25
 * includers that each define it (erlc compiles all of them), go to declaration from the header body offers all 25.
 */
public class ErlangMacroLayerManyIncludersTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final int INCLUDERS = 25;

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
  }

  public void testCallInHeaderMacroBodyGoesToMoreThan24Includers() throws Exception {
    List<PsiFile> files = new ArrayList<>();
    Set<String> expected = new TreeSet<>();
    PsiFile header = myFixture.addFileToProject("many/h.hrl", "-define(SEND(P), P ! target()).\n");
    for (int i = 1; i <= INCLUDERS; i++) {
      String name = String.format("u%02d", i);
      files.add(myFixture.addFileToProject("many/" + name + ".erl",
                                           "-module(" + name + ").\n" +
                                           "-include(\"h.hrl\").\n" +
                                           "-export([a/1]).\n" +
                                           "target() -> ok.\n" +
                                           "a(Y) -> ?SEND(Y).\n"));
      expected.add(name + ".erl:4:target() -> ok.");
    }
    files.add(header);
    for (PsiFile f : files) {
      myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
      myFixture.doHighlighting();
    }
    int offset = header.getText().indexOf("target()).");
    myFixture.configureFromExistingVirtualFile(header.getVirtualFile());
    Editor editor = myFixture.getEditor();
    String found = inBackground(() -> {
      Set<String> r = new TreeSet<>();
      PsiElement leaf = header.findElementAt(offset);
      for (GotoDeclarationHandler h : GotoDeclarationHandler.EP_NAME.getExtensionList()) {
        PsiElement[] t = h.getGotoDeclarationTargets(leaf, offset, editor);
        if (t != null) for (PsiElement e : t) r.add(ErlangMacroLayerNavigationTest.where(e));
      }
      return r.size() + " " + String.join(" ", r);
    });
    assertEquals(expected.size() + " " + String.join(" ", expected), found);
  }
}
