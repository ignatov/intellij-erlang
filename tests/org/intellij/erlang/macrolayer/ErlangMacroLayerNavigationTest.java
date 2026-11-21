package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.*;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Paths;
import java.util.*;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 5: navigation into and out of macros, on testData/macroLayer/nav (erlc compiles s.erl, i.erl and i2.erl).
 * s.erl uses same-file macros; i.erl and i2.erl use the macros of h.hrl. Navigation is computed on a pooled thread in
 * a read action, as the IDE does; every target is reported as file:line:text of its navigation element.
 */
public class ErlangMacroLayerNavigationTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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
    ErlangMacroLayerLog.enableForTests(true);
    ErlangMacroLayerLog.reset();
    for (String n : List.of("h.hrl", "i.erl", "i2.erl", "s.erl")) {
      myFiles.put(n, myFixture.addFileToProject("nav/" + n, read(Paths.get("testData/macroLayer/nav", n))));
    }
    for (PsiFile f : myFiles.values()) {
      myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
      myFixture.doHighlighting();
    }
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      assertEquals("a copy was built on the UI thread", List.of(), uiThreadBuilds());
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      ErlangMacroLayerLog.enableForTests(false);
      super.tearDown();
    }
  }

  static String where(PsiElement t) {
    if (t == null) return "-";
    PsiElement n = t.getNavigationElement();
    PsiFile f = n.getContainingFile();
    if (f == null) return "?" + t;
    assertNull("an element of a copy left the layer: " + n, f.getUserData(ErlangMacroLayer.COPY_OF));
    assertTrue("not physical: " + n, f.isPhysical());
    int line = StringUtil.offsetToLineNumber(f.getText(), n.getTextOffset()) + 1;
    String text = n.getText();
    if (text.length() > 24) text = text.substring(0, 24) + "..";
    return f.getName() + ":" + line + ":" + text.replace('\n', ' ');
  }

  /** Go to declaration at marker+shift: the references at the offset and the go-to handlers. */
  private String navigate(String file, String marker, int shift) throws Exception {
    PsiFile f = myFiles.get(file);
    int at = f.getText().indexOf(marker);
    assertTrue(marker, at >= 0);
    int offset = at + shift;
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    Editor editor = myFixture.getEditor();
    return inBackground(() -> {
      Set<String> r = new TreeSet<>();
      PsiReference ref = f.findReferenceAt(offset);
      if (ref instanceof PsiPolyVariantReference pr) for (ResolveResult rr : pr.multiResolve(false)) r.add(where(rr.getElement()));
      if (ref != null) r.add(where(ref.resolve()));
      PsiElement leaf = f.findElementAt(offset);
      for (GotoDeclarationHandler h : GotoDeclarationHandler.EP_NAME.getExtensionList()) {
        PsiElement[] t = h.getGotoDeclarationTargets(leaf, offset, editor);
        if (t != null) for (PsiElement e : t) r.add(where(e));
      }
      r.remove("-");
      return r.isEmpty() ? "NOTHING" : String.join(" ", r);
    });
  }

  // ------------------------------------------------------------------------------------------------ (c) made by a macro

  public void testRecordMadeByMacro() throws Exception {
    assertEquals("s.erl:11:mr", navigate("s.erl", "#mr{", 1));
    assertEquals("h.hrl:9:hmr", navigate("i.erl", "#hmr{", 1));
  }

  public void testRecordFieldMadeByMacro() throws Exception {
    assertEquals("s.erl:11:mf", navigate("s.erl", "mf = 1", 0));
    assertEquals("h.hrl:9:hmf", navigate("i.erl", "hmf = 1", 0));
  }

  public void testTypeMadeByMacro() throws Exception {
    assertEquals("s.erl:12:mt", navigate("s.erl", "mt().\n", 0));
    assertEquals("h.hrl:10:hmt", navigate("i.erl", "hmt().\n", 0));
  }
}
