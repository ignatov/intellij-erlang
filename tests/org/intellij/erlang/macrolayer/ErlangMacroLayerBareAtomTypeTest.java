package org.intellij.erlang.macrolayer;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

/** Issue 1554: a bare atom in a type is not a use of a type that a macro makes. */
public class ErlangMacroLayerBareAtomTypeTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R29/", ErlangSdkRelease.V_29_0);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    ErlangMacroLayer.setEdtBuildForTests(true, getTestRootDisposable());
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
  }

  private static PsiReference refAt(PsiFile file, int offset) {
    PsiReference ref = file.findReferenceAt(offset);
    assertNotNull("reference at " + offset, ref);
    return ref;
  }

  public void testBareAtomDoesNotResolveToTypeMadeByMacro() {
    PsiFile file = myFixture.configureByText("a.erl", """
      -module(a).
      -export_type([t/0]).
      -define(DEF(N), -type N() :: []).
      ?DEF(foo).
      -type t() :: foo | {foo}.
      -type u() :: foo().
      """);
    String text = file.getText();
    int t = text.indexOf("-type t() :: ");
    int bare1 = text.indexOf("foo", t);
    int bare2 = text.indexOf("foo", bare1 + 3);
    int call = text.indexOf("foo()", text.indexOf("-type u() :: "));

    PsiElement made = refAt(file, call + 1).resolve();
    assertNotNull("foo() in u() resolves to the type made by ?DEF(foo)", made);
    assertTrue("resolves into ?DEF(foo), not " + made.getText(), made.getTextRange().getStartOffset() >= text.indexOf("?DEF(foo)"));
    assertTrue(made.getTextRange().getEndOffset() <= text.indexOf("?DEF(foo)") + "?DEF(foo)".length());

    assertNull("the first bare foo in t()", refAt(file, bare1 + 1).resolve());
    assertNull("the second bare foo in t()", refAt(file, bare2 + 1).resolve());
  }
}
