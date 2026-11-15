package org.intellij.erlang.highlighting;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;

import java.util.List;

public class ErlangNoSdkHighlightingTest extends ErlangHighlightingTestBase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return null;
      }
    };
  }

  public void testAutoImportWithoutErlangSdk() {
    addErlangModuleWithFunctionsAutoImportedSince28();
    doTest();
  }

  public void testAutoImportCompletionWithoutErlangSdk() {
    addErlangModuleWithFunctionsAutoImportedSince28();
    myFixture.configureByText("a.erl", "f(P) -> exit_sig<caret>");
    myFixture.completeBasic();
    List<String> strings = myFixture.getLookupElementStrings();
    assertNotNull(strings);
    assertContainsElements(strings, "exit_signal");
  }
}
