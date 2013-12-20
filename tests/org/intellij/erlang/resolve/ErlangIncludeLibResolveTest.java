package org.intellij.erlang.resolve;


import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

public class ErlangIncludeLibResolveTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
  }

  @Override
  protected String getTestDataPath() {
    return "testData/resolve/includeLib/";
  }

  public void testLatestVersionIsUsed() throws Exception {
    myFixture.configureByFiles("testappuser.erl",
      "testapp-1.0/ebin/testapp.app", "testapp-1.0/include/testapp.hrl",
      "testapp-2.0/ebin/testapp.app", "testapp-2.0/include/testapp.hrl");

    doTestAppResolveTest("testapp-2.0");
  }

  public void testNoVersionIsUsed() throws Exception {
    myFixture.configureByFiles("testappuser.erl",
      "testapp-1.0/ebin/testapp.app", "testapp-1.0/include/testapp.hrl",
      "testapp-2.0/ebin/testapp.app", "testapp-2.0/include/testapp.hrl",
      "testapp/ebin/testapp.app", "testapp/include/testapp.hrl");

    doTestAppResolveTest("testapp");
  }

  public void test375() throws Exception {
    myFixture.configureByFiles("testappuser.erl",
      "testapp-1.0/ebin/testapp.app", "testapp-1.0/include/testapp.hrl",
      "patches/testapp/testapp.app", "patches/include/testapp.hrl");
    doTestAppResolveTest("testapp-1.0");
  }

  private void doTestAppResolveTest(String expectedAppDirName) throws Exception {
    PsiFile testappuserErl = myFixture.getFile();
    assertTrue(testappuserErl instanceof ErlangFile);
    List<ErlangFile> directlyIncludedFiles = ErlangPsiImplUtil.getDirectlyIncludedFiles((ErlangFile) testappuserErl);
    assertEquals(1, directlyIncludedFiles.size());
    PsiDirectory ebinDirectory = directlyIncludedFiles.get(0).getParent();
    assertNotNull(ebinDirectory);
    PsiDirectory appDir = ebinDirectory.getParent();
    assertNotNull(appDir);
    assertEquals(expectedAppDirName, appDir.getName());
  }
}
