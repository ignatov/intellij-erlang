package org.intellij.erlang.resolve;

import com.intellij.util.PlatformUtils;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

/**
 * @author savenko
 */
public class ErlangIncludeResolveTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected void setUp() throws Exception {
    super.setUp();
  }

  @Override
  protected String getTestDataPath() {
    return "testData/resolve/include/" + getTestName(true) + "/";
  }

  public void testRelativeToDirectParent() throws Exception {
    doTest("testmodule.erl", "testinclude.hrl");
  }

  public void testRelativeToDirectParentWithDots() throws Exception {
    doTest("src/testmodule.erl", "include/testinclude.hrl");
  }

  public void testRelativeToIncludeDirectory() throws Exception {
    doSmallIdeTest("src/testmodule.erl", "include/testinclude.hrl");
  }

  public void testIncludePathsInRebarConfig() throws Exception {
    doSmallIdeTest("src/testmodule.erl", "include/forTestModule/testinclude.hrl", "rebar.config");
  }

  private void doSmallIdeTest(String... files) throws Exception {
    doTest(true, files);
  }

  private void doTest(String... files) throws Exception {
    doTest(false, files);
  }

  private void doTest(boolean smallIde, String... files) throws Exception {
    if (smallIde) {
      System.setProperty(PlatformUtils.PLATFORM_PREFIX_KEY, PlatformUtils.PYCHARM_PREFIX);
    }
    myFixture.configureByFiles(files);
    ErlangFile file = (ErlangFile) myFixture.getFile();
    List<ErlangFile> directlyIncludedFiles = ErlangPsiImplUtil.getDirectlyIncludedFiles(file);
    assertEquals(1, directlyIncludedFiles.size());
  }
}
