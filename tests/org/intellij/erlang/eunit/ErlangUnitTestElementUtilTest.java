package org.intellij.erlang.eunit;

import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;

public class ErlangUnitTestElementUtilTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
  }

  @Override
  protected String getTestDataPath() {
    return "testData/eunit/config/";
  }

  public void testFunctionSelection() throws Exception {
    myFixture.configureByFiles("tests1.erl", "tests2.erl");
    Collection<ErlangFunction> functions = ErlangUnitTestElementUtil.findFunctionTestElements(myFixture.getElementAtCaret());

    assertNotNull(functions);
    assertEquals(1, functions.size());
    assertEquals(myFixture.getElementAtCaret(), functions.iterator().next());
  }

  public void testSingleFileSelection() throws Exception {
    myFixture.configureByFiles("tests1.erl", "tests2.erl");
    MyMockDataContext dataContext = new MyMockDataContext(myFixture.getFile().getVirtualFile());
    Collection<ErlangFile> files = ErlangUnitTestElementUtil.findFileTestElements(myFixture.getProject(), dataContext);

    assertNotNull(files);
    assertEquals(1, files.size());
    assertEquals(myFixture.getFile(), files.iterator().next());
  }

  public void testMultipleFilesSelection() throws Exception {
    PsiFile[] psiFiles = myFixture.configureByFiles("tests1.erl", "tests2.erl");
    MyMockDataContext dataContext = new MyMockDataContext(getVirtualFiles(psiFiles));
    Collection<ErlangFile> files = ErlangUnitTestElementUtil.findFileTestElements(myFixture.getProject(), dataContext);

    assertNotNull(files);
    assertEquals(psiFiles.length, files.size());
    assertContainsElements(files, psiFiles);
  }

  private static VirtualFile[] getVirtualFiles(PsiFile[] psiFiles) {
    VirtualFile[] virtualFiles = new VirtualFile[psiFiles.length];
    for (int i = 0; i < psiFiles.length; i++) {
      virtualFiles[i] = psiFiles[i].getVirtualFile();
    }
    return virtualFiles;
  }

  private static class MyMockDataContext implements DataContext {
    private final VirtualFile[] myVirtualFiles;

    private MyMockDataContext(VirtualFile ... virtualFiles) {
      myVirtualFiles = virtualFiles;
    }

    @Nullable
    @Override
    public Object getData(@NonNls String dataId) {
      if (CommonDataKeys.VIRTUAL_FILE_ARRAY.getName().equals(dataId)) {
        return myVirtualFiles;
      }
      return null;
    }
  }
}
