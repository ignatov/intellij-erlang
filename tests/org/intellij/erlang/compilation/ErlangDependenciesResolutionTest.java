package org.intellij.erlang.compilation;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.util.ThrowableComputable;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.ModuleTestCase;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.editor.ErlangModuleType;
import org.intellij.erlang.jps.builder.ErlangModuleBuildOrderDescriptor;

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * @author savenko
 */
public class ErlangDependenciesResolutionTest extends ModuleTestCase {
  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpModule();
    String sourceDirectoryPath = getSourceDirectoryPath();
    if (directoryExists(sourceDirectoryPath)) {
      setUpSourcePath(sourceDirectoryPath);
    }
    String testsDirectoryPath = getTestsDirectoryPath();
    if (directoryExists(testsDirectoryPath)) {
      setUpTestsPath(testsDirectoryPath);
    }
  }

  @Override
  protected ModuleType getModuleType() {
    return ErlangModuleType.getInstance();
  }

  private void setUpSourcePath(String sourcePath) throws IOException {
    doSetUpSourcePath(sourcePath, false);
  }

  private void setUpTestsPath(String sourcePath) throws IOException {
    doSetUpSourcePath(sourcePath, true);
  }

  private void doSetUpSourcePath(final String sourcePath, boolean isTestSource) throws IOException {
    final String sourceDirectoryName = isTestSource ? "test" : "src";
    final VirtualFile moduleFile = myModule.getModuleFile();
    assertNotNull(moduleFile);
    VirtualFile moduleSourceDir = ApplicationManager.getApplication().runWriteAction(new ThrowableComputable<VirtualFile, IOException>() {
      @Override
      public VirtualFile compute() throws IOException {
        VirtualFile moduleSourceDir = VfsUtil.createDirectoryIfMissing(moduleFile.getParent(), sourceDirectoryName);
        FileUtil.copyDirContent(new File(sourcePath), new File(moduleSourceDir.getPath()));
        VfsUtil.markDirtyAndRefresh(false, true, true, moduleSourceDir);
        return moduleSourceDir;
      }
    });
    PsiTestUtil.addSourceRoot(myModule, moduleSourceDir, isTestSource);
  }

  private String getSourceDirectoryPath() {
    return getTestDataRoot() + "src";
  }

  private String getTestsDirectoryPath() {
    return getTestDataRoot() + "test";
  }

  private String getTestDataRoot() {
    return "testData/compilation/" + getTestName(false) + "/";
  }

  private static boolean directoryExists(String dirPath) {
    File dir = new File(dirPath);
    return dir.exists() && dir.isDirectory();
  }

  public void testCyclicDependency() throws Exception {
    try {
      ErlangPrepareDependenciesCompileTask.getModuleBuildOrder(myModule);
      fail("Expected a cyclic dependency exception to be thrown.");
    } catch (ErlangPrepareDependenciesCompileTask.CyclicDependencyFoundException ignored) {
    }
  }

  public void testDependenciesAreCompiledFirst() throws Exception {
    ErlangModuleBuildOrderDescriptor moduleBuildOrder = ErlangPrepareDependenciesCompileTask.getModuleBuildOrder(myModule);
    assertSameErlangModules(moduleBuildOrder.myOrderedErlangModulePaths, "parse_transform1", "parse_transform2", "behaviour1", "module1");
  }

  public void testTestsDependency() throws Exception {
    ErlangModuleBuildOrderDescriptor moduleBuildOrder = ErlangPrepareDependenciesCompileTask.getModuleBuildOrder(myModule);
    assertSameErlangModules(moduleBuildOrder.myOrderedErlangModulePaths, "src_parse_transform");
    assertSameErlangModules(moduleBuildOrder.myOrderedErlangTestModulePaths, "test_parse_transform", "test");
  }

  private static void assertSameErlangModules(List<String> modulePaths, String... expectedModules) {
    List<String> actualModules = ContainerUtil.map(modulePaths, new Function<String, String>() {
      @Override
      public String fun(String path) {
        return FileUtil.getNameWithoutExtension(new File(path));
      }
    });
    assertOrderedEquals(actualModules, expectedModules);
  }
}
