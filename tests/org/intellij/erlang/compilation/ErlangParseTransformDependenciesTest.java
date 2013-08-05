package org.intellij.erlang.compilation;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
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
public class ErlangParseTransformDependenciesTest extends ModuleTestCase {

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

  private void doSetUpSourcePath(String sourcePath, boolean isTestSource) throws IOException {
    String sourceDirectoryName = isTestSource ? "test" : "src";
    VirtualFile moduleFile = myModule.getModuleFile();
    assertNotNull(moduleFile);
    final VirtualFile moduleSourceDir = VfsUtil.createDirectories(moduleFile.getParent().getPath() + "/" + sourceDirectoryName);
    final VirtualFile originalSourceDir = LocalFileSystem.getInstance().refreshAndFindFileByPath(sourcePath);
    assertNotNull(originalSourceDir);
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        try {
          VfsUtil.copyDirectory(this, originalSourceDir, moduleSourceDir, null);
        } catch (IOException e) {
          throw new RuntimeException(e);
        }
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
    } catch (ErlangPrepareDependenciesCompileTask.CyclicDependencyFoundException expected) {
    }
  }

  public void testDependenciesAreCompiledFirst() throws Exception {
    ErlangModuleBuildOrderDescriptor moduleBuildOrder = ErlangPrepareDependenciesCompileTask.getModuleBuildOrder(myModule);
    assertSameErlangModules(moduleBuildOrder.myOrderedErlangModulePaths, "parse_transform1", "parse_transform2", "module1");
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
