/*
 * Copyright 2012-2015 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.compilation;

import com.intellij.compiler.impl.ModuleCompileScope;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.compiler.CompileScope;
import com.intellij.openapi.compiler.CompilerMessage;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.impl.SdkConfigurationUtil;
import com.intellij.openapi.roots.CompilerModuleExtension;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.ThrowableComputable;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.CompilerTester;
import com.intellij.testFramework.PlatformTestCase;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.facet.ErlangFacet;
import org.intellij.erlang.jps.model.ErlangIncludeSourceRootType;
import org.intellij.erlang.module.ErlangModuleType;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.model.java.JavaSourceRootType;
import org.jetbrains.jps.model.module.JpsModuleSourceRootType;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;


public abstract class ErlangCompilationTestBase extends PlatformTestCase {
  public static final String SDK_PATH = "/usr/lib/erlang/";
  protected CompilationRunner myCompilationRunner;

  public ErlangCompilationTestBase() {
    assertTrue("Unsupported OS.", SystemInfo.isLinux);
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myCompilationRunner = new CompilationRunner(myModule);
    ApplicationManager.getApplication().runWriteAction(new ThrowableComputable<Object, Exception>() {
      @Nullable
      @Override
      public Object compute() throws Exception {
        Sdk sdk = SdkConfigurationUtil.createAndAddSDK(SDK_PATH, ErlangSdkType.getInstance());
        ProjectRootManager.getInstance(myProject).setProjectSdk(sdk);
        addSourceRoot(myModule, "src", false);
        addSourceRoot(myModule, "tests", true);
        ErlangFacet.createFacet(myModule);
        return null;
      }
    });
  }

  @Override
  protected ModuleType getModuleType() {
    return ErlangModuleType.getInstance();
  }

  @Override
  protected void tearDown() throws Exception {
    myCompilationRunner.tearDown();
    super.tearDown();
  }

  @Override
  protected boolean isRunInWriteAction() {
    return false;
  }

  protected Module createModuleInOwnDirectoryWithSourceAndTestRoot(final String moduleName) throws Exception {
    return ApplicationManager.getApplication().runWriteAction(new ThrowableComputable<Module, IOException>() {
      @Override
      public Module compute() throws IOException {
        Module module = createModuleInDirectory(moduleName);
        ErlangFacet.createFacet(module);
        addSourceRoot(module, "src", false);
        addSourceRoot(module, "tests", true);
        return module;
      }
    });
  }

  @NotNull
  private Module createModuleInDirectory(String moduleName) throws IOException {
    VirtualFile baseDir = VfsUtil.createDirectoryIfMissing(myProject.getBaseDir(), moduleName);
    File moduleFile = new File(FileUtil.toSystemDependentName(baseDir.getPath()), moduleName + ".iml");
    FileUtil.createIfDoesntExist(moduleFile);
    myFilesToDelete.add(moduleFile);
    VirtualFile virtualFile = LocalFileSystem.getInstance().refreshAndFindFileByIoFile(moduleFile);
    assertNotNull(virtualFile);
    return ModuleManager.getInstance(myProject).newModule(virtualFile.getPath(), ErlangModuleType.getInstance().getId());
  }

  private CompileScope createModulesCompileScope(Module[] modules) {
    return new ModuleCompileScope(myProject, modules, false);
  }

  protected static VirtualFile addTestFile(Module module, String relativePath, String content) throws IOException {
    return addFile(module, relativePath, content, true);
  }

  protected static VirtualFile addSourceFile(Module module, String relativePath, String content) throws IOException {
    return addFile(module, relativePath, content, false);
  }

  protected static VirtualFile addFile(final @NotNull Module module,
                                       final @NotNull String relativePath,
                                       final @NotNull String content,
                                       final boolean toTests) throws IOException {
    return ApplicationManager.getApplication().runWriteAction(new ThrowableComputable<VirtualFile, IOException>() {
      @Override
      public VirtualFile compute() throws IOException {
        JavaSourceRootType rootType = toTests ? JavaSourceRootType.TEST_SOURCE : JavaSourceRootType.SOURCE;
        List<VirtualFile> sourceRoots = ModuleRootManager.getInstance(module).getSourceRoots(rootType);
        VirtualFile sourceDir = ContainerUtil.getFirstItem(sourceRoots);
        assertNotNull(sourceDir);
        return addFile(sourceDir, relativePath, content);
      }
    });
  }
  protected static VirtualFile addFileToDirectory(final @NotNull VirtualFile sourceDir,
                                                  final @NotNull String relativePath,
                                                  final @NotNull String content) throws IOException {
    return ApplicationManager.getApplication().runWriteAction(new ThrowableComputable<VirtualFile, IOException>() {
      @Override
      public VirtualFile compute() throws IOException {
        return addFile(sourceDir, relativePath, content);
      }
    });
  }
  protected static VirtualFile addIncludeRoot(@NotNull final Module module,
                                              @NotNull final String sourceRootName) throws IOException {
    return ApplicationManager.getApplication().runWriteAction(new ThrowableComputable<VirtualFile, IOException>() {
      @Override
      public VirtualFile compute() throws IOException {
        return addSourceRoot(module, sourceRootName, ErlangIncludeSourceRootType.INSTANCE);
      }
    });

  }

  protected static void addGlobalParseTransform(final Module module, final Collection<String> parseTransform) {
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        ErlangFacet erlangFacet = ErlangFacet.getFacet(module);
        assertNotNull(erlangFacet);
        erlangFacet.getConfiguration().addParseTransforms(parseTransform);
      }
    });
  }

  protected static void assertSourcesCompiled(@NotNull Module module, boolean tests) {
    String[] sources = getSourceFiles(module, tests);
    assertContains(getOutputDirectory(module, tests), ContainerUtil.mapNotNull(sources, new Function<String, String>() {
      @Nullable
      @Override
      public String fun(String source) {
        return getExpectedOutputFileName(source);
      }
    }));
  }

  protected static void assertContains(@Nullable VirtualFile parentPath, List<String> fileNames) {
    assertNotNull(parentPath);
    List<String> actual = getChildrenNames(parentPath);
    assertUnorderedElementsAreEqual(actual, fileNames);
  }

  protected static <T> void assertUnorderedElementsAreEqual(Collection<T> actual, Collection<T> expected) {
    assertEquals(ContainerUtil.newHashSet(expected), ContainerUtil.newHashSet(actual));
  }

  @Nullable
  protected static File getOutputFile(Module module, VirtualFile sourceFile, boolean isTest) {
    VirtualFile outputDirectory = getOutputDirectory(module, isTest);
    assertNotNull(outputDirectory);
    String expectedOutputFileName = getExpectedOutputFileName(sourceFile.getName());
    return expectedOutputFileName == null ? null : new File(outputDirectory.getCanonicalPath(), expectedOutputFileName);
  }

  @NotNull
  private static VirtualFile addFile(VirtualFile sourceDir, String relativePath, String content) throws IOException {
    VirtualFile sourceFile = sourceDir.createChildData(ErlangCompilationTestBase.class, relativePath);
    VfsUtil.saveText(sourceFile, content);
    return sourceFile;
  }

  @NotNull
  private static VirtualFile addSourceRoot(@NotNull Module module,
                                           @NotNull String sourceRootName,
                                           @NotNull JpsModuleSourceRootType<?> rootType) throws IOException {
    VirtualFile moduleFile = module.getModuleFile();
    assertNotNull(moduleFile);
    PsiTestUtil.addContentRoot(module, moduleFile.getParent());
    VirtualFile[] contentRoots = ModuleRootManager.getInstance(module).getContentRoots();
    assertSize(1, contentRoots);

    VirtualFile sourceDir = contentRoots[0].createChildDirectory(ErlangCompilationTestBase.class, sourceRootName);
    PsiTestUtil.addSourceRoot(module, sourceDir, rootType);
    return sourceDir;
  }

  @NotNull
  private static VirtualFile addSourceRoot(@NotNull Module module,
                                           @NotNull String sourceRootName,
                                           boolean isTestSourceRoot) throws IOException {
    JavaSourceRootType rootType = isTestSourceRoot? JavaSourceRootType.TEST_SOURCE:JavaSourceRootType.SOURCE;
    return addSourceRoot(module, sourceRootName,rootType);
  }

  @NotNull
  private static String[] getSourceFiles(@NotNull Module module, boolean isTestSources) {
    List<VirtualFile> sourceRoots = ModuleRootManager.getInstance(module).getSourceRoots(JavaSourceRootType.SOURCE);
    if (isTestSources) {
      List<VirtualFile> testRoots = ModuleRootManager.getInstance(module).getSourceRoots(JavaSourceRootType.TEST_SOURCE);
      sourceRoots = ContainerUtil.concat(sourceRoots, testRoots);
    }
    List<String> result = ContainerUtil.concat(ContainerUtil.mapNotNull(sourceRoots, new Function<VirtualFile, List<String>>() {
      @Override
      public List<String> fun(VirtualFile root) {
        return getChildrenNames(root);
      }
    }));
    return ArrayUtil.toStringArray(result);
  }

  @Nullable
  private static VirtualFile getOutputDirectory(@NotNull Module module, boolean isTest) {
    CompilerModuleExtension instance = CompilerModuleExtension.getInstance(module);
    assertNotNull(instance);
    return isTest ? instance.getCompilerOutputPathForTests() : instance.getCompilerOutputPath();
  }

  @Nullable
  private static String getExpectedOutputFileName(@NotNull String relativePath) {
    File file = new File(relativePath);
    String name = FileUtil.getNameWithoutExtension(file);
    CharSequence extension = FileUtilRt.getExtension(relativePath);
    if ("erl".equals(extension)) {
      return name + ".beam";
    }
    if ("app".equals(extension) || "app.src".equals(extension)) {
      return name + ".app";
    }
    return null;
  }

  @NotNull
  private static List<String> getChildrenNames(VirtualFile root) {
    return ContainerUtil.mapNotNull(root.getChildren(), new Function<VirtualFile, String>() {
      @Override
      public String fun(VirtualFile virtualFile) {
        return virtualFile.getName();
      }
    });
  }

  protected class CompilationRunner {
    private CompileScope myScope;
    private CompilerTester myTester;

    CompilationRunner(@NotNull Module... moduleNames) throws Exception {
      this(createModulesCompileScope(moduleNames));
    }

    CompilationRunner(@NotNull CompileScope scope) throws Exception {
      myScope = scope;
      myTester = new CompilerTester(myProject, Arrays.asList(scope.getAffectedModules()));
    }

    public void compile() {
      List<CompilerMessage> messages = myTester.make(myScope);
      for (CompilerMessage message : messages) {
        switch (message.getCategory()) {
          case ERROR:
            fail(message.getMessage());
            break;
          case WARNING:
            LOG.warn(message.getMessage());
            break;
          case INFORMATION:
            LOG.info(message.getMessage());
            break;
          case STATISTICS:
            LOG.info(message.getMessage());
            break;
          default:
            throw new AssertionError();
        }
      }
    }

    public void touch(@NotNull VirtualFile file) throws IOException {
      myTester.touch(file);
    }

    public void tearDown() {
      myTester.tearDown();
    }
  }
}
