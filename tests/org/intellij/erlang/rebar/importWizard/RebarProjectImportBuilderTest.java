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

package org.intellij.erlang.rebar.importWizard;

import com.intellij.compiler.CompilerWorkspaceConfiguration;
import com.intellij.facet.FacetManager;
import com.intellij.ide.projectWizard.ProjectWizardTestCase;
import com.intellij.ide.util.projectWizard.ModuleWizardStep;
import com.intellij.ide.util.projectWizard.WizardContext;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ex.ApplicationInfoEx;
import com.intellij.openapi.components.PathMacroManager;
import com.intellij.openapi.module.ModifiableModuleModel;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.JavaSdk;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.*;
import com.intellij.openapi.roots.ui.configuration.ModulesProvider;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.ui.TestDialogManager;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileOperationsHandler;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.IdeaTestUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.configuration.ErlangCompilerSettings;
import org.intellij.erlang.console.ErlangConsoleUtil;
import org.intellij.erlang.facet.ErlangFacet;
import org.intellij.erlang.facet.ErlangFacetType;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.jps.model.ErlangIncludeSourceRootType;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jdom.Document;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.model.java.JavaSourceRootType;
import org.jetbrains.jps.model.module.JpsModuleSourceRootType;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;

public class RebarProjectImportBuilderTest extends ProjectWizardTestCase {
  private static final String TEST_DATA = "testData/";
  private static final String TEST_DATA_IMPORT = TEST_DATA + "rebar/import/";
  private static final String MOCK_SDK_DIR = TEST_DATA + "mockSdk-R15B02/";

  @Override
  public void setUp() throws Exception {
    super.setUp();
    createMockSdk();
    File currentTestRoot = new File(TEST_DATA_IMPORT, getTestName(true));
    String basePath = getProject().getBasePath();
    assertNotNull(basePath);
    if (currentTestRoot.isDirectory()) FileUtil.copyDir(currentTestRoot, new File(basePath));
  }

  @Override
  public void tearDown() throws Exception {
    ApplicationManager.getApplication().runWriteAction(() -> {
      ProjectJdkTable table = ProjectJdkTable.getInstance();
      List<Sdk> sdksOfType = table.getSdksOfType(ErlangSdkType.getInstance());
      for (Sdk sdk : sdksOfType) {
        table.removeJdk(sdk);
      }
    });
    super.tearDown();
  }

  public void testFromEbinAppFile() throws Exception { doTest(null); }
  public void testFromSrcAppSrcFile() throws Exception { doTest(null); }
  public void testContentIncludesAndExcludes() throws Exception { doTest(null); }
  public void testRebarSubDirs() throws Exception { doTest(null); }
  public void testMissingSubDir() throws Exception { doTest(null); }
  public void testExtraSubDir() throws Exception { doTest(null); }
  public void testMultipleRebarConfigs() throws Exception { doTest(null); }
  public void testDepsOnOtherApps() throws Exception { doTest(null); }
  public void testDepsOnSdkApps() throws Exception { doTest(null); }
  public void testDepsOnMissingApps() throws Exception { doTest(null); }

  public void testModuleNameConflict() throws Exception {
    doTest(moduleWizardStep -> {
      if (moduleWizardStep instanceof SelectImportedOtpAppsStep theStep) {
        theStep.autoResolveConflicts();
      }
    });
  }

  // disabled as we use rebar 3 as default right now
  public void _testEmbeddedRebar() throws Exception {
    Project createdProject = doTest(null);
    if (SystemInfo.isWindows) return;
    assertEquals(createdProject.getBasePath() + "/rebar", RebarSettings.getInstance(createdProject).getRebarPath());
  }

  public void testRebarlessDeps() throws Exception { doTest(null); }
  public void testRebarDependencies() throws Exception { doTest(null); }
  public void testRebarBareAtomDependency() throws Exception { doTest(null); }
  public void testRebarProfileDependency() throws Exception { doTest(null); }
  public void testRebarProfileTupleDependency() throws Exception { doTest(null); }
  public void testRebarDependencyAppProfiles() throws Exception { doTest(null); }

  public void testRebarTestProfileBuildDependency() throws Exception {
    Project createdProject = doTest(null);
    ModuleManager moduleManager = ModuleManager.getInstance(createdProject);
    Module a = moduleManager.findModuleByName("a");
    Module d = moduleManager.findModuleByName("d");
    assertNotNull(a);
    assertNotNull("The dependency fetched only into _build/test/lib is not imported.", d);
    assertEquals(2, moduleManager.getModules().length);
    assertSameElements(Arrays.asList(ModuleRootManager.getInstance(a).getDependencies()), d);
  }

  public void testTransitiveRebarDependencies() throws Exception { doTest(null); }
  public void testIncludePathsInRebarConfig1() throws Exception { doTest(null); }
  public void testIncludePathsInRebarConfig2() throws Exception { doTest(null); }

  public void testParseTransformInRebarConfig() throws Exception {
    Project project = doTest(null);
    Module[] modules = ModuleManager.getInstance(project).getModules();
    assertEquals(1, modules.length);
    ErlangFacet facet = ErlangFacet.getFacet(modules[0]);
    assertNotNull(facet);
    assertSameElements(facet.getConfiguration().getParseTransforms(), "lager_transform");
  }

  public void testRebar3AppCodePath() throws Exception {
    String projectPath = getProject().getBasePath();
    Module module = importProjectFrom(projectPath + "/test/", null, new RebarProjectImportProvider());
    String appRoot = ModuleRootManager.getInstance(module).getContentRoots()[0].getPath();
    // What `rebar3 compile` writes for the app, as the build or the debug session does after the import.
    File rebar3Ebin = new File(appRoot, "_build/default/lib/myapp/ebin");
    FileUtil.writeToFile(new File(rebar3Ebin, "myapp.app"), "{application, myapp, [{modules, [myapp_app, myapp_sup]}]}.");
    FileUtil.writeToFile(new File(rebar3Ebin, "myapp_app.beam"), "");
    FileUtil.writeToFile(new File(rebar3Ebin, "myapp_sup.beam"), "");
    assertNotNull(LocalFileSystem.getInstance().refreshAndFindFileByIoFile(rebar3Ebin));

    List<String> codePath = ErlangConsoleUtil.getCodePath(module, false);
    int index = codePath.indexOf(FileUtil.toSystemIndependentName(rebar3Ebin.getPath()));
    assertTrue("The rebar3 output directory is not on the code path: " + codePath, index > 0);
    assertEquals("-pa", codePath.get(index - 1));
  }

  public void testRebar3OutputNotOnCodePathWithoutRebarBuild() throws Exception {
    Module module = importRebar3App();
    ErlangCompilerSettings.getInstance(module.getProject()).setUseRebarCompilerEnabled(false);
    String appRoot = ModuleRootManager.getInstance(module).getContentRoots()[0].getPath();
    // Left over from `rebar3 compile` in a terminal; the IDE compiler writes the module output <app>/ebin.
    File rebar3Ebin = createRebar3Output(appRoot, "myapp");
    File moduleOutput = new File(appRoot, "ebin");
    FileUtil.writeToFile(new File(moduleOutput, "myapp_app.beam"), "");
    assertNotNull(LocalFileSystem.getInstance().refreshAndFindFileByIoFile(moduleOutput));
    assertNotNull(LocalFileSystem.getInstance().refreshAndFindFileByIoFile(rebar3Ebin));

    for (boolean forTests : new boolean[]{false, true}) {
      assertNotOnCodePath(module, forTests, rebar3Ebin);
    }
  }

  public void testRebar3BuildAboveContentRootNotOnCodePath() throws Exception {
    String projectPath = getProject().getBasePath();
    Module module = importProjectFrom(projectPath + "/test/", null, new RebarProjectImportProvider());
    assertEquals("foo", module.getName());
    // The Rebar build runs rebar3 in apps/foo, which writes into apps/foo/_build, so a _build above it is not its output.
    File rebar3Ebin = createRebar3Output(projectPath + "/test", "foo");
    assertNotNull(LocalFileSystem.getInstance().refreshAndFindFileByIoFile(rebar3Ebin));

    for (boolean forTests : new boolean[]{false, true}) {
      assertNotOnCodePath(module, forTests, rebar3Ebin);
    }
  }

  public void testRebar3OutputOfModuleNamedOtherThanApp() throws Exception {
    Module module = importRebar3App();
    ModifiableModuleModel moduleModel = ModuleManager.getInstance(module.getProject()).getModifiableModel();
    moduleModel.renameModule(module, "other");
    ApplicationManager.getApplication().runWriteAction(moduleModel::commit);
    assertEquals("other", module.getName());
    String appRoot = ModuleRootManager.getInstance(module).getContentRoots()[0].getPath();
    File rebar3Ebin = createRebar3Output(appRoot, "myapp");
    assertNotNull(LocalFileSystem.getInstance().refreshAndFindFileByIoFile(rebar3Ebin));

    assertOnCodePath(module, rebar3Ebin);
  }

  public void testRebar3OutputCreatedOutsideTheIde() throws Exception {
    Module module = importRebar3App();
    VirtualFile appRoot = ModuleRootManager.getInstance(module).getContentRoots()[0];
    VfsUtil.markDirtyAndRefresh(false, true, true, appRoot);
    appRoot.getChildren();
    // rebar3 creates _build during the build before the launch, and nothing refreshes the IDE's view of the files.
    File rebar3Ebin = createRebar3Output(appRoot.getPath(), "myapp");
    assertNull(appRoot.findChild("_build"));

    assertOnCodePath(module, rebar3Ebin);
  }

  @NotNull
  private Module importRebar3App() throws Exception {
    String projectPath = getProject().getBasePath();
    FileUtil.copyDir(new File(TEST_DATA_IMPORT, "rebar3AppCodePath"), new File(projectPath));
    return importProjectFrom(projectPath + "/test/", null, new RebarProjectImportProvider());
  }

  @NotNull
  private static File createRebar3Output(@NotNull String root, @NotNull String appName) throws IOException {
    File rebar3Ebin = new File(root, "_build/default/lib/" + appName + "/ebin");
    FileUtil.writeToFile(new File(rebar3Ebin, appName + ".app"), "{application, " + appName + ", []}.");
    return rebar3Ebin;
  }

  private static void assertOnCodePath(@NotNull Module module, @NotNull File dir) {
    List<String> codePath = ErlangConsoleUtil.getCodePath(module, false);
    int index = codePath.indexOf(FileUtil.toSystemIndependentName(dir.getPath()));
    assertTrue("The rebar3 output directory is not on the code path: " + codePath, index > 0);
    assertEquals("-pa", codePath.get(index - 1));
  }

  private static void assertNotOnCodePath(@NotNull Module module, boolean forTests, @NotNull File dir) {
    List<String> codePath = ErlangConsoleUtil.getCodePath(module, forTests);
    assertFalse("Unexpected directory on the code path: " + codePath, codePath.contains(FileUtil.toSystemIndependentName(dir.getPath())));
  }

  // File | Open of a directory with rebar.config: the platform's ProjectOpenProcessorBase calls doQuickImport,
  // then getBuilder().validate(...) and getBuilder().commit(project, null, EMPTY_MODULES_PROVIDER), without the wizard.
  public void testOpenProcessorQuickImport() throws Exception {
    String rebarPath = createRebarInProjectRoot();
    assertEquals("modules: [demo at test], rebar path: '" + rebarPath + "', compile with rebar: true",
                 doQuickImport(new RebarProjectOpenProcessor()));
  }

  // After rebar3 compile the app is also found in _build/default/lib; the project root copy must win.
  public void testOpenProcessorQuickImportCompiled() throws Exception {
    String rebarPath = createRebarInProjectRoot();
    assertEquals("modules: [demo at test], rebar path: '" + rebarPath + "', compile with rebar: true",
                 doQuickImport(new RebarProjectOpenProcessor()));
  }

  // A release under _build/<profile>/rel holds OTP's own apps, copied or, in a dev mode release, linked into the OTP
  // installation; none of them becomes a module.
  public void testOpenProcessorQuickImportRelease() throws Exception {
    String rebarPath = createRebarInProjectRoot();
    if (!SystemInfo.isWindows) {
      File otpApp = FileUtil.createTempDirectory("crypto-1.0", null);
      FileUtil.writeToFile(new File(otpApp, "ebin/crypto.app"), "{application, crypto, [{vsn, \"1.0\"}, {modules, []}]}.\n");
      Files.createSymbolicLink(Paths.get(getProject().getBasePath(), "test/_build/prod/rel/demo/lib/crypto-1.0"), otpApp.toPath());
    }
    assertEquals("modules: [demo at test], rebar path: '" + rebarPath + "', compile with rebar: true",
                 doQuickImport(new RebarProjectOpenProcessor()));
  }

  // The app directory name is much longer than the app name; the app outside _build must still win.
  public void testOpenProcessorQuickImportUmbrella() throws Exception {
    String rebarPath = createRebarInProjectRoot();
    assertEquals("modules: [umb at test/apps/umb_application_core_service], rebar path: '" + rebarPath + "', compile with rebar: true",
                 doQuickImport(new RebarProjectOpenProcessor()));
  }

  // An app in _checkouts linked to a checkout outside the project, as rebar3 users set _checkouts up.
  public void testOpenProcessorQuickImportSymlinkedCheckout() throws Exception {
    if (SystemInfo.isWindows) return;
    String rebarPath = createRebarInProjectRoot();
    File checkout = FileUtil.createTempDirectory("mylib", null);
    FileUtil.writeToFile(new File(checkout, "src/mylib.app.src"), "{application, mylib, [{vsn, \"1.0\"}, {applications, [kernel, stdlib]}]}.\n");
    Files.createDirectories(Paths.get(getProject().getBasePath(), "test/_checkouts"));
    Files.createSymbolicLink(Paths.get(getProject().getBasePath(), "test/_checkouts/mylib"), checkout.toPath());
    assertEquals("modules: [demo at test, mylib at test/_checkouts/mylib], rebar path: '" + rebarPath + "', compile with rebar: true",
                 doQuickImport(new RebarProjectOpenProcessor()));
  }

  public void testOpenProcessorQuickImportWithoutRebar() throws Exception {
    RebarProjectOpenProcessor processor = new RebarProjectOpenProcessor() {
      @NotNull
      @Override
      protected String findRebarPath(@NotNull VirtualFile projectRoot) {
        return "";
      }
    };
    assertEquals("modules: [demo at test], rebar path: '', compile with rebar: false", doQuickImport(processor));
  }

  public void testBuilderPresentation() {
    RebarProjectImportBuilder builder = new RebarProjectImportBuilder();
    assertEquals("Rebar", builder.getName());
    assertSame(ErlangIcons.REBAR, builder.getIcon());
    assertTrue(builder.isSuitableSdkType(ErlangSdkType.getInstance()));
    assertFalse(builder.isSuitableSdkType(JavaSdk.getInstance()));
  }

  // Apps with the same parent directory are listed by name.
  public void testBuilderSetProjectRootAgainKeepsFoundApps() throws Exception {
    RebarProjectImportBuilder builder = new RebarProjectImportBuilder();
    VirtualFile projectRoot = copyBuilderFixture("builderApps");
    assertTrue(builder.setProjectRoot(projectRoot));
    assertEquals(Arrays.asList("a", "b"), appNames(builder.getList()));

    FileUtil.writeToFile(new File(projectRoot.getPath(), "apps/c/src/c.app.src"), "{application, c, []}.\n");
    VfsUtil.markDirtyAndRefresh(false, true, true, projectRoot);
    assertNotNull(projectRoot.findFileByRelativePath("apps/c/src/c.app.src"));
    assertTrue(builder.setProjectRoot(projectRoot));
    assertEquals(Arrays.asList("a", "b"), appNames(builder.getList()));
  }

  public void testBuilderSelection() throws Exception {
    RebarProjectImportBuilder builder = new RebarProjectImportBuilder();
    assertTrue(builder.setProjectRoot(copyBuilderFixture("builderApps")));
    List<ImportedOtpApp> apps = builder.getList();
    ImportedOtpApp a = apps.get(0);
    ImportedOtpApp b = apps.get(1);
    assertTrue(builder.isMarked(a));
    assertTrue(builder.isMarked(b));
    assertFalse(builder.isMarked(null));

    builder.setList(Collections.singletonList(a));
    assertTrue(builder.isMarked(a));
    assertFalse(builder.isMarked(b));

    builder.setList(null);
    assertTrue(builder.isMarked(a));
    assertFalse(builder.isMarked(b));
    assertEquals(Arrays.asList("a", "b"), appNames(builder.getList()));
  }

  // The examples directory is scanned only on request, the rel directory of the project root never,
  // and a _build/test directory without lib holds no apps.
  public void testBuilderScanSkipsExamplesAndRel() throws Exception {
    VirtualFile projectRoot = copyBuilderFixture("builderScan");
    RebarProjectImportBuilder builder = new RebarProjectImportBuilder();
    assertTrue(builder.setProjectRoot(projectRoot));
    assertEquals(Collections.singletonList("main"), appNames(builder.getList()));

    RebarProjectImportBuilder withExamples = new RebarProjectImportBuilder();
    withExamples.setImportExamples(true);
    assertTrue(withExamples.setProjectRoot(projectRoot));
    assertEquals(Arrays.asList("main", "ex"), appNames(withExamples.getList()));
  }

  // _build/test/lib may hold files next to the app directories; they are not apps.
  public void testBuilderScanIgnoresFilesInTestProfileLib() throws Exception {
    String projectPath = getProject().getBasePath();
    FileUtil.copyDir(new File(TEST_DATA_IMPORT, "builderScan"), new File(projectPath));
    FileUtil.writeToFile(new File(projectPath, "test/_build/test/lib/c.app.src"), "{application, c, []}.\n");
    FileUtil.writeToFile(new File(projectPath, "test/_build/test/lib/d/src/d.app.src"), "{application, d, []}.\n");
    VirtualFile projectRoot = LocalFileSystem.getInstance().refreshAndFindFileByPath(projectPath + "/test");
    assertNotNull(projectRoot);
    RebarProjectImportBuilder builder = new RebarProjectImportBuilder();
    assertTrue(builder.setProjectRoot(projectRoot));
    assertEquals(Arrays.asList("main", "d"), appNames(builder.getList()));
  }

  public void testBuilderValidateReusesModuleFiles() throws Exception {
    VirtualFile projectRoot = copyBuilderFixture("builderApps");
    VirtualFile imlFile = createModuleFile(projectRoot, "apps/a/a.iml");
    RebarProjectImportBuilder builder = new RebarProjectImportBuilder();
    assertTrue(builder.setProjectRoot(projectRoot));
    List<String> messages = answerModuleFilesFound(Messages.YES);

    assertTrue(builder.validate(null, getProject()));
    assertEquals(Collections.singletonList(moduleFilesFoundMessage(imlFile)), messages);
    assertTrue(imlFile.isValid());
    ImportedOtpApp a = builder.getList().get(0);
    assertEquals(imlFile, a.getIdeaModuleFile());
    assertNull(builder.getList().get(1).getIdeaModuleFile());
  }

  public void testBuilderValidateFindsEmlFiles() throws Exception {
    VirtualFile projectRoot = copyBuilderFixture("builderApps");
    VirtualFile emlFile = createModuleFile(projectRoot, "apps/b/b.eml");
    RebarProjectImportBuilder builder = new RebarProjectImportBuilder();
    assertTrue(builder.setProjectRoot(projectRoot));
    List<String> messages = answerModuleFilesFound(Messages.YES);

    assertTrue(builder.validate(null, getProject()));
    assertEquals(Collections.singletonList(moduleFilesFoundMessage(emlFile)), messages);
    assertEquals(emlFile, builder.getList().get(1).getIdeaModuleFile());
  }

  public void testBuilderValidateDeletesModuleFiles() throws Exception {
    VirtualFile projectRoot = copyBuilderFixture("builderApps");
    VirtualFile imlFile = createModuleFile(projectRoot, "apps/a/a.iml");
    File imlIoFile = new File(imlFile.getPath());
    RebarProjectImportBuilder builder = new RebarProjectImportBuilder();
    assertTrue(builder.setProjectRoot(projectRoot));
    List<String> messages = answerModuleFilesFound(Messages.NO);

    assertTrue(builder.validate(null, getProject()));
    assertEquals(1, messages.size());
    assertFalse(imlIoFile.exists());
    assertNull(builder.getList().get(0).getIdeaModuleFile());
  }

  public void testBuilderValidateCancelled() throws Exception {
    VirtualFile projectRoot = copyBuilderFixture("builderApps");
    VirtualFile imlFile = createModuleFile(projectRoot, "apps/a/a.iml");
    RebarProjectImportBuilder builder = new RebarProjectImportBuilder();
    assertTrue(builder.setProjectRoot(projectRoot));
    List<String> messages = answerModuleFilesFound(Messages.CANCEL);

    assertFalse(builder.validate(null, getProject()));
    assertEquals(1, messages.size());
    assertTrue(new File(imlFile.getPath()).exists());
    assertEquals(imlFile, builder.getList().get(0).getIdeaModuleFile());
  }

  // An Erlang SDK set as the project SDK stays, though a newer Erlang SDK exists.
  public void testBuilderCommitKeepsErlangProjectSdk() throws Exception {
    Sdk r15 = ProjectJdkTable.getInstance().getSdksOfType(ErlangSdkType.getInstance()).get(0);
    Sdk r16 = ErlangSdkType.createMockSdk(TEST_DATA + "mockSdk-R16B/", ErlangSdkRelease.V_R16B);
    ApplicationManager.getApplication().runWriteAction(() -> {
      ProjectJdkTable.getInstance().addJdk(r16);
      ProjectRootManager.getInstance(getProject()).setProjectSdk(r15);
    });
    assertSame(r16, ProjectJdkTable.getInstance().findMostRecentSdkOfType(ErlangSdkType.getInstance()));

    RebarProjectImportBuilder builder = new RebarProjectImportBuilder();
    assertTrue(builder.setProjectRoot(copyBuilderFixture("builderApps")));
    ModifiableModuleModel moduleModel = ModuleManager.getInstance(getProject()).getModifiableModel();
    List<Module> modules = builder.commit(getProject(), moduleModel, ModulesProvider.EMPTY_MODULES_PROVIDER);

    assertSame(r15, ProjectRootManager.getInstance(getProject()).getProjectSdk());
    assertEquals(Arrays.asList("a", "b"), ContainerUtil.map(modules, Module::getName));
    for (Module module : modules) {
      assertSame(module, ModuleManager.getInstance(getProject()).findModuleByName(module.getName()));
    }
  }

  public void testBuilderSetProjectRootWithoutApps() throws Exception {
    RebarProjectImportBuilder builder = new RebarProjectImportBuilder();
    assertFalse(builder.setProjectRoot(copyBuilderFixture("builderNoApps")));
    assertEmpty(builder.getList());
  }

  // A project SDK that is not an Erlang SDK is replaced with the most recent Erlang SDK.
  public void testBuilderCommitReplacesNonErlangProjectSdk() throws Exception {
    Sdk erlangSdk = ProjectJdkTable.getInstance().getSdksOfType(ErlangSdkType.getInstance()).get(0);
    Sdk javaSdk = IdeaTestUtil.getMockJdk17();
    ApplicationManager.getApplication().runWriteAction(() -> {
      ProjectJdkTable.getInstance().addJdk(javaSdk, getTestRootDisposable());
      ProjectRootManager.getInstance(getProject()).setProjectSdk(javaSdk);
    });
    assertSame(javaSdk, ProjectRootManager.getInstance(getProject()).getProjectSdk());

    RebarProjectImportBuilder builder = new RebarProjectImportBuilder();
    assertTrue(builder.setProjectRoot(copyBuilderFixture("builderApps")));
    builder.commit(getProject(), null, ModulesProvider.EMPTY_MODULES_PROVIDER);

    assertSame(erlangSdk, ProjectRootManager.getInstance(getProject()).getProjectSdk());
  }

  // The file system refuses to delete the module file, the same way on every OS.
  public void testBuilderValidateReportsModuleFileNotDeleted() throws Exception {
    VirtualFile projectRoot = copyBuilderFixture("builderApps");
    VirtualFile imlFile = createModuleFile(projectRoot, "apps/a/a.iml");
    RebarProjectImportBuilder builder = new RebarProjectImportBuilder();
    assertTrue(builder.setProjectRoot(projectRoot));
    answerModuleFilesFound(Messages.NO);

    LocalFileOperationsHandler refuseDelete = new LocalFileOperationsHandler() {
      @Override
      public boolean delete(@NotNull VirtualFile file) throws IOException {
        throw new IOException("Cannot delete " + file.getPath());
      }

      @Override
      public boolean move(@NotNull VirtualFile file, @NotNull VirtualFile toDir) {
        return false;
      }

      @Override
      public boolean rename(@NotNull VirtualFile file, @NotNull String newName) {
        return false;
      }

      @Override
      public boolean createFile(@NotNull VirtualFile dir, @NotNull String name) {
        return false;
      }

      @Override
      public boolean createDirectory(@NotNull VirtualFile dir, @NotNull String name) {
        return false;
      }
    };
    AssertionError loggedError = null;
    LocalFileSystem.getInstance().registerAuxiliaryFileOperationsHandler(refuseDelete);
    try {
      builder.validate(null, getProject());
    }
    catch (AssertionError e) {
      loggedError = e;
    }
    finally {
      LocalFileSystem.getInstance().unregisterAuxiliaryFileOperationsHandler(refuseDelete);
    }
    assertNotNull("The failed deletion is not reported.", loggedError);
    assertInstanceOf(loggedError.getCause(), IOException.class);
    assertTrue(new File(imlFile.getPath()).exists());
    assertEquals(imlFile, builder.getList().get(0).getIdeaModuleFile());
  }

  @NotNull
  private VirtualFile copyBuilderFixture(@NotNull String fixture) throws IOException {
    String projectPath = getProject().getBasePath();
    FileUtil.copyDir(new File(TEST_DATA_IMPORT, fixture), new File(projectPath));
    VirtualFile projectRoot = LocalFileSystem.getInstance().refreshAndFindFileByPath(projectPath + "/test");
    assertNotNull(projectRoot);
    return projectRoot;
  }

  @NotNull
  private static VirtualFile createModuleFile(@NotNull VirtualFile projectRoot, @NotNull String relativePath) throws IOException {
    File moduleFile = new File(projectRoot.getPath(), relativePath);
    FileUtil.writeToFile(moduleFile, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<module type=\"ERLANG_MODULE\" version=\"4\" />\n");
    VirtualFile file = LocalFileSystem.getInstance().refreshAndFindFileByIoFile(moduleFile);
    assertNotNull(file);
    return file;
  }

  @NotNull
  private List<String> answerModuleFilesFound(int answer) {
    List<String> messages = new ArrayList<>();
    TestDialogManager.setTestDialog(message -> {
      messages.add(message);
      return answer;
    }, getTestRootDisposable());
    return messages;
  }

  @NotNull
  private static String moduleFilesFoundMessage(@NotNull VirtualFile moduleFile) {
    return ApplicationInfoEx.getInstanceEx().getFullApplicationName() + " module files found:\n\n" +
           "    " + moduleFile.getPath() + "\n" +
           "\nWould you like to reuse them?";
  }

  @NotNull
  private static List<String> appNames(@NotNull List<ImportedOtpApp> apps) {
    return ContainerUtil.map(apps, ImportedOtpApp::getName);
  }

  @NotNull
  private String createRebarInProjectRoot() throws Exception {
    File rebar = new File(getProject().getBasePath() + "/test", "rebar3");
    FileUtil.writeToFile(rebar, "#!/bin/sh\n");
    assertTrue(rebar.setExecutable(true));
    return rebar.getPath();
  }

  @NotNull
  private String doQuickImport(@NotNull RebarProjectOpenProcessor processor) {
    Project project = getProject();
    VirtualFile projectRoot = LocalFileSystem.getInstance().refreshAndFindFileByPath(project.getBasePath() + "/test");
    assertNotNull(projectRoot);
    VirtualFile rebarConfig = projectRoot.findChild("rebar.config");
    assertNotNull(rebarConfig);

    WizardContext wizardContext = new WizardContext(null, getTestRootDisposable());
    assertTrue(processor.doQuickImport(rebarConfig, wizardContext));
    assertTrue(processor.getBuilder().validate(null, project));
    processor.getBuilder().commit(project, null, ModulesProvider.EMPTY_MODULES_PROVIDER);

    List<String> importedModules = new ArrayList<>();
    for (Module module : ModuleManager.getInstance(project).getModules()) {
      if (module == getModule()) continue;
      VirtualFile[] contentRoots = ModuleRootManager.getInstance(module).getContentRoots();
      assertEquals(1, contentRoots.length);
      importedModules.add(module.getName() + " at " + VfsUtilCore.getRelativePath(contentRoots[0], projectRoot.getParent()));
    }
    return "modules: " + importedModules +
           ", rebar path: '" + RebarSettings.getInstance(project).getRebarPath() + "'" +
           ", compile with rebar: " + ErlangCompilerSettings.getInstance(project).isUseRebarCompilerEnabled();
  }

  private static void createMockSdk() {
    final Sdk mockSdk = ErlangSdkType.createMockSdk(MOCK_SDK_DIR, ErlangSdkRelease.V_R15B02);
    ApplicationManager.getApplication().runWriteAction(() -> ProjectJdkTable.getInstance().addJdk(mockSdk));
  }

  private Project doTest(@Nullable Consumer<ModuleWizardStep> adjuster) throws Exception {
    String projectPath = getProject().getBasePath();
    String importFromPath = projectPath + "/test/";
    Module firstModule = importProjectFrom(importFromPath, adjuster, new RebarProjectImportProvider());
    Project createdProject = firstModule.getProject();
    validateProject(createdProject);
    for (Module importedModule : ModuleManager.getInstance(createdProject).getModules()) {
      validateModule(importedModule);
    }
    return createdProject;
  }

  private static void validateProject(@NotNull Project project) {
    ErlangCompilerSettings compilerSettings = ErlangCompilerSettings.getInstance(project);
    assertNotNull("Erlang compiler settings are not created.", compilerSettings);
    assertTrue("Rebar compiler is not set as default compiler.", compilerSettings.isUseRebarCompilerEnabled());
    assertFalse("Clear output directory flag was not unset.", CompilerWorkspaceConfiguration.getInstance(project).CLEAR_OUTPUT_DIRECTORY);
  }

  private void validateModule(@NotNull Module module) throws Exception {
    Element actualImlElement = serializeModuleRoots(module);
    PathMacroManager.getInstance(module).collapsePaths(actualImlElement);

    String projectPath = getProject().getBasePath();
    String expectedImlPath = "/expected/" + module.getName() + ".iml";
    File expectedImlFile = new File(projectPath + expectedImlPath);
    Document expectedIml = JDOMUtil.loadDocument(expectedImlFile);
    Element expectedImlElement = expectedIml.getRootElement();

    Document document = new Document(actualImlElement);
    String actualString = JDOMUtil.writeDocument(document, "\n").trim();
    String errorMsg = "Configuration of module " + module.getName() +
                      " does not meet expectations.\nExpected:\n" +
                      JDOMUtil.writeDocument(expectedIml, "\n") +
                      "\nBut got:\n" +
                      actualString;

    if (OVERWRITE_TESTDATA) {
      FileUtil.writeToFile(new File(TEST_DATA_IMPORT, getTestName(true) + expectedImlPath), actualString);
    }

    assertTrue(errorMsg, JDOMUtil.areElementsEqual(expectedImlElement, actualImlElement));
    validateFacet(module);
  }

  /**
   * Writes the module roots in the format of the module's root manager component in a .iml file,
   * which the expected test data uses. The root manager no longer exposes its serialized state,
   * so the element is built from the public root model API.
   */
  @NotNull
  private static Element serializeModuleRoots(@NotNull Module module) {
    ModuleRootManager rootManager = ModuleRootManager.getInstance(module);
    Element root = new Element("root");

    CompilerModuleExtension compilerExtension = rootManager.getModuleExtension(CompilerModuleExtension.class);
    if (compilerExtension != null) {
      if (compilerExtension.isCompilerOutputPathInherited()) {
        root.setAttribute("inherit-compiler-output", "true");
      }
      addUrlElement(root, "output", compilerExtension.getCompilerOutputUrl());
      addUrlElement(root, "output-test", compilerExtension.getCompilerOutputUrlForTests());
      if (compilerExtension.isExcludeOutput()) {
        root.addContent(new Element("exclude-output"));
      }
    }

    for (ContentEntry contentEntry : rootManager.getContentEntries()) {
      Element content = addUrlElement(root, "content", contentEntry.getUrl());
      for (SourceFolder sourceFolder : contentEntry.getSourceFolders()) {
        Element sourceFolderElement = addUrlElement(content, "sourceFolder", sourceFolder.getUrl());
        JpsModuleSourceRootType<?> rootType = sourceFolder.getRootType();
        if (rootType instanceof JavaSourceRootType) {
          sourceFolderElement.setAttribute("isTestSource", String.valueOf(sourceFolder.isTestSource()));
          if (!sourceFolder.getPackagePrefix().isEmpty()) {
            sourceFolderElement.setAttribute("packagePrefix", sourceFolder.getPackagePrefix());
          }
        }
        else if (rootType == ErlangIncludeSourceRootType.INSTANCE) {
          sourceFolderElement.setAttribute("type", "erlang-include");
        }
        else {
          fail("Unexpected source root type " + rootType + " of " + sourceFolder.getUrl());
        }
      }
      // The .iml serializer writes exclude folders sorted by url. Only the folders excluded explicitly: in 2026.1
      // getExcludeFolderUrls() also returns the output directories that <exclude-output /> excludes.
      List<String> excludeFolderUrls = new ArrayList<>();
      for (ExcludeFolder excludeFolder : contentEntry.getExcludeFolders()) {
        excludeFolderUrls.add(excludeFolder.getUrl());
      }
      Collections.sort(excludeFolderUrls);
      for (String excludeFolderUrl : excludeFolderUrls) {
        addUrlElement(content, "excludeFolder", excludeFolderUrl);
      }
    }

    for (OrderEntry orderEntry : rootManager.getOrderEntries()) {
      Element entry = new Element("orderEntry");
      if (orderEntry instanceof InheritedJdkOrderEntry) {
        entry.setAttribute("type", "inheritedJdk");
      }
      else if (orderEntry instanceof ModuleSourceOrderEntry) {
        entry.setAttribute("type", "sourceFolder");
        entry.setAttribute("forTests", "false");
      }
      else if (orderEntry instanceof ModuleOrderEntry moduleEntry) {
        entry.setAttribute("type", "module");
        entry.setAttribute("module-name", moduleEntry.getModuleName());
        if (moduleEntry.isExported()) entry.setAttribute("exported", "");
        if (moduleEntry.getScope() != DependencyScope.COMPILE) entry.setAttribute("scope", moduleEntry.getScope().name());
      }
      else {
        fail("Unexpected order entry " + orderEntry.getPresentableName() + " (" + orderEntry.getClass().getName() + ")");
      }
      root.addContent(entry);
    }
    return root;
  }

  @NotNull
  private static Element addUrlElement(@NotNull Element parent, @NotNull String name, @Nullable String url) {
    Element element = new Element(name);
    if (url != null) {
      element.setAttribute("url", url);
      parent.addContent(element);
    }
    return element;
  }

  private static void validateFacet(@NotNull Module module) {
    FacetManager facetManager = FacetManager.getInstance(module);
    ErlangFacet facet = facetManager.getFacetByType(ErlangFacetType.TYPE_ID);
    assertNotNull("Erlang facet was not added.", facet);
  }
}
