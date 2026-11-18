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
import com.intellij.openapi.components.PathMacroManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.*;
import com.intellij.openapi.roots.ui.configuration.ModulesProvider;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import org.intellij.erlang.configuration.ErlangCompilerSettings;
import org.intellij.erlang.facet.ErlangFacet;
import org.intellij.erlang.facet.ErlangFacetType;
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
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
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
    FileUtil.copyDir(currentTestRoot, new File(basePath));
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

  // #516: a rebar that is neither in the project nor on PATH was entered once and saved in the project's rebar.xml;
  // importing the project again must keep that path instead of asking for it again.
  public void testOpenProcessorQuickImportKeepsSavedRebarPath() throws Exception {
    File rebar = new File(FileUtil.createTempDirectory("rebar_bin", null), "rebar3");
    FileUtil.writeToFile(rebar, "#!/bin/sh\n");
    assertTrue(rebar.setExecutable(true));
    RebarSettings.getInstance(getProject()).setRebarPath(rebar.getPath());
    RebarProjectOpenProcessor processor = new RebarProjectOpenProcessor() {
      @NotNull
      @Override
      protected String findRebarPath(@NotNull VirtualFile projectRoot) {
        return ""; // no rebar in the project directory and none on PATH
      }
    };
    assertEquals("modules: [demo at test], rebar path: '" + rebar.getPath() + "', compile with rebar: true",
                 doQuickImport(processor));
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
