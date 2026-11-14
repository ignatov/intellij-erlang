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
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.PathMacroManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.*;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
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
      // The .iml serializer writes exclude folders sorted by url.
      List<String> excludeFolderUrls = new ArrayList<>(contentEntry.getExcludeFolderUrls());
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
