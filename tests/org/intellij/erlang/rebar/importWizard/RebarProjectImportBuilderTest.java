/*
 * Copyright 2012-2013 Sergey Ignatov
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
import com.intellij.openapi.application.PathMacros;
import com.intellij.openapi.components.PathMacroManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.impl.ModuleRootManagerImpl;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.Consumer;
import junit.framework.Assert;
import org.intellij.erlang.configuration.ErlangCompilerSettings;
import org.intellij.erlang.facet.ErlangFacet;
import org.intellij.erlang.facet.ErlangFacetType;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jdom.Document;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class RebarProjectImportBuilderTest extends ProjectWizardTestCase {
  private static final String MODULE_DIR = "MODULE_DIR";
  private static final String TEST_DATA = "testData/";
  private static final String TEST_DATA_IMPORT = TEST_DATA + "rebar/import/";
  private static final String MOCK_SDK_DIR = TEST_DATA + "mockSdk-R15B02/";

  @Override
  public void setUp() throws Exception {
    super.setUp();
    createMockSdk();
    final File currentTestRoot = new File(TEST_DATA_IMPORT, getTestName(true));
    FileUtil.copyDir(currentTestRoot, new File(getProject().getBaseDir().getPath()));
  }

  @Override
  public void tearDown() throws Exception {
    super.tearDown();
  }

  public void testFromEbinAppFile() throws Exception {
    doTest(null);
  }

  public void testFromSrcAppSrcFile() throws Exception {
    doTest(null);
  }

  public void testContentIncludesAndExcludes() throws Exception {
    doTest(null);
  }

  public void testRebarSubDirs() throws Exception {
    doTest(null);
  }

  public void testMissingSubDir() throws Exception {
    doTest(null);
  }

  public void testExtraSubDir() throws Exception {
    doTest(null);
  }

  public void testMultipleRebarConfigs() throws Exception {
    doTest(null);
  }

  public void testDepsOnOtherApps() throws Exception {
    doTest(null);
  }

  public void testDepsOnSdkApps() throws Exception {
    doTest(null);
  }

  public void testDepsOnMissingApps() throws Exception {
    doTest(null);
  }

  public void testModuleNameConflict() throws Exception {
    doTest(new Consumer<ModuleWizardStep>() {
      @Override
      public void consume(@NotNull ModuleWizardStep moduleWizardStep) {
        if (moduleWizardStep instanceof SelectImportedOtpAppsStep) {
          final SelectImportedOtpAppsStep theStep = (SelectImportedOtpAppsStep) moduleWizardStep;
          theStep.autoResolveConflicts();
        }
      }
    });
  }

  public void testEmbeddedRebar() throws Exception {
    final Project createdProject = doTest(null);
    if (SystemInfo.isWindows) return;
    assertEquals(createdProject.getBasePath() + "/rebar", RebarSettings.getInstance(createdProject).getRebarPath());
  }

  public void testRebarlessDeps() throws Exception {
    doTest(null);
  }

  private static void createMockSdk() {
    final Sdk mockSdk = ErlangSdkType.createMockSdk(MOCK_SDK_DIR);
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        ProjectJdkTable.getInstance().addJdk(mockSdk);
      }
    });
  }

  private Project doTest(@Nullable Consumer<ModuleWizardStep> adjuster) throws Exception {
    final String projectPath = getProject().getBaseDir().getPath();
    final String importFromPath = projectPath + "/test/";
    final Module firstModule = importProjectFrom(importFromPath, adjuster,
      new RebarProjectImportProvider(new RebarProjectImportBuilder()));
    final Project createdProject = firstModule.getProject();
    validateProject(createdProject);
    for (Module importedModule : ModuleManager.getInstance(createdProject).getModules()) {
      validateModule(importedModule);
    }
    return createdProject;
  }

  private static void validateProject(@NotNull Project project) throws Exception {
    ErlangCompilerSettings compilerSettings = ErlangCompilerSettings.getInstance(project);
    assertNotNull("Erlang compiler settings are not created.", compilerSettings);
    assertTrue("Rebar compiler is not set as default compiler.", compilerSettings.isUseRebarCompilerEnabled());
    assertFalse("Clear output directory flag was not unset.", CompilerWorkspaceConfiguration.getInstance(project).CLEAR_OUTPUT_DIRECTORY);
  }

  private void validateModule(@NotNull Module module) throws Exception {
    final String importedModulePath = getProject().getBaseDir().getPath();

    final Element actualImlElement = new Element("root");
    ((ModuleRootManagerImpl)ModuleRootManager.getInstance(module)).getState().writeExternal(actualImlElement);
    PathMacros.getInstance().setMacro(MODULE_DIR, importedModulePath);
    PathMacroManager.getInstance(module).collapsePaths(actualImlElement);
    PathMacroManager.getInstance(getProject()).collapsePaths(actualImlElement);
    PathMacros.getInstance().removeMacro(MODULE_DIR);

    final String projectPath = getProject().getBaseDir().getPath();
    final File expectedImlFile = new File(projectPath + "/expected/" + module.getName() + ".iml");
    final Document expectedIml = JDOMUtil.loadDocument(expectedImlFile);
    final Element expectedImlElement = expectedIml.getRootElement();

    final String errorMsg = "Configuration of module " + module.getName() +
      " does not meet expectations.\nExpected:\n" +
      new String(JDOMUtil.printDocument(expectedIml, "\n")) +
      "\nBut got:\n" +
      new String(JDOMUtil.printDocument(new Document(actualImlElement), "\n"));
    Assert.assertTrue(errorMsg, JDOMUtil.areElementsEqual(expectedImlElement, actualImlElement));
    validateFacet(module);
  }

  private static void validateFacet(@NotNull Module module) throws Exception {
    FacetManager facetManager = FacetManager.getInstance(module);
    ErlangFacet facet = facetManager.getFacetByType(ErlangFacetType.TYPE_ID);
    assertNotNull("Erlang facet was not added.", facet);
    List<String> actualIncludePaths = facet.getConfiguration().getIncludePaths();
    List<String> expectedIncludePaths = new ArrayList<String>();
    for (VirtualFile contentRoot : ModuleRootManager.getInstance(module).getContentRoots()) {
      VirtualFile includeDirectory = VfsUtil.findRelativeFile(contentRoot, "include");
      if (includeDirectory != null) {
        expectedIncludePaths.add(includeDirectory.getPath());
      }
    }
    assertSameElements(actualIncludePaths, expectedIncludePaths);
  }
}
