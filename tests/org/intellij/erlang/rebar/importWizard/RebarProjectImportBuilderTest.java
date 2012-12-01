/*
 * Copyright 2012 Sergey Ignatov
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

import com.intellij.ide.projectWizard.ProjectWizardTestCase;
import com.intellij.ide.util.projectWizard.ModuleWizardStep;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.PathMacros;
import com.intellij.openapi.components.PathMacroManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.impl.ProjectJdkImpl;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.impl.ModuleRootManagerImpl;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.util.Consumer;
import junit.framework.Assert;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jdom.Document;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;

public class RebarProjectImportBuilderTest extends ProjectWizardTestCase {
  private static final String MODULE_DIR = "MODULE_DIR";
  private static final String TEST_ROOT = "testData/rebar/import";

  private Project myCreatedProject;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    createMockSdk();
    final File currentTestRoot = new File(TEST_ROOT, getTestName(true));
    FileUtil.copyDir(currentTestRoot, new File(getProject().getBaseDir().getPath()));
  }

  @Override
  public void tearDown() throws Exception {
    if (myCreatedProject != null) {
      myProjectManager.closeProject(myCreatedProject);
      ApplicationManager.getApplication().runWriteAction(new Runnable() {
        @Override
        public void run() {
          Disposer.dispose(myCreatedProject);
        }
      });
    }
    super.tearDown();
  }

  public void test1_fromEbinAppFile() throws Exception {
    doTest(null);
  }

  public void test2_fromSrcAppSrcFile() throws Exception {
    doTest(null);
  }

  public void test3_contentIncludesAndExcludes() throws Exception {
    doTest(null);
  }

  public void test4_rebarSubDirs() throws Exception {
    doTest(null);
  }

  public void test5_missingSubDir() throws Exception {
    doTest(null);
  }

  public void test6_extraSubDir() throws Exception {
    doTest(null);
  }

  public void test7_multipleRebarConfigs() throws Exception {
    doTest(null);
  }

  public void test8_moduleNameConflict() throws Exception {
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

  private static void createMockSdk() {
    final ProjectJdkImpl mockSdk = new ProjectJdkImpl("Mock", ErlangSdkType.getInstance());
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        ProjectJdkTable.getInstance().addJdk(mockSdk);
      }
    });
  }

  private void doTest(@Nullable Consumer<ModuleWizardStep> adjuster) throws Exception {
    final String projectPath = getProject().getBaseDir().getPath();
    final String importFromPath = projectPath + "/test/";
    final Module firstModule = importProjectFrom(importFromPath, adjuster,
      new RebarProjectImportProvider(new RebarProjectImportBuilder()));
    final Project createdProject = firstModule.getProject();
    for (Module importedModule : ModuleManager.getInstance(createdProject).getModules()) {
      validateModule(importedModule);
    }
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
    final Element expectedIml = JDOMUtil.loadDocument(expectedImlFile).getRootElement();

    final String errorMsg = "Configuration of module " + module.getName() +
      " does not meet expectations\n" +
      new String(JDOMUtil.printDocument(new Document(actualImlElement), "\n"));
    Assert.assertTrue(errorMsg, JDOMUtil.areElementsEqual(expectedIml, actualImlElement));
  }
}
