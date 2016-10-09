/*
 * Copyright 2012-2014 Sergey Ignatov
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

package org.intellij.erlang.runconfig;

import com.intellij.execution.Location;
import com.intellij.execution.PsiLocation;
import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.actions.ConfigurationFromContext;
import com.intellij.execution.actions.RunConfigurationProducer;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.LangDataKeys;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.roots.ModifiableRootModel;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.testFramework.MapDataContext;
import com.intellij.testFramework.ModuleTestCase;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.util.ArrayUtil;
import org.intellij.erlang.application.ErlangApplicationRunConfigurationProducer;
import org.intellij.erlang.eunit.ErlangUnitRunConfigurationProducer;
import org.intellij.erlang.module.ErlangModuleType;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangModulesUtil;
import org.jetbrains.annotations.NotNull;

public class ErlangDebuggableRunConfigurationProducerTest extends ModuleTestCase {
  @Override
  protected ModuleType getModuleType() {
    return ErlangModuleType.getInstance();
  }

  @Override
  protected String getTestDirectoryName() {
    return "testData/runconfig/debuggableConfigurationProducer/";
  }


  public void testNoNifs() throws Exception {
    Module noNifs = createErlangModule("no_nifs");
    doTestDebugOptionsForSource(noNifs);
    doTestDebugOptionsForTests(noNifs);
  }

  public void testNifsInTests() throws Exception {
    Module nifsInTests = createErlangModule("nifs_in_tests");
    doTestDebugOptionsForSource(nifsInTests);
    doTestDebugOptionsForTests(nifsInTests, "nifs_in_tests_test");
  }

  public void testNifsInSource() throws Exception {
    Module nifsInSource = createErlangModule("nifs_in_source");
    doTestDebugOptionsForSource(nifsInSource, "nifs_in_source");
    doTestDebugOptionsForTests(nifsInSource, "nifs_in_source");
  }

  public void testNoNifsDependentOnNifsInTests() throws Exception {
    Module noNifs = createErlangModule("no_nifs");
    Module nifsInTests = createErlangModule("nifs_in_tests");
    addDependency(noNifs, nifsInTests);

    doTestDebugOptionsForSource(noNifs);
    doTestDebugOptionsForTests(noNifs, "nifs_in_tests_test");
  }

  public void testNoNifsDependentOnNifsInSource() throws Exception {
    Module noNifs = createErlangModule("no_nifs");
    Module nifsInSource = createErlangModule("nifs_in_source");
    addDependency(noNifs, nifsInSource);

    doTestDebugOptionsForSource(noNifs, "nifs_in_source");
    doTestDebugOptionsForTests(noNifs, "nifs_in_source");
  }

  public void testDependentNifsOfDependentModulesAreNotExcluded() throws Exception {
    Module noNifs = createErlangModule("no_nifs");
    Module nifsInSource = createErlangModule("nifs_in_source");
    Module nifsInTests = createErlangModule("nifs_in_tests");
    addDependency(nifsInSource, noNifs);
    addDependency(nifsInTests, noNifs);

    doTestDebugOptionsForSource(noNifs);
    doTestDebugOptionsForTests(noNifs);
  }


  private void doTestDebugOptionsForSource(Module module, String... expectedModulesNotToInterpret) {
    doTestDebugOptions(ErlangApplicationRunConfigurationProducer.class, module, false, expectedModulesNotToInterpret);
  }

  private void doTestDebugOptionsForTests(Module module, String... expectedModulesNotToInterpret) {
    doTestDebugOptions(ErlangUnitRunConfigurationProducer.class, module, true, expectedModulesNotToInterpret);
  }

  private void doTestDebugOptions(Class<? extends ErlangDebuggableRunConfigurationProducer> producerClass,
                                  Module module, boolean useTestSource, String... expectedModulesNotToInterpret) {
    PsiElement elementToProduceFor = getElementToProduceFor(module, useTestSource);
    MapDataContext dataContext = new MapDataContext();
    dataContext.put(CommonDataKeys.PROJECT, myProject);
    dataContext.put(LangDataKeys.MODULE, ModuleUtilCore.findModuleForPsiElement(elementToProduceFor));
    dataContext.put(CommonDataKeys.PSI_ELEMENT, elementToProduceFor);
    dataContext.put(Location.DATA_KEY, PsiLocation.fromPsiElement(elementToProduceFor));

    ConfigurationContext configurationContext = ConfigurationContext.getFromContext(dataContext);
    RunConfigurationProducer producer = RunConfigurationProducer.getInstance(producerClass);
    ConfigurationFromContext confFromCtx = producer.createConfigurationFromContext(configurationContext);
    assertNotNull(confFromCtx);

    RunConfiguration configuration = confFromCtx.getConfiguration();
    assertNotNull(configuration);
    assertInstanceOf(configuration, ErlangRunConfigurationBase.class);

    @SuppressWarnings("unchecked")
    ErlangRunConfigurationBase c = (ErlangRunConfigurationBase) configuration;
    ErlangRunConfigurationBase.ErlangDebugOptions debugOptions = c.getDebugOptions();
    assertTrue(debugOptions.isAutoUpdateModulesNotToInterpret());
    assertSameElements(debugOptions.getModulesNotToInterpret(), expectedModulesNotToInterpret);
  }

  // We're producing for function named the same as it's containing module.
  // The containing module has the same name as it's IntelliJ's module and _test suffix if we're producing in test source.
  private PsiElement getElementToProduceFor(Module module, boolean useTestSource) {
    String erlangModuleName = module.getName() + (useTestSource ? "_test" : "");
    GlobalSearchScope scope = GlobalSearchScope.moduleScope(module);
    ErlangFile erlangModule = ErlangModulesUtil.getErlangModuleFile(myProject, erlangModuleName, scope);
    ErlangFunction function = erlangModule != null ? erlangModule.getFunction(erlangModuleName, 0) : null;
    assertNotNull(function);
    return function;
  }

  @NotNull
  private Module createErlangModule(String moduleName) throws Exception {
    String dirInTestData = getTestDirectoryName() + moduleName;
    Module module = createModuleFromTestData(dirInTestData, moduleName, getModuleType(), false);
    ModuleRootManager rootManager = ModuleRootManager.getInstance(module);
    VirtualFile contentRoot = ArrayUtil.getFirstElement(rootManager.getContentRoots());
    assertNotNull(contentRoot);

    VirtualFile sourceRoot = contentRoot.findChild("src");
    if (sourceRoot != null) {
      PsiTestUtil.addSourceRoot(module, sourceRoot, false);
    }

    VirtualFile testRoot = contentRoot.findChild("test");
    if (testRoot != null) {
      PsiTestUtil.addSourceRoot(module, testRoot, true);
    }

    return module;
  }

  private static void addDependency(final Module to, final Module what) throws Exception {
    final Ref<Exception> ex = Ref.create();
    ApplicationManager.getApplication().runWriteAction(() -> {
      ModifiableRootModel modifiableModel = ModuleRootManager.getInstance(to).getModifiableModel();
      try {
        modifiableModel.addModuleOrderEntry(what);
        modifiableModel.commit();
      }
      catch (Exception e) {
        modifiableModel.dispose();
        ex.set(e);
      }
    });

    if (!ex.isNull()) {
      throw ex.get();
    }
  }
}
