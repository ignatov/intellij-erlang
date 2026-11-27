/*
 * Copyright 2012-2026 Sergey Ignatov
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

package org.intellij.erlang.eunit;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Location;
import com.intellij.execution.PsiLocation;
import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.actions.ConfigurationFromContext;
import com.intellij.execution.actions.RunConfigurationProducer;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.PlatformCoreDataKeys;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.MapDataContext;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangModule;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class ErlangUnitQuotedNamesTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/eunit/quotedNames/";
  }

  public void testModuleNameThatNeedsQuotes() throws ExecutionException {
    ErlangFile file = (ErlangFile) myFixture.configureByFile("my-mod_tests.erl");
    assertEquals("['my-mod_tests']", getTestObjects(file));
  }

  public void testFunctionInModuleThatNeedsQuotes() throws ExecutionException {
    ErlangFile file = (ErlangFile) myFixture.configureByFile("my-mod_tests.erl");
    ErlangFunction function = file.getFunction("a_test", 0);
    assertNotNull(function);
    assertEquals("[{\"module 'my-mod_tests'\", [fun 'my-mod_tests':a_test/0]}]", getTestObjects(function.getNameIdentifier()));
  }

  public void testFunctionNameThatNeedsQuotes() throws ExecutionException {
    ErlangFile file = (ErlangFile) myFixture.configureByFile("my-mod_tests.erl");
    ErlangFunction function = file.getFunction("my-case_test", 0);
    assertNotNull(function);
    assertEquals("[{\"module 'my-mod_tests'\", [fun 'my-mod_tests':'my-case_test'/0]}]", getTestObjects(function.getNameIdentifier()));
  }

  public void testPlainNamesStayUnquoted() throws ExecutionException {
    assertEquals("[sample_tests, other_tests]", getTestObjects(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE,
                                                               "sample_tests", "other_tests"));
    assertEquals("[{\"module 'sample_tests'\", [fun sample_tests:a_test/0, {generator, fun sample_tests:b_test_/0}]}]",
                 getTestObjects(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION,
                                "sample_tests:a_test", "sample_tests:b_test_"));
  }

  public void testQuotedGeneratorFunction() throws ExecutionException {
    assertEquals("[{\"module 'my-mod_tests'\", [{generator, fun 'my-mod_tests':'my-gen_test_'/0}]}]",
                 getTestObjects(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION, "my-mod_tests:my-gen_test_"));
  }

  public void testNamesTypedWithQuotesAreNotQuotedAgain() throws ExecutionException {
    assertEquals("['my-mod_tests']", getTestObjects(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE, "'my-mod_tests'"));
    String functions = getTestObjects(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION, "'my-mod_tests':'my-case_test'");
    assertTrue(functions, functions.contains("[fun 'my-mod_tests':'my-case_test'/0]"));
  }

  public void testDebugEntryPointQuotesNames() throws ExecutionException {
    ErlangUnitRunConfiguration configuration = createConfiguration(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE, "my-mod_tests");
    String debugTests = new ErlangUnitRunningState(null, getModule(), configuration).getDebugEntryPoint().getArgsList().get(0);
    assertEquals("[{timeout, " + Integer.MAX_VALUE + ", ['my-mod_tests']}]", debugTests);
  }

  public void testStringInModulesFieldIsWrittenUnchanged() throws ExecutionException {
    assertEquals("[\".\", sample_tests]",
                 getTestObjects(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE, "\".\"", "sample_tests"));
  }

  public void testReservedWordModuleName() throws ExecutionException {
    assertEquals("['maybe', 'receive', 'div']",
                 getTestObjects(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE, "maybe", "receive", "div"));
    assertEquals("[{\"module 'maybe'\", [fun 'maybe':a_test/0]}]",
                 getTestObjects(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION, "maybe:a_test"));
  }

  public void testFileNameWithQuoteOrBackslash() throws ExecutionException {
    assertEquals("['it\\'s_tests', 'a\\\\b_tests']",
                 getTestObjects(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE, "it's_tests", "a\\b_tests"));
  }

  public void testLocationOfTestInModuleThatNeedsQuotes() {
    myFixture.configureByFile("my-mod_tests.erl");
    GlobalSearchScope scope = GlobalSearchScope.projectScope(getProject());
    assertInstanceOf(getOnlyLocation("my-mod_tests", scope), ErlangModule.class);
    assertEquals("a_test", ((ErlangFunction) getOnlyLocation("my-mod_tests:a_test:-1", scope)).getName());
    assertEquals("my-case_test", ((ErlangFunction) getOnlyLocation("my-mod_tests:my-case_test:-1", scope)).getName());
  }

  public void testGeneratedFunInModuleThatNeedsQuotes() {
    myFixture.configureByText("my-gen_tests.erl", "-module('my-gen_tests').\n\ngen_test_() ->\n  [fun() -> ok end].\n");
    GlobalSearchScope scope = GlobalSearchScope.projectScope(getProject());
    PsiElement location = getOnlyLocation("my-gen_tests:-gen_test_/0-fun-0-:4", scope);
    assertEquals("gen_test_", PsiTreeUtil.getParentOfType(location, ErlangFunction.class, false).getName());
    ErlangFunction generator =
      ErlangTestLocationProvider.findGeneratingFunction(getProject(), "eunit://my-gen_tests:-gen_test_/0-fun-0-:4");
    assertNotNull(generator);
    assertEquals("gen_test_", generator.getName());
  }

  private PsiElement getOnlyLocation(String path, GlobalSearchScope scope) {
    List<Location> locations =
      ErlangTestLocationProvider.INSTANCE.getLocation(ErlangUnitRunConfigurationType.PROTOCOL, path, getProject(), scope);
    assertSize(1, locations);
    return locations.get(0).getPsiElement();
  }

  private String getTestObjects(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind kind, String... names) throws ExecutionException {
    ErlangUnitRunConfiguration configuration = createConfiguration(kind, names);
    return new ErlangUnitRunningState(null, getModule(), configuration).getEntryPoint().getArgsList().get(0);
  }

  private ErlangUnitRunConfiguration createConfiguration(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind kind, String... names) {
    ErlangUnitRunConfiguration configuration =
      new ErlangUnitRunConfiguration(getProject(), "eunit", ErlangUnitRunConfigurationType.getInstance());
    configuration.getConfigData().setKind(kind);
    Set<String> nameSet = new LinkedHashSet<>(Arrays.asList(names));
    if (kind == ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE) {
      configuration.getConfigData().setModuleNames(nameSet);
    }
    else {
      configuration.getConfigData().setFunctionNames(nameSet);
    }
    return configuration;
  }

  private String getTestObjects(PsiElement element) throws ExecutionException {
    MapDataContext dataContext = new MapDataContext();
    dataContext.put(CommonDataKeys.PROJECT, getProject());
    dataContext.put(PlatformCoreDataKeys.MODULE, getModule());
    dataContext.put(CommonDataKeys.PSI_ELEMENT, element);
    dataContext.put(CommonDataKeys.VIRTUAL_FILE_ARRAY, new VirtualFile[]{element.getContainingFile().getVirtualFile()});
    dataContext.put(Location.DATA_KEY, PsiLocation.fromPsiElement(element));

    ConfigurationContext context = ConfigurationContext.getFromContext(dataContext, null);
    ConfigurationFromContext fromContext =
      RunConfigurationProducer.getInstance(ErlangUnitRunConfigurationProducer.class).createConfigurationFromContext(context);
    assertNotNull(fromContext);
    ErlangUnitRunConfiguration configuration = (ErlangUnitRunConfiguration) fromContext.getConfiguration();
    return new ErlangUnitRunningState(null, getModule(), configuration).getEntryPoint().getArgsList().get(0);
  }
}
