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
import com.intellij.testFramework.MapDataContext;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

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
