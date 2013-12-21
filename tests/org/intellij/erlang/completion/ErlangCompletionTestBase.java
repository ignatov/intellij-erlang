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

package org.intellij.erlang.completion;

import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.psi.impl.source.tree.injected.InjectedLanguageManagerImpl;
import com.intellij.testFramework.UsefulTestCase;
import com.intellij.testFramework.fixtures.IdeaProjectTestFixture;
import com.intellij.testFramework.fixtures.IdeaTestFixtureFactory;
import com.intellij.testFramework.fixtures.TestFixtureBuilder;
import com.intellij.testFramework.fixtures.impl.TempDirTestFixtureImpl;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

abstract public class ErlangCompletionTestBase extends ErlangLightPlatformCodeInsightFixtureTestCase {
  protected ErlangCompletionTestBase() {
  }

  protected ErlangCompletionTestBase(String platformPrefix) {
    super(platformPrefix);
  }

  protected String getTestDataPath() {
    return "testData/completion/";
  }

  protected void localFileSystemSetUp() throws Exception {
    IdeaTestFixtureFactory factory = IdeaTestFixtureFactory.getFixtureFactory();
    TestFixtureBuilder<IdeaProjectTestFixture> fixtureBuilder = factory.createLightFixtureBuilder(getProjectDescriptor());

    final IdeaProjectTestFixture fixture = fixtureBuilder.getFixture();
    myFixture = IdeaTestFixtureFactory.getFixtureFactory().createCodeInsightFixture(fixture, new TempDirTestFixtureImpl());

    InjectedLanguageManagerImpl.checkInjectorsAreDisposed(getProject());
    myFixture.setUp();
    myFixture.setTestDataPath(getTestDataPath());
    myModule = myFixture.getModule();
  }

  protected void doCheckResult(@NotNull String before, @NotNull String after) { doCheckResult(before, after, null); }

  protected void doCheckResult(@NotNull String before, @NotNull String after, @Nullable Character c) {
    myFixture.configureByText("a.erl", before);
    myFixture.completeBasic();
    if (c != null) myFixture.type(c);
    myFixture.checkResult(after);
  }

  protected void doTestInclude(String txt, String... variants) throws Throwable {
    doTestVariants(txt, CompletionType.BASIC, 1, CheckType.INCLUDES, variants);
  }

  protected void doTestEquals(String txt, String... variants) throws Throwable {
    doTestVariants(txt, CompletionType.BASIC, 1, CheckType.EQUALS, variants);
  }

  protected void doTestVariants(String txt, CompletionType type, int count, CheckType checkType, String... variants) throws Throwable {
    myFixture.configureByText("a.erl", txt);
    doTestVariantsInner(type, count, checkType, variants);
  }

  protected void doTestVariantsInner(CompletionType type, int count, CheckType checkType, String... variants) throws Throwable {
    myFixture.complete(type, count);
    List<String> stringList = myFixture.getLookupElementStrings();

    assertNotNull(
      "\nPossibly the single variant has been completed.\n" +
      "File after:\n" +
      myFixture.getFile().getText(),
      stringList);
    Collection<String> varList = new ArrayList<String>(Arrays.asList(variants));
    if (checkType == CheckType.EQUALS) {
      UsefulTestCase.assertSameElements(stringList, variants);
    }
    else if (checkType == CheckType.INCLUDES) {
      varList.removeAll(stringList);
      assertTrue("Missing variants: " + varList, varList.isEmpty());
    }
    else if (checkType == CheckType.EXCLUDES) {
      varList.retainAll(stringList);
      assertTrue("Unexpected variants: " + varList, varList.isEmpty());
    }
  }

  protected void doSmartTest(String text, CheckType type, String... variants) throws Throwable { doTestVariants(text, CompletionType.SMART, 1, type, variants); }

  protected enum CheckType { EQUALS, INCLUDES, EXCLUDES }
}
