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

package org.intellij.erlang.quickfixes;

import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.facet.ErlangFacet;
import org.intellij.erlang.inspection.ErlangUnresolvedIncludeInspection;
import org.intellij.erlang.inspection.ErlangUnresolvedIncludeLibInspection;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

public class ErlangFindIncludeQuickFixTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/quickfixes/find_include/" + getTestName(true) + "/";
  }

  @Override
  protected boolean isWriteActionRequired() {
    return false;
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    //noinspection unchecked
    myFixture.enableInspections(ErlangUnresolvedIncludeInspection.class, ErlangUnresolvedIncludeLibInspection.class);
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        ErlangFacet.createFacet(myFixture.getModule());
      }
    });
  }

  public void testSimple() throws Exception {
    doIncludeTest("inc", "test.erl", "inc/inc.hrl");
  }

  private void doIncludeTest(String expectedIncludePath, String ... files) throws Exception {
    myFixture.configureByFiles(files);
    runQuickFix();
    checkFacetContainsIncludePath(expectedIncludePath);
  }

  private void runQuickFix() {
    List<IntentionAction> availableIntentions = myFixture.filterAvailableIntentions("Find include");
    IntentionAction action = ContainerUtil.getFirstItem(availableIntentions);
    assertNotNull(action);
    myFixture.launchAction(action);
  }

  private void checkFacetContainsIncludePath(String expectedIncludePath) {
    ErlangFacet facet = ErlangFacet.getFacet(myFixture.getModule());
    assert facet != null;
    List<String> includePaths = facet.getConfiguration().getIncludePaths();
    assertEquals(1, includePaths.size());
    String includePath = includePaths.get(0);
    assertEquals("/src/" + expectedIncludePath, includePath);
  }
}
