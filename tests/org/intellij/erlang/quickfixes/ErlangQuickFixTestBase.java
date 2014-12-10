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
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

public abstract class ErlangQuickFixTestBase extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected boolean isWriteActionRequired() {
    return false;
  }

  protected void doTest(String quickFixName) {
    String testName = getTestName(true);
    myFixture.configureByFile(testName + ".erl");
    List<IntentionAction> availableIntentions = myFixture.filterAvailableIntentions(quickFixName);
    IntentionAction action = ContainerUtil.getFirstItem(availableIntentions);
    assertNotNull(action);
    myFixture.launchAction(action);
    String after = String.format("%s-after.erl", testName);
    myFixture.checkResultByFile(after);
  }

  protected void assertNoIntention(String prefix) {
    String testName = getTestName(true);
    myFixture.configureByFile(testName + ".erl");
    List<IntentionAction> availableIntentions = myFixture.filterAvailableIntentions(prefix);
    IntentionAction action = ContainerUtil.getFirstItem(availableIntentions);
    assertNull(action);
  }
}
