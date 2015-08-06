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

package org.intellij.erlang.inspection;

import com.intellij.spellchecker.inspections.SpellCheckingInspection;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangSpellcheckingTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  public void setUp() throws Exception {
    super.setUp();
    myFixture.enableInspections(new SpellCheckingInspection());
  }

  public void testFunctionName() throws Exception { doTest(); }

  private void doTest() {
    myFixture.testHighlighting(false, false, true, getTestName(true) + ".erl");
  }

  @Override
  protected String getTestDataPath() {
    return "testData/spellchecker/";
  }

  @Override
  protected boolean isWriteActionRequired() {
    return false;
  }
}
