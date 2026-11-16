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

package org.intellij.erlang.highlighting;

import org.intellij.erlang.inspection.ErlangSingleClauseCaseInspection;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangSingleClauseCaseInspectionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected void setUp() throws Exception {
    super.setUp();
    //noinspection unchecked
    myFixture.enableInspections(ErlangSingleClauseCaseInspection.class);
  }

  @Override
  protected String getTestDataPath() {
    return "testData/highlighting/singleClauseCase/";
  }

  public void testSingleClause()         { doTest(); }
  public void testGuardedSingleClause()  { doTest(); }
  public void testWildcardSingleClause() { doTest(); }
  public void testTwoClauses()           { doTest(); }
  public void testNested()               { doTest(); }

  private void doTest() {
    myFixture.configureByFile(getTestName(true) + ".erl");
    myFixture.checkHighlighting(true, false, true);
  }
}
