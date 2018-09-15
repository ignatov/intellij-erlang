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

package org.intellij.erlang.quickfixes;

import org.intellij.erlang.inspection.ErlangUnresolvedRecordInspection;

public class ErlangIntroduceRecordFixTest extends ErlangQuickFixTestBase {

  @SuppressWarnings("unchecked")
  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
    myFixture.enableInspections(ErlangUnresolvedRecordInspection.class);
  }

  @Override
  protected String getTestDataPath() {
    return "testData/quickfixes/introduce_record/";
  }

  private void doIntroduceRecordTest()       { doTest("Introduce new record"); }

  public void testSimple()                   { doIntroduceRecordTest(); }
  public void testWithPrecedingFunctions()   { doIntroduceRecordTest(); }
  public void testWithPrecedingRecords()     { doIntroduceRecordTest(); }
  public void testSurroundedByDeclarations() { doIntroduceRecordTest(); }
  public void testWithFields()               { doIntroduceRecordTest(); }
  public void testAloneMacro()               { doIntroduceRecordTest(); }
}
